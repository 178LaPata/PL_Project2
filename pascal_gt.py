import sys
import re

import ply.yacc as yacc
from pascal_lex import tokens

parser = None
parser_success = True
parser_var = {}
parser_var_count = 0

precedence = (
    ('nonassoc', 'IFX'),
    ('nonassoc', 'ELSE'),
)

def p_program(p):
    """program : PROGRAM ID SEMI declarations BEGIN statements END DOT"""
    p[0] = p[4] + "\nstart\n" + p[6] + "\nstop"


def p_declarations(p):
    """declarations : VAR var_declaration_list
                    | empty"""
    p[0] = p[2] if len(p) > 2 else ""


def p_var_declaration_list(p):
    """var_declaration_list : var_declaration_list var_declaration
                            | var_declaration"""
    p[0] = p[1] + p[2] if len(p) == 3 else p[1]


def p_var_declaration(p):
    """var_declaration : id_list COLON type SEMI"""
    global parser_var, parser_var_count, parser_success
    for var in p[1]:
        if var in parser_var:
            print(f"Erro: variável duplicada {var}")
            parser_success = False
        else:
            parser_var[var] = parser_var_count
            parser_var_count += 1
    p[0] = ""


def p_id_list(p):
    """id_list : ID
              | ID COMMA id_list"""
    p[0] = [p[1]] if len(p) == 2 else [p[1]] + p[3]


def p_type(p):
    """type : INTEGER
            | BOOLEAN
            | STRING"""
    p[0] = p[1]


def p_statements(p):
    """statements : statements statement
                  | statement"""
    p[0] = p[1] + p[2] if len(p) == 3 else p[1]


def p_statement(p):
    """statement : assignment_statement
                 | writeln_statement
                 | readln_statement
                 | for_statement
                 | SEMI"""
    p[0] = p[1] if p[1] != ';' else ""


def p_assignment_statement(p):
    """assignment_statement : ID ASSIGN expression SEMI"""
    global parser_success
    if p[1] not in parser_var:
        print(f"Erro: variável não declarada {p[1]}")
        parser_success = False
        p[0] = ""
    else:
        p[0] = p[3] + f"storeg {parser_var[p[1]]}\n"


def p_writeln_statement(p):
    """writeln_statement : WRITELN LPAREN writelist RPAREN SEMI"""
    p[0] = p[3]


def p_writelist(p):
    """writelist : writelist COMMA writeitem
                 | writeitem"""
    p[0] = p[1] + p[3] if len(p) == 4 else p[1]


def p_writeitem_string(p):
    """writeitem : STRING_LITERAL"""
    p[0] = f'pushs "{p[1]}"\nwrites\n'


def p_writeitem_expr(p):
    """writeitem : expression"""
    p[0] = p[1] + "writei\n"


def p_readln_statement(p):
    """readln_statement : READLN LPAREN ID RPAREN SEMI"""
    global parser_success
    if p[3] not in parser_var:
        print(f"Erro: variável não declarada {p[3]}")
        parser_success = False
        p[0] = ""
    else:
        p[0] = "read\natoi\n" + f"storeg {parser_var[p[3]]}\n"


def p_for_statement(p):
    """for_statement : FOR ID ASSIGN expression TO expression DO statement"""
    global parser_success, parser_var_count
    var = p[2]
    if var not in parser_var:
        print(f"Erro: variável não declarada {var}")
        parser_success = False
        p[0] = ""
    else:
        index = parser_var[var]
        limit_index = parser_var_count
        parser_var_count += 1
        loop_label = f"forloop{index}"
        end_label = f"forend{index}"
        init_code = p[4]
        limit_code = p[6]
        body_code = p[8]
        p[0] = (
            init_code +
            f"storeg {index}\n" +
            limit_code +
            f"storeg {limit_index}\n" +
            f"{loop_label}:\n" +
            f"pushg {index}\n" +
            f"pushg {limit_index}\n" +
            "inf\n" +
            f"jz {end_label}\n" +
            body_code +
            f"pushg {index}\n" +
            "pushi 1\n" +
            "add\n" +
            f"storeg {index}\n" +
            f"jump {loop_label}\n" +
            f"{end_label}:\n"
        )

def p_expression_boolean(p):
    """expression : TRUE
                  | FALSE"""
    p[0] = f"pushi {1 if p[1].lower() == 'true' else 0}\n"

def p_expression_logical(p):
    """expression : expression AND expression
                  | expression OR expression"""
    op = {
        'and': 'mul',
        'or': 'add'
    }[p[2].lower()]
    p[0] = p[1] + p[3] + op + "\n"

def p_expression_relop(p):
    """expression : expression LT expression
                  | expression LE expression
                  | expression GT expression
                  | expression GE expression
                  | expression EQ expression
                  | expression NEQ expression"""
    op_map = {
        '<': 'inf',
        '<=': 'infeq',
        '>': 'sup',
        '>=': 'supeq',
        '=': 'equal',
        '<>': 'nequal'
    }
    p[0] = p[1] + p[3] + op_map[p[2]] + "\n"

def p_expression_paren(p):
    """expression : LPAREN expression RPAREN"""
    p[0] = p[2]

def p_expression_div(p):
    """expression : expression DIV expression"""
    p[0] = p[1] + p[3] + "div\n"

def p_expression_mod(p):
    """expression : expression MOD expression"""
    p[0] = p[1] + p[3] + "mod\n"

def p_statement_compound(p):
    """statement : BEGIN statements END"""
    p[0] = p[2]

def p_if_statement(p):
    """statement : IF expression THEN statement %prec IFX
                 | IF expression THEN statement ELSE statement"""
    global parser_var_count
    idx = parser_var_count
    parser_var_count += 1
    if len(p) == 5:  # IF ... THEN ...
        label_end = f"ifend_{idx}"
        p[0] = (
            p[2] +  # Avalia a expressão
            f"jz {label_end}\n" +  # Salta para o final se falso
            p[4] +  # Executa o bloco do THEN
            f"{label_end}:\n"  # Rótulo do final
        )
    else:  # IF ... THEN ... ELSE ...
        label_else = f"ifelse_{idx}"
        label_end = f"ifend_{idx}"
        p[0] = (
            p[2] +  # Avalia a expressão
            f"jz {label_else}\n" +  # Salta para o ELSE se falso
            p[4] +  # Executa o bloco do THEN
            f"jump {label_end}\n" +  # Salta para o final
            f"{label_else}:\n" +  # Rótulo do ELSE
            p[6] +  # Executa o bloco do ELSE
            f"{label_end}:\n"  # Rótulo do final
        )

def p_while_statement(p):
    """statement : WHILE expression DO statement"""
    global parser_var_count
    loop_index = parser_var_count
    parser_var_count += 1
    end_label = f"while_end_{loop_index}"
    start_label = f"while_start_{loop_index}"
    condition = p[2]
    body = p[4]
    p[0] = (
        f"{start_label}:\n" +
        condition +
        f"jz {end_label}\n" +
        body +
        f"jump {start_label}\n" +
        f"{end_label}:\n"
    )

def p_expression_binop(p):
    """expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression"""
    op = {
        '+': 'add',
        '-': 'sub',
        '*': 'mul',
        '/': 'div'
    }[p[2]]
    p[0] = p[1] + p[3] + op + "\n"


def p_expression_number(p):
    """expression : NUMBER"""
    p[0] = f"pushi {p[1]}\n"


def p_expression_variable(p):
    """expression : ID"""
    global parser_success
    if p[1] not in parser_var:
        print(f"Erro: variável não declarada {p[1]}")
        parser_success = False
        p[0] = ""
    else:
        p[0] = f"pushg {parser_var[p[1]]}\n"


def p_empty(p):
    'empty :'
    p[0] = ""


def p_error(p):
    if p:
        print(f"Erro de sintaxe: {p}")
    else:
        print("Erro de sintaxe: EOF inesperado")
    global parser_success
    parser_success = False


# Inicializar parser DEPOIS das regras
parser = yacc.yacc(debug=True)

# Leitura de código Pascal
with open('input.txt', 'r') as file:
    source = file.read()

codigo = parser.parse(source)

with open('output.txt', 'w') as f:
    if parser_success:
        print('Parsing completado com sucesso!')
        for _ in range(parser_var_count):
            f.write("pushi 0\n")
        f.write(codigo)
    else:
        print('Parsing falhou!')
