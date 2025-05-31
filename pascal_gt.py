import sys
import re

import ply.yacc as yacc
from pascal_lex import tokens

parser = None
parser_success = True
parser_functions = {}
parser_params = {}
parser_var = {}
# Dicionário global para tipos e endereços de variáveis
parser_var_count = 0
parser_var_types = {}

precedence = (
    ('nonassoc', 'ELSE'),
    ('nonassoc', 'IFX'),
)
start = 'program'

# Regra principal: estrutura de um programa Pascal
def p_program(p):
    """program : PROGRAM ID SEMI declarations functions BEGIN statements END DOT
               | PROGRAM ID SEMI declarations BEGIN statements END DOT"""
    if len(p) == 9:
        # Sem funções
        p[0] = "\nstart\n" + p[6] + "\nstop"
    else:
        # Com funções
        p[0] = "\n".join(parser_functions.values()) + "\nstart\n" + p[7] + "\nstop"

# Regra para declarações de variáveis ou ausência delas
def p_declarations(p):
    """declarations : VAR var_declaration_list
                    | empty"""
    p[0] = p[2] if len(p) > 2 else ""

def p_var_declaration_list(p):
    """var_declaration_list : var_declaration_list var_declaration
                            | var_declaration"""
    p[0] = p[1] + p[2] if len(p) == 3 else p[1]

# Regista variáveis e os seus tipos, verifica duplicações
def p_var_declaration(p):
    """var_declaration : id_list COLON type SEMI"""
    global parser_var, parser_var_count, parser_success, parser_var_types
    for var in p[1]:
        if var in parser_var:
            print(f"Erro: variável duplicada {var}")
            parser_success = False
        else:
            parser_var[var] = parser_var_count
            parser_var_types[var] = p[3].lower()
            parser_var_count += 1
    p[0] = ""

def p_id_list(p):
    """id_list : ID
              | ID COMMA id_list"""
    p[0] = [p[1]] if len(p) == 2 else [p[1]] + p[3]

def p_type(p):
    """type : INTEGER
            | BOOLEAN
            | STRING
            | REAL"""
    p[0] = p[1]

def p_functions(p):
    """functions : function functions
                 | empty"""
    p[0] = ""

# Definição de função com parâmetros e corpo
def p_function(p):
    """function : FUNCTION ID LPAREN param_list RPAREN COLON type SEMI declarations BEGIN statements END SEMI"""
    name = p[2]
    param_code = p[4]
    local_code = p[8]
    body_code = p[10]
    full_code = f"{name}:{param_code}{local_code}{body_code}RETURN\n"
    parser_functions[name] = full_code
    parser_params[name] = param_code.count("STOREL")
    p[0] = ""

# Parâmetro único de função
def p_param_list_single(p):
    "param_list : ID COLON type"
    p[0] = "STOREL 0\n"

# Múltiplos parâmetros de função
def p_param_list_multiple(p):
    "param_list : param_list SEMI ID COLON type"
    idx = p[1].count("STOREL")
    p[0] = p[1] + f"STOREL {idx}\n"

# Argumento único passado à função
def p_argument_list_single(p):
    "argument_list : expression"
    p[0] = p[1][0] if isinstance(p[1], tuple) else p[1]

# Múltiplos argumentos passados à função
def p_argument_list_multiple(p):
    "argument_list : argument_list COMMA expression"
    left = p[1] if isinstance(p[1], str) else p[1][0]
    right = p[3] if isinstance(p[3], str) else p[3][0]
    p[0] = left + right

# Chamada a função
def p_expression_function_call(p):
    "expression : ID LPAREN argument_list RPAREN"
    fname = p[1]
    args = p[3] if isinstance(p[3], str) else p[3][0]
    p[0] = (args + f"PUSHA {fname}\nCALL\n", "integer")

def p_statement_list(p):
    """statement_list : statement
                      | statement SEMI statement_list
                      | SEMI statement_list  # Allows empty statements like S1;;S2
                      | empty"""
    if len(p) == 2: # single statement or empty
        p[0] = [p[1]]
    elif p[1] == ';': # SEMI statement_list (first part is a semicolon)
        p[0] = p[2] # Skip the semicolon's code, keep the rest of the list
    else: # statement SEMI statement_list
        p[0] = [p[1]] + p[3]

def p_write_statement(p):
    """write_statement : WRITE LPAREN writelist RPAREN SEMI"""
    p[0] = p[3]

def p_statement(p):
    """statement : assignment_statement
                 | writeln_statement
                 | write_statement
                 | readln_statement
                 | for_statement
                 | if_statement
                 | while_statement 
                 | statement_compound """
    p[0] = p[1] if p[1] != ';' else ""

def p_assignment_statement(p):
    """assignment_statement : ID ASSIGN expression SEMI"""
    global parser_success, parser_var_types
    if p[1] not in parser_var:
        print(f"Erro: variável não declarada {p[1]}")
        parser_success = False
        p[0] = ""
    else:
        var_type = parser_var_types.get(p[1], "integer")
        expr_code, expr_type = p[3] if isinstance(p[3], tuple) else (p[3], var_type)
        if var_type == "real" and expr_type == "integer":
            expr_code += "ITOF\n"
        elif var_type == "integer" and expr_type == "real":
            expr_code += "FTOI\n"
        p[0] = expr_code + f"storeg {parser_var[p[1]]}\n"

def p_writeln_statement(p):
    """writeln_statement : WRITELN LPAREN writelist RPAREN SEMI"""
    p[0] = p[3] + "WRITELN\n" # Added WRITELN instruction

def p_writelist(p):
    """writelist : writelist COMMA writeitem
                 | writeitem"""
    p[0] = p[1] + p[3] if len(p) == 4 else p[1]

def p_writeitem_string(p):
    """writeitem : STRING_LITERAL"""
    p[0] = f'pushs "{p[1]}"\nwrites\n'

def p_writeitem_expr(p):
    """writeitem : expression"""
    code = p[1][0] if isinstance(p[1], tuple) else p[1]
    p[0] = code + "writei\n"

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
        init_code, _ = p[4] if isinstance(p[4], tuple) else (p[4], "integer")
        limit_code, _ = p[6] if isinstance(p[6], tuple) else (p[6], "integer")
        body_code = p[8]
        loop_label = f"forloop{index}"
        end_label = f"forend{index}"
        p[0] = (
            init_code +
            f"storeg {index}\n" +
            limit_code +
            f"storeg {limit_index}\n" +
            f"{loop_label}:\n" +
            f"pushg {index}\n" +
            f"pushg {limit_index}\n" +
            "infeq\n" +
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
    p[0] = (f"pushi {1 if p[1].lower() == 'true' else 0}\n", "integer")

def p_expression_logical(p):
    """expression : expression AND expression
                  | expression OR expression"""
    op = {
        'and': 'mul',
        'or': 'add'
    }[p[2].lower()]
    left_code, _ = p[1] if isinstance(p[1], tuple) else (p[1], "integer")
    right_code, _ = p[3] if isinstance(p[3], tuple) else (p[3], "integer")
    p[0] = (left_code + right_code + op + "\n", "integer")

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
    left_code, _ = p[1] if isinstance(p[1], tuple) else (p[1], "integer")
    right_code, _ = p[3] if isinstance(p[3], tuple) else (p[3], "integer")
    p[0] = (left_code + right_code + op_map[p[2]] + "\n", "integer")

def p_expression_paren(p):
    """expression : LPAREN expression RPAREN"""
    p[0] = p[2]

def p_expression_div(p):
    """expression : expression DIV expression"""
    left_code, _ = p[1] if isinstance(p[1], tuple) else (p[1], "integer")
    right_code, _ = p[3] if isinstance(p[3], tuple) else (p[3], "integer")
    p[0] = (left_code + right_code + "div\n", "integer")

def p_expression_mod(p):
    """expression : expression MOD expression"""
    left_code, _ = p[1] if isinstance(p[1], tuple) else (p[1], "integer")
    right_code, _ = p[3] if isinstance(p[3], tuple) else (p[3], "integer")
    p[0] = (left_code + right_code + "mod\n", "integer")

def p_statement_compound(p):
    """statement_compound : BEGIN statements END"""
    p[0] = p[2]

def p_if_statement(p):
    """if_statement : IF expression THEN statement %prec IFX
                    | IF expression THEN statement ELSE statement"""
    global parser_var_count
    idx = parser_var_count
    parser_var_count += 1
    cond_code, _ = p[2] if isinstance(p[2], tuple) else (p[2], "integer")
    if len(p) == 5:  # IF ... THEN ...
        label_end = f"ifend_{idx}"
        p[0] = (
            cond_code +
            f"jz {label_end}\n" +
            p[4] +
            f"{label_end}:\n"
        )
    else:  # IF ... THEN ... ELSE ...
        label_else = f"ifelse_{idx}"
        label_end = f"ifend_{idx}"
        p[0] = (
            cond_code +
            f"jz {label_else}\n" +
            p[4] +
            f"jump {label_end}\n" +
            f"{label_else}:\n" +
            p[6] +
            f"{label_end}:\n"
        )

def p_while_statement(p):
    """while_statement : WHILE expression DO statement"""
    global parser_var_count
    loop_index = parser_var_count
    parser_var_count += 1
    cond_code, _ = p[2] if isinstance(p[2], tuple) else (p[2], "integer")
    end_label = f"while_end_{loop_index}"
    start_label = f"while_start_{loop_index}"
    body = p[4]
    p[0] = (
        f"{start_label}:\n" +
        cond_code +
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
    left_code, left_type = p[1] if isinstance(p[1], tuple) else (p[1], "integer")
    right_code, right_type = p[3] if isinstance(p[3], tuple) else (p[3], "integer")
    result_type = "real" if left_type == "real" or right_type == "real" else "integer"
    p[0] = (left_code + right_code + op + "\n", result_type)

def p_expression_variable(p):
    """expression : ID"""
    global parser_success, parser_var_types
    if p[1] not in parser_var:
        print(f"Erro: variável não declarada {p[1]}")
        parser_success = False
        p[0] = ("", "integer")
    else:
        var_type = parser_var_types.get(p[1], "integer")
        p[0] = (f"pushg {parser_var[p[1]]}\n", var_type)

def p_expression_number(p):
    "expression : NUMBER"
    p[0] = ("pushi " + str(p[1]) + "\n", "integer")

def p_expression_real(p):
    "expression : REAL"
    p[0] = ("pushf " + str(p[1]) + "\n", "real")

# Produção vazia (útil em alternativas opcionais)
def p_empty(p):
    'empty :'
    p[0] = ""

def p_error(p):
    if p:
        print(f"Erro de sintaxe: {p.type}({p.value}) na linha {p.lineno}")
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