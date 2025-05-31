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

# Define precedence and associativity for operators
# Ordered from lowest precedence to highest precedence
precedence = (
    # Dangling else resolution: ELSE has higher precedence than IFX
    ('nonassoc', 'ELSE'),
    ('nonassoc', 'IFX'), # Used for IF-THEN without ELSE

    # Logical operators (OR is lowest, then AND)
    ('left', 'OR'),
    ('left', 'AND'),

    # Relational operators (non-associative)
    ('nonassoc', 'EQ', 'NEQ', 'LT', 'LE', 'GT', 'GE'),

    # Additive operators
    ('left', 'PLUS', 'MINUS'),

    # Multiplicative operators
    ('left', 'TIMES', 'DIVIDE', 'DIV', 'MOD'),

    # Unary NOT (highest precedence of all operators)
    ('right', 'NOT'), # Assign precedence directly to the NOT token
)

start = 'program'

# Regra principal: estrutura de um programa Pascal
def p_program(p):
    """program : PROGRAM ID SEMI declarations functions BEGIN statement_list END DOT
               | PROGRAM ID SEMI declarations BEGIN statement_list END DOT"""
    # p[6] or p[7] will now be a list of code strings from statement_list
    if len(p) == 9: # Case with functions
        # Join the function codes and then the main statement list code
        p[0] = "\n".join(parser_functions.values()) + "\nstart\n" + "".join(p[7]) + "\nstop"
    else: # Case without functions
        # Join the main statement list code
        p[0] = "\nstart\n" + "".join(p[6]) + "\nstop"

# Regra para declarações de variáveis ou ausência delas
def p_declarations(p):
    """declarations : VAR var_declaration_list
                    | empty"""
    p[0] = p[2] if len(p) > 2 else ""

def p_var_declaration_list(p):
    """var_declaration_list : var_declaration_list var_declaration
                            | var_declaration"""
    # Concatenate the generated code for variable declarations
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
    p[0] = "" # Declarations themselves don't generate VM code directly

def p_id_list(p):
    """id_list : ID
              | ID COMMA id_list"""
    p[0] = [p[1]] if len(p) == 2 else [p[1]] + p[3]

def p_type(p):
    """type : INTEGER
            | BOOLEAN
            | STRING
            | REAL"""
    p[0] = p[1] # Return the type string

def p_functions(p):
    """functions : function functions
                 | empty"""
    p[0] = "" # Function definitions are stored globally, not returned here

# Definição de função com parâmetros e corpo
def p_function(p):
    """function : FUNCTION ID LPAREN param_list RPAREN COLON type SEMI declarations BEGIN statement_list END SEMI"""
    name = p[2]
    param_code = "".join(p[4]) # param_list returns a list of codes
    local_code = p[8] # declarations returns a string
    body_code = "".join(p[10]) # statement_list returns a list of codes
    full_code = f"{name}:\n{param_code}{local_code}{body_code}RETURN\n"
    parser_functions[name] = full_code
    parser_params[name] = len(p[4]) # Count of parameters
    p[0] = ""

# Parâmetro único de função
def p_param_list_single(p):
    "param_list : ID COLON type"
    # Store the parameter in local scope if needed, or just count them
    # For simplicity, let's assume parameters are pushed to stack and stored
    p[0] = [f"STOREL {0}\n"] # Placeholder, actual local stack management is complex

# Múltiplos parâmetros de função
def p_param_list_multiple(p):
    "param_list : param_list SEMI ID COLON type"
    # Append the new parameter's store instruction to the list
    idx = len(p[1]) # Get current count of parameters
    p[0] = p[1] + [f"STOREL {idx}\n"]

# Argumento único passado à função
def p_argument_list_single(p):
    "argument_list : expression"
    p[0] = [p[1][0]] if isinstance(p[1], tuple) else [p[1]]

# Múltiplos argumentos passados à função
def p_argument_list_multiple(p):
    "argument_list : argument_list COMMA expression"
    left_codes = p[1] # This is already a list of codes
    right_code = p[3][0] if isinstance(p[3], tuple) else p[3]
    p[0] = left_codes + [right_code]

# Chamada a função
def p_expression_function_call(p):
    "expression : ID LPAREN argument_list RPAREN"
    fname = p[1]
    args_code_list = p[3] # This is a list of argument codes
    # Check if function is declared (optional, but good for semantic analysis)
    if fname not in parser_functions:
        print(f"Erro: função não declarada {fname}")
        global parser_success
        parser_success = False
        p[0] = ("", "integer") # Return empty code and default type
        return

    # Generate code to push arguments and then call the function
    p[0] = ("".join(args_code_list) + f"PUSHA {fname}\nCALL\n", "integer")


# --- Statement List and Statement Rules ---
# This structure handles semicolons as separators and allows empty statements
def p_statement_list(p):
    """statement_list : statement
                      | statement SEMI statement_list
                      | SEMI statement_list
                      | empty"""
    if len(p) == 2: # single statement or empty
        p[0] = [p[1]] if p[1] != "" else [] # Return a list containing the statement's code, or empty list
    elif p[1] == ';': # SEMI statement_list (first part is a semicolon)
        p[0] = p[2] # Skip the semicolon's code, keep the rest of the list
    else: # statement SEMI statement_list
        p[0] = [p[1]] + p[3] # Concatenate the current statement's code with the rest of the list

def p_statement(p):
    """statement : assignment_statement
                 | writeln_statement
                 | write_statement
                 | readln_statement
                 | for_statement
                 | if_statement
                 | while_statement
                 | statement_compound"""
    p[0] = p[1] # Statement rules return their generated code as a string

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
        # Type conversion if necessary
        if var_type == "real" and expr_type == "integer":
            expr_code += "ITOF\n"
        elif var_type == "integer" and expr_type == "real":
            expr_code += "FTOI\n"
        p[0] = expr_code + f"storeg {parser_var[p[1]]}\n"

def p_writeln_statement(p):
    """writeln_statement : WRITELN LPAREN writelist RPAREN SEMI"""
    # writelist returns a list of code strings, join them
    p[0] = "".join(p[3]) + "WRITELN\n" # Added WRITELN instruction

def p_write_statement(p):
    """write_statement : WRITE LPAREN writelist RPAREN SEMI"""
    # writelist returns a list of code strings, join them
    p[0] = "".join(p[3])

def p_writelist(p):
    """writelist : writelist COMMA writeitem
                 | writeitem"""
    p[0] = p[1] + [p[3]] if len(p) == 4 else [p[1]] # writelist returns a list of writeitem codes

def p_writeitem_string(p):
    """writeitem : STRING_LITERAL"""
    # String literals are already correctly formatted for PUSHS
    p[0] = f'pushs "{p[1]}"\nwrites\n'

def p_writeitem_expr(p):
    """writeitem : expression"""
    code = p[1][0] if isinstance(p[1], tuple) else p[1]
    # Assuming writei handles both integer and real, or needs conversion
    # For now, just pushing and writing integer. Add FWRITE if real is needed.
    p[0] = code + "writei\n"

def p_readln_statement(p):
    """readln_statement : READLN LPAREN ID RPAREN SEMI"""
    global parser_success
    if p[3] not in parser_var:
        print(f"Erro: variável não declarada {p[3]}")
        parser_success = False
        p[0] = ""
    else:
        # Assuming readln reads an integer and atoi converts it
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
        # Use a temporary global variable for the loop limit to avoid conflicts
        limit_var_address = parser_var_count
        parser_var_count += 1
        
        init_code, _ = p[4] if isinstance(p[4], tuple) else (p[4], "integer")
        limit_code, _ = p[6] if isinstance(p[6], tuple) else (p[6], "integer")
        body_code = p[8] # statement returns a single string

        loop_label = f"forloop_{index}"
        end_label = f"forend_{index}"

        p[0] = (
            init_code +
            f"storeg {index}\n" + # Store initial value of loop counter
            limit_code +
            f"storeg {limit_var_address}\n" + # Store loop limit
            f"{loop_label}:\n" +
            f"pushg {index}\n" + # Push current loop counter
            f"pushg {limit_var_address}\n" + # Push loop limit
            "infeq\n" + # Check if i <= limit
            f"jz {end_label}\n" + # If not, jump to end
            body_code + # Loop body
            f"pushg {index}\n" + # Push current loop counter
            "pushi 1\n" +
            "add\n" + # Increment loop counter
            f"storeg {index}\n" + # Store incremented loop counter
            f"jump {loop_label}\n" + # Jump back to loop start
            f"{end_label}:\n"
        )

# --- Expression Rules ---
def p_expression_boolean(p):
    """expression : TRUE
                  | FALSE"""
    p[0] = (f"pushi {1 if p[1].lower() == 'true' else 0}\n", "boolean") # Return boolean type

def p_expression_logical(p):
    """expression : expression AND expression
                  | expression OR expression"""
    op = {
        'and': 'mul', # Assuming 0 for false, 1 for true, so AND is multiplication
        'or': 'add'   # Assuming 0 for false, 1 for true, so OR is addition (clamped to 1 if >0)
                      # For proper boolean logic, you might need JZ/JNZ
    }[p[2].lower()]
    left_code, left_type = p[1] if isinstance(p[1], tuple) else (p[1], "boolean")
    right_code, right_type = p[3] if isinstance(p[3], tuple) else (p[3], "boolean")
    # Basic type checking for logical operations
    if left_type != "boolean" or right_type != "boolean":
        print(f"Erro de tipo: Operadores lógicos esperam booleanos.")
        parser_success = False
    p[0] = (left_code + right_code + op + "\n", "boolean")

def p_expression_not(p):
    """expression : NOT expression""" # Removed %prec UNARY_NOT
    code, type = p[2] if isinstance(p[2], tuple) else (p[2], "boolean")
    if type != "boolean":
        print(f"Erro de tipo: NOT espera um booleano.")
        parser_success = False
    # Assuming 0/1 for boolean, NOT 0 is 1, NOT 1 is 0
    # A simple way for 0/1: push 1, swap, sub (1 - value)
    p[0] = (code + "pushi 1\nswap\nsub\n", "boolean")


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
    left_code, left_type = p[1] if isinstance(p[1], tuple) else (p[1], "integer")
    right_code, right_type = p[3] if isinstance(p[3], tuple) else (p[3], "integer")
    # Basic type compatibility check for relational operations
    if (left_type == "integer" and right_type == "real") or \
       (left_type == "real" and right_type == "integer"):
        # Promote integer to real for comparison
        if left_type == "integer":
            left_code += "ITOF\n"
        else:
            right_code += "ITOF\n"
        result_type = "real" # Result of comparison is boolean, but intermediate stack might be real
    elif left_type != right_type:
        print(f"Erro de tipo: Comparação entre tipos incompatíveis ({left_type} e {right_type}).")
        parser_success = False
        result_type = "boolean" # Default to boolean result
    else:
        result_type = "boolean" # Result of comparison is always boolean

    p[0] = (left_code + right_code + op_map[p[2]] + "\n", result_type)

def p_expression_paren(p):
    """expression : LPAREN expression RPAREN"""
    p[0] = p[2] # Pass through the expression code and type

def p_expression_div(p):
    """expression : expression DIV expression"""
    left_code, left_type = p[1] if isinstance(p[1], tuple) else (p[1], "integer")
    right_code, right_type = p[3] if isinstance(p[3], tuple) else (p[3], "integer")
    # DIV is integer division in Pascal, so result is integer
    if left_type == "real" or right_type == "real":
        print(f"Erro de tipo: DIV espera operandos inteiros.")
        parser_success = False
    p[0] = (left_code + right_code + "div\n", "integer")

def p_expression_mod(p):
    """expression : expression MOD expression"""
    left_code, left_type = p[1] if isinstance(p[1], tuple) else (p[1], "integer")
    right_code, right_type = p[3] if isinstance(p[3], tuple) else (p[3], "integer")
    # MOD is integer modulus in Pascal, so result is integer
    if left_type == "real" or right_type == "real":
        print(f"Erro de tipo: MOD espera operandos inteiros.")
        parser_success = False
    p[0] = (left_code + right_code + "mod\n", "integer")

def p_statement_compound(p):
    """statement_compound : BEGIN statement_list END"""
    p[0] = "".join(p[2]) # statement_list returns a list of codes, join them

def p_if_statement(p):
    """if_statement : IF expression THEN statement %prec IFX
                    | IF expression THEN statement ELSE statement"""
    global parser_var_count
    idx = parser_var_count # Unique index for labels
    parser_var_count += 1
    cond_code, cond_type = p[2] if isinstance(p[2], tuple) else (p[2], "boolean")
    if cond_type != "boolean":
        print(f"Erro de tipo: Condição IF espera um booleano.")
        parser_success = False

    if len(p) == 5:  # IF ... THEN ...
        label_end = f"ifend_{idx}"
        p[0] = (
            cond_code +
            f"jz {label_end}\n" + # Jump if condition is false (0)
            p[4] + # THEN statement code
            f"{label_end}:\n"
        )
    else:  # IF ... THEN ... ELSE ...
        label_else = f"ifelse_{idx}"
        label_end = f"ifend_{idx}"
        p[0] = (
            cond_code +
            f"jz {label_else}\n" + # Jump to ELSE if condition is false
            p[4] + # THEN statement code
            f"jump {label_end}\n" + # Jump over ELSE block
            f"{label_else}:\n" +
            p[6] + # ELSE statement code
            f"{label_end}:\n"
        )

def p_while_statement(p):
    """while_statement : WHILE expression DO statement"""
    global parser_var_count
    loop_index = parser_var_count # Unique index for labels
    parser_var_count += 1
    cond_code, cond_type = p[2] if isinstance(p[2], tuple) else (p[2], "boolean")
    if cond_type != "boolean":
        print(f"Erro de tipo: Condição WHILE espera um booleano.")
        parser_success = False

    end_label = f"while_end_{loop_index}"
    start_label = f"while_start_{loop_index}"
    body = p[4] # statement returns a single string

    p[0] = (
        f"{start_label}:\n" +
        cond_code +
        f"jz {end_label}\n" + # Jump to end if condition is false
        body +
        f"jump {start_label}\n" + # Jump back to loop start
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
        '/': 'div' # Note: '/' is float division in Pascal
    }[p[2]]
    left_code, left_type = p[1] if isinstance(p[1], tuple) else (p[1], "integer")
    right_code, right_type = p[3] if isinstance(p[3], tuple) else (p[3], "integer")

    # Type promotion for arithmetic operations
    if left_type == "real" or right_type == "real":
        result_type = "real"
        # Convert integer operands to real if mixed types
        if left_type == "integer":
            left_code += "ITOF\n"
        if right_type == "integer":
            right_code += "ITOF\n"
        # Use float division for '/'
        if p[2] == '/':
            op = 'fdiv'
        elif op == 'div' or op == 'mod': # DIV and MOD are integer-only
             print(f"Erro de tipo: Operador {p[2]} espera operandos inteiros com tipos reais.")
             parser_success = False

    elif left_type == "integer" and right_type == "integer":
        result_type = "integer"
    else: # Incompatible types
        print(f"Erro de tipo: Operação aritmética entre tipos incompatíveis ({left_type} e {right_type}).")
        parser_success = False
        result_type = "integer" # Default to integer

    p[0] = (left_code + right_code + op + "\n", result_type)

def p_expression_variable(p):
    """expression : ID"""
    global parser_success, parser_var_types
    if p[1] not in parser_var:
        print(f"Erro: variável não declarada {p[1]}")
        parser_success = False
        p[0] = ("", "integer") # Return empty code and default type
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

# Main execution block
if __name__ == "__main__":
    # Ensure input.txt is available or passed as argument
    input_file_path = 'input.txt'
    if len(sys.argv) > 1:
        input_file_path = sys.argv[1]

    try:
        with open(input_file_path, 'r', encoding="utf-8") as file:
            source = file.read()
    except FileNotFoundError:
        print(f"Erro: O ficheiro '{input_file_path}' não foi encontrado.")
        sys.exit(1)

    codigo = parser.parse(source)

    output_file_path = 'output.txt'
    with open(output_file_path, 'w', encoding="utf-8") as f:
        if parser_success:
            print('Parsing completado com sucesso!')
            # Initialize global variables in VM code
            for _ in range(parser_var_count):
                f.write("pushi 0\n") # Initialize all global variables to 0
            f.write(codigo)
        else:
            print('Parsing falhou!')
