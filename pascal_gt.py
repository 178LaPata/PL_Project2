import sys
import re

import ply.yacc as yacc
from pascal_lex import tokens

parser = None
parser_success = True
parser_functions = {}
parser_params = {}
parser_var = {}
parser_var_count = 0
parser_var_types = {}

precedence = (
    ('nonassoc', 'IFX'),
    ('nonassoc', 'ELSE'),
    ('left', 'OR'),
    ('left', 'AND'),
    ('nonassoc', 'EQ', 'NEQ', 'LT', 'LE', 'GT', 'GE'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE', 'DIV', 'MOD'),
    ('right', 'NOT'),
)

start = 'program'

# Regra principal: estrutura de um programa Pascal
def p_program(p):
    """program : PROGRAM ID SEMI declarations functions BEGIN statements END DOT"""
    # This logic for combining functions and main code seems a bit complex based on len(p)
    # for a single grammar rule. Assuming p[7] is consistently the 'statements' code.
    # A more direct way to combine might be:
    # functions_code_str = "\n".join(f_code for f_code in parser_functions.values() if f_code)
    # if functions_code_str:
    #     functions_code_str += "\n"
    # main_statements_code = p[7] # from statements
    # p[0] = functions_code_str + "start\n" + main_statements_code + "\nstop"
    # Sticking to original structure for now, assuming it has a purpose:
    if len(p) == 9: # This condition is unusual for a fixed grammar rule.
        p[0] = "\nstart\n" + p[7] + "\nstop" # Assuming p[7] is statements if this path is taken. (Original was p[6])
    else: # Defaulting to this structure which seems more likely intended
        functions_code_str = "\n".join(parser_functions.values())
        main_statements_code = p[7] # p[7] is 'statements'
        if functions_code_str:
            p[0] = functions_code_str + "\nstart\n" + main_statements_code + "\nstop"
        else:
            p[0] = "start\n" + main_statements_code + "\nstop"


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
    type_representation = p[3]  # This now comes from the updated p_type

    for var_name in p[1]: # p[1] is the id_list
        if var_name in parser_var:
            print(f"Erro: variável duplicada {var_name}")
            parser_success = False
        else:
            parser_var[var_name] = parser_var_count
            # If type_representation is always a string (even for arrays), .lower() might be too simple.
            # For "array[1..5]_of_integer", .lower() is fine.
            # If you use a structured type for arrays, you'd store that structure directly.
            parser_var_types[var_name] = type_representation # Store the direct representation
            parser_var_count += 1
    p[0] = ""

def p_id_list(p):
    """id_list : ID
              | ID COMMA id_list"""
    p[0] = [p[1]] if len(p) == 2 else [p[1]] + p[3]

def p_type(p):
    """type : simple_type
            | array_type"""
    p[0] = p[1] # Pass the type representation (string or structured object) up

def p_simple_type(p):
    """simple_type : INTEGER
                   | BOOLEAN
                   | STRING
                   | REAL"""
    p[0] = p[1].lower() # Store simple types as lowercase strings e.g., "integer"

def p_array_type(p):
    """array_type : ARRAY LBRACKET index_range RBRACKET OF type"""
    # p[1] = ARRAY token
    # p[2] = LBRACKET token '['
    # p[3] = index_range, which will be a tuple (low, high)
    # p[4] = RBRACKET token ']'
    # p[5] = OF token
    # p[6] = type (this is the base type of the array, e.g., "integer")

    low_bound, high_bound = p[3]
    base_type = p[6] # This will be the string from the recursive call to p_type

    # You can choose how to represent this array type.
    # Option 1: A descriptive string (useful for simple storage)
    p[0] = f"array[{low_bound}..{high_bound}]_of_{base_type}"

    # Option 2: A structured representation (better for later semantic analysis/code generation)
    # p[0] = {'kind': 'array', 'low': low_bound, 'high': high_bound, 'base_type': base_type}
    # For now, the string representation is simpler to integrate with existing p_var_declaration.

def p_index_range(p):
    """index_range : NUMBER DOTDOT NUMBER"""
    # p[1] = value of the first NUMBER token
    # p[2] = DOTDOT token '..'
    # p[3] = value of the second NUMBER token
    p[0] = (p[1], p[3]) # Store range as a tuple (low, high)


def p_functions(p):
    """functions : function functions
                 | empty"""
    p[0] = ""

# Definição de função com parâmetros e corpo
def p_function(p):
    """function : FUNCTION ID LPAREN param_list RPAREN COLON type SEMI declarations BEGIN statements END SEMI""" # SEMI terminates function def
    name = p[2]
    param_code = p[4]
    local_code = p[9] # declarations index changed due to SEMI after type
    body_code = p[11] # statements index changed
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
    "param_list : param_list SEMI ID COLON type" # SEMI here separates parameters
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
    p[0] = (args + f"PUSHA {fname}\nCALL\n", "integer") # Assuming integer return for now

# --- MODIFIED STATEMENT HANDLING ---
def p_statements(p):
    """statements : statement_sequence""" # REMOVED: | empty_statement_block
    p[0] = "".join(p[1]) # p[1] is a list of statement codes from statement_sequence

def p_statement_sequence(p):
    """statement_sequence : statement
                          | statement_sequence SEMI statement"""
    if len(p) == 2:  # Single statement in sequence (or first statement)
        # p[1] is the code from the statement.
        # If p[1] is "" (from concrete_empty_statement), it means an empty statement.
        if p[1] != "":  # Only add code if the statement is not an empty one
            p[0] = [p[1]]
        else:
            p[0] = [] # An empty statement results in an empty list of code for this part
    else:  # statement_sequence SEMI statement
        # p[1] is the list of codes from previous statements.
        # p[3] is the code of the current statement.
        if p[3] != "": # Only add code if the current statement is not an empty one
            p[0] = p[1] + [p[3]]
        else: # Current statement is empty, so just carry forward the previous list
            p[0] = p[1]

# --- END OF MODIFIED STATEMENT HANDLING ---

def p_statement(p):
    """statement : assignment_statement
                 | writeln_statement
                 | write_statement
                 | readln_statement
                 | for_statement
                 | if_statement
                 | while_statement
                 | statement_compound
                 | concrete_empty_statement""" # Added concrete_empty_statement
    p[0] = p[1]

def p_concrete_empty_statement(p):
    'concrete_empty_statement :'
    p[0] = "" # An empty statement produces no executable code

# --- MODIFIED simple statement rules: REMOVED TRAILING SEMI ---
def p_assignment_statement(p):
    """assignment_statement : ID ASSIGN expression""" # REMOVED SEMI
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
    """writeln_statement : WRITELN LPAREN writelist RPAREN""" # REMOVED SEMI
    p[0] = p[3] + "WRITELN\n"

def p_write_statement(p):
    """write_statement : WRITE LPAREN writelist RPAREN""" # REMOVED SEMI
    p[0] = p[3]
# --- END OF MODIFIED simple statement rules ---

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
    p[0] = code + "writei\n" # Assuming writei for integers, writer for reals might be needed

def p_readln_statement(p):
    """readln_statement : READLN LPAREN ID RPAREN""" # REMOVED SEMI
    global parser_success
    if p[3] not in parser_var:
        print(f"Erro: variável não declarada {p[3]}")
        parser_success = False
        p[0] = ""
    else:
        # Assuming readln reads into an integer variable.
        # If other types are supported, type checking might be needed.
        p[0] = "read\natoi\n" + f"storeg {parser_var[p[3]]}\n"


# Compound statements (FOR, IF, WHILE, BEGIN...END) do not have trailing SEMI in their rules.
# The SEMI that follows them (if any) is handled by p_statement_sequence.
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
        # Create a temporary variable for the loop limit if not already handled
        # This implementation assumes a simple integer loop.
        # A dedicated memory location for the limit might be better.
        limit_index = parser_var_count # Using a new temp var slot
        parser_var_count +=1 

        init_code, _ = p[4] if isinstance(p[4], tuple) else (p[4], "integer")
        limit_code, _ = p[6] if isinstance(p[6], tuple) else (p[6], "integer")
        body_code = p[8] # Code from the DO statement part
        
        loop_label = f"forloop{index}" # Make labels more unique
        end_label = f"forend{index}"
        
        p[0] = (
            init_code +
            f"storeg {index}\n" +         # Initialize loop variable
            limit_code +
            f"storeg {limit_index}\n" +   # Store loop limit
            f"{loop_label}:\n" +          # Loop start label
            f"pushg {index}\n" +          # Push current value of loop variable
            f"pushg {limit_index}\n" +    # Push loop limit
            "infeq\n" +                   # Check if var <= limit (for 'to') or var >= limit (for 'downto')
                                          # This is infeq (<=), for 'TO'. For 'DOWNTO' it would be 'supeq' (>=)
            f"jz {end_label}\n" +         # If condition false (var > limit), jump to end
            body_code +                   # Execute loop body
            f"pushg {index}\n" +          # Increment loop variable part
            "pushi 1\n" +                 # Push 1 (for 'TO'). For 'DOWNTO', push -1 or 'pushi 1\nsub\n'
            "add\n" +
            f"storeg {index}\n" +         # Store incremented value
            f"jump {loop_label}\n" +      # Jump back to loop start
            f"{end_label}:\n"             # Loop end label
        )

def p_expression_boolean(p):
    """expression : TRUE
                  | FALSE"""
    p[0] = (f"pushi {1 if p[1].lower() == 'true' else 0}\n", "integer") # Represent boolean as integer 0 or 1

def p_expression_logical(p):
    """expression : expression AND expression
                  | expression OR expression"""
    op = {
        'and': 'mul', # Assumes 0/1 logic for AND
        'or': 'add'   # Assumes 0/1 logic for OR (careful if >1 values are considered true)
    }[p[2].lower()]
    left_code, _ = p[1] if isinstance(p[1], tuple) else (p[1], "integer")
    right_code, _ = p[3] if isinstance(p[3], tuple) else (p[3], "integer")
    # For OR, to ensure 0 or 1 result: (left + right > 0)
    # For now, simple add/mul, assuming inputs are 0/1
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
        '<>': 'nequal' # Check if VM has 'nequal' or if it's 'equal\nnot'
    }
    left_code, left_type = p[1] if isinstance(p[1], tuple) else (p[1], "integer") # Defaulting to int if not tuple
    right_code, right_type = p[3] if isinstance(p[3], tuple) else (p[3], "integer")

    # Rudimentary type handling for comparison (extend as needed)
    # If one is real, convert other to real for comparison if VM requires
    # Assuming VM handles mixed-type comparisons or types are compatible
    # For now, just pass through. Add ITOF/FTOI if necessary before op.

    p[0] = (left_code + right_code + op_map[p[2]] + "\n", "integer") # Result of comparison is boolean (0/1)

def p_expression_paren(p):
    """expression : LPAREN expression RPAREN"""
    p[0] = p[2]

def p_expression_div(p): # Integer division
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
    p[0] = p[2] # p[2] is already the concatenated string of statements code

def p_if_statement(p):
    """if_statement : IF expression THEN statement %prec IFX
                    | IF expression THEN statement ELSE statement"""
    global parser_var_count # Using this to generate unique labels
    
    idx = parser_var_count
    parser_var_count += 1 # Increment for next unique label
    
    cond_code, _ = p[2] if isinstance(p[2], tuple) else (p[2], "integer")
    then_statement_code = p[4]

    if len(p) == 5:  # IF ... THEN ... (no ELSE)
        label_end = f"ifend{idx}"
        p[0] = (
            cond_code +
            f"jz {label_end}\n" + # Jump to end if condition is false (0)
            then_statement_code +
            f"{label_end}:\n"
        )
    else:  # IF ... THEN ... ELSE ... (len(p) == 7)
        else_statement_code = p[6]
        label_else = f"ifelse{idx}"
        label_end = f"ifend{idx}"
        p[0] = (
            cond_code +
            f"jz {label_else}\n" + # Jump to else part if condition is false
            then_statement_code +
            f"jump {label_end}\n" + # Skip else part
            f"{label_else}:\n" +
            else_statement_code +
            f"{label_end}:\n"
        )

def p_while_statement(p):
    """while_statement : WHILE expression DO statement"""
    global parser_var_count
    loop_idx = parser_var_count
    parser_var_count += 1

    cond_code, _ = p[2] if isinstance(p[2], tuple) else (p[2], "integer")
    body_code = p[4]
    
    start_label = f"whilestart{loop_idx}"
    end_label = f"whileend{loop_idx}"
    
    p[0] = (
        f"{start_label}:\n" +       # Label for the start of the loop (condition check)
        cond_code +                 # Evaluate condition
        f"jz {end_label}\n" +       # If condition false, jump to end
        body_code +                 # Execute loop body
        f"jump {start_label}\n" +   # Jump back to condition check
        f"{end_label}:\n"           # Label for the end of the loop
    )

def p_expression_binop(p):
    """expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression""" # Real division
    op_map = {
        '+': 'add',
        '-': 'sub',
        '*': 'mul',
        '/': 'div' # Assuming 'div' is used for real division too, or a 'fdiv' exists
    }
    op_code = op_map[p[2]]

    left_code, left_type = p[1] if isinstance(p[1], tuple) else (p[1], "integer")
    right_code, right_type = p[3] if isinstance(p[3], tuple) else (p[3], "integer")
    
    result_type = "integer" # Default
    final_left_code = left_code
    final_right_code = right_code

    # Type promotion: if one operand is real, promote the other to real
    if left_type == "real" and right_type == "integer":
        final_right_code = right_code + "itof\n"
        result_type = "real"
    elif left_type == "integer" and right_type == "real":
        final_left_code = left_code + "itof\n"
        result_type = "real"
    elif left_type == "real" and right_type == "real":
        result_type = "real"
    
    # If operator is division '/', result is always real.
    if p[2] == '/':
        if result_type == "integer": # Both were integers
            final_left_code = left_code + "itof\n"
            final_right_code = right_code + "itof\n"
        result_type = "real"
        # Use 'fdiv' if VM has separate float division, else 'div' might handle it
        # op_code = "fdiv" # if applicable

    p[0] = (final_left_code + final_right_code + op_code + "\n", result_type)


def p_expression_variable(p):
    """expression : ID"""
    global parser_success, parser_var_types
    if p[1] not in parser_var:
        print(f"Erro: variável não declarada {p[1]}")
        parser_success = False
        p[0] = ("", "integer") # Return empty code and default type on error
    else:
        var_type = parser_var_types.get(p[1], "integer") # Default to integer if not found (should not happen for declared vars)
        p[0] = (f"pushg {parser_var[p[1]]}\n", var_type)

def p_expression_number(p): # Integer
    "expression : NUMBER"
    p[0] = (f"pushi {str(p[1])}\n", "integer")

def p_expression_real(p): # Real number
    "expression : REAL"
    p[0] = (f"pushf {str(p[1])}\n", "real")

# Produção vazia (útil em alternativas opcionais)
def p_empty(p): # Generic empty rule for declarations, functions etc.
    'empty :'
    p[0] = "" # Produces an empty string, not None or [] like empty_statement_block

def p_error(p):
    if p:
        print(f"Erro de sintaxe: {p.type}({p.value}) na linha {p.lineno}")
    else:
        print("Erro de sintaxe: EOF inesperado")
    global parser_success
    parser_success = False

# Inicializar parser DEPOIS das regras
parser = yacc.yacc(debug=True)

# Leitura de código Pascal (Example usage, you might get this from an argument)
with open('input4.txt', 'r') as file:
    source = file.read()

codigo = parser.parse(source)

with open('output.txt', 'w') as f:
    if parser_success:
        print('Parsing completado com sucesso!')
        # Initialize global variables storage if parser_var_count is used for stack allocation
        for _ in range(parser_var_count): # This needs to be accurate for global vars
            f.write("pushi 0\n") # Or pushf for reals, or based on types
        f.write(codigo)
    else:
        print('Parsing falhou!')