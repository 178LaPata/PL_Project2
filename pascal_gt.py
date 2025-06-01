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
label_seq_num = 0 # Global counter for unique labels

def generate_unique_label_num():
    global label_seq_num
    label_seq_num += 1
    return label_seq_num

precedence = (
    ('nonassoc', 'IFX'),    # IFX (para IF..THEN sem ELSE) tem menor precedência
    ('nonassoc', 'ELSE'),   # ELSE (token) tem maior precedência
    ('left', 'OR'),
    ('left', 'AND'),
    ('nonassoc', 'EQ', 'NEQ', 'LT', 'LE', 'GT', 'GE'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE', 'DIV', 'MOD'),
    ('right', 'NOT'),
)

start = 'program'

def p_program(p):
    """program : PROGRAM ID SEMI declarations functions BEGIN statements END DOT"""
    functions_code_str = "\n".join(f_code for f_code in parser_functions.values() if f_code)
    main_statements_code = p[7] 
    
    final_code = []
    if functions_code_str:
        final_code.append(functions_code_str)
    final_code.append("start")
    if main_statements_code: 
        final_code.append(main_statements_code)
    final_code.append("stop")
    
    p[0] = "\n".join(final_code) + "\n"


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
    global parser_var, parser_var_count, parser_success, parser_var_types
    type_representation = p[3]

    for var_name in p[1]: 
        if var_name in parser_var:
            print(f"Erro: variável duplicada {var_name}")
            parser_success = False
        else:
            parser_var[var_name] = parser_var_count
            parser_var_types[var_name] = type_representation 
            
            # Increment parser_var_count based on type for allocation
            # Simple types take 1 slot. Arrays need more complex handling for size.
            # For now, assuming each declared var name takes 1 slot for its base/descriptor.
            # Actual array element storage needs a different mechanism (e.g. ALLOC or assuming contiguous global slots)
            # This basic increment is for the variable descriptor itself, not necessarily the full array.
            # If arrays are to be stored in global var space directly, this needs adjustment.
            # E.g., if 'numeros: array[1..5] of integer' means 5 slots are reserved starting at parser_var['numeros']
            
            # For simplicity now: each var declaration (ID or array ID) gets one slot counter incremented.
            # Semantic analysis should later determine actual memory size.
            parser_var_count += 1 
            
    p[0] = ""

def p_id_list(p):
    """id_list : ID
              | ID COMMA id_list"""
    p[0] = [p[1]] if len(p) == 2 else [p[1]] + p[3]

def p_type(p):
    """type : simple_type
            | array_type"""
    p[0] = p[1] 

def p_simple_type(p):
    """simple_type : INTEGER
                   | BOOLEAN
                   | STRING
                   | REAL"""
    p[0] = p[1].lower() 

def p_array_type(p):
    """array_type : ARRAY LBRACKET index_range RBRACKET OF type"""
    low_bound, high_bound = p[3]
    base_type = p[6] 
    p[0] = f"array[{low_bound}..{high_bound}]_of_{base_type}"

def p_index_range(p):
    """index_range : NUMBER DOTDOT NUMBER"""
    p[0] = (p[1], p[3]) 

def p_variable(p):
    """variable : ID
                | ID LBRACKET expression RBRACKET"""
    global parser_success, parser_var, parser_var_types
    if len(p) == 2: 
        var_name = p[1]
        if var_name not in parser_var:
            print(f"Erro: Variável '{var_name}' não declarada.")
            parser_success = False
            p[0] = {'type': 'error', 'name': var_name, 'basetype': 'unknown'}
        else:
            p[0] = {'type': 'simple', 
                    'name': var_name, 
                    'basetype': parser_var_types.get(var_name, 'unknown')}
    else:  
        array_name = p[1]
        index_expr_code, index_expr_type = p[3] 

        if array_name not in parser_var:
            print(f"Erro: Array '{array_name}' não declarado.")
            parser_success = False
            p[0] = {'type': 'error', 'name': array_name, 'basetype': 'unknown'}
        else:
            array_type_full_str = parser_var_types.get(array_name)
            element_basetype = 'unknown' 
            low_bound = 0 # Default, should be extracted
            
            if isinstance(array_type_full_str, str) and array_type_full_str.startswith("array") and "_of_" in array_type_full_str:
                element_basetype = array_type_full_str.split("_of_")[-1]
                try:
                    # Extract low_bound from "array[L..H]_of_T"
                    range_part = array_type_full_str.split('[')[1].split(']')[0] # "L..H"
                    low_bound_str = range_part.split('..')[0]
                    low_bound = int(low_bound_str)
                except (IndexError, ValueError):
                    print(f"Aviso: Formato de tipo array inválido ou não foi possível extrair limite inferior de '{array_type_full_str}' para '{array_name}'. Assumindo 1.")
                    low_bound = 1 # Fallback, consider this an error path
                    parser_success = False # Potentially an error if bounds can't be parsed
            elif not (isinstance(array_type_full_str, str) and array_type_full_str.startswith("array")):
                 print(f"Erro: Variável '{array_name}' não é um array.")
                 parser_success = False
                 p[0] = {'type': 'error', 'name': array_name, 'basetype': 'unknown'}
                 return
            
            p[0] = {
                'type': 'indexed',
                'name': array_name, 
                'index_code': index_expr_code, 
                'basetype': element_basetype,
                'low_bound': low_bound # Store low_bound for address calculation
            }

def p_functions(p):
    """functions : function functions
                 | empty"""
    p[0] = ""

def p_function(p):
    """function : FUNCTION ID LPAREN param_list RPAREN COLON type SEMI declarations BEGIN statements END SEMI"""
    name = p[2]
    param_code = p[4]
    local_code = p[9] 
    body_code = p[11] 
    full_code = f"{name}\n{param_code}{local_code}{body_code}RETURN\n" 
    parser_functions[name] = full_code
    parser_params[name] = param_code.count("STOREL")
    p[0] = ""

def p_param_list_single(p):
    "param_list : ID COLON type"
    p[0] = "STOREL 0\n"

def p_param_list_multiple(p):
    "param_list : param_list SEMI ID COLON type"
    idx = p[1].count("STOREL")
    p[0] = p[1] + f"STOREL {idx}\n"

def p_argument_list_single(p):
    "argument_list : expression"
    p[0] = p[1][0] if isinstance(p[1], tuple) else p[1]

def p_argument_list_multiple(p):
    "argument_list : argument_list COMMA expression"
    left = p[1] if isinstance(p[1], str) else p[1][0]
    right = p[3] if isinstance(p[3], str) else p[3][0]
    p[0] = left + right

def p_expression_function_call(p):
    "expression : ID LPAREN argument_list RPAREN"
    fname = p[1]
    args = p[3] if isinstance(p[3], str) else p[3][0]
    p[0] = (args + f"PUSHA {fname}\nCALL\n", "integer")

def p_statements(p):
    """statements : statement_sequence""" 
    p[0] = "".join(p[1]) 

def p_statement_sequence(p):
    """statement_sequence : statement
                          | statement_sequence SEMI statement"""
    if len(p) == 2:  
        if p[1] != "":  
            p[0] = [p[1]]
        else:
            p[0] = [] 
    else:  
        if p[3] != "": 
            p[0] = p[1] + [p[3]]
        else: 
            p[0] = p[1]

def p_statement(p):
    """statement : assignment_statement
                 | writeln_statement
                 | write_statement
                 | readln_statement
                 | for_statement
                 | if_statement
                 | while_statement
                 | statement_compound
                 | concrete_empty_statement""" 
    p[0] = p[1]

def p_concrete_empty_statement(p):
    'concrete_empty_statement :'
    p[0] = "" 

def p_assignment_statement(p):
    """assignment_statement : variable ASSIGN expression"""
    global parser_success, parser_var, parser_var_types
    
    lhs_var_info = p[1]
    rhs_expr_code, rhs_expr_type = p[3]

    if lhs_var_info.get('type') == 'error':
        parser_success = False 
        p[0] = ""
        return

    final_rhs_code = rhs_expr_code
    target_basetype = lhs_var_info.get('basetype', 'integer')

    if target_basetype == "real" and rhs_expr_type == "integer":
        final_rhs_code += "itof\n"
    elif target_basetype == "integer" and rhs_expr_type == "real":
        final_rhs_code += "ftoi\n"

    if lhs_var_info['type'] == 'simple':
        var_name = lhs_var_info['name']
        p[0] = final_rhs_code + f"storeg {parser_var[var_name]}\n"
    elif lhs_var_info['type'] == 'indexed':
        array_name = lhs_var_info['name']
        index_code = lhs_var_info['index_code']
        array_slot = parser_var[array_name]
        low_bound = lhs_var_info.get('low_bound', 1) # Get low_bound stored in p_variable

        # Code to store:
        # Stack must be: ..., address_a, offset_o, value_v  (for STOREN)
        # 1. Calculate address_a (base_address of array)
        # 2. Calculate offset_o (index_expr_val - low_bound)
        # 3. Push value_v (from final_rhs_code)
        
        code = "pushgp\n"                             # Stack: GP_addr
        code += f"pushi {array_slot}\n"             # Stack: GP_addr, slot_offset
        code += "add\n"                             # Stack: base_address_a (Addr(array[low_bound_slot_itself]))
        
        code += index_code                          # Stack: base_address_a, index_value
        code += f"pushi {low_bound}\n"              # Stack: base_address_a, index_value, low_bound
        code += "sub\n"                             # Stack: base_address_a, offset_o
        
        code += final_rhs_code                      # Stack: base_address_a, offset_o, value_v
        code += "storen\n"                          # Pops v, o, a; stores v at a+o
        p[0] = code
    else:
        p[0] = ""

def p_writeln_statement(p):
    """writeln_statement : WRITELN LPAREN writelist RPAREN""" 
    p[0] = p[3] + "WRITELN\n"

def p_write_statement(p):
    """write_statement : WRITE LPAREN writelist RPAREN""" 
    p[0] = p[3]

def p_writelist(p):
    """writelist : writelist COMMA writeitem
                 | writeitem"""
    p[0] = p[1] + p[3] if len(p) == 4 else p[1]

def p_writeitem_string(p):
    """writeitem : STRING_LITERAL"""
    p[0] = f'pushs "{p[1]}"\nwrites\n' # Corrected from PUSHS to pushs per EWVM

def p_writeitem_expr(p):
    """writeitem : expression"""
    code, expr_type = p[1] 
    if expr_type == "real":
        p[0] = code + "writef\n" # Use WRITEF for reals
    elif expr_type == "boolean": # Booleans are represented as 0 or 1
        p[0] = code + "writei\n" # Print as integer
    else: # Assume integer
        p[0] = code + "writei\n" 

def p_readln_statement(p):
    """readln_statement : READLN LPAREN variable RPAREN"""
    global parser_success, parser_var
    var_info = p[3]

    if var_info.get('type') == 'error':
        parser_success = False
        p[0] = ""
        return

    code = "read\n" # Reads string, pushes its address
    # Assuming integer read for now, add ATOF for reals if var_info['basetype'] is real
    if var_info.get('basetype') == 'real':
        code += "atof\n" # Converts string on stack to float
    else: # Assume integer or boolean (reads as int)
        code += "atoi\n" # Converts string on stack to int

    if var_info['type'] == 'simple':
        var_name = var_info['name']
        code += f"storeg {parser_var[var_name]}\n"
    elif var_info['type'] == 'indexed':
        array_name = var_info['name']
        index_code = var_info['index_code']
        array_slot = parser_var[array_name]
        low_bound = var_info.get('low_bound', 1)
        
        # Value from read (converted) is on stack.
        # Then calculate address for storen (a, o, v)
        # Stack: value_from_read
        addr_calc_code = "pushgp\n"                         # Stack: value_from_read, GP_addr
        addr_calc_code += f"pushi {array_slot}\n"           # Stack: value_from_read, GP_addr, slot
        addr_calc_code += "add\n"                           # Stack: value_from_read, base_address_a
        
        addr_calc_code += index_code                        # Stack: value_from_read, base_address_a, index_val
        addr_calc_code += f"pushi {low_bound}\n"            # Stack: value_from_read, base_address_a, index_val, low_bound
        addr_calc_code += "sub\n"                           # Stack: value_from_read, base_address_a, offset_o
        
        # Need to swap 'value' to top for STOREN (expects a, o, v)
        # Current stack: ..., value, a, o
        # We need: ..., a, o, value
        # This means the value from read needs to be pushed AFTER a and o are calculated,
        # or we use SWAP if available and convenient.
        # Let's recalculate order: calculate a, o first, then read value, then storen.
        
        addr_calc_for_store = "pushgp\n"                    # Stack: GP_addr
        addr_calc_for_store += f"pushi {array_slot}\n"      # Stack: GP_addr, slot
        addr_calc_for_store += "add\n"                      # Stack: base_address_a
        
        addr_calc_for_store += index_code                   # Stack: base_address_a, index_val
        addr_calc_for_store += f"pushi {low_bound}\n"       # Stack: base_address_a, index_val, low_bound
        addr_calc_for_store += "sub\n"                      # Stack: base_address_a, offset_o
        
        code = addr_calc_for_store + code # code here is "read\natoi_or_f\n" which pushes value
        code += "storen\n"                 # Stack: a, o, value -> STOREN consumes
    p[0] = code


def p_for_statement(p):
    """for_statement : FOR ID ASSIGN expression TO expression DO statement"""
    global parser_success, parser_var_count, parser_var
    loop_var_name = p[2]
    if loop_var_name not in parser_var:
        print(f"Erro: variável de ciclo '{loop_var_name}' não declarada")
        parser_success = False
        p[0] = ""
    else:
        loop_var_slot = parser_var[loop_var_name]
        
        # Temporary slot for the loop's upper limit value
        limit_storage_slot = parser_var_count 
        parser_var_count +=1 # Allocate a new global slot for the limit

        init_expr_code, _ = p[4] 
        limit_expr_code, _ = p[6] 
        body_code = p[8] 
        
        label_num = generate_unique_label_num()
        loop_label = f"forloop{label_num}"
        end_label = f"forend{label_num}"
        
        p[0] = (
            init_expr_code +                            # Push initial value
            f"storeg {loop_var_slot}\n" +               # Store initial value in loop variable: i := init
            limit_expr_code +                           # Push limit expression value
            f"storeg {limit_storage_slot}\n" +          # Store limit value in its temp slot: limit_temp := limit_expr
            f"{loop_label}\n" +                         # Loop start label
            f"pushg {loop_var_slot}\n" +                # Push current value of loop variable (i)
            f"pushg {limit_storage_slot}\n" +           # Push loop limit from its temp slot
            "infeq\n" +                                 # i <= limit_temp ?
            f"jz {end_label}\n" +                       # If false (i > limit_temp), jump to end
            body_code +                                 # Execute loop body
            f"pushg {loop_var_slot}\n" +                # Push current i
            "pushi 1\n" +                               # Push 1
            "add\n" +                                   # i + 1
            f"storeg {loop_var_slot}\n" +               # Store back to i: i := i + 1
            f"jump {loop_label}\n" +                    # Jump back to loop start
            f"{end_label}\n"                            # Loop end label
        )

def p_expression_boolean(p):
    """expression : TRUE
                  | FALSE"""
    p[0] = (f"pushi {1 if p[1].lower() == 'true' else 0}\n", "boolean") 

def p_expression_logical(p):
    """expression : expression AND expression
                  | expression OR expression"""
    # EWVM AND/OR pop numeric values, 0 is false, non-zero is true.
    # The EWVM manual for AND: "Pops a numeric value a and a numeric value b from the Operand Stack and pushes the result of a && b."
    # The EWVM manual for OR: "Pops a numeric value a and a numeric value b from the Operand Stack and pushes the result of a || b." (manual says "a b", assuming "a || b")
    # The VM seems to handle the boolean logic for these directly.
    op_code = p[2].upper() # Use AND, OR directly as instruction names
    left_code, _ = p[1] 
    right_code, _ = p[3] 
    p[0] = (left_code + right_code + op_code + "\n", "boolean")

def p_expression_relop(p):
    """expression : expression LT expression
                  | expression LE expression
                  | expression GT expression
                  | expression GE expression
                  | expression EQ expression
                  | expression NEQ expression"""
    op_map = { '<': 'inf', '<=': 'infeq', '>': 'sup', '>=': 'supeq', '=': 'equal', '<>': 'equal\nnot\n'} # NEQ is 'equal' then 'not'
    # Check if VM has separate float comparison, e.g. FINF, FINFEQ
    # For now, assuming integer operations. Type promotion would be needed for mixed int/real.
    
    left_code, left_type = p[1]
    right_code, right_type = p[3]
    
    # Basic type promotion for comparison (integer vs float)
    # This assumes comparison ops (inf, infeq etc.) work on floats if operands are floats
    # Or specific float versions like FINF, FINFEQ exist and should be chosen.
    # For now, if one is real, convert int to real using ITOF.
    op_instruction = op_map[p[2]]
    final_code = ""

    if left_type == "real" and right_type == "integer":
        final_code = left_code + right_code + "itof\n"
        if op_instruction in ['inf', 'infeq', 'sup', 'supeq']: op_instruction = 'f' + op_instruction # e.g. finf
    elif left_type == "integer" and right_type == "real":
        final_code = left_code + "itof\n" + right_code
        if op_instruction in ['inf', 'infeq', 'sup', 'supeq']: op_instruction = 'f' + op_instruction
    elif left_type == "real" and right_type == "real":
        final_code = left_code + right_code
        if op_instruction in ['inf', 'infeq', 'sup', 'supeq']: op_instruction = 'f' + op_instruction
    else: # both integer
        final_code = left_code + right_code
    
    if p[2] == '<>': # NEQ specific: result of EQUAL, then NOT
         p[0] = (final_code + "equal\nnot\n", "boolean")
    else:
         p[0] = (final_code + op_instruction + "\n", "boolean")


def p_expression_paren(p):
    """expression : LPAREN expression RPAREN"""
    p[0] = p[2]

def p_expression_div(p): 
    """expression : expression DIV expression""" # Pascal DIV is integer division
    left_code, left_type = p[1] 
    right_code, right_type = p[3]
    # Ensure both are integers for DIV, or handle type errors/conversions
    if left_type == "real" or right_type == "real":
        print("Aviso: Operador DIV usado com operandos reais. Considerar conversão ou erro.")
        # Potentially convert to integer: final_left = left_code + ("ftoi\n" if left_type=="real" else "") ...
    p[0] = (left_code + right_code + "div\n", "integer")

def p_expression_mod(p):
    """expression : expression MOD expression"""
    left_code, _ = p[1] 
    right_code, _ = p[3] 
    p[0] = (left_code + right_code + "mod\n", "integer")

def p_statement_compound(p):
    """statement_compound : BEGIN statements END"""
    p[0] = p[2] 

def p_if_statement(p):
    """if_statement : IF expression THEN statement %prec IFX
                    | IF expression THEN statement ELSE statement"""
    cond_code, _ = p[2] 
    then_statement_code = p[4]
    
    label_num = generate_unique_label_num()

    if len(p) == 5:  
        label_end = f"ifend{label_num}"
        p[0] = (
            cond_code +
            f"jz {label_end}\n" + 
            then_statement_code +
            f"{label_end}\n" 
        )
    else:  
        else_statement_code = p[6]
        label_else = f"ifelse{label_num}"
        label_end = f"ifend{label_num}"
        p[0] = (
            cond_code +
            f"jz {label_else}\n" + 
            then_statement_code +
            f"jump {label_end}\n" + 
            f"{label_else}\n" + 
            else_statement_code +
            f"{label_end}\n"  
        )

def p_while_statement(p):
    """while_statement : WHILE expression DO statement"""
    cond_code, _ = p[2] 
    body_code = p[4]
    
    label_num = generate_unique_label_num()
    start_label = f"whilestart{label_num}" 
    end_label = f"whileend{label_num}"   
    
    p[0] = (
        f"{start_label}\n" +       
        cond_code +                 
        f"jz {end_label}\n" +       
        body_code +                 
        f"jump {start_label}\n" +   
        f"{end_label}\n"           
    )

def p_expression_binop(p):
    """expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression""" 
    
    left_code, left_type = p[1] 
    right_code, right_type = p[3] 
    
    op_char = p[2]
    op_code = ""
    result_type = "integer" # Default

    # Determine operation and result type based on operand types
    if left_type == "real" or right_type == "real":
        result_type = "real"
        # Ensure both operands are float for float operations
        final_left_code = left_code + ("itof\n" if left_type == "integer" else "")
        final_right_code = right_code + ("itof\n" if right_type == "integer" else "")
        
        op_map_float = { '+': 'fadd', '-': 'fsub', '*': 'fmul', '/': 'fdiv'}
        op_code = op_map_float.get(op_char)
        if not op_code: # Should not happen if ops are standard
            print(f"Erro: Operador float desconhecido '{op_char}'")
            parser_success = False
            p[0] = ("", "real")
            return
    else: # Both are integer
        result_type = "integer"
        final_left_code = left_code
        final_right_code = right_code
        op_map_int = { '+': 'add', '-': 'sub', '*': 'mul', '/': 'div'} # Pascal '/' on integers is real division
        if op_char == '/': # Integer operands but '/' operator means real division
            final_left_code += "itof\n"
            final_right_code += "itof\n"
            op_code = "fdiv" # Use float division
            result_type = "real"
        else:
            op_code = op_map_int.get(op_char)

        if not op_code:
            print(f"Erro: Operador inteiro desconhecido '{op_char}'")
            parser_success = False
            p[0] = ("", "integer")
            return

    p[0] = (final_left_code + final_right_code + op_code + "\n", result_type)


def p_expression_from_variable(p): 
    """expression : variable"""
    var_info = p[1]
    
    if var_info.get('type') == 'error':
        p[0] = ("", "integer") 
        return

    if var_info['type'] == 'simple':
        var_name = var_info['name']
        if var_name not in parser_var: 
            p[0] = ("", "integer") 
        else:
            basetype_str = var_info.get('basetype', "integer") 
            p[0] = (f"pushg {parser_var[var_name]}\n", basetype_str)
    
    elif var_info['type'] == 'indexed':
        array_name = var_info['name']
        index_code = var_info['index_code'] # Code that pushes index_value
        element_basetype = var_info.get('basetype', "integer") 
        array_slot = parser_var[array_name]
        low_bound = var_info.get('low_bound', 1) # Get low_bound
        
        # Code to load:
        # Stack target for LOADN: ..., address_a, offset_o
        code = "pushgp\n"                             # Stack: GP_addr
        code += f"pushi {array_slot}\n"             # Stack: GP_addr, slot_offset_of_array_start
        code += "add\n"                             # Stack: base_address_a (Addr_array_start)
        
        code += index_code                          # Stack: base_address_a, index_value
        code += f"pushi {low_bound}\n"              # Stack: base_address_a, index_value, low_bound
        code += "sub\n"                             # Stack: base_address_a, 0_based_offset_o
                                                    # Assuming element size 1 for now
        code += "loadn\n"                           # Pops o, pops a, pushes value_at(a+o)
        p[0] = (code, element_basetype)
    else:
        p[0] = ("", "integer")

def p_expression_number(p): 
    "expression : NUMBER"
    p[0] = (f"pushi {str(p[1])}\n", "integer")

def p_expression_real(p): 
    "expression : REAL"
    p[0] = (f"pushf {str(p[1])}\n", "real")

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

parser = yacc.yacc(debug=True)

# Main execution block (example)
if __name__ == "__main__":
    # Determine input file: use 'input.txt' if no argument, else use argument
    input_filename = 'input4.txt'
    if len(sys.argv) > 1:
        input_filename = sys.argv[1]
    
    try:
        with open(input_filename, 'r', encoding='utf-8') as file:
            source = file.read()
    except FileNotFoundError:
        print(f"Erro: Ficheiro de entrada '{input_filename}' não encontrado.")
        sys.exit(1)

    codigo = parser.parse(source)

    if parser_success and codigo is not None:
        try:
            with open('output.txt', 'w', encoding='utf-8') as f:
                # Initialize global variables area on stack
                # This should reflect the *maximum* number of global slots used,
                # including those for array descriptors and temporary compiler variables like loop limits.
                if parser_var_count > 0:
                    f.write(f"pushn {parser_var_count}\n") # Push n nil (0) values [cite: 1, 71]
                
                f.write(codigo)
            print(f"Parsing completado com sucesso!")
        except IOError:
            print("Erro: Não foi possível escrever no ficheiro 'output.txt'.")
    else:
        print('Parsing falhou!')