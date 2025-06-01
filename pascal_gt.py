import sys
import ply.yacc as yacc
from pascal_lex import tokens

parser = None
parser_success = True
parser_functions = {}
parser_params = {}
parser_var = {}
parser_var_count = 0 
parser_var_types = {}
label_seq_num = 0 

def generate_unique_label_num():
    global label_seq_num
    label_seq_num += 1
    return label_seq_num

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
            
            if isinstance(type_representation, str) and type_representation.startswith("array["):
                try:
                    range_part = type_representation.split('[')[1].split(']')[0] 
                    low_bound_str, high_bound_str = range_part.split('..')
                    low = int(low_bound_str)
                    high = int(high_bound_str)
                    size = high - low + 1
                    parser_var_count +=size 
                except:
                    print(f"Aviso: Não foi possível determinar o tamanho para o array {var_name}. A contagem de vars pode estar incorreta.")
                    parser_var_count += 1 
            else:
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
        var_name = p[1]
        index_expr_code, index_expr_type = p[3]

        if index_expr_type != 'integer':
            print(f"Erro: Índice para '{var_name}' deve ser um inteiro, mas foi {index_expr_type}.")
            parser_success = False
            p[0] = {'type': 'error', 'name': var_name, 'basetype': 'unknown'}
            return

        if var_name not in parser_var:
            print(f"Erro: Variável '{var_name}' não declarada.")
            parser_success = False
            p[0] = {'type': 'error', 'name': var_name, 'basetype': 'unknown'}
            return

        var_actual_type = parser_var_types.get(var_name)

        if isinstance(var_actual_type, str) and var_actual_type.startswith("array[") and "_of_" in var_actual_type:
            element_basetype = var_actual_type.split("_of_")[-1]
            low_bound_val = 1 
            try:
                range_part = var_actual_type.split('[')[1].split(']')[0]
                low_bound_str = range_part.split('..')[0]
                low_bound_val = int(low_bound_str)
            except (IndexError, ValueError):
                print(f"Aviso: Formato de tipo array inválido ou não foi possível extrair limite inferior de '{var_actual_type}' para '{var_name}'. Assumindo 1.")
            
            p[0] = {
                'type': 'indexed_array', 
                'name': var_name,
                'index_code': index_expr_code,
                'basetype': element_basetype, 
                'low_bound': low_bound_val
            }
        elif var_actual_type == "string":
            p[0] = {
                'type': 'indexed_string_char',
                'name': var_name,
                'index_code': index_expr_code,
                'basetype': 'char' 
            }
        else:
            print(f"Erro: Variável '{var_name}' do tipo '{var_actual_type}' não pode ser indexada (não é array nem string).")
            parser_success = False
            p[0] = {'type': 'error', 'name': var_name, 'basetype': 'unknown'}
            return
        
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
    full_code = f"{name}:\n{param_code}{local_code}{body_code}RETURN\n" 
    parser_functions[name] = full_code
    parser_params[name] = param_code.count("STOREL")
    p[0] = ""

def p_param_list_single(p):
    "param_list : ID COLON type"
    p[0] = "storel 0\n" 

def p_param_list_multiple(p):
    "param_list : param_list SEMI ID COLON type"
    idx = p[1].count("storel") 
    p[0] = p[1] + f"storel {idx}\n" 

def p_argument_list_single(p):
    "argument_list : expression"
    expr_val = p[1]
    p[0] = expr_val[0] if isinstance(expr_val, tuple) else expr_val 

def p_argument_list_multiple(p):
    "argument_list : argument_list COMMA expression"
    new_expr_code = p[3][0] if isinstance(p[3], tuple) else p[3]
    p[0] = p[1] + new_expr_code


def p_expression_function_call(p):
    """expression : ID LPAREN argument_list RPAREN
                  | ID LPAREN RPAREN""" 
    global parser_success, parser_functions 
    
    fname = p[1].lower() 
    
    if len(p) == 5: 
        args_code = p[3] 
    else: 
        args_code = ""

    if fname == "length":
        if not args_code:
            print(f"Erro: Função 'length' requer um argumento string.")
            parser_success = False
            p[0] = ("", "integer")
            return
        p[0] = (args_code + "STRLEN\n", "integer")
    else:
        if fname not in parser_functions:
            print(f"Erro: Função '{p[1]}' não declarada.")
            parser_success = False
            p[0] = ("", "integer")
            return
        
        p[0] = (args_code + f"pusha {fname}\ncall\n", "integer")

def p_statements(p):
    """statements : statement_sequence""" 
    p[0] = "".join(p[1]) 

def p_statement_sequence(p):
    """statement_sequence : statement
                          | statement_sequence SEMI statement"""
    if len(p) == 2:  
        if p[1] is not None and p[1] != "":  
            p[0] = [p[1]]
        else:
            p[0] = [] 
    else:  
        current_stmt_code = p[3]
        if current_stmt_code is not None and current_stmt_code != "": 
            p[0] = p[1] + [current_stmt_code]
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
        low_bound = lhs_var_info.get('low_bound', 1)
        
        addr_calc_code = "pushgp\n"                             
        addr_calc_code += f"pushi {array_slot}\n"             
        addr_calc_code += "padd\n"                            
        
        addr_calc_code += index_code                          
        addr_calc_code += f"pushi {low_bound}\n"              
        addr_calc_code += "sub\n"                             
        
        p[0] = addr_calc_code + final_rhs_code + "storen\n" 
    else:
        p[0] = ""

def p_writeln_statement(p):
    """writeln_statement : WRITELN LPAREN writelist RPAREN""" 
    p[0] = p[3] + "writeln\n" 

def p_write_statement(p):
    """write_statement : WRITE LPAREN writelist RPAREN""" 
    p[0] = p[3]

def p_writelist(p):
    """writelist : writelist COMMA writeitem
                 | writeitem"""
    p[0] = p[1] + p[3] if len(p) == 4 else p[1]

def p_writeitem_expr(p):
    """writeitem : expression"""
    code, expr_type = p[1]
    if expr_type == "string": 
        p[0] = code + "writes\n"
    elif expr_type == "real":
        p[0] = code + "writef\n"
    elif expr_type == "boolean": 
        p[0] = code + "writei\n"
    elif expr_type == "integer": 
        p[0] = code + "writei\n"
    else:
        print(f"Aviso: Tipo de expressão desconhecido '{expr_type}' em p_writeitem_expr. Usando writei por defeito.")
        p[0] = code + "writei\n" 

def p_readln_statement(p):
    """readln_statement : READLN LPAREN variable RPAREN"""
    global parser_success, parser_var, parser_var_types 
    var_info = p[3]

    if var_info.get('type') == 'error':
        parser_success = False
        p[0] = ""
        return

    addr_calc_code = ""
    is_indexed = False
    if var_info['type'] == 'indexed_array':
        is_indexed = True
        array_name = var_info['name']
        index_code_for_addr = var_info.get('index_code', "")
        array_slot = parser_var[array_name]
        low_bound = var_info.get('low_bound', 1)

        addr_calc_code += "pushgp\n"                     
        addr_calc_code += f"pushi {array_slot}\n"       
        addr_calc_code += "padd\n"                      
        addr_calc_code += index_code_for_addr 
        addr_calc_code += f"pushi {low_bound}\n"        
        addr_calc_code += "sub\n" 

    base_read_code = "read\n" 
    conversion_code = ""
    actual_target_type = var_info.get('basetype') 

    if actual_target_type == 'real':
        conversion_code = "atof\n" 
    elif actual_target_type == 'integer' or actual_target_type == 'boolean': 
        conversion_code = "atoi\n" 
    elif actual_target_type == 'string':
        conversion_code = "" 
    else:
        print(f"Erro: Tipo de variável desconhecido ou não suportado '{actual_target_type}' para READLN.")
        parser_success = False
        p[0] = ""
        return

    full_read_and_convert_code = base_read_code + conversion_code

    if not is_indexed and var_info['type'] == 'simple':
        var_name = var_info['name']
        if var_name not in parser_var:
            print(f"Erro: Variável '{var_name}' não declarada para READLN.")
            parser_success = False
            p[0] = ""
            return
        p[0] = full_read_and_convert_code + f"storeg {parser_var[var_name]}\n"
    elif is_indexed:
        p[0] = addr_calc_code + full_read_and_convert_code + "storen\n" 
    else:
        print(f"Erro: Tipo de variável não suportado '{var_info['type']}' para atribuição em READLN.")
        parser_success = False
        p[0] = ""

def p_for_statement(p):
    """for_statement : FOR ID ASSIGN expression TO expression DO statement
                     | FOR ID ASSIGN expression DOWNTO expression DO statement""" 
    global parser_success, parser_var_count, parser_var
    
    loop_var_name = p[2]
    if loop_var_name not in parser_var:
        print(f"Erro: variável de ciclo '{loop_var_name}' não declarada.")
        parser_success = False
        p[0] = ""
        return

    loop_var_slot = parser_var[loop_var_name]
    
    limit_storage_slot = parser_var_count 
    parser_var_count += 1 

    init_expr_code, init_expr_type = p[4] 
    limit_expr_code, limit_expr_type = p[6] 
    body_code = p[8] 


    label_num = generate_unique_label_num()
    loop_label = f"forloop{label_num}"
    end_label = f"forend{label_num}"
    
    direction_token_type = p.slice[5].type 

    comparison_instruction = ""
    step_instruction = ""

    if direction_token_type == 'TO':
        comparison_instruction = "infeq"  
        step_instruction = "add"         
    elif direction_token_type == 'DOWNTO':
        comparison_instruction = "supeq" 
        step_instruction = "sub"        
    else:
        print(f"Erro interno: token de direção desconhecido '{direction_token_type}' no loop FOR.")
        parser_success = False
        p[0] = ""
        return
        
    p[0] = (
        init_expr_code +                            
        f"storeg {loop_var_slot}\n" +               
        limit_expr_code +                           
        f"storeg {limit_storage_slot}\n" +     
        f"{loop_label}:\n" +                        
        f"pushg {loop_var_slot}\n" +                
        f"pushg {limit_storage_slot}\n" +          
        f"{comparison_instruction}\n" +            
        f"jz {end_label}\n" +                      
        body_code +                                
        f"pushg {loop_var_slot}\n" +               
        "pushi 1\n" +                              
        f"{step_instruction}\n" +                  
        f"storeg {loop_var_slot}\n" +              
        f"jump {loop_label}\n" +                   
        f"{end_label}:\n"                          
    )

def p_expression_boolean(p):
    """expression : TRUE
                  | FALSE"""
    p[0] = (f"pushi {1 if p[1].lower() == 'true' else 0}\n", "boolean") 

def p_expression_logical(p):
    """expression : expression AND expression
                  | expression OR expression"""
    op_code = p[2].upper() 
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
    global parser_success 

    op_map_int = { '<': 'inf', '<=': 'infeq', '>': 'sup', '>=': 'supeq', '=': 'equal'}
    op_map_float = { '<': 'finf', '<=': 'finfeq', '>': 'fsup', '>=': 'fsupeq', '=': 'equal'}
    
    left_code, left_type = p[1]
    right_code, right_type = p[3]
    operator_symbol = p[2]
    
    final_code_parts = []
    op_instruction = ""
    result_type = "boolean"

    if (left_type == "integer" and right_type == "string") or \
       (left_type == "string" and right_type == "integer"):
        
        int_expr_code = ""
        str_expr_code = ""

        if left_type == "integer":
            int_expr_code = left_code
            str_expr_code = right_code 
        else:
            int_expr_code = right_code
            str_expr_code = left_code  

        final_code_parts.append(int_expr_code)
        final_code_parts.append(str_expr_code)
        final_code_parts.append("CHRCODE\n") 

        if operator_symbol == '=':
            op_instruction = "equal\n"
        elif operator_symbol == '<>':
            op_instruction = "equal\nnot\n"
        else:
            print(f"Erro: Comparação relacional '{operator_symbol}' entre um char (integer) e uma string não é suportada. Apenas '=' e '<>'.")
            parser_success = False
            p[0] = ("", result_type)
            return
        
        final_code_parts.append(op_instruction)
        p[0] = ("".join(final_code_parts), result_type)
        return

    if operator_symbol == '<>':
        if left_type == "real" or right_type == "real":
            final_left = left_code + ("itof\n" if left_type == "integer" else "")
            final_right = right_code + ("itof\n" if right_type == "integer" else "")
            p[0] = (final_left + final_right + "equal\nnot\n", result_type)
        else: 
            p[0] = (left_code + right_code + "equal\nnot\n", result_type)
        return

    if left_type == "real" or right_type == "real":
        final_left_code = left_code + ("itof\n" if left_type == "integer" else "")
        final_right_code = right_code + ("itof\n" if right_type == "integer" else "")
        final_code_parts.append(final_left_code)
        final_code_parts.append(final_right_code)
        op_instruction = op_map_float.get(operator_symbol)
        if not op_instruction and operator_symbol == '=': 
            op_instruction = op_map_int.get(operator_symbol) 
    else: 
        final_code_parts.append(left_code)
        final_code_parts.append(right_code)
        op_instruction = op_map_int.get(operator_symbol)
            
    if not op_instruction:
        print(f"Erro: Operador relacional '{operator_symbol}' não suportado para os tipos {left_type} e {right_type}.")
        parser_success = False
        p[0] = ("", result_type)
        return
        
    final_code_parts.append(op_instruction + "\n")
    p[0] = ("".join(final_code_parts), result_type)


def p_expression_paren(p):
    """expression : LPAREN expression RPAREN"""
    p[0] = p[2]

def p_expression_div(p): 
    """expression : expression DIV expression""" 
    left_code, left_type = p[1] 
    right_code, right_type = p[3]
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
            f"{label_end}:\n"
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
            f"{label_else}:\n" + 
            else_statement_code +
            f"{label_end}:\n"   
        )

def p_while_statement(p):
    """while_statement : WHILE expression DO statement"""
    cond_code, _ = p[2] 
    body_code = p[4]
    
    label_num = generate_unique_label_num()
    start_label = f"whilestart{label_num}" 
    end_label = f"whileend{label_num}"   
    
    p[0] = (
        f"{start_label}:\n" +     
        cond_code +                 
        f"jz {end_label}\n" +       
        body_code +                 
        f"jump {start_label}\n" +   
        f"{end_label}:\n"          
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
    result_type = "integer" 

    if op_char == '/':
        final_left_code = left_code + ("itof\n" if left_type == "integer" else "")
        final_right_code = right_code + ("itof\n" if right_type == "integer" else "")
        op_code = "fdiv" 
        result_type = "real"
    elif left_type == "real" or right_type == "real": 
        result_type = "real"
        final_left_code = left_code + ("itof\n" if left_type == "integer" else "")
        final_right_code = right_code + ("itof\n" if right_type == "integer" else "")
        op_map_float = { '+': 'fadd', '-': 'fsub', '*': 'fmul'}
        op_code = op_map_float.get(op_char)
    else: 
        result_type = "integer"
        final_left_code = left_code
        final_right_code = right_code
        op_map_int = { '+': 'add', '-': 'sub', '*': 'mul'}
        op_code = op_map_int.get(op_char)

    if not op_code:
        print(f"Erro: Operador binário desconhecido ou tipo incompatível '{op_char}' para tipos {left_type}, {right_type}")
        parser_success = False
        p[0] = ("", result_type) 
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
            print(f"Erro: Variável '{var_name}' não declarada (em p_expression_from_variable).")
            parser_success = False
            p[0] = ("", "integer")
            return
        basetype_str = var_info.get('basetype', "integer")
        p[0] = (f"pushg {parser_var[var_name]}\n", basetype_str)

    elif var_info['type'] == 'indexed_array': 
        array_name = var_info['name']
        index_code = var_info['index_code'] 
        element_basetype = var_info.get('basetype', "integer")
        array_slot = parser_var[array_name]
        low_bound = var_info.get('low_bound', 1)

        code = "pushgp\n"
        code += f"pushi {array_slot}\n"
        code += "padd\n"
        code += index_code 
        code += f"pushi {low_bound}\n"
        code += "sub\n" 
        code += "loadn\n" 
        p[0] = (code, element_basetype)

    elif var_info['type'] == 'indexed_string_char':
        string_var_name = var_info['name']
        index_code = var_info['index_code'] 

        if string_var_name not in parser_var:
            print(f"Erro: Variável string '{string_var_name}' não encontrada.")
            parser_success = False
            p[0] = ("", "integer") 
            return

        string_slot = parser_var[string_var_name]
        gvm_code = f"pushg {string_slot}\n"  
        gvm_code += index_code             
        gvm_code += "pushi 1\nsub\n"        
        gvm_code += "CHARAT\n"              
        
        p[0] = (gvm_code, "integer")

    else:
        print(f"Erro: Tipo de variável desconhecido ou não suportado em p_expression_from_variable: {var_info.get('type')}")
        parser_success = False
        p[0] = ("", "integer")

def p_expression_from_literal_string(p):
    """expression : STRING_LITERAL"""
    val = p[1]
    p[0] = (f"pushs \"{val}\"\n", "string")

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

if __name__ == "__main__":
    input_filename = 'inputs/input4.txt'
    if len(sys.argv) > 1:
        input_filename = sys.argv[1]
    
    try:
        with open(input_filename, 'r', encoding='utf-8') as file:
            source = file.read()
    except FileNotFoundError:
        print(f"Erro: Ficheiro de entrada '{input_filename}' não encontrado.")
        sys.exit(1)

    parser_success = True
    parser_functions.clear()
    parser_params.clear()
    parser_var.clear()
    parser_var_count = 0
    parser_var_types.clear()
    label_seq_num = 0
    
    codigo = parser.parse(source)

    if parser_success and codigo is not None:
        output_filename = 'output.txt'
        try:
            with open(output_filename, 'w', encoding='utf-8') as f:
                if parser_var_count > 0:
                    f.write(f"pushn {parser_var_count}\n") 
                
                f.write(codigo)
            print(f"Parsing completado com sucesso!")
        except IOError:
            print(f"Erro: Não foi possível escrever no ficheiro '{output_filename}'.")
    else:
        print('Parsing falhou!')