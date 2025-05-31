import ply.lex as lex
import re

tokens = [
    'AND',
    'ARRAY',
    'ASSIGN',
    'BEGIN',
    'BOOLEAN',
    'CASE',
    'COLON',
    'COMMA',
    'CONST',
    'DIV',
    'DIVIDE',
    'DO',
    'DOT',
    'DOTDOT',
    'DOWNTO',
    'ELSE',
    'END',
    'EQ',
    'FALSE',
    'FILE',
    'FOR',
    'FUNCTION',
    'GE',
    'GOTO',
    'GT',
    'ID',
    'IF',
    'IN',
    'INTEGER',
    'LABEL',
    'LBRACKET',
    'LE',
    'LPAREN',
    'LT',
    'MINUS',
    'MOD',
    'NEQ',
    'NIL',
    'NOT',
    'NUMBER',
    'OF',
    'OR',
    'PACKED',
    'PLUS',
    'PROCEDURE',
    'PROGRAM',
    'RBRACKET',
    'READ',
    'READLN',
    'REAL',
    'RECORD',
    'REPEAT',
    'RPAREN',
    'SEMI',
    'SET',
    'STRING', 
    'STRING_LITERAL',
    'THEN',
    'TIMES',
    'TO',
    'TRUE',
    'TYPE',
    'UNTIL',
    'VAR',
    'WHILE',
    'WITH',
    'WRITE',
    'WRITELN'
]

t_ASSIGN = r':='
t_EQ = r'='
t_NEQ = r'<>'
t_LE = r'<='
t_GE = r'>='
t_LT = r'<'
t_GT = r'>'
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_SEMI = r';'
t_COLON = r':'
t_COMMA = r','
t_DOTDOT = r'\.\.'
t_DOT = r'\.'

def t_REAL(t):
    r'\d+\.\d+([eE][+-]?\d+)?'
    t.value = float(t.value)
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRING_LITERAL(t):
    r"'([^']|'')*'"
    t.value = t.value[1:-1].replace("''", "'")
    return t

def t_STRING(t):
    r'\bstring\b'
    t.type = 'STRING'
    return t

def t_AND(t):
    r'\band\b'
    t.type = 'AND'
    return t

def t_ARRAY(t):
    r'\barray\b'
    t.type = 'ARRAY'
    return t

def t_BEGIN(t):
    r'\bbegin\b'
    t.type = 'BEGIN'
    return t

def t_CASE(t):
    r'\bcase\b'
    t.type = 'CASE'
    return t

def t_CONST(t):
    r'\bconst\b'
    t.type = 'CONST'
    return t

def t_DIV(t):
    r'\bdiv\b'
    t.type = 'DIV'
    return t

def t_DO(t):
    r'\bdo\b'
    t.type = 'DO'
    return t

def t_DOWNTO(t):
    r'\bdownto\b'
    t.type = 'DOWNTO'
    return t

def t_ELSE(t):
    r'\belse\b'
    t.type = 'ELSE'
    return t

def t_END(t):
    r'\bend\b'
    t.type = 'END'
    return t

def t_FILE(t):
    r'\bfile\b'
    t.type = 'FILE'
    return t

def t_FOR(t):
    r'\bfor\b'
    t.type = 'FOR'
    return t

def t_FUNCTION(t):
    r'\bfunction\b'
    t.type = 'FUNCTION'
    return t

def t_GOTO(t):
    r'\bgoto\b'
    t.type = 'GOTO'
    return t

def t_IF(t):
    r'\bif\b'
    t.type = 'IF'
    return t

def t_IN(t):
    r'\bin\b'
    t.type = 'IN'
    return t

def t_LABEL(t):
    r'\blabel\b'
    t.type = 'LABEL'
    return t

def t_MOD(t):
    r'\bmod\b'
    t.type = 'MOD'
    return t

def t_NIL(t):
    r'\bnil\b'
    t.type = 'NIL'
    return t

def t_NOT(t):
    r'\bnot\b'
    t.type = 'NOT'
    return t

def t_OF(t):
    r'\bof\b'
    t.type = 'OF'
    return t

def t_OR(t):
    r'\bor\b'
    t.type = 'OR'
    return t

def t_PACKED(t):
    r'\bpacked\b'
    t.type = 'PACKED'
    return t

def t_PROCEDURE(t):
    r'\bprocedure\b'
    t.type = 'PROCEDURE'
    return t

def t_PROGRAM(t):
    r'\bprogram\b'
    t.type = 'PROGRAM'
    return t

def t_RECORD(t):
    r'\brecord\b'
    t.type = 'RECORD'
    return t

def t_REPEAT(t):
    r'\brepeat\b'
    t.type = 'REPEAT'
    return t

def t_SET(t):
    r'\bset\b'
    t.type = 'SET'
    return t

def t_THEN(t):
    r'\bthen\b'
    t.type = 'THEN'
    return t

def t_TO(t):
    r'\bto\b'
    t.type = 'TO'
    return t

def t_TYPE(t):
    r'\btype\b'
    t.type = 'TYPE'
    return t

def t_UNTIL(t):
    r'\buntil\b'
    t.type = 'UNTIL'
    return t

def t_VAR(t):
    r'\bvar\b'
    t.type = 'VAR'
    return t

def t_WHILE(t):
    r'\bwhile\b'
    t.type = 'WHILE'
    return t

def t_WITH(t):
    r'\bwith\b'
    t.type = 'WITH'
    return t

def t_INTEGER(t):
    r'\binteger\b'
    t.type = 'INTEGER'
    return t

def t_BOOLEAN(t):
    r'\bboolean\b'
    t.type = 'BOOLEAN'
    return t

def t_WRITELN(t):
    r'\bwriteln\b'
    t.type = 'WRITELN'
    return t

def t_WRITE(t):
    r'\bwrite\b'
    t.type = 'WRITE'
    return t

def t_READLN(t):
    r'\breadln\b'
    t.type = 'READLN'
    return t

def t_READ(t):
    r'\bread\b'
    t.type = 'READ'
    return t

def t_TRUE(t):
    r'\btrue\b'
    t.type = 'TRUE'
    return t

def t_FALSE(t):
    r'\bfalse\b'
    t.type = 'FALSE'
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    return t

t_ignore = ' \t\r'

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_comment_braces(t):
    r'\{[^}]*\}'
    pass

def t_comment_parens(t):
    r'\(\*[^*]*\*+([^)*][^*]*\*+)*\)'
    pass

def t_error(t):
    print(f"Caracter ilegal: {t.value[0]}")
    t.lexer.skip(1)

lexer = lex.lex(reflags=re.IGNORECASE)

def main():
    import sys
    if len(sys.argv) < 2:
        print("Uso: python pascal_lex.py <ficheiro_pascal>")
        return
    with open(sys.argv[1], encoding="utf-8") as f:
        data = f.read()
    lexer.input(data)
    for token in lexer:
        print(f"{token.type}({token.value}) na linha {token.lineno}")

if __name__ == "__main__":
    main()