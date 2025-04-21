import ply.lex as lex

reserved = {
    'and': 'AND',
    'array': 'ARRAY',
    'begin': 'BEGIN',
    'case': 'CASE',
    'const': 'CONST',
    'div': 'DIV',
    'do': 'DO',
    'downto': 'DOWNTO',
    'else': 'ELSE',
    'end': 'END',
    'file': 'FILE',
    'for': 'FOR',
    'function': 'FUNCTION',
    'goto': 'GOTO',
    'if': 'IF',
    'in': 'IN',
    'label': 'LABEL',
    'mod': 'MOD',
    'nil': 'NIL',
    'not': 'NOT',
    'of': 'OF',
    'or': 'OR',
    'packed': 'PACKED',
    'procedure': 'PROCEDURE',
    'program': 'PROGRAM',
    'record': 'RECORD',
    'repeat': 'REPEAT',
    'set': 'SET',
    'then': 'THEN',
    'to': 'TO',
    'type': 'TYPE',
    'until': 'UNTIL',
    'var': 'VAR',
    'while': 'WHILE',
    'with': 'WITH',
    'integer': 'INTEGER',
    'boolean': 'BOOLEAN',
    'string': 'STRING',
    'writeln': 'WRITELN',
    'write': 'WRITE',
    'readln': 'READLN',
    'read': 'READ',
    'true': 'TRUE',
    'false': 'FALSE'
}

tokens = [
    'ID', 'NUMBER', 'REAL', 'STRING_LITERAL',
    'ASSIGN', 'EQ', 'NEQ', 'LT', 'GT', 'LE', 'GE',
    'PLUS', 'MINUS', 'TIMES', 'DIVIDE',
    'LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET',
    'SEMI', 'COLON', 'COMMA', 'DOT', 'DOTDOT'
] + list(reserved.values())

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

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value.lower(), 'ID')
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

lexer = lex.lex()

