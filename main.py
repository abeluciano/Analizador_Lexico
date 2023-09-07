# ------------------------------------------------------------
# calclex.py
#
# tokenizer for a simple expression evaluator for
# numbers and +,-,*,/
# ------------------------------------------------------------
import ply.lex as lex

# r'atring' -> r significa que la cadena es tradada sin caracteres de escape,
# es decir r'\n' seria un \ seguido de n (no se interpretaria como salto de linea)

# List of token names.   This is always required
reserved = {
    'int': 'TYPO_INT',
    'bool': 'TYPO_BOOL',
    'real': 'TYPO_REAL',
    'char': 'TYPO_CHAR',
    'string': 'TYPO_CADENA',
    'wl': 'WHILE',
    'for': 'FOR',
    'if': 'IF',
    'els': 'ELSE',
    'void': 'TIPO_VOID',
    'rt': 'RETURN',
    'swch': 'SWITCH',
    'cs': 'CASE',
    'bkr': 'BREAK',
    'ctn': 'CONTINUE',
    'true': 'TRUE',
    'false': 'FALSE',
    'func': 'FUNCION',
    'in': 'ENTRADA',
    'out': 'SALIDA',
    'of': 'DE',
}

tokens = [
    'NUMBER', 'REAL', 'MAS', 'MENOS', 'MULTI', 'DIVIDE', 'ASIG', 'LPAREN', 'RPAREN',
    'MOD', 'ID', 'CARACTER', 'CADENA', 'INCRE', 'DECRE', 'SUMASIG', 'MODASIG', 'IGUAL', 
    'DIFE', 'MENOR', 'MAYOR', 'MENORIGUAL', 'MAYORIGUAL', 'AND', 'OR', 'LKEY', 'RKEY' ,
    'MULASIG', 'DIVASIG', 'RESTASIG', 'DOSP', 'COMA', 'COMENTARIO'
] + list(reserved.values())

# Regular expression rules for simple tokens
t_MAS = r'\+'
t_MENOS = r'-'
t_MULTI = r'\*'
t_DIVIDE = r'/'
t_ASIG = r'\='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_MOD = r'\%'
t_INCRE = r'\++'
t_DECRE = r'\--'
t_SUMASIG = r'\+='
t_RESTASIG = r'-\='
t_MULASIG = r'\*\='
t_DIVASIG = r'\/\='
t_MODASIG = r'\%\='
t_IGUAL = r'\=='
t_DIFE = r'\!='
t_MENOR = r'\<'
t_MAYOR = r'\>'
t_MENORIGUAL = r'\<='
t_MAYORIGUAL = r'\>='
t_AND = r'\&\&'
t_OR = r'\|\|'
t_LKEY = r'\{'
t_RKEY = r'\}'
t_DOSP = r':'
t_COMA = r','
#t_NUMBER  = r'\d+'


def t_REAL(t):
  r'\d+(\.\d+)'
  t.value = float(t.value)  # Check for reserved words
  return t

def t_ID(t):
  r'[a-zA-Z]+ ( [a-zA-Z0-9]* )'
  t.type = reserved.get(t.value, 'ID')  # Check for reserved words
  return t

def t_COMENTARIO(t):
  r'\/\/.*|\/\*[\s\S]*\*\/'
  return t

def t_CADENA(t):
  r'".*"'
  t.value = str(t.value)  # Check for reserved words
  return t

# A regular expression rule with some action code


def t_NUMBER(t):
  r'\d+'
  t.value = int(t.value)  # guardamos el valor del lexema
  #print("se reconocio el numero")
  return t

def t_CARACTER(t):
  r'\'.\''
  t.value = str(t.value)  # Check for reserved words
  return t

# Define a rule so we can track line numbers
def t_newline(t):
  r'\n+'
  t.lexer.lineno += len(t.value)


# A string containing ignored characters (spaces and tabs)
t_ignore = ' \t'


# Error handling rule
def t_error(t):
  print("Illegal character '%s'" % t.value[0])
  t.lexer.skip(1)


# Build the lexer
lexer = lex.lex()

# Test it out
data = '''
/**/
func: pepeL(int a != 3.14){
  for: 1 of 4
    out: "Hola mundo"
}
'''

# Give the lexer some input
lexer.input(data)

# Tokenize
while True:
  tok = lexer.token()
  if not tok:
    break  # No more input
  #print(tok)
  print(tok.type, tok.value)
