
TOKEN_INVALID  = '?'
TOKEN_OPEN     = '('
TOKEN_CLOSE    = ')'
TOKEN_LITERAL  = 'literal'
TOKEN_EOF      = 'EOF'

def isspace(ch):

class Lexer:

   def __init__(self, f):
      self.f = f
      self.current = (TOKEN_INVALID, '')
      self.last = ''

   def get_type(self):
      return self.current[0]

   def get_value(self):
      return self.current[1]

   def match(self, t):
      actual = self.get_type()
      if t == actual:
         self.read_next();
      else:
         raise ParseError("unexpected token: got " + actual + " expected " + t)

   def read_next(self):
      if self.last == '':
         ch = self.f.read(1)
      else:
         ch = self.last
      if ch == '':
         self.current = (TOKEN_EOF, '')
      elif ch == TOKEN_OPEN:
         self.current = (TOKEN_OPEN, '')
      elif ch == TOKEN_CLOSE:
         self.current = (TOKEN_CLOSE, '')
      elif ch == ' '
      else:
         temp = ch
         while True:
            ch = self.f.read(1)
            if ch == TOKEN_EOF or ch == TOKEN_OPEN or ch == TOKEN_CLOSE:
               self.last = ch
               break;
            elif ch == ' ' or ch == "\n" or ch == "\t" or ch == "\r":
               break;

