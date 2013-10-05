
TOKEN_INVALID  = '?'
TOKEN_OPEN     = '('
TOKEN_CLOSE    = ')'
TOKEN_LITERAL  = 'literal'
TOKEN_EOF      = ''

def isspace(ch):
	return ch == ' ' or ch == '\n' or ch == '\t' or ch == '\r'

def isend(ch):
   return isspace(ch) or ch == '(' or ch == ')' or ch == ''

class ParseError(Exception):
   def __init__(self, msg):
      print "ParseError: " + msg
      self.message = msg

class Lexer:

   def __init__(self, f):
      self.f = f
      self.last = None
      self.read_next()

   def get_type(self):
      return self.current[0]

   def get_value(self):
      return self.current[1]

   def match(self, t):
      actual = self.get_type()
      if t == actual:
         self.read_next();
      else:
         raise ParseError("unexpected token: got '" + actual +
                          "' expected '" + t + "'")

   def read_next(self):
      if self.last == None:
         ch = self.f.read(1)
      else:
         ch = self.last
         self.last = None
      if ch == '':
         self.current = (TOKEN_EOF, '')
      elif ch == ';':
         self.last = ch
         while self.last != '\n' and self.last != '':
            self.last = self.f.read(1)
         self.read_next()
      elif ch == TOKEN_OPEN:
         self.current = (TOKEN_OPEN, '')
      elif ch == TOKEN_CLOSE:
         self.current = (TOKEN_CLOSE, '')
      elif isspace(ch):
         return self.read_next()
      else:
         temp = ch
         while True:
            ch = self.f.read(1)
            if isend(ch):
               self.last = ch
               break;
            temp += ch
         self.current = (TOKEN_LITERAL, temp)

