
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

   def __init__(self, lexer, msg):
      self.fname = lexer.f.name
      self.line = lexer.line
      self.message = msg

   def __str__(self):
      return self.fname + "[" + str(self.line) + "]: " + self.message

class Lexer:
   """Lexer for parsing s-expressions."""

   def __init__(self, f):
      """Initialize a lexer using file f."""
      self.f = f
      self.last = None
      self.line = 1
      self._read_next()

   def get_type(self):
      """Get the type of the current token."""
      return self.current[0]

   def get_value(self):
      """Get the value of the current token (for literals)."""
      return self.current[1]

   def match(self, t):
      """Match the current token and move to the next.
         If the current token doesn't match t, ParseError is raised.
      """
      actual = self.get_type()
      if t == actual:
         self._read_next();
      else:
         raise ParseError(self, "unexpected token: got '" + actual +
                                "' expected '" + t + "'")

   def _read_next(self):
      """Read the next token."""
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
         if self.last == '\n': self.line += 1
         self._read_next()
      elif ch == TOKEN_OPEN:
         self.current = (TOKEN_OPEN, '')
      elif ch == TOKEN_CLOSE:
         self.current = (TOKEN_CLOSE, '')
      elif isspace(ch):
         if ch == '\n': self.line += 1
         return self._read_next()
      else:
         temp = ch
         while True:
            ch = self.f.read(1)
            if isend(ch):
               self.last = ch
               break;
            temp += ch
         self.current = (TOKEN_LITERAL, temp)

