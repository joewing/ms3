
import Lexer

def parse(lexer, constructors, state = None):
   lexer.match(Lexer.TOKEN_OPEN)
   name = lexer.get_value()
   lexer.match(Lexer.TOKEN_LITERAL)
   if name in constructors:
      args = parse_arguments(lexer, constructors, state)
      c = constructors[name]
      result = c(state, args)
   else:
      raise Lexer.ParseError("invalid component: " + name)
   lexer.match(Lexer.TOKEN_CLOSE)
   return result

def parse_arguments(lexer, constructors = None, state = None):
   result = dict()
   while lexer.get_type() == Lexer.TOKEN_OPEN:
      lexer.match(Lexer.TOKEN_OPEN)
      name = lexer.get_value()
      lexer.match(Lexer.TOKEN_LITERAL)
      if lexer.get_type() == Lexer.TOKEN_LITERAL:
         value = lexer.get_value()
         lexer.match(Lexer.TOKEN_LITERAL)
      else:
         value = parse(lexer, constructors, state)
      result[name] = value
      lexer.match(Lexer.TOKEN_CLOSE)
   return result

def get_argument(args, name, default = None):
   if name in args:
      value = args[name]
      if isinstance(default, int):
         return int(value)
      elif isinstance(default, float):
         return float(value)
      elif isinstance(default, bool):
         if value == 'true':
            return True
         elif value == 'false':
            return False
         else:
            raise Lexer.ParseError("invalid boolean value: '" + value + "'")
      else:
         return args[name]
   else:
      return default

