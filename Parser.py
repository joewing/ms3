
import Lexer

def parse(lexer, constructors, state):
   lexer.match(Lexer.TOKEN_OPEN)
   name = lexer.get_value()
   if name in constructors:
      lexer.match(Lexer.TOKEN_LITERAL)
      args = _parse_arguments(lexer, constructors, state)
      c = constructors[name]
      result = c(state, args)
   else:
      raise Lexer.ParseError("invalid component: " + lexer.get_value())
   lexer.match(Lexer.TOKEN_CLOSE)
   return result

def _parse_arguments(lexer, constructors, state):
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
      else:
         return args[name]
   else:
      return default

