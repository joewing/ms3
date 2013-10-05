
import lex

def parse(lexer, constructors):
   lexer.match(lex.TOKEN_OPEN)
   name = lexer.get_value()
   lexer.match(lex.TOKEN_LITERAL)
   if name in constructors:
      args = parse_arguments(lexer, constructors)
      c = constructors[name]
      result = c(args)
   else:
      raise lex.ParseError("invalid component: " + name)
   lexer.match(lex.TOKEN_CLOSE)
   return result

def parse_arguments(lexer, constructors = None):
   result = dict()
   while lexer.get_type() == lex.TOKEN_OPEN:
      lexer.match(lex.TOKEN_OPEN)
      name = lexer.get_value()
      lexer.match(lex.TOKEN_LITERAL)
      if lexer.get_type() == lex.TOKEN_LITERAL:
         value = lexer.get_value()
         lexer.match(lex.TOKEN_LITERAL)
      else:
         value = parse(lexer, constructors)
      result[name] = value
      lexer.match(lex.TOKEN_CLOSE)
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
            raise lex.ParseError("invalid boolean value: '" + value + "'")
      else:
         return args[name]
   else:
      return default

