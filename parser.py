
import lex

def parse(lexer, constructors):
   """Parse key-value pairs using the specified constructors.
      lexer should be an instance of Lexer.
      constructors should be a dictionary that maps names to
      functions taking a dictionary of arguments as a parameter.
   """
   lexer.match(lex.TOKEN_OPEN)
   name = lexer.get_value()
   lexer.match(lex.TOKEN_LITERAL)
   if name in constructors:
      args = parse_arguments(lexer, constructors)
      c = constructors[name]
      result = c(args)
   elif name == "include":
      value = lexer.get_value()
      lexer.match(lex.TOKEN_LITERAL)
      result = parse(lex.Lexer(open(value, "r")), constructors)
   else:
      raise lex.ParseError("invalid component: " + name)
   lexer.match(lex.TOKEN_CLOSE)
   return result

def parse_arguments(lexer, constructors = None):
   """Parse arguments to a constructor.
      lexer is the Lexer instance to use.
      constructors is a dictionary of contructors to use (see parse).
   """
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
   """Get an argument from a dictionary of arguments.
      args should be a dictionary of arguments returned from parse_arguments.
      name is the name of the argument.
      default is the default value (which also determines the argument type).
   """
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

