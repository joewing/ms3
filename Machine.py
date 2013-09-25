
class MachineType:
   def __init__(self, word_size=8, addr_bits=32):
      self.word_size = word_size
      self.word_mask = (1 << word_size) - 1
      self.addr_bits = addr_bits
      self.addr_mask = (1 << addr_bits) - 1
      self.time = 0

def create_access(is_write, address, size=0, cycles=0):
   return (is_write, address, size, cycles)

def is_write(access):
   return access[0]

def get_address(access):
   return access[1]

def get_size(access):
   return access[2]

def get_cycles(access):
   return access[3]

def clone_access(access, address=-1, size=-1, cycles=-1):
   if address < 0: address = access[1]
   if size < 0: size = access[2]
   if cycles < 0: cycles = access[3]
   return (access[0], address, size, cycles)

