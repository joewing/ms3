
class TargetType:
   SIMPLE = 0
   ASIC = 1

def parse_target(s):
   if s == 'simple':
      return TargetType.SIMPLE
   elif s == 'asic':
      return TargetType.ASIC
   else:
      return None

class MachineType:
   def __init__(self,
                target = TargetType.SIMPLE,
                word_size = 8,
                addr_bits = 32):
      self.target = target
      self.word_size = word_size
      self.word_bits = log2(self.word_size)
      self.word_mask = word_size - 1
      self.addr_bits = addr_bits
      self.addr_mask = (1 << addr_bits) - 1
      self.time = 0

   def reset(self):
      self.time = 0
      for i in range(len(self.ports)):
         self.ports[i] = 0

   def produce(self, port):
      while len(self.ports) <= port:
         self.ports.append(0)
      self.ports[port] += 1
      return True

   def consume(self, port):
      while len(self.ports) <= port:
         self.ports.append(0)
      if self.ports[port] == 0:
         return False
      else:
         self.ports[port] -= 1
         return True

   def flip(self, value):
      """Reverse the bits in an addr_bits sized value."""
      src_mask = 1 << (self.addr_bits - 1)
      dest_mask = 1
      result = 0
      for i in range(self.addr_bits):
         if (value & src_mask) != 0:
            result |= dest_mask
         src_mask >>= 1
         dest_mask <<= 1
      return result

def log2(n):
   r = 1
   while n > 0:
      r += 1
      n >>= 2
   return r

