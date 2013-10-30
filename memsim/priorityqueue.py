
class BinaryHeap:
    def __init__(self):
        self.heap = [0]
        self.size = 0

    def push(self, key, value):
        self.size += 1
        if len(self.heap) <= self.size:
            self.heap.append((key, value))
        else:
            self.heap[self.size] = (key, value)
        i = self.size
        while i > 1:
            ni = i >> 1
            if self.heap[i][0] < self.heap[ni][0]:
                self._swap(i, ni)
                i = ni
            else:
                break

    def key(self):
        return self.heap[1][0]

    def value(self):
        return self.heap[1][1]

    def empty(self):
        return self.size == 0

    def reset(self):
        self.size = 0

    def get_values(self):
        result = []
        for i in range(1, self.size + 1):
            result.append(self.heap[i][1])
        return result

    def _swap(self, a, b):
        self.heap[a], self.heap[b] = self.heap[b], self.heap[a]

    def pop(self):
        self.heap[1] = self.heap[self.size]
        self.size -= 1
        i = 1
        while True:
            left = i << 1
            right = left + 1
            if right <= self.size:
                if self.heap[left][0] < self.heap[right][0]:
                    if self.heap[i][0] > self.heap[left][0]:
                        self._swap(i, left)
                        i = left
                    else:
                        break
                elif self.heap[i][0] > self.heap[right][0]:
                    self._swap(i, right)
                    i = right
                else:
                    break
            elif left <= self.size and self.heap[i][0] > self.heap[left][0]:
                self._swap(i, left)
                i = left
            else:
                break

# Select the priority queue implementation to use.
PriorityQueue = BinaryHeap
