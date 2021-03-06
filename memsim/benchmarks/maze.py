from __future__ import print_function
import random

from memsim import parser
from memsim.benchmarks import base


class Maze(base.Benchmark):

    def __init__(self, index, width, height, seed):
        base.Benchmark.__init__(self, index, word_size=1)
        self.width = width * 2 + 1
        self.height = height * 2 + 1
        self.seed = seed

    def __str__(self):
        result = '(maze '
        result += '(id ' + str(self.index) + ')'
        result += '(seed ' + str(self.seed) + ')'
        result += '(width ' + str(self.width // 2) + ')'
        result += '(height ' + str(self.height // 2) + ')'
        result += ')'
        return result

    def set(self, maze, x, y, value):
        index = y * self.width + x
        maze[index] = value
        return self.write(index)

    def clear(self, maze):
        for x in xrange(self.width):
            yield self.set(maze, x, 0, 0)
            yield self.set(maze, x, self.height - 1, 0)
        for y in xrange(1, self.height - 1):
            yield self.set(maze, 0, y, 0)
            for x in xrange(1, self.width - 1):
                yield self.set(maze, x, y, 1)
            yield self.set(maze, self.width - 1, y, 0)

    def carve(self, rand, maze):
        stack_offset = self.width * self.width
        stack_offset += 4 - stack_offset % 4
        item_size = 4 * 3
        dirs = [(1, 0), (-1, 0), (0, 1), (0, -1)]
        stack = [(2, 2, 0), (2, 2, 2)]
        yield self.write(stack_offset)
        stack_offset += item_size
        while len(stack) > 0:
            stack_offset -= item_size
            yield self.read(stack_offset)
            x, y, d = stack.pop()
            yield self.set(maze, x, y, 0)
            dx, dy = dirs[d & 3]
            nx, ny, nx2, ny2 = x + dx, y + dy, x + 2 * dx, y + 2 * dy
            yield self.read(ny * self.width + nx)
            if maze[ny * self.width + nx] == 1:
                yield self.read(ny2 * self.width + nx2)
                if maze[ny2 * self.width + nx2] == 1:
                    yield self.set(maze, nx, ny, 0)
                    d = rand.randint(0, 3)
                    for i in xrange(4):
                        yield self.write(stack_offset)
                        stack_offset += item_size
                        stack.append((nx2, ny2, (d + i) & 3))

    def show(self, maze):
        for y in xrange(self.height):
            line = ''
            for x in xrange(self.width):
                if maze[y * self.width + x] == 0:
                    line += "[]"
                else:
                    line += "  "
            print(line)

    def run(self):
        rand = random.Random(self.seed)
        maze = [0] * (self.width * self.height)
        for a in self.clear(maze):
            yield a
        for a in self.carve(rand, maze):
            yield a
        #self.show(maze)


def _create_maze(lexer, args):
    index = parser.get_argument(lexer, args, 'id', 0)
    width = parser.get_argument(lexer, args, 'width', 32)
    height = parser.get_argument(lexer, args, 'height', 32)
    seed = parser.get_argument(lexer, args, 'seed', 5)
    return Maze(index, width, height, seed)
base.constructors['maze'] = _create_maze
