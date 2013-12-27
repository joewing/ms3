
from multiprocessing.queues import SimpleQueue

from memsim.database.shared import SharedDatabase


class DatabaseServer(object):

    def __init__(self, db, update_status, signal_exit):
        self.queues = []
        self.db = db
        self.update_status = update_status
        self.signal_exit = signal_exit
        self.request_count = 0
        self.free = []

    def get_client_count(self):
        return len(self.queues) - len(self.free)

    def add_client(self, name):
        if self.free:
            return self.free.pop()
        else:
            request_queue = SimpleQueue()
            response_queue = SimpleQueue()
            result = request_queue, response_queue
            self.queues.append(result)
            return SharedDatabase(name, request_queue, response_queue)

    def remove_client(self, key):
        self.free.append(key)

    def process(self, ident, request_queue, response_queue):
        if request_queue.empty():
            return False
        self.request_count += 1
        request = request_queue.get()
        response = None
        name = request[0]
        args = request[1]
        if name == 'update_status':
            self.update_status(ident, *args)
        elif name == 'signal_exit':
            self.signal_exit(ident, *args)
        else:
            func = getattr(self.db, request[0])
            args = request[1]
            response = func(*args)
        response_queue.put(response)
        return True

    def run(self):
        got_data = False
        for i in xrange(len(self.queues)):
            request_queue, response_queue = self.queues[i]
            if self.process(i, request_queue, response_queue):
                got_data = True
        return got_data
