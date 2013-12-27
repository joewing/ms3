
from multiprocessing.queues import SimpleQueue

from memsim.database.shared import SharedDatabase


class DatabaseServer(object):

    def __init__(self, db, update_status, signal_exit):
        self.queues = []
        self.db = db
        self.update_status = update_status
        self.signal_exit = signal_exit

    def add_client(self, name):
        request_queue = SimpleQueue()
        response_queue = SimpleQueue()
        result = request_queue, response_queue
        self.queues.append(result)
        return SharedDatabase(name, request_queue, response_queue)

    def process(self, ident, request_queue, response_queue):
        if request_queue.empty():
            return False
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
