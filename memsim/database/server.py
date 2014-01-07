
from memsim.database.shared import SharedDatabase


class DatabaseServer(object):

    def __init__(self, manager, db, update_status):
        self.queues = []
        self.manager = manager
        self.db = db
        self.update_status = update_status
        self.request_count = 0
        self.free = []

    def add_client(self, name):
        if self.free:
            key = self.free.pop()
            request_queue, response_queue = self.queues[key]
        else:
            request_queue = self.manager.Queue(1)
            response_queue = self.manager.Queue(1)
            temp = request_queue, response_queue
            key = len(self.queues)
            self.queues.append(temp)
        return SharedDatabase(key, name, request_queue, response_queue)

    def remove_client(self, key):
        self.free.append(key)

    def process(self, ident, request_queue, response_queue):
        self.request_count += 1
        request = request_queue.get()
        response = None
        name = request[0]
        needs_response = request[1]
        args = request[2]
        if name == 'update_status':
            self.update_status(ident, *args)
        else:
            func = getattr(self.db, name)
            response = func(*args)
        if needs_response:
            response_queue.put(response)

    def run(self):
        got_data = False
        for i in xrange(len(self.queues)):
            request_queue, response_queue = self.queues[i]
            if not request_queue.empty():
                self.process(i, request_queue, response_queue)
                got_data = True
        return got_data
