
import multiprocessing
import time

class DatabaseServer(object):

    def __init__(self, db):
        self.queues = []
        self.db = db

    def add_client(self):
        request_queue = multiprocessing.Queue(2)
        response_queue = multiprocessing.Queue(2)
        result = request_queue, response_queue
        self.queues.append(result)
        return result

    def process(self, request_queue, response_queue):
        if request_queue.empty():
            return False
        if response_queue.full():
            return False
        request = request_queue.get()
        func = getattr(self, request[0])
        args = request[1]
        response = func(self.db, *args)
        response_queue.put(response)
        return True

    def run(self):
        while True:
            got_data = False
            for queue in self.queues:
                if self.process(queue[0], queue[1]):
                    got_data = True
            if not got_data:
                time.sleep(1)
