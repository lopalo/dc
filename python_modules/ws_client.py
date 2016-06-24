import json
from websocket import create_connection


class Client(object):

    def __init__(self, url, timeout):
        self._request_counter = 1
        self._conn = create_connection(url, timeout)

    def send(self, cmd, body):
        self._conn.send(json.dumps([cmd, body, 0]))

    def req(self, cmd, body):
        count = self._request_counter
        self._conn.send(json.dumps([cmd, body, count]))
        self._request_counter += 1
        return self.recv('response:' + str(count))[1]

    def recv(self, exp_cmd=None):
        res = (cmd, body) = json.loads(self._conn.recv())
        if exp_cmd is None:
            return res
        if exp_cmd == cmd:
            return res
        return self.recv(exp_cmd)

    def close(self):
        self._conn.close()

    def settimeout(self, timeout):
        return self._conn.settimeout(timeout)

