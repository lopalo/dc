import os
import signal
import subprocess
import unittest
import time
import json

from websocket import create_connection

SERVER_DIR = "../../src"
SERVER_NAME = "Main.hs"
SERVER_OUTPUT_DIR = "logs"
TIMEOUT = 1.5
URL = "ws://127.0.0.1:10501"


class _Any(object):

    def __eq__(self, other): return True


ANY = _Any()

class Client(object):

    def __init__(self):
        self._request_counter = 1
        self._conn = create_connection(URL, TIMEOUT)

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


class FuncTestCase(unittest.TestCase):
    maxDiff = None
    _server_proc = None
    _server_stdout = None


    def setUp(self):
        super(FuncTestCase, self).setUp()
        self._clients = []
        fname = "{}/{}.log".format(SERVER_OUTPUT_DIR, self.__class__.__name__)
        self._server_stdout = stdout = open(fname, "wb")
        self._server_proc = subprocess.Popen(['runghc', SERVER_NAME],
                                             cwd=SERVER_DIR,
                                             stdin=subprocess.PIPE,
                                             stdout=stdout,
                                             stderr=stdout,
                                             preexec_fn=os.setsid)
        time.sleep(2)

    def tearDown(self):
        stdout = self._server_stdout
        self._server_stdout = None
        stdout.flush()
        stdout.close()
        time.sleep(.1)
        s = self._server_proc
        self._server_proc = None
        os.killpg(s.pid, signal.SIGTERM)
        s.wait()
        for c in self._clients:
            c.close()
        self._clients = None

    def client(self):
        c = Client()
        self._clients.append(c)
        return c

