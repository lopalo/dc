import os
import signal
import subprocess
import unittest
import time

from ws_client import Client


SERVER_CWD = "../server/src"
### paths are relative to the SERVER_CWD ###
SERVER_NAME = "Main.hs"
SERVER_SETTINGS = '../settings/test.yaml'
######

SERVER_OUTPUT_DIR = "logs"
TIMEOUT = 1.5
URL = "ws://127.0.0.1:11501"


class _Any(object):

    def __eq__(self, other): return True


ANY = _Any()

class SubDict(dict):

    def __eq__(self, other):
        if not isinstance(other, dict):
            return False
        for k, v in self.items():
            if k not in other:
                return False
            if v != other[k]:
                return False
        return True

subdict = SubDict


class FuncTestCase(unittest.TestCase):
    maxDiff = None
    _server_proc = None
    _server_stdout = None


    def setUp(self):
        super(FuncTestCase, self).setUp()
        self._clients = []
        fname = "{}/{}.log".format(SERVER_OUTPUT_DIR, self.__class__.__name__)
        self._server_stdout = stdout = open(fname, "wb")
        args = ['runghc', SERVER_NAME, SERVER_SETTINGS]
        self._server_proc = subprocess.Popen(args,
                                             cwd=SERVER_CWD,
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
        c = Client(URL, TIMEOUT)
        self._clients.append(c)
        return c

