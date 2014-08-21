from kivy.clock import Clock
from kivy.logger import Logger as log
from network import Connection
from mediator import Mediator

UPDATE_PERIOD = 1. / 40.
FAKE_PING = 0 # milliseconds


class Controller(object):

    def __init__(self):
        self._conn = None
        self._request_counter = 1

    def _send(self, cmd, body, request_number=0):
        #request_number == 0 means no request
        if FAKE_PING:
            cb = lambda dt: self._conn.send(cmd, body, request_number)
            Clock.schedule_once(cb, FAKE_PING / 1000. / 2)
        else:
            self._conn.send(cmd, body, request_number)
        log.debug('Message sent: %s, %s', cmd, body)

    def send(self, cmd, body):
        self._send(cmd, body, 0)

    def request(self, cmd, body, (obj, method_name)):
        num = self._request_counter
        self._request_counter += 1
        self._send(cmd, body, num)
        msg = 'recv.response:' + str(num)
        Mediator.subscribe(obj, method_name, msg, once=True)

    def activate(self):
        Clock.schedule_interval(self.check_inbox, UPDATE_PERIOD)
        Mediator.subscribe(self, "connect")
        Mediator.subscribe(self, "send")
        Mediator.subscribe(self, "request")

    def deactivate(self):
        Clock.unschedule(self.check_inbox)
        Mediator.unsubscribe_object(self)

    def connect(self, address, username):
        self._conn = Connection(address)
        self._conn.connect(wait=True)
        Mediator.publish("connected", username)

    def check_inbox(self, dt):
        conn = self._conn
        if conn is None:
            return
        data = conn.receive()
        while data is not None:
            cmd, body = data
            log.debug('Message received: %s, %s', cmd, body)
            if '.' in cmd:
                prefix, cmd = cmd.split('.')
                args = "recv." + prefix, cmd, body
            else:
                args = "recv." + cmd, body
            forward = lambda dt: Mediator.publish(*args)
            if FAKE_PING:
                Clock.schedule_once(forward, FAKE_PING / 1000. / 2)
            else:
                forward(None)
            data = conn.receive()



