from random import randint

from kivy.clock import Clock
from kivy.logger import Logger as log
from network import Connection
from mediator import Mediator

CHECK_PERIOD = 1. / 200. # seconds
INPUT_DELAY = 400 # milliseconds


class Controller(object):

    def __init__(self):
        self._conn = None
        self._request_counter = 1

    def _send(self, cmd, body, request_number=0):
        #request_number == 0 means no request
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
        if not INPUT_DELAY:
            Clock.schedule_interval(self.check_inbox, CHECK_PERIOD)
        else:
            Clock.schedule_once(self.delayed_check_inbox, CHECK_PERIOD)
        Mediator.subscribe(self, "connect")
        Mediator.subscribe(self, "send")
        Mediator.subscribe(self, "request")

    def deactivate(self):
        if not INPUT_DELAY:
            Clock.unschedule(self.check_inbox)
        else:
            Clock.unschedule(self.delayed_check_inbox)
        Mediator.unsubscribe_object(self)

    def connect(self, address, username):
        self._conn = Connection(address)
        self._conn.connect(wait=True)
        Mediator.publish("connected", username)

    def delayed_check_inbox(self, dt):
        self.check_inbox(None)
        period = CHECK_PERIOD
        if randint(0, 3) == 0:
            period += INPUT_DELAY / 1000.
        Clock.schedule_once(self.delayed_check_inbox, period)

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
            Mediator.publish(*args)
            data = conn.receive()



