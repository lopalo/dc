import weakref
from types import NoneType
from collections import defaultdict
from utils import configurable
from kivy.logger import Logger as log


class BaseMediator(object):

    def __init__(self):
        self._subscribers = defaultdict(list)

    def subscribe(self, obj, method_name, message=None, once=False):
        assert isinstance(method_name, str)
        assert isinstance(message, (str, NoneType))
        message = message or method_name
        meth = getattr(obj, method_name)
        if not callable(meth):
            err = "Method '{}' of the instance of '{}' is not callable"
            raise ValueError(err.format(method_name, obj.__class__.__name__))
        handler = (weakref.ref(obj), method_name, once)
        self._subscribers[message].append(handler)

    def publish(self, msg, *args, **kwargs):
        subscribers = self._subscribers[msg]
        new_subscribers = self._subscribers[msg] = []
        for handler in subscribers:
            ref, method_name, once = handler
            obj = ref()
            if obj is None:
                continue
            if not once:
                new_subscribers.append(handler)
            try:
                getattr(obj, method_name)(*args, **kwargs)
            except Exception:
                log.exception('Exception occurred in a function call')

    def unsubscribe(self, obj, method_name=None, message=None):
        message = message or method_name
        if message is None:
            subscribers_lst = self._subscribers.values()
        else:
            subscribers_lst = [self._subscribers[message]]
        for subscribers in subscribers_lst:
            new_subscribers = []
            for handler in subscribers:
                ref, _method_name, _ = handler
                _obj = ref()
                if _obj is None:
                    continue
                if _obj is obj:
                    if method_name is None:
                        continue
                    elif _method_name == method_name:
                        continue
                new_subscribers.append(handler)
            subscribers[:] = new_subscribers

    def unsubscribe_object(self, obj, message=None):
        self.unsubscribe(obj, None, message)


Mediator = configurable(BaseMediator)


