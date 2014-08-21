
class _ConfigurableProxy(object):

    def __init__(self, cls):
        self._cls_ = cls
        self._obj_ = None

    def configure(self, obj):
        cls = self._cls_
        if not isinstance(obj, cls):
            raise TypeError("Object must be an instance of " + cls.__name__)
        if self._obj_ is not None:
            raise ValueError("Object is already configured")
        self._obj_ = obj

    def __getattribute__(self, name):
        try:
            return super(_ConfigurableProxy, self).__getattribute__(name)
        except AttributeError:
            pass
        if self._obj_ is None:
            raise ValueError("Object is not configured")
        return getattr(self._obj_, name)


configurable = _ConfigurableProxy

