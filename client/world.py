from kivy.lang import Builder
from kivy.properties import ListProperty, NumericProperty, StringProperty
from kivy.uix.widget import Widget
from kivy.vector import Vector
from kivy.animation import Animation
from kivy.core.window import Window
from kivy.logger import Logger as log
from mediator import Mediator


class World(object):

    def __init__(self, parent, username):
        Builder.load_file('world.kv', rulesonly=True)
        self._widget = w = Widget()
        self._background_layer = background = Widget()
        main_pos = Window.width / 2, Window.height / 2
        self._main_layer = main = WorldLayer(pos=main_pos)
        self._uid = username
        self._objects = {}
        w.add_widget(background, 0)
        w.add_widget(main, 1)


        Mediator.subscribe(self, 'dispatch', 'recv.area')
        Mediator.subscribe(self, 'move_to', 'world.move_to')
        Mediator.subscribe(self, 'focus', 'world.focus')
        parent.add_widget(self._widget, 0)

    def deactivate(self):
        Mediator.unsubscribe_object(self)
        self._widget.parent.remove_widget(self._widget)

    def dispatch(self, cmd, body):
        getattr(self, "handle_" + cmd)(body)

    def _add_object(self, ident, data=None, index=0):
        tag = data['tag']
        if tag in ("User",):
            obj = Unit()
        else:
            raise ValueError("Unknown type '{}'".format(type))
        self._objects[ident] = obj
        self._main_layer.add_widget(obj, index)
        if tag in ("User",):
            obj.rel_x, obj.rel_y = data['pos']
            set_rel_pos(self.world_pos, obj)
            #obj.angle = data['angle']
        if tag == "User":
            obj.name = data['name']

    def _remove_object(self, ident):
        obj = self._objects.pop(ident)
        self._widget.remove_widget(obj)

    def handle_init(self, data):
        self._background_layer.add_widget(Background(x=0, y=0))

    @property
    def world_pos(self):
        return self._main_layer.pos

    def objects_info(self, objects):
        for data in objects:
            ident = data['id']
            if ident in self._objects:
                continue
            self._add_object(ident, data)

    def handle_tick(self, data):
        objects = self._objects
        unknown_objects = set()
        idents = set()
        pos = self.world_pos
        for value in data['objects']:
            ident = value['id']
            idents.add(ident)
            if ident not in objects:
                unknown_objects.add(ident)
                continue
            obj = objects[ident]
            obj.rel_x, obj.rel_y = value['pos']
            set_rel_pos(pos, obj)
        if unknown_objects:
            Mediator.publish("request",
                             "area.get_objects_info",
                             list(unknown_objects),
                             (self, 'objects_info'))
        for ident in set(objects) - idents:
            self._remove_object(ident)

    def focus(self, x, y):
        #TODO: use Vector
        layer = self._main_layer
        layer.x -= x - Window.width / 2
        layer.y -= y - Window.height / 2

    def move_to(self, x, y):
        wx, wy = self.world_pos
        Mediator.publish("send", "area.move_to", [x - wx, y - wy])


class Background(Widget):
    pass

class WorldLayer(Widget):

    def on_pos(self, _, pos):
        for ch in self.children:
            set_rel_pos(pos, ch)


class WorldObject(Widget):
    rel_x = NumericProperty(0)
    rel_y = NumericProperty(0)


class UnitImage(Widget):
    angle = NumericProperty(0)


class Unit(WorldObject):
    name = StringProperty()
    angle = NumericProperty(0)


def set_rel_pos((x, y), obj):
    obj.center_x = x + obj.rel_x
    obj.center_y = y + obj.rel_y
