from time import time

from kivy.clock import Clock
from kivy.vector import Vector
from kivy.lang import Builder
from kivy.properties import (
    NumericProperty, StringProperty,
    ReferenceListProperty)
from kivy.uix.widget import Widget
from kivy.animation import Animation
from kivy.core.window import Window
from mediator import Mediator


class World(object):

    def __init__(self, parent, username):
        Builder.load_file('world.kv', rulesonly=True)
        self._widget = w = Widget()
        self._background_layer = background = Widget()
        main_pos = Window.width / 2, Window.height / 2
        self._main_layer = main = WorldLayer(pos=main_pos)
        self._area_id = None
        self._server_time_diff = None
        self._uid = username
        self._objects = {}
        w.add_widget(background, 0)
        w.add_widget(main, 1)

        Mediator.subscribe(self, 'dispatch', 'recv.area')
        Mediator.subscribe(self, 'move_to', 'world.move_to')
        Mediator.subscribe(self, 'move_focus', 'world.move_focus')
        parent.add_widget(self._widget, 0)

    @property
    def time(self):
        #TODO: use CLOCK_MONOTONIC
        return time()

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
        obj.world_pos = self.world_pos
        obj.rel_pos = data['pos']
        if tag in ("User",):
            obj.angle = data['angle']
        if tag == "User":
            obj.name = data['name']

    def _remove_object(self, ident):
        obj = self._objects.pop(ident)
        self._widget.remove_widget(obj)

    def handle_init(self, data):
        self._area_id = data['areaId']
        self._server_time_diff = self.time - data['timestamp'] / 1000.
        self._background_layer.add_widget(Background(x=0, y=0))

    @property
    def server_time(self):
        return self.time - self._server_time_diff

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
        #TODO: skip it if an area ident is wrong
        if self._area_id is None:
            return
        tick_time = data['timestamp'] / 1000.
        if tick_time > self.server_time:
            to_delay = tick_time - self.server_time
            hd = lambda dt: self._handle_tick(data)
            Clock.schedule_once(hd, to_delay)
        else:
            self._handle_tick(data)

    def _handle_tick(self, data):
        if self._area_id is None:
            return
        server_ts = self.server_time * 1000
        tick_ts = data['timestamp']
        delay = 0
        if tick_ts < server_ts:
            delay = server_ts - tick_ts
            tick_ts = server_ts
        objects = self._objects
        unknown_objects = set()
        idents = set()
        for value in data['objects']:
            ident = value['id']
            idents.add(ident)
            if ident not in objects:
                unknown_objects.add(ident)
                continue
            obj = objects[ident]
            obj.rel_pos = value['pos']
            obj.angle = value['angle']
            if isinstance(obj, Active):
                obj.apply_actions(tick_ts, delay, value['actions'])
        if unknown_objects:
            Mediator.publish("request",
                             "area.get_objects_info",
                             list(unknown_objects),
                             (self, 'objects_info'))
        for ident in set(objects) - idents:
            self._remove_object(ident)

    def move_focus(self, dx, dy):
        layer = self._main_layer
        layer.x += dx
        layer.y += dy

    def move_to(self, x, y):
        wx, wy = self.world_pos
        Mediator.publish("send", "area.move_to", [x - wx, y - wy])


class Background(Widget):
    pass

class WorldLayer(Widget):

    def on_pos(self, _, pos):
        for ch in self.children:
            ch.world_pos = pos


class WorldObject(Widget):
    world_x = NumericProperty(0)
    world_y = NumericProperty(0)
    world_pos = ReferenceListProperty(world_x, world_y)

    rel_x = NumericProperty(0)
    rel_y = NumericProperty(0)
    rel_pos = ReferenceListProperty(rel_x, rel_y)


class Active(WorldObject):

    def apply_actions(self, tick_ts, delay, actions):
        #TODO: remove the delay parameter
        raise NotImplementedError()


class Unit(Active):
    name = StringProperty()
    angle = NumericProperty(0)

    def apply_actions(self, tick_ts, delay, actions):
        #TODO: remove the delay parameter
        for action in actions:
            tag = action['tag']
            if tag == "MoveDistance":
                self._apply_move_distance(tick_ts, delay, action)
            else:
                raise ValueError("Unknown action '{}'".format(tag))

    def _apply_move_distance(self, tick_ts, delay, action):
        #TODO: remove the delay parameter
        Animation.cancel_all(self, 'rel_x', 'rel_y')
        f = float(delay) / (action['endTs'] - action['startTs'])
        to_pos = tx, ty = Vector(action['to'])
        if tick_ts > action['endTs']:
            self.rel_pos = to_pos
            return
        from_pos = Vector(action['from'])
        dpos = (to_pos - from_pos) * f
        self.rel_pos = Vector(self.rel_pos) + dpos
        duration = (action['endTs'] - tick_ts) / 1000.
        Animation(duration=duration, rel_x=tx, rel_y=ty).start(self)


