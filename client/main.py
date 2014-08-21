import os
import json
import logging

from kivy.app import App
from kivy.uix.widget import Widget
from kivy.uix.boxlayout import BoxLayout
from kivy.properties import ObjectProperty
from kivy.clock import Clock
from kivy.logger import Logger

from world import World
from controller import Controller
from mediator import Mediator, BaseMediator


class StartMenu(BoxLayout):

    def connect(self, address, username):
        Mediator.publish("connect", address, username)


class UI(Widget):

    def on_touch_down(self, touch):
        if touch.button == "left":
            Mediator.publish("world.move_to", touch.x, touch.y)
        else:
            Mediator.publish("world.focus", touch.x, touch.y)


class GameWidget(Widget):

    ui = ObjectProperty(None)

    def __init__(self, name, *args, **kwargs):
        super(GameWidget, self).__init__(*args, **kwargs)
        self.world = World(self, name)


class MainWidget(Widget):

    start_menu = ObjectProperty(None)
    game_widget = ObjectProperty(None)

    def __init__(self, *args, **kwargs):
        super(MainWidget, self).__init__(*args, **kwargs)
        Mediator.subscribe(self, "connected")

    def connected(self, username):
        self.remove_widget(self.start_menu)
        self.game_widget = GameWidget(username)
        self.add_widget(self.game_widget)
        Mediator.publish("send", "login", username)


class GameApp(App):

    def build(self):
        return MainWidget()


if __name__ == '__main__':
    Logger.setLevel(logging.INFO)
    start_args = None
    if 'TRI_START_ARGS' in os.environ:
        with open(os.environ['TRI_START_ARGS']) as f:
            start_args = json.load(f)
            address, username = start_args['address'], start_args['username']
    #cleanup
    start_args = dict(address='ws://127.0.0.1:10501', username="anonymous")
    if start_args is not None:
        f = lambda dt: Mediator.publish("connect", **start_args)
        Clock.schedule_once(f, 0)
    Mediator.configure(BaseMediator())
    controller = Controller()
    controller.activate()
    GameApp(start_args=start_args).run()


