import unittest
from tools import FuncTestCase, ANY
from websocket import WebSocketTimeoutException


class TestBasicInteraction(FuncTestCase):

    def runTest(self):
        c1 = self.client()
        self.assertEqual('Echo: foo', c1.req("echo", "foo"))
        c1.send("login", "zozo")
        self.assertEqual(['init', None], c1.recv())
        exp = ['user.init', {'userId': "user-id:zozo",
                        'name': "zozo",
                        'areas': ["alpha", "beta"]}]
        self.assertEqual(exp, c1.recv())
        exp = ['area.init', {'areaId': 'alpha', 'timestamp': ANY}]
        self.assertEqual(exp, c1.recv())
        self.assertEqual('alpha echo: bar', c1.req("area.echo", "bar"))
        exp = ["area.tick",
               {"objects": [{"actions": [],
                             "durability": 100,
                             "angle": 0.0,
                             "pos": [10, 10],
                             "id": "user-id:zozo"}],
                "events":[],
                "areaId": "alpha",
                "timestamp": ANY}]
        self.assertEqual(exp, c1.recv())
        c1.send("area.move-to", [-1000, -1000])
        c1.send("area.move-to", [14, 20]) #cancel the previous action
        exp_actions = [{'endTs': ANY,
                        'from': [10, 10],
                        'startTs': ANY,
                        'tag': 'MoveDistance',
                        'to': [14, 20]}]
        exp = ['area.tick',
               {'events': [],
                "areaId": "alpha",
                'timestamp': ANY,
                'objects': [{'actions': exp_actions,
                             'durability': 100,
                             'angle': 68.19859,
                             'id': 'user-id:zozo',
                             'pos': [12, 16]}]}]
        self.assertEqual(exp, c1.recv())
        exp = ['area.tick',
               {'events': [],
                "areaId": "alpha",
                'timestamp': ANY,
                'objects': [{'actions': [],
                             'durability': 100,
                             'angle': 68.19859,
                             'id': 'user-id:zozo',
                             'pos': [14, 20]}]}]
        self.assertEqual(exp, c1.recv())
        c2 = self.client()
        c2.send("login", "dede")
        exp =  ['area.init',
                {'areaId': 'alpha',
                'timestamp': ANY}]
        self.assertEqual(exp, c2.recv("area.init"))
        exp = [{'id': 'user-id:dede',
                'name': 'dede',
                'angle': 0,
                'durability': 100,
                'pos': [10, 10],
                'tag': 'User'},
               {'id': 'user-id:zozo',
                'name': 'zozo',
                'angle': 68.19859,
                'durability': 100,
                'pos': [14, 20],
                'tag': 'User'}]
        self.assertEqual(exp, c1.req("area.get-objects-info",
                                     ['user-id:zozo', 'user-id:dede']))
        c1.send("area.ignite", 40)
        exp_actions = [{'damageSpeed': 40,
                        'previousTs': ANY,
                        'tag': 'Burning'}]
        exp = ['area.tick',
               {'events': [],
                "areaId": "alpha",
                'timestamp': ANY,
                'objects': [{'actions': [],
                             'durability': 100,
                             'id': 'user-id:dede',
                             'angle': 0,
                             'pos': [10, 10]},
                            {'actions': exp_actions,
                             'durability': 62,
                             'angle': 68.19859,
                             'id': 'user-id:zozo',
                             'pos': [14, 20]}]}]
        self.assertEqual(exp, c1.recv())
        self.assertEqual(exp, c2.recv())
        exp = ['area.tick',
               {'events': [],
                "areaId": "alpha",
                'timestamp': ANY,
                'objects': [{'actions': [],
                             'durability': 100,
                             'angle': 0,
                             'id': 'user-id:dede',
                             'pos': [10, 10]},
                            {'actions': exp_actions,
                             'durability': 22,
                             'angle': 68.19859,
                             'id': 'user-id:zozo',
                             'pos': [14, 20]}]}]
        self.assertEqual(exp, c1.recv())
        self.assertEqual(exp, c2.recv())
        exp = ['area.tick',
               {'events': [{'ident': 'user-id:zozo', 'tag': 'DeleteUser'}],
                "areaId": "alpha",
                'timestamp': ANY,
                'objects': [{'actions': [],
                             'durability': 100,
                             'angle': 0,
                             'id': 'user-id:dede',
                             'pos': [10, 10]}]}]
        self.assertEqual(exp, c2.recv())
        exp = [{'id': 'user-id:dede',
                'name': 'dede',
                'angle': 0,
                'durability': 100,
                'pos': [10, 10],
                'tag': 'User'}]
        self.assertEqual(exp, c2.req("area.get-objects-info",
                                     ['user-id:zozo', 'user-id:dede']))
        with self.assertRaises(WebSocketTimeoutException):
            c1.recv()


if __name__ == '__main__':
    unittest.main()
