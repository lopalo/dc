import unittest
from tools import FuncTestCase, ANY
from websocket import WebSocketTimeoutException


class TestBasicInteraction(FuncTestCase):

    def runTest(self):
        c1 = self.client()
        self.assertEqual('Echo: foo', c1.req("echo", "foo"))
        c1.send("login", "zozo")
        exp = ['area.init', {'areaId': 'alpha', 'timestamp': ANY}]
        self.assertEqual(exp, c1.recv())
        self.assertEqual('alpha echo: bar', c1.req("area.echo", "bar"))
        exp = ["area.tick",
               {"objects": [{"actions": [],
                             "durability": 100,
                             "pos": [10, 10],
                             "id": "user_id:zozo"}],
                "events":[],
                "timestamp": ANY}]
        self.assertEqual(exp, c1.recv())
        c1.send("area.move_to", [14, 20])
        exp_actions = [{'endTs': ANY,
                        'from': [10, 10],
                        'startTs': ANY,
                        'tag': 'MoveDistance',
                        'to': [14, 20]}]
        exp = ['area.tick',
               {'events': [],
                'timestamp': ANY,
                'objects': [{'actions': exp_actions,
                             'durability': 100,
                             'id': 'user_id:zozo',
                             'pos': [12, 16]}]}]
        self.assertEqual(exp, c1.recv())
        exp = ['area.tick',
               {'events': [],
                'timestamp': ANY,
                'objects': [{'actions': [],
                             'durability': 100,
                             'id': 'user_id:zozo',
                             'pos': [14, 20]}]}]
        self.assertEqual(exp, c1.recv())
        c2 = self.client()
        c2.send("login", "dede")
        exp =  ['area.init',
                {'areaId': 'alpha',
                'timestamp': ANY}]
        self.assertEqual(exp, c2.recv())
        exp = [{'id': 'user_id:dede',
                'name': 'dede',
                'durability': 100,
                'pos': [10, 10],
                'tag': 'User'},
               {'id': 'user_id:zozo',
                'name': 'zozo',
                'durability': 100,
                'pos': [14, 20],
                'tag': 'User'}]
        self.assertEqual(exp, c1.req("area.get_objects_info",
                                     ['user_id:zozo', 'user_id:dede']))
        c1.send("area.ignite", 40)
        exp_actions = [{'damageSpeed': 40,
                        'previousTs': ANY,
                        'tag': 'Burning'}]
        exp = ['area.tick',
               {'events': [],
                'timestamp': ANY,
                'objects': [{'actions': [],
                             'durability': 100,
                             'id': 'user_id:dede',
                             'pos': [10, 10]},
                            {'actions': exp_actions,
                             'durability': 60,
                             'id': 'user_id:zozo',
                             'pos': [14, 20]}]}]
        self.assertEqual(exp, c1.recv())
        self.assertEqual(exp, c2.recv())
        exp = ['area.tick',
               {'events': [],
                'timestamp': ANY,
                'objects': [{'actions': [],
                             'durability': 100,
                             'id': 'user_id:dede',
                             'pos': [10, 10]},
                            {'actions': exp_actions,
                             'durability': 20,
                             'id': 'user_id:zozo',
                             'pos': [14, 20]}]}]
        self.assertEqual(exp, c1.recv())
        self.assertEqual(exp, c2.recv())
        exp = ['area.tick',
               {'events': [{'ident': 'user_id:zozo', 'tag': 'DeleteUser'}],
                'timestamp': ANY,
                'objects': [{'actions': [],
                             'durability': 100,
                             'id': 'user_id:dede',
                             'pos': [10, 10]}]}]
        self.assertEqual(exp, c2.recv())
        c2.send("area.get_objects_info", ['user_id:zozo', 'user_id:dede'])
        exp = [{'id': 'user_id:dede',
                'name': 'dede',
                'durability': 100,
                'pos': [10, 10],
                'tag': 'User'}]
        self.assertEqual(exp, c2.req("area.get_objects_info",
                                     ['user_id:zozo', 'user_id:dede']))
        with self.assertRaises(WebSocketTimeoutException):
            c1.recv()


if __name__ == '__main__':
    unittest.main()
