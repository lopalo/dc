import unittest
from tools import FuncTestCase, ANY


class TestBasicInteraction(FuncTestCase):

    def runTest(self):
        c1 = self.client()
        c1.send("echo", "foo")
        self.assertEqual(['echo-reply', 'Echo: foo'], c1.recv())
        c1.send("login", "zozo")
        exp = ["area.entered",
               {"tag": "User",
                "actions": [],
                "pos": [10,10],
                "name": "zozo",
                "id": "user_id:zozo"}]
        self.assertEqual(exp, c1.recv())
        c1.send("area.echo", "bar")
        self.assertEqual(['area.echo-reply', 'alpha echo: bar'], c1.recv())
        exp = ["area.tick",
               {"objects": [{"actions": [],
                             "durability": 100,
                             "pos": [10, 10],
                             "id": "user_id:zozo"}],
                "events":[]}]
        self.assertEqual(exp, c1.recv())
        c1.send("area.move_to", [14, 20])
        exp_actions = [{'endTs': ANY,
                        'from': [10, 10],
                        'startTs': ANY,
                        'tag': 'MoveDistance',
                        'to': [14, 20]}]
        exp = ['area.tick',
               {'events': [],
                'objects': [{'actions': exp_actions,
                             'durability': 100,
                             'id': 'user_id:zozo',
                             'pos': [12, 16]}]}]
        self.assertEqual(exp, c1.recv())
        exp = ['area.tick',
               {'events': [],
                'objects': [{'actions': [],
                             'durability': 100,
                             'id': 'user_id:zozo',
                             'pos': [14, 20]}]}]
        self.assertEqual(exp, c1.recv())
        c2 = self.client()
        c2.send("login", "dede")
        exp = ["area.entered",
               {"tag": "User",
                "actions": [],
                "pos": [10,10],
                "name": "dede",
                "id": "user_id:dede"}]
        self.assertEqual(exp, c1.recv())
        self.assertEqual(exp, c2.recv())
        c1.send("area.ignite", 40)
        exp_actions = [{'damageSpeed': 40,
                        'previousTs': ANY,
                        'tag': u'Burning'}]
        exp = ['area.tick',
               {'events': [],
                'objects': [{'actions': [],
                             'durability': 100,
                             'id': u'user_id:dede',
                             'pos': [10, 10]},
                            {'actions': exp_actions,
                             'durability': 60,
                             'id': u'user_id:zozo',
                             'pos': [14, 20]}]}]
        self.assertEqual(exp, c2.recv())
        exp = ['area.tick',
               {'events': [],
                'objects': [{'actions': [],
                             'durability': 100,
                             'id': u'user_id:dede',
                             'pos': [10, 10]},
                            {'actions': exp_actions,
                             'durability': 20,
                             'id': u'user_id:zozo',
                             'pos': [14, 20]}]}]
        self.assertEqual(exp, c2.recv())
        exp = ['area.tick',
               {'events': [{'ident': 'user_id:zozo', 'tag': 'DeleteUser'}],
                'objects': [{'actions': [],
                             'durability': 100,
                             'id': u'user_id:dede',
                             'pos': [10, 10]}]}]
        self.assertEqual(exp, c2.recv())


if __name__ == '__main__':
    unittest.main()
