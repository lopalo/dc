import unittest
from tools import FuncTestCase


class TestEnterArea(FuncTestCase):

    def runTest(self):
        #TODO: use subdicts
        c1 = self.client()
        c1.send("login", "zozo")
        exp =  ['area.init',
                {'areaId': 'alpha',
                 'objects': [{'actions': [],
                              'id': 'user_id:zozo',
                              'name': 'zozo',
                              'pos': [10, 10],
                              'tag': 'User'}]}]
        self.assertEqual(exp, c1.recv())
        c2 = self.client()
        c2.send("login", "dede")
        exp1 = ["area.entered",
                {"tag": "User",
                 "actions": [],
                 "pos": [10, 10],
                 "name": "dede",
                 "id": "user_id:dede"}]
        exp2 =  ['area.init',
                 {'areaId': 'alpha',
                  'objects': [{'actions': [],
                               'id': 'user_id:dede',
                               'name': 'dede',
                               'pos': [10, 10],
                               'tag': 'User'},
                              {'actions': [],
                               'id': 'user_id:zozo',
                               'name': 'zozo',
                               'pos': [10, 10],
                               'tag': 'User'}]}]
        self.assertEqual(exp1, c1.recv())
        self.assertEqual(exp2, c2.recv())


        c1.send("area.enter_area", "beta")
        exp1 =  ['area.init',
                 {'areaId': 'beta',
                  'objects': [{'actions': [],
                               'id': 'user_id:zozo',
                               'name': 'zozo',
                               'pos': [10, 10],
                               'tag': 'User'}]}]
        exp2 = ["area.tick",
                {"objects": [{"actions": [],
                              "durability": 100,
                              "pos": [10, 10],
                              "id": "user_id:dede"}],
                 "events":[]}]
        self.assertEqual(exp1, c1.recv())
        self.assertEqual(exp2, c2.recv())

        exp1 = ["area.tick",
                {"objects": [{"actions": [],
                              "durability": 100,
                              "pos": [10, 10],
                              "id": "user_id:zozo"}],
                 "events":[]}]
        self.assertEqual(exp1, c1.recv())
        c1.send("area.echo", "foo")
        self.assertEqual("beta echo: foo", c1.recv()[1])


        c1.send("area.enter_area", "alpha")
        exp1 =  ['area.init',
                 {'areaId': 'alpha',
                  'objects': [{'actions': [],
                               'id': 'user_id:dede',
                               'name': 'dede',
                               'pos': [10, 10],
                               'tag': 'User'},
                              {'actions': [],
                               'id': 'user_id:zozo',
                               'name': 'zozo',
                               'pos': [10, 10],
                               'tag': 'User'}]}]
        exp2 = {"objects": [{"actions": [],
                              "durability": 100,
                              "pos": [10, 10],
                              "id": "user_id:dede"},
                            {"actions": [],
                              "durability": 100,
                              "pos": [10, 10],
                              "id": "user_id:zozo"}],
                "events":[]}
        self.assertEqual(exp1, c1.recv())
        self.assertEqual(exp2, c2.recv('area.tick')[1])




if __name__ == '__main__':
    unittest.main()
