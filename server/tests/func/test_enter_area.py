import unittest
from tools import FuncTestCase, ANY


class TestEnterArea(FuncTestCase):

    def runTest(self):
        #TODO: use subdicts
        c1 = self.client()
        c1.send("login", "zozo")
        exp =  ['area.init',
                {'areaId': 'alpha',
                 'timestamp': ANY}]
        self.assertEqual(exp, c1.recv())
        c2 = self.client()
        c2.send("login", "dede")
        exp =  ['area.init',
                {'areaId': 'alpha',
                 'timestamp': ANY}]
        self.assertEqual(exp, c2.recv())


        c1.send("area.enter_area", "beta")
        exp1 =  ['area.init',
                 {'areaId': 'beta',
                  'timestamp': ANY}]
        exp2 = ["area.tick",
                {"objects": [{"actions": [],
                              "durability": 100,
                              "pos": [10, 10],
                              "id": "user_id:dede"}],
                 "timestamp": ANY,
                 "events":[]}]
        self.assertEqual(exp1, c1.recv())
        self.assertEqual(exp2, c2.recv())

        exp1 = ["area.tick",
                {"objects": [{"actions": [],
                              "durability": 100,
                              "pos": [10, 10],
                              "id": "user_id:zozo"}],
                 "timestamp": ANY,
                 "events":[]}]
        self.assertEqual(exp1, c1.recv())
        c1.send("area.echo", "foo")
        self.assertEqual("beta echo: foo", c1.recv()[1])


        c1.send("area.enter_area", "alpha")
        exp1 =  ['area.init',
                 {'areaId': 'alpha',
                  'timestamp': ANY}]
        exp2 = {"objects": [{"actions": [],
                             "durability": 100,
                             "pos": [10, 10],
                             "id": "user_id:dede"},
                            {"actions": [],
                             "durability": 100,
                             "pos": [10, 10],
                             "id": "user_id:zozo"}],
                "timestamp": ANY,
                "events":[]}
        self.assertEqual(exp1, c1.recv())
        self.assertEqual(exp2, c2.recv('area.tick')[1])




if __name__ == '__main__':
    unittest.main()
