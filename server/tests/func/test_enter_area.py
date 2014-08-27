import unittest
from tools import FuncTestCase, subdict as sd


class TestEnterArea(FuncTestCase):

    def runTest(self):
        c1 = self.client()
        c1.send("login", "zozo")
        self.assertEqual(['area.init', sd(areaId='alpha')], c1.recv())
        c2 = self.client()
        c2.send("login", "dede")
        self.assertEqual(['area.init', sd(areaId='alpha')], c2.recv())


        c1.send("area.enter_area", "beta")
        self.assertEqual(['area.init', sd(areaId='beta')], c1.recv())
        exp = ["area.tick", sd(objects=[sd(id="user_id:dede")])]
        self.assertEqual(exp, c2.recv())

        exp = ["area.tick", sd(objects=[sd(id="user_id:zozo")])]
        self.assertEqual(exp, c1.recv())
        self.assertEqual("beta echo: foo", c1.req("area.echo", "foo"))


        c1.send("area.enter_area", "alpha")
        self.assertEqual(['area.init', sd(areaId='alpha')], c1.recv())
        exp = sd(objects=[sd(id="user_id:dede"),
                          sd(id="user_id:zozo")])
        self.assertEqual(exp, c2.recv('area.tick')[1])




if __name__ == '__main__':
    unittest.main()
