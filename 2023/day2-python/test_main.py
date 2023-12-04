import unittest

from main import *


class Test(unittest.TestCase):

  def test_fs1_test(self):
    with open("test.txt", 'r') as file:
      content = file.read()
      res = fs1(content)
      self.assertEqual(res, 8)

  def test_fs1_input(self):
    with open("input.txt", 'r') as file:
      content = file.read()
      res = fs1(content)
      self.assertEqual(res, 2265)
