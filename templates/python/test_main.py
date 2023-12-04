import unittest

from main import *


class Test(unittest.TestCase):

  def test_fs1_test(self):
    with open("test.txt", 'r') as file:
      content = file.read()
      res = fs1(content)
      self.assertEqual(res, 42)

  def test_fs1_input(self):
    with open("input.txt", 'r') as file:
      content = file.read()
      res = fs1(content)
      self.assertEqual(res, 42)

  def test_fs2_test(self):
    with open("test.txt", 'r') as file:
      content = file.read()
      res = fs2(content)
      self.assertEqual(res, 42)

  def test_fs2_input(self):
    with open("input.txt", 'r') as file:
      content = file.read()
      res = fs2(content)
      self.assertEqual(res, 42)
