from builtins import print
from builtins import str


def fs1(content: str):
  lines = content.splitlines()

  for line in lines:
    print(line)

  return 42


def fs2(content: str):
  lines = content.splitlines()

  for line in lines:
    print(line)

  return 42
