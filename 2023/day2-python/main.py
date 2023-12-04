from builtins import str


def fs1(content: str):
  MAX_RED = 12
  MAX_GREEN = 13
  MAX_BLUE = 14

  possible_count = 0
  game = 0
  lines = content.splitlines()
  for line in lines:
    game += 1
    idx = line.index(": ")
    line = line[idx + 2:]

    sets = line.split(";")
    valid = True
    for set in sets:
      set = set.strip()
      cubes = set.split(", ")
      continue_outer = False
      for cube in cubes:
        idx = cube.index(" ")
        count = int(cube[:idx])
        color = cube[idx + 1:]

        if color == "red" and count > MAX_RED:
          continue_outer = True
          valid = False
          break
        elif color == "green" and count > MAX_GREEN:
          continue_outer = True
          valid = False
          break
        elif color == "blue" and count > MAX_BLUE:
          continue_outer = True
          valid = False
          break
      if continue_outer:
        break
    if valid:
      possible_count += game

  return possible_count
