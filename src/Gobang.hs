module Gobang where


type BoardElem = Int

empty :: BoardElem
empty = 0

black :: BoardElem
black = 1

white :: BoardElem
white = 2


type Chess = Int

none :: Chess
none = 0

ally :: Chess
ally = 1

enemy :: Chess
enemy = 2

block :: Chess
block = 3

relation :: BoardElem -> BoardElem -> Chess
relation x 0 = 0
relation x y = 1 + abs (x - y)










