data Box = Open | Done
type Open = (Bool,Bool,Bool,Bool)
type Done = Player
type Player = String
type GameState = [[Box]]