data Player = PlayerOne | PlayerTwo deriving Show

type Point = (Int, Int)

data Direction = Right | Down

type Line = (Point, Direction)

data Move = Move Line Player

type Box = (Point, Player) -- top left point of box and who controls it

type GameState = (Player, [Move], [Box]) --whose turn it is, list of moves done, list of boxes completed

type Winner = Maybe Player
--starting out 5x4 boxes



-- read in data
    -- parse it to our data types

-- check to see if move is legal
    -- check if move has been done before
    -- check if move goes out of bounds

-- make move
    -- add to list of moves
    -- check if box is filled
        -- add to list of boxes
    -- check if game is done
        --compute points from list of boxes

-- output
    --print each game state
    --print out winner
    --stop reading in stuff