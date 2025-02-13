


directions = [(a,b) | a <- [-1..1], b <- [-1..1], (a,b) /= (0,0)]

type Board = [[TileState]]
type Tile = (Int, Int)
type Vector = (Int, Int)

startBoard = [
    [Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank],
    [Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank],
    [Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank],
    [Blank, Blank, Blank, White, Black, Blank, Blank, Blank],
    [Blank, Blank, Blank, Black, White, Blank, Blank, Blank],
    [Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank],
    [Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank],
    [Blank, Blank, Blank, Blank, Blank, Blank, Blank, Blank]
    ]



maxRow = 7
maxCol = 7

minRow = 0
minCol = 0

getTile :: Board -> Int -> Int -> TileState
getTile b row col = (b !! row) !! col


outOfBounds :: (Int, Int) -> Bool
outOfBounds (a,b) = a < minRow || b < minRow || a > maxCol || b > maxCol

getTargetRow :: Board -> Tile -> Vector -> TileState -> [(Int, Int, TileState)]
getTargetRow board (row, col) (dx, dy) targetState =
    let
        nextX = row + dx
        nextY = col + dy
        newPoint = (nextX, nextY)
        currState = getTile board row col
        vector = (dx, dy)
    in
        if (currState == targetState || outOfBounds newPoint) then
            [(row, col, currState)]
        else
            (row, col, currState) : getTargetRow board newPoint vector targetState

getValidMoves :: Board -> Tile -> [(Int, Int)]
getValidMoves board (row, col) = 
    let      
        candidates = [candidate | vector <- directions, candidate <- [getTargetRow board (row, col) vector Blank], length candidate >= 3]
        validMoves = [(a, b) | candidate <- candidates, (a, b, _) <- [last candidate], validityFunc candidate == True]
    in
        validMoves

placeTile :: Board -> Tile -> TileState -> Board
placeTile board (row, col) tile =
    let
        frontBoard = take row board
        backBoard = drop (row+1) board

        editRow = board !! row
        prevRow = take col editRow
        postRow = drop (col+1) editRow

        final = [prevRow ++ [tile] ++  postRow]
    in
        frontBoard ++ final ++ backBoard 
        


validityFunc :: [(Int, Int, TileState)] -> Bool
validityFunc (_:_:[]) = False
validityFunc tiles =
        let
            (x:xs) = tiles
            (r, c, state) = last xs
            mid = init xs
            (_, _, ts) = x
            target = flipTile ts
            midGood = all (\(_, _, t) -> t == target) mid

        in
            midGood && (state == Blank)

class Flippable t where
    flipTile :: t -> TileState

instance Flippable TileState where
    flipTile Black = White
    flipTile White = Black
    flipTile _ = Blank



data TileState = Blank | Black | White deriving (Show, Eq)


