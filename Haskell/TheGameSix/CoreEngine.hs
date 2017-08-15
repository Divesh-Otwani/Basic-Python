{-# LANGUAGE GADTs #-}
module CoreEngine where

{-
In this module, I implement the core engine of the game.
Here, we choose a representation and interface for
the board and the game in general. Then we ultimately
implement a method to perform one move.

The GUI implementation simply wraps around this engine
and converts that input into moves here.

Note: I make extensive use of types to avoid
commenting. Observe the names carefully.

-}

import MyMatrix ( Matrix, maybeIndexM, maybeSetM
                , queryDims, prettyPrint)


data GameState where
  GameState ::
    { redPlData :: PlayerData
    , blackPlData :: PlayerData
    , phase :: GamePhase
    , board :: SixBoard
    , status :: GameStatus
    } -> GameState

-- Ct ~ count
data PlayerData where
  PlayerData ::
    { color :: Piece
    , toPlayCt :: Int
    , playedCt :: Int
    , voidCt :: Int
    } -> PlayerData

data GamePhase where
  PhaseOne :: GamePhase
  PhaseTwo :: GamePhase

data GameStatus where
  OnGoing ::
    Piece -> -- This piece to play
    GameStatus
  Draw :: GameStatus
  Win :: Piece -> GameStatus

data Piece where
  RedPiece :: Piece
  BlackPiece :: Piece
  deriving (Show, Eq)

type SixBoard = Matrix (Maybe Piece)
type PieceIndex = (Int, Int)

-- ix ~ index; L ~ left; R ~ right
-- Bot ~ bottom

type GetNeighborIx = PieceIndex -> PieceIndex
type RelativeDirection = GetNeighborIx

ixL :: GetNeighborIx
ixL (row,col) = (row,col-1)

ixR :: GetNeighborIx
ixR (row,col) = (row,col+1)

ixTopL :: GetNeighborIx
ixTopL (row,col) = (row-1,col-1)

ixTopR :: GetNeighborIx
ixTopR (row,col) = (row-1,col)

ixBotL :: GetNeighborIx
ixBotL (row,col) = (row+1,col)

ixBotR :: GetNeighborIx
ixBotR (row,col) = (row+1,col+1)

data ConnectedPath where
  L :: ConnectedPath
  R :: ConnectedPath
  TopL :: ConnectedPath
  TopR :: ConnectedPath
  BottomL :: ConnectedPath
  BottomR :: ConnectedPath
  ComposedPath ::
    ConnectedPath -> -- do this path after
    ConnectedPath -> -- you do this path
    ConnectedPath
  deriving (Show, Eq)

(<.>) :: ConnectedPath -> ConnectedPath -> ConnectedPath
(<.>) = ComposedPath
infixr 5 <.>

pathToDirection :: ConnectedPath -> RelativeDirection
pathToDirection x = case x of
  L -> ixL
  R -> ixR
  TopL -> ixTopL
  TopR -> ixTopR
  BottomL -> ixBotL
  BottomR -> ixTopR
  (ComposedPath p q) ->
    pathToDirection p . pathToDirection q

rotatePath :: ConnectedPath -> ConnectedPath
rotatePath p = case p of
  L -> TopL
  TopL -> TopR
  TopR -> R
  R -> BottomR
  BottomR -> BottomL
  BottomL -> L
  (ComposedPath f g) ->
    ComposedPath (rotatePath f) (rotatePath g)


pathPieceIx :: PieceIndex -> ConnectedPath -> PieceIndex
pathPieceIx cell dir =
  case dir of
    L -> ixL cell
    R -> ixR cell
    TopL -> ixTopL cell
    TopR -> ixTopR cell
    BottomL -> ixBotL cell
    BottomR -> ixBotR cell
    (ComposedPath f g) -> pathPieceIx (pathPieceIx cell g) f


checkSameColor ::
  PieceIndex -> -- the start cell
  [RelativeDirection] -> -- paths to other cells
  SixBoard -> -- the board
  Bool
-- True iff these cells exist, have pieces, and are the same

checkSameColor cell dirs@(_:_) board =
  case maybeGetCells cell dirs board of
    Just pieces@(h:_) -> all (== h) pieces
    Just _ -> False -- something is wrong!
    Nothing -> False
-- all elems in ∅ are same color
checkSameColor _ _ _ = True


maybeGetCells ::
  PieceIndex ->
  [RelativeDirection] ->
  SixBoard ->
  Maybe [Piece]
maybeGetCells cell dirs board = do
   let queryPieces =
         map ((`maybeIndexM` board) . ($ cell)) dirs
   cellsThatExist <- maybeExtract queryPieces
   cellsThatHavePieces <- maybeExtract cellsThatExist
   return cellsThatHavePieces


-- Get a list iff all elements present
maybeExtract :: [Maybe a] -> Maybe [a]
maybeExtract [] = Just []
maybeExtract (x : xs) = do
  recur <- maybeExtract xs
  x' <- x
  return $ x' : recur



---------------------
-- Functions to check a win.
--------------------


-- Triangle Constructs
-- This is the crown from the
-- tip of a triangle
crown :: [ConnectedPath]
crown = [ TopL
        , TopR
        , TopL <.> TopL
        , TopL <.> TopR
        , TopR <.> TopR
        ]
rotatePaths :: [ConnectedPath] -> [ConnectedPath]
rotatePaths = map rotatePath

waterfall :: (a -> a) -> [a] -> [a]
waterfall _ [] = []
waterfall fn (x:xs) =
  fn x : waterfall (fn . fn) xs

rotationsOf :: [ConnectedPath] -> [[ConnectedPath]]
rotationsOf = waterfall rotatePaths . replicate 6

crowns :: [[ConnectedPath]]
crowns = rotationsOf crown


-- This is the arrow surrounding
-- a center triangle piece
arrow :: [ConnectedPath]
arrow = [ TopL
        , TopR
        , TopL <.> TopR
        , L
        , R
        ]
arrows :: [[ConnectedPath]]
arrows = rotationsOf arrow

triangleChecks :: [[ConnectedPath]]
triangleChecks = crowns ++ arrows


-- Circle Constructs
-- This is the center of a circle
circCenter :: [ConnectedPath]
circCenter = [ TopL
             , TopR
             , L
             , R
             , BottomL
             , BottomR
             ]

shiftPath :: ConnectedPath -> [ConnectedPath] -> [ConnectedPath]
shiftPath p = map (p <.>)

shiftCircCenter :: ConnectedPath -> [ConnectedPath]
shiftCircCenter = (`shiftPath` circCenter)

nbrDirections :: [ConnectedPath]
nbrDirections = circCenter

circChecks :: [[ConnectedPath]]
circChecks = map shiftCircCenter nbrDirections

-- Line Constructs

lineEnd :: [ConnectedPath]
lineEnd = R : waterfall (R <.>)  (replicate 4 R)

lineMiddle :: [ConnectedPath]
lineMiddle = L : take 4 lineEnd

lineCenter :: [ConnectedPath]
lineCenter = (L <.> L) : take 4 lineMiddle

lineTypes :: [[ConnectedPath]]
lineTypes = [lineEnd, lineMiddle, lineCenter]

lineChecks :: [[ConnectedPath]]
lineChecks = concatMap rotationsOf lineTypes

checksToDo :: [[ConnectedPath]]
checksToDo =
  triangleChecks ++ circChecks ++ lineChecks

dirsOfChecksToDo :: [[RelativeDirection]]
dirsOfChecksToDo = (map . map) pathToDirection checksToDo


--- Finally, to check if the game is won:

checkWinAt :: PieceIndex -> SixBoard -> Bool
checkWinAt cell brd =
  let
    colorChecker x = checkSameColor cell x brd
  in
    any colorChecker dirsOfChecksToDo


--------------------------------------------------------------------
--------------------------------------------------------------------
--             Now, the function to advance the game one step.
--------------------------------------------------------------------
--------------------------------------------------------------------

data Error a where
  Error :: a -> Error a
type SimpleError = Error String

data GameMove where
  -- In phase one, you just place
  PhaseOneMove :: PieceIndex -> GameMove
  PhaseTwoMove ::
    PieceIndex -> -- take from here
    PieceIndex -> -- place here
    GameMove

type Hopefully a = Either SimpleError a

makeMove :: GameMove -> GameState -> Hopefully GameState
makeMove
  (PhaseOneMove spot)
  (GameState { status = OnGoing xToMove
             , phase = PhaseOne
             , board = brd
             , redPlData = redData
             , blackPlData = blackData
             }) = undefined

makeMove _ _ = undefined




boardPlace :: PieceIndex -> SixBoard -> Hopefully SixBoard
boardPlace = undefined

-- Say (red, black)
type SixPlayers = (PlayerData, PlayerData)

newPlayerData :: GameMove -> SixPlayers -> SixPlayers
newPlayerData = undefined

-- args: curr phase, updated player data
newPhase :: GamePhase -> SixPlayers -> GamePhase
newPhase = undefined































