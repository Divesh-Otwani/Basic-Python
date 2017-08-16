{-# LANGUAGE GADTs, FlexibleInstances #-}
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
import Control.Monad ( guard )
import Control.Applicative (Alternative, liftA2)


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
type PiecePathFn = GetNeighborIx

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

data AbstractPiecePath where
  L :: AbstractPiecePath
  R :: AbstractPiecePath
  TopL :: AbstractPiecePath
  TopR :: AbstractPiecePath
  BottomL :: AbstractPiecePath
  BottomR :: AbstractPiecePath
  ComposedPath ::
    AbstractPiecePath -> -- do this path first
    AbstractPiecePath -> -- and then do this path
    AbstractPiecePath
  deriving (Show, Eq)

(<.>) :: AbstractPiecePath -> AbstractPiecePath -> AbstractPiecePath
(<.>) = ComposedPath
infixr 5 <.>

absPathToPathFn :: AbstractPiecePath -> PiecePathFn
absPathToPathFn x = case x of
  L -> ixL
  R -> ixR
  TopL -> ixTopL
  TopR -> ixTopR
  BottomL -> ixBotL
  BottomR -> ixTopR
  (ComposedPath p q) ->
    -- Observe the order. We do path p first.
    absPathToPathFn q . absPathToPathFn p

rotatePath :: AbstractPiecePath -> AbstractPiecePath
rotatePath p = case p of
  L -> TopL
  TopL -> TopR
  TopR -> R
  R -> BottomR
  BottomR -> BottomL
  BottomL -> L
  (ComposedPath f g) ->
    ComposedPath (rotatePath f) (rotatePath g)


pathPieceIx :: PieceIndex -> AbstractPiecePath -> PieceIndex
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
  [PiecePathFn] -> -- paths to other cells
  SixBoard -> -- the board
  Bool
-- True iff these cells exist, have pieces, and are the same

checkSameColor cell paths@(_:_) brd =
  case maybeGetPieces cell paths brd of
    Just pieces@(fstPiece:_) -> all (== fstPiece) pieces
    Just _ -> False -- something is wrong!
    Nothing -> False
-- all elems in ∅ are same color
checkSameColor _ _ _ = True


maybeGetPieces ::
  PieceIndex ->
  [PiecePathFn] ->
  SixBoard ->
  Maybe [Piece]
maybeGetPieces cell paths brd = do
   let queryPieces =
         map ((`maybeIndexM` brd) . ($ cell)) paths
   cellsThatExist <- maybeExtract queryPieces
   cellsThatHavePieces <- maybeExtract cellsThatExist
   return cellsThatHavePieces


-- Get a list iff ALL elements present
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
crown :: [AbstractPiecePath]
crown = [ TopL
        , TopR
        , TopL <.> TopL
        , TopL <.> TopR
        , TopR <.> TopR
        ]
rotatePaths :: [AbstractPiecePath] -> [AbstractPiecePath]
rotatePaths = map rotatePath

waterfall :: (a -> a) -> [a] -> [a]
waterfall _ [] = []
waterfall fn (x:xs) =
  fn x : waterfall (fn . fn) xs

rotationsOf :: [AbstractPiecePath] -> [[AbstractPiecePath]]
rotationsOf = waterfall rotatePaths . replicate 6

crowns :: [[AbstractPiecePath]]
crowns = rotationsOf crown


-- This is the arrow surrounding
-- a center triangle piece
arrow :: [AbstractPiecePath]
arrow = [ TopL
        , TopR
        , TopL <.> TopR
        , L
        , R
        ]
arrows :: [[AbstractPiecePath]]
arrows = rotationsOf arrow

triangleChecks :: [[AbstractPiecePath]]
triangleChecks = crowns ++ arrows


-- Circle Constructs
-- This is the center of a circle
circCenter :: [AbstractPiecePath]
circCenter = [ TopL
             , TopR
             , L
             , R
             , BottomL
             , BottomR
             ]

shiftPath :: AbstractPiecePath -> [AbstractPiecePath] -> [AbstractPiecePath]
shiftPath p = map (p <.>)

shiftCircCenter :: AbstractPiecePath -> [AbstractPiecePath]
shiftCircCenter = (`shiftPath` circCenter)

nbrDirections :: [AbstractPiecePath]
nbrDirections = circCenter

circChecks :: [[AbstractPiecePath]]
circChecks = map shiftCircCenter nbrDirections

-- Line Constructs

lineEnd :: [AbstractPiecePath]
lineEnd = R : waterfall (R <.>)  (replicate 4 R)

lineMiddle :: [AbstractPiecePath]
lineMiddle = L : take 4 lineEnd

lineCenter :: [AbstractPiecePath]
lineCenter = (L <.> L) : take 4 lineMiddle

lineTypes :: [[AbstractPiecePath]]
lineTypes = [lineEnd, lineMiddle, lineCenter]

lineChecks :: [[AbstractPiecePath]]
lineChecks = concatMap rotationsOf lineTypes

checksToDo :: [[AbstractPiecePath]]
checksToDo =
  triangleChecks ++ circChecks ++ lineChecks

pathFnsOfChecksToDo :: [[PiecePathFn]]
pathFnsOfChecksToDo = (map . map) absPathToPathFn checksToDo


--- Finally, to check if the game is won:

checkWinAt :: PieceIndex -> SixBoard -> Bool
checkWinAt cell brd =
  let
    colorChecker x = checkSameColor cell x brd
  in
    any colorChecker pathFnsOfChecksToDo


--------------------------------------------------------------------
--------------------------------------------------------------------
--             Now, the function to advance the game one step.
--------------------------------------------------------------------
--------------------------------------------------------------------

data Error a where
  Error :: a -> Error a

type SimpleError = Error String
type MaybeError = Either SimpleError

instance Applicative MaybeError where
  pure = Right

  liftA2 fn (Right a) (Right b) = pure $ fn a b
  liftA2 _  (Left e)  (Right _) = Left e
  liftA2 _   _        someError = someError



instance Alternative (Either SimpleError) where


data GameMove where
  -- In phase one, you just place
  PhaseOneMove :: PieceIndex -> GameMove
  PhaseTwoMove ::
    PieceIndex -> -- take from here
    PieceIndex -> -- place here
    GameMove

type Hopefully a = Either SimpleError a

-- Convention: (red, black)
type SixPlayers = (PlayerData, PlayerData)
type PrevPlayerData = SixPlayers
type NextPlayerData = SixPlayers

type PrevBoard = SixBoard
type NextBoard = SixBoard

type PrevPhase = GamePhase
type NextPhase = GamePhase

type PrevStatus = GameStatus
type NextStatus = GameStatus

type PrevGameState = GameState
type NextGameState = GameState

type ColorToPlay = Piece

makeMove :: GameMove -> PrevGameState -> Hopefully NextGameState
makeMove
  move
  GameState { status = s@(OnGoing xToMove)
             , phase = thePhase
             , board = brd
             , redPlData = redData
             , blackPlData = blackData
             } =
  if not phaseMatchesMove move thePhase
  then Left $ Error "Phase != Move Phase"
  else do
    nextBrd <- boardStep xToMove move brd
    let nextPlData = plDataStep (nextBrd, (redData, blackData))
    let nextPhase = phaseStep PhaseOne nextPlData
    let nextStatus = statusStep s move nextBrd
    return GameState { redPlData = fst nextPlData
                    , blackPlData = snd nextPlData
                    , phase = nextPhase
                    , board = nextBrd
                    , status = nextStatus
                    }
makeMove _ _ = Left $ Error "Can't move in an end state"

phaseMatchesMove :: GameMove -> GamePhase -> Bool
phaseMatchesMove (PhaseOneMove _)   PhaseOne = True
phaseMatchesMove (PhaseTwoMove _ _) PhaseTwo = True
phaseMatchesMove _                  _        = False



boardStep ::
  ColorToPlay -> GameMove -> PrevBoard -> Hopefully NextBoard
boardStep pieceToPlay move brd = undefined


plDataStep :: (NextBoard, PrevPlayerData) -> NextPlayerData
plDataStep = undefined


phaseStep :: PrevPhase -> NextPlayerData -> NextPhase
phaseStep PhaseTwo _ = PhaseTwo
phaseStep PhaseOne (p1, p2) =
  if all ((== 0) . toPlayCt) [p1,p2]
  then PhaseTwo
  else PhaseOne

statusStep :: PrevStatus -> GameMove -> NextBoard -> NextStatus
statusStep = undefined































