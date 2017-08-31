{-# LANGUAGE GADTs, FlexibleInstances #-}
module CoreEngine where

{-

In this module, I implement the core engine of the game.
Here, we choose a representation and interface for
the board and the game in general. Then we ultimately
implement a method to perform one move.

The GUI implementation simply wraps around this engine
and converts input into moves here.

Note: I make extensive use of types to avoid
commenting. Observe the names carefully.

Idea:
For voiding pieces,
walk along the empty slots
like the mouse holding their hand
on the cheese


-}

import MyMatrix ( Matrix, maybeIndexM, maybeSetM
                , queryDims, prettyPrint)


data GameState where
  GameState ::
    { players :: SixPlayers
    , phase :: GamePhase
    , board :: SixBoard
    , status :: GameStatus
    } -> GameState

data SixPlayers where
  SixPlayers :: { getRedPl :: PlayerData
                , getBlackPl :: PlayerData
                } -> SixPlayers

-- Ct ~ count
data PlayerData where
  PlayerData ::
    { toPlayCt :: Int
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

type Cell = Maybe Piece
type SixBoard = Matrix Cell
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
  fn x : map fn (waterfall fn xs)

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

data MyError a where
  MyError :: a -> MyError a

type SimpleError = MyError String
type MaybeError = Either SimpleError

data GameMove where
  -- In phase one, you just place
  PhaseOneMove :: PieceIndex -> GameMove
  PhaseTwoMove ::
    PieceIndex -> -- take from here
    PieceIndex -> -- place here
    GameMove

type Hopefully a = Either SimpleError a

mkErr :: String -> Hopefully a
mkErr s = Left $ MyError s

type PrevPlayers = SixPlayers
type NextPlayers = SixPlayers

type PrevPlayerData = PlayerData
type NextPlayerData = PlayerData

type PrevBoard = SixBoard
type NextBoard = SixBoard

type PrevPhase = GamePhase
type NextPhase = GamePhase

type PrevStatus = GameStatus
type NextStatus = GameStatus

type PrevGameState = GameState
type NextGameState = GameState

type ColorToPlay = Piece
type Player = Piece
type PlayerMakingMove = Piece

type PiecesInPlayCount = Int


makeMove :: GameMove -> PrevGameState -> Hopefully NextGameState
makeMove
  move
  GameState { status = stat@(OnGoing xToMove)
            , phase = thePhase
            , board = brd
            , players =  plrs@(SixPlayers
                               { getRedPl = redData
                               , getBlackPl = blackData
                               }
                              )
            } =
  if not $ phaseMatchesMove move thePhase
  then mkErr "Phase != Move Phase"
  else if not $ dataMatchesPhase plrs thePhase
  then mkErr "Data inconsistent with phase."
  else do
    nextBrd <- boardStep xToMove move brd
    let nextPlData = plDataStep xToMove thePhase nextBrd plrs
    let nextPhase = phaseStep PhaseOne nextPlData
    nextStatus <- statusStep stat move nextBrd nextPlData
    return GameState { players = nextPlData
                     , phase = nextPhase
                     , board = nextBrd
                     , status = nextStatus
                     }

makeMove _ _ = mkErr "Can't move in an end state"

phaseMatchesMove :: GameMove -> GamePhase -> Bool
phaseMatchesMove (PhaseOneMove _)   PhaseOne = True
phaseMatchesMove (PhaseTwoMove _ _) PhaseTwo = True
phaseMatchesMove _                  _        = False

dataMatchesPhase :: SixPlayers -> GamePhase -> Bool
dataMatchesPhase plrs phase' = case phase' of
  PhaseOne ->
    all ((/= 0) . toPlayCt) plrsList &&
    all ((== 0) . voidCt) plrsList
  PhaseTwo ->
    all ((== 0) . toPlayCt) plrsList
  where
    plrsList = [getRedPl plrs, getBlackPl plrs]



--   Need to figure out a nice way to resize the board,
-- and deal with the fact that the location of the move changes.
boardStep ::
  ColorToPlay -> GameMove -> PrevBoard -> Hopefully NextBoard
boardStep pieceToPlay (PhaseOneMove spot) brd =
  placeOnBoard pieceToPlay spot brd
boardStep pieceToPlay (PhaseTwoMove fromSpot toSpot) brd =
  undefined -- THIS IS HARD. Voiding pieces!!!


placeOnBoard ::
  ColorToPlay -> PieceIndex -> SixBoard -> Hopefully SixBoard
placeOnBoard pieceToPlay spot brd = do
  currentlyThere <- hfullyIndexBrd spot brd
  case currentlyThere of
    Nothing ->
      hfullySetBrd spot pieceToPlay brd
    Just _ ->
      mkErr "Can't place since piece already there."


emsg :: String
emsg = "That spot does not exist on the board"

hfullyIndexBrd :: PieceIndex -> SixBoard -> Hopefully (Maybe Piece)
hfullyIndexBrd spot brd = case maybeIndexM spot brd of
  Nothing -> mkErr emsg
  Just spot' -> Right spot'

hfullySetBrd :: PieceIndex -> Piece -> SixBoard -> Hopefully SixBoard
hfullySetBrd spot piece brd =
  case maybeSetM spot (Just piece) brd of
    Nothing -> mkErr emsg
    Just brd' -> Right brd'

data SixBoardCount where
  SixBoardCount :: { getRedCnt :: Int
                   , getBlackCnt :: Int
                   } -> SixBoardCount

countSixBoard :: SixBoard -> SixBoardCount
countSixBoard brd =
  let
    pieces = concatMap getJusts brd
    redCnt = countList RedPiece pieces
    blackCnt = countList BlackPiece pieces
  in
    SixBoardCount {getRedCnt = redCnt, getBlackCnt = blackCnt}

-- Should update this: -----------
countList :: Eq a => a -> [a] -> Int
countList = countListInternal 0
  where
    countListInternal :: Eq a => Int -> a -> [a] -> Int
    countListInternal n _ [] = n
    countListInternal n a (x:xs) | x == a    =
                                   countListInternal (n+1) a xs
                                 | otherwise =
                                   countListInternal n a xs
getJusts :: [Maybe a] -> [a]
getJusts [] = []
getJusts (x:xs) = case x of
  Nothing -> getJusts xs
  Just a -> a : getJusts xs
---------------------------

plDataStep ::
  PlayerMakingMove ->
  PrevPhase ->
  NextBoard ->
  PrevPlayers ->
  NextPlayers
plDataStep plr PhaseOne _ plrData =
  updatePlayer plr phaseOneUpdate plrData
  where

    phaseOneUpdate :: PlayerData -> PlayerData
    phaseOneUpdate plr' = PlayerData { toPlayCt = toPlayCt plr' - 1
                                    , playedCt = playedCt plr' + 1
                                    , voidCt = voidCt plr' }
    updatePlayer ::
      Player -> (PlayerData -> PlayerData) -> SixPlayers -> SixPlayers
    updatePlayer plr' fn SixPlayers
      { getRedPl = red
      , getBlackPl = black
      } =
        case plr' of
          RedPiece ->
            SixPlayers {getRedPl = fn red, getBlackPl = black}
          BlackPiece ->
            SixPlayers {getRedPl = red, getBlackPl = fn black}

plDataStep _ PhaseTwo brd plrData =
  SixPlayers {getRedPl = nextRed, getBlackPl = nextBlack}
  where
    brdCount :: SixBoardCount
    brdCount = countSixBoard brd

    phaseTwoMove :: PrevPlayerData -> PiecesInPlayCount -> NextPlayerData
    phaseTwoMove plr newInPlay =
      let
        newVoid = voidCt plr + (playedCt plr - newInPlay)
      in PlayerData { toPlayCt = toPlayCt plr -- should be 0
                    , playedCt = newInPlay
                    , voidCt = newVoid }

    nextRed = phaseTwoMove (getRedPl plrData) (getRedCnt brdCount)
    nextBlack = phaseTwoMove (getBlackPl plrData) (getBlackCnt brdCount)



phaseStep :: PrevPhase -> NextPlayers -> NextPhase
phaseStep PhaseTwo _ = PhaseTwo
phaseStep PhaseOne SixPlayers {getRedPl = red, getBlackPl = black} =
  if all ((== 0) . toPlayCt) [red, black]
  then PhaseTwo
  else PhaseOne


statusStep :: PrevStatus -> GameMove -> 
              NextBoard -> NextPlayers -> Hopefully NextStatus
statusStep (OnGoing player) move newBrd newPlData
  | hasWon player (getPlacement move) newBrd newPlData =
    Right $ Win player
  | isDraw newPlData =
    Right Draw
  | otherwise = Right $ OnGoing $ oppPlayer player
    where
      getPlacement :: GameMove -> PieceIndex
      getPlacement (PhaseOneMove spot)   = spot
      getPlacement (PhaseTwoMove _ spot) = spot

      hasWon ::
        PlayerMakingMove ->
        PieceIndex ->
        SixBoard ->
        SixPlayers ->
        Bool
      hasWon player' spot brd newPlData' =
        checkWinAt spot brd || otherPlNoPieces player' newPlData'
        where
          otherPlNoPieces :: Player -> SixPlayers -> Bool
          otherPlNoPieces justplayed SixPlayers
                                      { getRedPl = red
                                      , getBlackPl = black
                                      } =
            case justplayed of
              RedPiece -> fstWonByBleeding (red, black)
              BlackPiece -> fstWonByBleeding (black, red)
              where
                fstWonByBleeding (p1,p2) =
                  playedCt p1 >= 6 && -- not a draw
                  toPlayCt p2 == 0 &&
                  playedCt p2 == 0

      isDraw :: SixPlayers -> Bool
      isDraw SixPlayers { getRedPl = red
                        , getBlackPl = black
                        }
             = all ((== 0) . toPlayCt) [red, black] &&
               all ((< 6) . playedCt) [red, black]

      oppPlayer = oppPiece
statusStep _ _ _ _ =
  mkErr "Game status doesn't progress after win | draw"


oppPiece :: Piece -> Piece
oppPiece RedPiece = BlackPiece
oppPiece BlackPiece = RedPiece




























