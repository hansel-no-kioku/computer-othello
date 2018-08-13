module Game.Othello.Board
  ( Side(..)
  , enemy
  , Piece(..)
  , CellType(..)
  , Board
  , Pos(..)
  , blankBoard
  , initialBoard
  , move
  , getMovablePos
  , getFlippablePieces
  , cellTypes
  , getSafePlaces
  , getScore
  ) where

import Prelude

import Data.Array (length, null, reverse, (:))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Matrix (Matrix, catMaybes, filter, fromArray, index, replicate, updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Primitivizable (class Primitivizable)
import Data.Traversable (foldl)

data Side = Dark | Light

derive instance eqSide ∷ Eq Side
derive instance ordSide ∷ Ord Side

instance showSide ∷ Show Side where
  show Dark = "Dark"
  show Light = "Light"

instance primitivizableSide ∷ Primitivizable Side String where
  primitivize = show
  evolve "Light" = Light
  evolve _ = Dark

enemy ∷ Side → Side
enemy Dark = Light
enemy Light = Dark


data Piece = Blank | Off | Piece Side

derive instance eqPiece ∷ Eq Piece

instance showPiece ∷ Show Piece where
  show Blank = "Blank"
  show Off = "Off"
  show (Piece side) = show side

instance primitivizablePiece ∷ Primitivizable Piece String where
  primitivize = show
  evolve "Blank" = Blank
  evolve "Dark" = Piece Dark
  evolve "Light" = Piece Light
  evolve _ = Off

isFriend ∷ Side → Piece → Boolean
isFriend side (Piece side') = side == side'
isFriend _ _ = false

isEnemy ∷ Side → Piece → Boolean
isEnemy side (Piece side') = side /= side'
isEnemy _ _ = false


data CellType = Corner
              | NCorner
              | Corner2
              | Edge
              | Edge2
              | Inside


type Board = Matrix Piece

type Pos = {x ∷ Int, y ∷ Int}


blankBoard ∷ Board
blankBoard = replicate 8 8 Blank


initialBoard ∷ Board
initialBoard
  = blankBoard
  # updateAt' 3 3 (Piece Light)
  # updateAt' 4 4 (Piece Light)
  # updateAt' 3 4 (Piece Dark)
  # updateAt' 4 3 (Piece Dark)
  where
    updateAt' x y a m = fromMaybe m $ updateAt x y a m


move ∷ Board → Side → Pos → Maybe Board
move board side pos = do
  board' ← updateAt pos.x pos.y (Piece side) board
  let flippable = getFlippablePieces board side pos
  foldl (\b p → b >>= updateAt p.x p.y (Piece side)) (Just board') flippable


getMovablePos ∷ Board → Side → Array Pos
getMovablePos board side =
  catMaybes $ mapWithIndex f board
  where
    f pos Blank = maybeMovable board side pos
    f _ _ = Nothing


maybeMovable ∷ Board → Side → Pos → Maybe Pos
maybeMovable board side pos =
  if isMovable board side pos then Just pos else Nothing


isMovable ∷ Board → Side → Pos → Boolean
isMovable board side pos = not $ null $ getFlippablePieces board side pos


getFlippablePieces ∷ Board → Side → Pos → Array Pos
getFlippablePieces board side pos =
  case getPieceByPos board pos of
    Off → []
    Piece _ → []
    Blank → foldl (\f dir → f <> getFlippable dir pos []) [] directions

  where
    getFlippable dir pos' flippable =
      let
        pos'' = dir pos'
        piece = getPieceByPos board pos''
      in
             if isEnemy side piece  then getFlippable dir pos'' $ pos'' : flippable
        else if isFriend side piece then flippable
        else                             []


getPieceByPos ∷ Board → Pos → Piece
getPieceByPos board pos = fromMaybe Off $ index pos.x pos.y board


cellTypes ∷ Matrix CellType
cellTypes = fromArray $ cellTypeHalf <> reverse cellTypeHalf
  where
    cellTypeHalf =
      [ [ Corner  , NCorner , Corner2 , Edge    , Edge    , Corner2 , NCorner , Corner  ]
      , [ NCorner , NCorner , Corner2 , Inside  , Inside  , Corner2 , NCorner , NCorner ]
      , [ Corner2 , Corner2 , Corner2 , Edge2   , Edge2   , Corner2 , Corner2 , Corner2 ]
      , [ Edge    , Inside  , Edge2   , Inside  , Inside  , Edge2   , Inside  , Edge    ]
      ]


getSafePlaces ∷ Board → Side → Matrix Boolean
getSafePlaces board side = mapWithIndex (\pos p → checkSafe pos p) board

  where
    checkSafe pos piece =
         piece == Piece side
      && (checkSafeEach direction.n  pos || checkSafeEach direction.s  pos)
      && (checkSafeEach direction.ne pos || checkSafeEach direction.sw pos)
      && (checkSafeEach direction.e  pos || checkSafeEach direction.w  pos)
      && (checkSafeEach direction.se pos || checkSafeEach direction.nw pos)

    checkSafeEach dir pos =
      let
        nextPos = dir pos
        piece = getPieceByPos board nextPos
      in
        case piece of
          Off                         → true
          Piece side' | side' == side → checkSafeEach dir nextPos
          _                           → false

getScore ∷ Board → Side → Int
getScore board side = length $ filter (_ == Piece side) board

--

type Dir = Pos → Pos

direction ∷ {n ∷ Dir, ne ∷ Dir, e ∷ Dir, se ∷ Dir, s ∷ Dir, sw ∷ Dir, w ∷ Dir, nw ∷ Dir}
direction =
  { n : \p → {x:  p.x     , y: p.y - 1}
  , ne: \p → {x:  p.x + 1 , y: p.y - 1}
  , e : \p → {x:  p.x + 1 , y: p.y    }
  , se: \p → {x:  p.x + 1 , y: p.y + 1}
  , s : \p → {x:  p.x     , y: p.y + 1}
  , sw: \p → {x:  p.x - 1 , y: p.y + 1}
  , w : \p → {x:  p.x - 1 , y: p.y    }
  , nw: \p → {x:  p.x - 1 , y: p.y - 1}
  }

directions ∷ Array Dir
directions =
  [ direction.n
  , direction.ne
  , direction.e
  , direction.se
  , direction.s
  , direction.sw
  , direction.w
  , direction.nw
  ]
