module Tile exposing (
  Tile,
  TileLeft(..), TileMiddle(..), TileRight(..),
  tileLeft, tileMiddle, tileRight,
  tileEncode, tileDecode)

type alias Tile = ( TileLeft, TileMiddle, TileRight )
type TileLeft = Two | Six | Seven
type TileMiddle = One | Five | Nine
type TileRight = Three | Four | Eight

tileLeft : Tile -> Int
tileLeft tile =
  case tile of
    (Two, _, _) -> 2
    (Six, _, _) -> 6
    (Seven, _, _) -> 7

tileMiddle : Tile -> Int
tileMiddle tile =
  case tile of
    (_, One, _) -> 1
    (_, Five, _) -> 5
    (_, Nine, _) -> 9

tileRight : Tile -> Int
tileRight tile =
  case tile of
     (_, _, Three) -> 3
     (_, _, Four) -> 4
     (_, _, Eight) -> 8


-- Encode/decode

tileEncode : Maybe Tile -> Char
tileEncode tile =
  Maybe.map tileEncode_ tile
    |> Maybe.withDefault 'A'

tileEncode_ : Tile -> Char
tileEncode_ tile =
  case tile of
    (Two, One, Three)    -> 'a'
    (Two, One, Four)     -> 'b'
    (Two, One, Eight)    -> 'c'
    (Two, Five, Three)   -> 'd'
    (Two, Five, Four)    -> 'e'
    (Two, Five, Eight)   -> 'f'
    (Two, Nine, Three)   -> 'g'
    (Two, Nine, Four)    -> 'h'
    (Two, Nine, Eight)   -> 'i'
    (Six, One, Three)    -> 'j'
    (Six, One, Four)     -> 'k'
    (Six, One, Eight)    -> 'l'
    (Six, Five, Three)   -> 'm'
    (Six, Five, Four)    -> 'n'
    (Six, Five, Eight)   -> 'o'
    (Six, Nine, Three)   -> 'p'
    (Six, Nine, Four)    -> 'q'
    (Six, Nine, Eight)   -> 'r'
    (Seven, One, Three)  -> 's'
    (Seven, One, Four)   -> 't'
    (Seven, One, Eight)  -> 'u'
    (Seven, Five, Three) -> 'v'
    (Seven, Five, Four)  -> 'w'
    (Seven, Five, Eight) -> 'x'
    (Seven, Nine, Three) -> 'y'
    (Seven, Nine, Four)  -> 'z'
    (Seven, Nine, Eight) -> 'B'

tileDecode : Char -> Maybe Tile
tileDecode c =
  case c of
    'a' -> Just (Two, One, Three)
    'b' -> Just (Two, One, Four)
    'c' -> Just (Two, One, Eight)
    'd' -> Just (Two, Five, Three)
    'e' -> Just (Two, Five, Four)
    'f' -> Just (Two, Five, Eight)
    'g' -> Just (Two, Nine, Three)
    'h' -> Just (Two, Nine, Four)
    'i' -> Just (Two, Nine, Eight)
    'j' -> Just (Six, One, Three)
    'k' -> Just (Six, One, Four)
    'l' -> Just (Six, One, Eight)
    'm' -> Just (Six, Five, Three)
    'n' -> Just (Six, Five, Four)
    'o' -> Just (Six, Five, Eight)
    'p' -> Just (Six, Nine, Three)
    'q' -> Just (Six, Nine, Four)
    'r' -> Just (Six, Nine, Eight)
    's' -> Just (Seven, One, Three)
    't' -> Just (Seven, One, Four)
    'u' -> Just (Seven, One, Eight)
    'v' -> Just (Seven, Five, Three)
    'w' -> Just (Seven, Five, Four)
    'x' -> Just (Seven, Five, Eight)
    'y' -> Just (Seven, Nine, Three)
    'z' -> Just (Seven, Nine, Four)
    'B' -> Just (Seven, Nine, Eight)
    'A' -> Nothing
    _ -> Nothing
