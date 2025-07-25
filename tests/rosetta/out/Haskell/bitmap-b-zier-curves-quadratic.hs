-- Generated by Mochi compiler v0.10.26 on 2025-07-16T09:30:23Z
-- Code generated by Mochi compiler; DO NOT EDIT.
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.List (intercalate, isInfixOf, isPrefixOf)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)

forLoop :: Int -> Int -> (Int -> Maybe a) -> Maybe a
forLoop start end f = go start
  where
    go i
      | i < end =
          case f i of
            Just v -> Just v
            Nothing -> go (i + 1)
      | otherwise = Nothing

whileLoop :: (() -> Bool) -> (() -> Maybe a) -> Maybe a
whileLoop cond body = go ()
  where
    go _
      | cond () =
          case body () of
            Just v -> Just v
            Nothing -> go ()
      | otherwise = Nothing

avg :: (Integral a) => [a] -> a
avg xs
  | null xs = 0
  | otherwise = div (sum xs) (fromIntegral (length xs))

data MGroup k a = MGroup {key :: k, items :: [a]} deriving (Show)

_group_by :: (Ord k) => [a] -> (a -> k) -> [MGroup k a]
_group_by src keyfn =
  let go [] m order = (m, order)
      go (x : xs) m order =
        let k = keyfn x
         in case Map.lookup k m of
              Just is -> go xs (Map.insert k (is ++ [x]) m) order
              Nothing -> go xs (Map.insert k [x] m) (order ++ [k])
      (m, order) = go src Map.empty []
   in [MGroup k (fromMaybe [] (Map.lookup k m)) | k <- order]

_indexString :: String -> Int -> String
_indexString s i =
  let idx = if i < 0 then i + length s else i
   in if idx < 0 || idx >= length s
        then error "index out of range"
        else [s !! idx]

_append :: [a] -> a -> [a]
_append xs x = xs ++ [x]

_input :: IO String
_input = getLine

_readInput :: Maybe String -> IO String
_readInput Nothing = getContents
_readInput (Just p)
  | null p || p == "-" = getContents
  | otherwise = readFile p

_writeOutput :: Maybe String -> String -> IO ()
_writeOutput mp text = case mp of
  Nothing -> putStr text
  Just p
    | null p || p == "-" -> putStr text
    | otherwise -> writeFile p text

_split :: Char -> String -> [String]
_split _ "" = [""]
_split d s =
  let (h, t) = break (== d) s
   in h : case t of
        [] -> []
        (_ : rest) -> _split d rest

_parseCSV :: String -> Bool -> Char -> [Map.Map String String]
_parseCSV text header delim =
  let ls = filter (not . null) (lines text)
   in if null ls
        then []
        else
          let heads =
                if header
                  then _split delim (head ls)
                  else ["c" ++ show i | i <- [0 .. length (_split delim (head ls)) - 1]]
              start = if header then 1 else 0
              row line =
                let parts = _split delim line
                 in Map.fromList
                      [ (heads !! j, if j < length parts then parts !! j else "")
                        | j <- [0 .. length heads - 1]
                      ]
           in map row (drop start ls)

data Pixel = Pixel
  { r :: Int,
    g :: Int,
    b :: Int
  }
  deriving (Eq, Show, Generic)

pixelFromRgb :: Int -> Pixel
pixelFromRgb rgb =
  fromMaybe (()) $
    (let r = (read ((((div rgb 65536)) `mod` 256)) :: Int) in (let g = (read ((((div rgb 256)) `mod` 256)) :: Int) in (let b = (read ((rgb `mod` 256)) :: Int) in Just (Pixel {r = r, g = g, b = b}))))
  where
    r = (read ((((div rgb 65536)) `mod` 256)) :: Int)
    g = (read ((((div rgb 256)) `mod` 256)) :: Int)
    b = (read ((rgb `mod` 256)) :: Int)

newBitmap :: Int -> Int -> Map.Map String ()
newBitmap cols rows =
  fromMaybe (()) $
    (let d = [] in (let y = 0 in case whileLoop (\() -> (y < rows)) (\() -> (let row = [] in (let x = 0 in case whileLoop (\() -> (x < cols)) (\() -> (let row = (row ++ [Pixel {r = 0, g = 0, b = 0}]) in (let x = (_asInt (x) + 1) in Nothing))) of Just v -> Just v; Nothing -> (let d = (d ++ [row]) in (let y = (_asInt (y) + 1) in Nothing))))) of Just v -> Just v; Nothing -> Just (Map.fromList [("cols", cols), ("rows", rows), ("data", d)])))

setPx :: Map.Map String () -> Int -> Int -> Pixel -> ()
setPx b x y p =
  fromMaybe (()) $
    (let cols = (read fromMaybe (error "missing") (Map.lookup "cols" b) :: Int) in (let rows = (read fromMaybe (error "missing") (Map.lookup "rows" b) :: Int) in if (((((((_asInt (x) >= 0) && x) < cols) && y) >= 0) && y) < rows) then (let b = Map.adjust (\m -> Map.adjust (\m -> Map.insert x p m) y m) "data" b in Nothing) else Nothing))
  where
    cols = (read fromMaybe (error "missing") (Map.lookup "cols" b) :: Int)
    rows = (read fromMaybe (error "missing") (Map.lookup "rows" b) :: Int)

fill :: Map.Map String () -> Pixel -> ()
fill b p =
  fromMaybe (()) $
    (let cols = (read fromMaybe (error "missing") (Map.lookup "cols" b) :: Int) in (let rows = (read fromMaybe (error "missing") (Map.lookup "rows" b) :: Int) in (let y = 0 in whileLoop (\() -> (y < rows)) (\() -> (let x = 0 in case whileLoop (\() -> (x < cols)) (\() -> (let b = Map.adjust (\m -> Map.adjust (\m -> Map.insert x p m) y m) "data" b in (let x = (_asInt (x) + 1) in Nothing))) of Just v -> Just v; Nothing -> (let y = (_asInt (y) + 1) in Nothing))))))
  where
    cols = (read fromMaybe (error "missing") (Map.lookup "cols" b) :: Int)
    rows = (read fromMaybe (error "missing") (Map.lookup "rows" b) :: Int)

fillRgb :: Map.Map String () -> Int -> ()
fillRgb b rgb =
  fromMaybe (()) $
    (let _ = fill b pixelFromRgb rgb in Nothing)

line :: Map.Map String () -> Int -> Int -> Int -> Int -> Pixel -> ()
line b x0 y0 x1 y1 p =
  fromMaybe (()) $
    (let dx = (x1 - x0) in case if (_asInt (dx) < 0) then (let dx = (-(_asInt (dx))) in Nothing) else Nothing of Just v -> Just v; Nothing -> (let dy = (y1 - y0) in case if (_asInt (dy) < 0) then (let dy = (-(_asInt (dy))) in Nothing) else Nothing of Just v -> Just v; Nothing -> (let sx = (-1) in case if (x0 < x1) then (let sx = 1 in Nothing) else Nothing of Just v -> Just v; Nothing -> (let sy = (-1) in case if (y0 < y1) then (let sy = 1 in Nothing) else Nothing of Just v -> Just v; Nothing -> (let err = (dx - dy) in whileLoop (\() -> True) (\() -> case (let _ = setPx b x0 y0 p in Nothing) of Just v -> Just v; Nothing -> case if (((x0 == x1) && y0) == y1) then Just () else Nothing of Just v -> Just v; Nothing -> (let e2 = (2 * _asInt (err)) in case if (e2 > ((0 - _asInt (dy)))) then (let err = (err - dy) in (let x0 = (x0 + sx) in Nothing)) else Nothing of Just v -> Just v; Nothing -> if (e2 < dx) then (let err = (err + dx) in (let y0 = (y0 + sy) in Nothing)) else Nothing)))))))

bezier2 :: Map.Map String () -> Int -> Int -> Int -> Int -> Int -> Int -> Pixel -> ()
bezier2 b x1 y1 x2 y2 x3 y3 p =
  fromMaybe (()) $
    (let px = [] in (let py = [] in (let i = 0 in case whileLoop (\() -> (_asInt (i) <= b2Seg)) (\() -> (let px = (px ++ [0]) in (let py = (py ++ [0]) in (let i = (_asInt (i) + 1) in Nothing)))) of Just v -> Just v; Nothing -> (let fx1 = (read x1 :: Double) in (let fy1 = (read y1 :: Double) in (let fx2 = (read x2 :: Double) in (let fy2 = (read y2 :: Double) in (let fx3 = (read x3 :: Double) in (let fy3 = (read y3 :: Double) in (let i = 0 in case whileLoop (\() -> (_asInt (i) <= b2Seg)) (\() -> (let c = (((read i :: Double)) / ((read b2Seg :: Double))) in (let a = (1.0 - _asDouble (c)) in (let a2 = (a * a) in (let b2 = ((2.0 * _asDouble (c)) * _asDouble (a)) in (let c2 = (c * c) in (let px = Map.insert i (read ((((((a2 * fx1) + b2) * fx2) + c2) * fx3)) :: Int) px in (let py = Map.insert i (read ((((((a2 * fy1) + b2) * fy2) + c2) * fy3)) :: Int) py in (let i = (_asInt (i) + 1) in Nothing))))))))) of Just v -> Just v; Nothing -> (let x0 = (px !! 0) in (let y0 = (py !! 0) in (let i = 1 in whileLoop (\() -> (_asInt (i) <= b2Seg)) (\() -> (let x = (px !! i) in (let y = (py !! i) in case (let _ = line b x0 y0 x y p in Nothing) of Just v -> Just v; Nothing -> (let x0 = x in (let y0 = y in (let i = (_asInt (i) + 1) in Nothing)))))))))))))))))))

b2Seg = 20

b = newBitmap 400 300

main :: IO ()
main = do
  fillRgb b 14614575
  bezier2 b 20 150 500 (-100) 300 280 pixelFromRgb 4165615
