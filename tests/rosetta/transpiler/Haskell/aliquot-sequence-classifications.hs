{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
-- Generated by Mochi transpiler v0.10.52 on 2025-08-02 01:54 GMT+7
import qualified Data.Map as Map
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.IO (isEOF)
input :: IO String
input = do
    eof <- isEOF
    if eof then return "" else getLine
int :: String -> Int
int = read
float :: Int -> Double
float n = fromIntegral n
deref :: IORef a -> a
{-# NOINLINE deref #-}
deref r = unsafePerformIO (atomicModifyIORef' r (\x -> (x, x)))
powInt :: Int -> Int -> Int
powInt b e = go 1 b e where
    go r b e | e > 0 = go (if e `mod` 2 == 1 then r*b else r) (b*b) (e `div` 2)
              | otherwise = r
indexOf xs value = do
    i <- newIORef (0 :: Int)
    let
        loop = do
            if (deref i) < length xs then do
                if (xs !! (deref i)) == value then do
                    return ((deref i))
                else do
                    writeIORef i $! ((deref i) + 1)

                loop
            else return ()
    loop
    return (0 - 1)


contains xs value = do
    return (unsafePerformIO (indexOf xs value) /= 0 - 1)


maxOf a b = if a > b then a else b

intSqrt n = do
    if n == 0 then do
        return (0)
    else do
        x <- newIORef (n)
        y <- newIORef (((deref x) + fromIntegral 1) / 2)
        let
            loop = do
                if (deref y) < (deref x) then do
                    writeIORef x $! ((deref y))
                    writeIORef y $! (((deref x) + fromIntegral n / (deref x)) / 2)
                    loop
                else return ()
        loop
        return ((deref x))



sumProperDivisors n = do
    if n < 2 then do
        return (0)
    else do
        let sqrt = unsafePerformIO (intSqrt n)
        sum <- newIORef (1 :: Int)
        i <- newIORef (2 :: Int)
        let
            loop = do
                if (deref i) <= sqrt then do
                    if n `mod` (deref i) == 0 then do
                        writeIORef sum $! ((deref sum) + (deref i) + n `div` (deref i))
                    else return ()

                    writeIORef i $! ((deref i) + 1)
                    loop
                else return ()
        loop
        if sqrt * sqrt == n then do
            writeIORef sum $! ((deref sum) - sqrt)
        else return ()

        return ((deref sum))



classifySequence k = do
    last <- newIORef (k)
    seq <- newIORef ([k])
    let
        loop = do
            if True then do
                writeIORef last $! (unsafePerformIO (sumProperDivisors (deref last)))
                writeIORef seq $! ((deref seq) ++ [(deref last)])
                let n = length (deref seq)
                aliquot <- newIORef ("")
                if (deref last) == 0 then do
                    writeIORef aliquot $! ("Terminating")
                else do
                    if n == 2 && (deref last) == k then do
                        writeIORef aliquot $! ("Perfect")
                    else do
                        if n == 3 && (deref last) == k then do
                            writeIORef aliquot $! ("Amicable")
                        else do
                            if n >= 4 && (deref last) == k then do
                                writeIORef aliquot $! ("Sociable[" ++ show (n - 1) ++ "]")
                            else do
                                if (deref last) == ((deref seq) !! n - 2) then do
                                    writeIORef aliquot $! ("Aspiring")
                                else do
                                    if unsafePerformIO (contains (take (maxOf 1 (n - 2) - 1) (drop 1 (deref seq))) (deref last)) then do
                                        let idx = unsafePerformIO (indexOf (deref seq) (deref last))
                                        writeIORef aliquot $! ("Cyclic[" ++ show (n - 1 - idx) ++ "]")
                                    else do
                                        if n == 16 || (deref last) > tHRESHOLD then do
                                            writeIORef aliquot $! ("Non-Terminating")
                                        else return ()







                if (deref aliquot) /= "" then do
                    return (Map.fromList [("seq", (deref seq)), ("aliquot", (deref aliquot))])
                else return ()

                loop
            else return ()
    loop
    return (Map.fromList [("seq", (deref seq)), ("aliquot", "")])


padLeft n w = do
    s <- newIORef (unsafePerformIO (show n))
    let
        loop = do
            if length (deref s) < w then do
                writeIORef s $! (" " ++ (deref s))
                loop
            else return ()
    loop
    return ((deref s))


padRight s w = do
    r <- newIORef (s)
    let
        loop = do
            if length (deref r) < w then do
                writeIORef r $! ((deref r) ++ " ")
                loop
            else return ()
    loop
    return ((deref r))


joinWithCommas seq = do
    s <- newIORef ("[")
    i <- newIORef (0 :: Int)
    let
        loop = do
            if (deref i) < length seq then do
                writeIORef s $! ((deref s) ++ show ((seq !! (deref i))))
                if (deref i) < length seq - 1 then do
                    writeIORef s $! ((deref s) ++ ", ")
                else return ()

                writeIORef i $! ((deref i) + 1)
                loop
            else return ()
    loop
    writeIORef s $! ((deref s) ++ "]")
    return ((deref s))


main = do
    putStrLn ("Aliquot classifications - periods for Sociable/Cyclic in square brackets:\n")
    k <- newIORef (1 :: Int)
    let
        loop = do
            if (deref k) <= 10 then do
                let res = unsafePerformIO (classifySequence (deref k))
                putStrLn (unsafePerformIO (padLeft (deref k) 2) ++ ": " ++ unsafePerformIO (padRight (((res !! "aliquot") :: String)) 15) ++ " " ++ unsafePerformIO (joinWithCommas (((res !! "seq") :: [Int]))))
                writeIORef k $! ((deref k) + 1)
                loop
            else return ()
    loop
    putStrLn ("")
    let s = [11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488]
    i <- newIORef (0 :: Int)
    let
        loop = do
            if (deref i) < length s then do
                let val = (s !! (deref i))
                let res = unsafePerformIO (classifySequence val)
                putStrLn (unsafePerformIO (padLeft val 7) ++ ": " ++ unsafePerformIO (padRight (((res !! "aliquot") :: String)) 15) ++ " " ++ unsafePerformIO (joinWithCommas (((res !! "seq") :: [Int]))))
                writeIORef i $! ((deref i) + 1)
                loop
            else return ()
    loop
    putStrLn ("")
    let big = 15355717786080
    let r = unsafePerformIO (classifySequence big)
    putStrLn (show big ++ ": " ++ unsafePerformIO (padRight (((r !! "aliquot") :: String)) 15) ++ " " ++ unsafePerformIO (joinWithCommas (((r !! "seq") :: [Int]))))


tHRESHOLD = 140737488355328

