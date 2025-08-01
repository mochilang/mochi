//go:build slow

package hscode

const runtime = `
forLoop :: Int -> Int -> (Int -> Maybe a) -> Maybe a
forLoop start end f = go start
  where
    go i | i < end =
            case f i of
              Just v -> Just v
              Nothing -> go (i + 1)
         | otherwise = Nothing

whileLoop :: (() -> Bool) -> (() -> Maybe a) -> Maybe a
whileLoop cond body = go ()
  where
    go _ | cond () =
            case body () of
              Just v -> Just v
              Nothing -> go ()
         | otherwise = Nothing

avg :: (Real a, Fractional b) => [a] -> b
avg xs | null xs = 0
       | otherwise = realToFrac (sum xs) / fromIntegral (length xs)

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
  Just p | null p || p == "-" -> putStr text
         | otherwise -> writeFile p text

_split :: Char -> String -> [String]
_split _ "" = [""]
_split d s =
  let (h, t) = break (== d) s
  in h : case t of
            []      -> []
            (_:rest) -> _split d rest

_parseCSV :: String -> Bool -> Char -> [Map.Map String String]
_parseCSV text header delim =
  let ls = filter (not . null) (lines text)
  in if null ls then [] else
       let heads = if header
                      then _split delim (head ls)
                      else ["c" ++ show i | i <- [0 .. length (_split delim (head ls)) - 1]]
           start = if header then 1 else 0
           row line =
             let parts = _split delim line
             in Map.fromList [ (heads !! j, if j < length parts then parts !! j else "")
                             | j <- [0 .. length heads - 1] ]
      in map row (drop start ls)

`

const groupHelpers = `
data MGroup k a = MGroup { gKey :: k, gItems :: [a] } deriving (Show)

_group_by :: Ord k => [a] -> (a -> k) -> [MGroup k a]
_group_by src keyfn =
  let go [] m order = (m, order)
      go (x:xs) m order =
        let k = keyfn x
        in case Map.lookup k m of
             Just is -> go xs (Map.insert k (is ++ [x]) m) order
             Nothing -> go xs (Map.insert k [x] m) (order ++ [k])
      (m, order) = go src Map.empty []
  in [ MGroup k (fromMaybe [] (Map.lookup k m)) | k <- order ]
`

const jsonHelper = `
_json :: Aeson.ToJSON a => a -> IO ()
_json v = BSL.putStrLn (Aeson.encode v)

`

const expectHelper = `
expect :: Bool -> IO ()
expect True = pure ()
expect False = error "expect failed"
`

const timeRuntime = `
_now :: IO Int
_now = fmap round getPOSIXTime
`

const sliceHelpers = `
_slice :: [a] -> Int -> Int -> [a]
_slice xs i j =
  let n = length xs
      start0 = if i < 0 then i + n else i
      end0 = if j < 0 then j + n else j
      start = max 0 start0
      end = min n end0
      end' = if end < start then start else end
  in take (end' - start) (drop start xs)

_sliceString :: String -> Int -> Int -> String
_sliceString s i j =
  let n = length s
      start0 = if i < 0 then i + n else i
      end0 = if j < 0 then j + n else j
      start = max 0 start0
      end = min n end0
      end' = if end < start then start else end
  in take (end' - start) (drop start s)
`

const updateHelpers = `
_updateAt :: Int -> (a -> a) -> [a] -> [a]
_updateAt i f xs = take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs
`

const fetchHelper = `
_fetch :: Aeson.FromJSON a => String -> Maybe (Map.Map String String) -> IO a
_fetch url _
  | isPrefixOf "file://" url = do
      let path = drop 7 url
      txt <- readFile path
      case Aeson.decode (BSL.pack txt) of
        Just v -> pure v
        Nothing -> error "failed to decode JSON"
  | otherwise = do
      out <- readProcess "curl" ["-s", url] ""
      case Aeson.decode (BSL.pack out) of
        Just v -> pure v
        Nothing -> error "failed to decode JSON"
`

const anyValueRuntime = `
data AnyValue = VInt Int | VDouble Double | VString String | VBool Bool deriving (Show)

_showAny :: AnyValue -> String
_showAny (VInt n) = show n
_showAny (VDouble d) = show d
_showAny (VString s) = s
_showAny (VBool b) = if b then "true" else "false"
`

const asIntRuntime = `
_asInt :: AnyValue -> Int
_asInt (VInt n) = n
_asInt v = error ("expected int, got " ++ show v)
`

const asDoubleRuntime = `
_asDouble :: AnyValue -> Double
_asDouble (VDouble d) = d
_asDouble v = error ("expected double, got " ++ show v)
`

const asStringRuntime = `
_asString :: AnyValue -> String
_asString (VString s) = s
_asString v = error ("expected string, got " ++ show v)
`

const asBoolRuntime = `
_asBool :: AnyValue -> Bool
_asBool (VBool b) = b
_asBool v = error ("expected bool, got " ++ show v)
`

const loadRuntime = `
instance Aeson.ToJSON AnyValue where
  toJSON (VInt n) = Aeson.toJSON n
  toJSON (VDouble d) = Aeson.toJSON d
  toJSON (VString s) = Aeson.toJSON s
  toJSON (VBool b) = Aeson.toJSON b

_parseJSON :: String -> [Map.Map String String]
_parseJSON text =
  case Aeson.decode (BSL.pack text) of
    Just (Aeson.Array arr) -> map _valueToMap (V.toList arr)
    Just v -> [_valueToMap v]
    Nothing -> []

_valueToMap :: Aeson.Value -> Map.Map String String
_valueToMap (Aeson.Object o) =
  Map.fromList [ (T.unpack (Key.toText k), _valueToString v) | (k,v) <- KeyMap.toList o ]
_valueToMap _ = Map.empty

_valueToString :: Aeson.Value -> String
_valueToString (Aeson.String s) = T.unpack s
_valueToString (Aeson.Number n) = show n
_valueToString (Aeson.Bool b) = if b then "true" else "false"
_valueToString _ = ""

_mapToValue :: Map.Map String String -> Aeson.Value
_mapToValue m =
  Aeson.Object $ KeyMap.fromList [ (Key.fromString k, Aeson.String (T.pack v)) | (k,v) <- Map.toList m ]

_load :: Maybe String -> Maybe (Map.Map String String) -> IO [Map.Map String String]
_load path opts = do
  txt <- _readInput path
  let fmt = fromMaybe "csv" (opts >>= Map.lookup "format")
  pure $ case fmt of
    "json" -> _parseJSON txt
    _ -> _parseCSV txt True ','

_save :: [Map.Map String String] -> Maybe String -> Maybe (Map.Map String String) -> IO ()
_save rows path opts =
  let fmt = fromMaybe "csv" (opts >>= Map.lookup "format")
  in case fmt of
    "json" ->
      let objs = map _mapToValue rows
          val = if length objs == 1 then head objs else Aeson.Array (V.fromList objs)
      in _writeOutput path (BSL.unpack (Aeson.encode val))
    _ ->
      let headers = if null rows then [] else Map.keys (head rows)
          toLine m = intercalate "," [Map.findWithDefault "" h m | h <- headers]
          text = unlines (if null headers then [] else intercalate "," headers : map toLine rows)
      in _writeOutput path text
`
