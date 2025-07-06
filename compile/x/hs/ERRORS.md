# Haskell roundtrip VM test failures

## tests/vm/valid/append_builtin.mochi

```
runhaskell error: exit status 1

tests/vm/valid/append_builtin.hs.out:183:10: error:
    Variable not in scope: append :: [a0] -> t0 -> a1
    Suggested fix:
      Perhaps use one of these:
        ‘BSL.append’ (imported from Data.ByteString.Lazy.Char8),
        ‘T.append’ (imported from Data.Text),
        ‘mappend’ (imported from Prelude)
    |
183 |   print (append a 3)
    |          ^^^^^^

```

## tests/vm/valid/avg_builtin.mochi

```
output mismatch
-- hs --
2.0
-- vm --
2
```

## tests/vm/valid/basic_compare.mochi

```
output mismatch
-- hs --
7
True
True
-- vm --
7
true
true
```

## tests/vm/valid/binary_precedence.mochi

```
output mismatch
-- hs --
9
9
7
8
-- vm --
7
9
7
8
```

## tests/vm/valid/bool_chain.mochi

```
output mismatch
-- hs --
True
False
False
-- vm --
true
false
false
```

## tests/vm/valid/break_continue.mochi

```
runhaskell error: exit status 1

tests/vm/valid/break_continue.hs.out:183:26: error:
    • Couldn't match expected type ‘IO b0’ with actual type ‘()’
    • In the first argument of ‘fromMaybe’, namely ‘()’
      In the expression:
        fromMaybe
          ()
          (case if ((n `mod` 2) == 0) then Nothing else Nothing of
             Just v -> Just v
             Nothing
               -> case if (n > 7) then Just () else Nothing of
                    Just v -> Just v
                    Nothing -> (let _ = ... in Nothing))
      In the first argument of ‘mapM_’, namely
        ‘(\ n
            -> fromMaybe
                 ()
                 (case if ((n `mod` 2) == 0) then Nothing else Nothing of
                    Just v -> Just v
                    Nothing
                      -> case if (n > 7) then Just () else Nothing of
                           Just v -> Just v
                           Nothing -> (let ... in Nothing)))’
    |
183 |   mapM_ (\n -> fromMaybe () (case if ((n `mod` 2) == 0) then Nothing else Nothing of Just v -> Just v; Nothing -> case if (n > 7) then Just () else Nothing of Just v -> Just v; Nothing -> (let _ = putStrLn (unwords ["odd number:", show n]) in Nothing))) numbers
    |                          ^^

tests/vm/valid/break_continue.hs.out:183:175: error:
    • Couldn't match expected type ‘IO b0’ with actual type ‘()’
    • In the first argument of ‘Just’, namely ‘v’
      In the expression: Just v
      In a case alternative: Just v -> Just v
    |
183 |   mapM_ (\n -> fromMaybe () (case if ((n `mod` 2) == 0) then Nothing else Nothing of Just v -> Just v; Nothing -> case if (n > 7) then Just () else Nothing of Just v -> Just v; Nothing -> (let _ = putStrLn (unwords ["odd number:", show n]) in Nothing))) numbers
    |                                                                                                                                                                               ^

```

## tests/vm/valid/cast_struct.mochi

```
runhaskell error: exit status 1

tests/vm/valid/cast_struct.hs.out:191:20: error:
    • Couldn't match expected type ‘Todo’
                  with actual type ‘Map.Map String String’
    • In the first argument of ‘title’, namely ‘(todo)’
      In the first argument of ‘putStrLn’, namely ‘(title (todo))’
      In a stmt of a 'do' block: putStrLn (title (todo))
    |
191 |   putStrLn (title (todo))
    |                    ^^^^

```

## tests/vm/valid/cross_join.mochi

```
runhaskell error: exit status 1

tests/vm/valid/cross_join.hs.out:183:258: error:
    • Couldn't match type ‘AnyValue’ with ‘[Char]’
      Expected: Map.Map String String
        Actual: Map.Map String AnyValue
    • In the second argument of ‘Map.lookup’, namely ‘c’
      In the second argument of ‘fromMaybe’, namely
        ‘(Map.lookup "name" c)’
      In the first argument of ‘VString’, namely
        ‘(fromMaybe (error "missing") (Map.lookup "name" c))’
    |
183 | result = [Map.fromList [("orderId", VInt (fromMaybe (error "missing") (Map.lookup "id" o))), ("orderCustomerId", VInt (fromMaybe (error "missing") (Map.lookup "customerId" o))), ("pairedCustomerName", VString (fromMaybe (error "missing") (Map.lookup "name" c))), ("orderTotal", VInt (fromMaybe (error "missing") (Map.lookup "total" o)))] | o <- orders, c <- customers]
    |                                                                                                                                                                                                                                                                  ^

tests/vm/valid/cross_join.hs.out:188:48: error:
    • Couldn't match expected type: t0 -> Maybe AnyValue -> String
                  with actual type: [Char]
    • The function ‘show’ is applied to three value arguments,
        but its type ‘(a0 -> Maybe a0 -> a0) -> [Char]’ has only one
      In the expression:
        show fromMaybe (error "missing") (Map.lookup "orderId" entry)
      In the first argument of ‘unwords’, namely
        ‘["Order",
          show fromMaybe (error "missing") (Map.lookup "orderId" entry),
          "(customerId:",
          show
            fromMaybe (error "missing") (Map.lookup "orderCustomerId" entry),
          ....]’
    |
188 |   mapM_ (\entry -> putStrLn (unwords ["Order", show fromMaybe (error "missing") (Map.lookup "orderId" entry), "(customerId:", show fromMaybe (error "missing") (Map.lookup "orderCustomerId" entry), ", total: $", show fromMaybe (error "missing") (Map.lookup "orderTotal" entry), ") paired with", show fromMaybe (error "missing") (Map.lookup "pairedCustomerName" entry)])) result
    |                                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

tests/vm/valid/cross_join.hs.out:188:127: error:
    • Couldn't match expected type: t1 -> Maybe AnyValue -> String
                  with actual type: [Char]
    • The function ‘show’ is applied to three value arguments,
        but its type ‘(a1 -> Maybe a1 -> a1) -> [Char]’ has only one
      In the expression:
        show
          fromMaybe (error "missing") (Map.lookup "orderCustomerId" entry)
      In the first argument of ‘unwords’, namely
        ‘["Order",
          show fromMaybe (error "missing") (Map.lookup "orderId" entry),
          "(customerId:",
          show
            fromMaybe (error "missing") (Map.lookup "orderCustomerId" entry),
          ....]’
    |
188 |   mapM_ (\entry -> putStrLn (unwords ["Order", show fromMaybe (error "missing") (Map.lookup "orderId" entry), "(customerId:", show fromMaybe (error "missing") (Map.lookup "orderCustomerId" entry), ", total: $", show fromMaybe (error "missing") (Map.lookup "orderTotal" entry), ") paired with", show fromMaybe (error "missing") (Map.lookup "pairedCustomerName" entry)])) result
    |                                                                                                                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

tests/vm/valid/cross_join.hs.out:188:212: error:
    • Couldn't match expected type: t2 -> Maybe AnyValue -> String
                  with actual type: [Char]
    • The function ‘show’ is applied to three value arguments,
        but its type ‘(a2 -> Maybe a2 -> a2) -> [Char]’ has only one
      In the expression:
        show fromMaybe (error "missing") (Map.lookup "orderTotal" entry)
      In the first argument of ‘unwords’, namely
        ‘["Order",
          show fromMaybe (error "missing") (Map.lookup "orderId" entry),
          "(customerId:",
          show
            fromMaybe (error "missing") (Map.lookup "orderCustomerId" entry),
          ....]’
    |
188 |   mapM_ (\entry -> putStrLn (unwords ["Order", show fromMaybe (error "missing") (Map.lookup "orderId" entry), "(customerId:", show fromMaybe (error "missing") (Map.lookup "orderCustomerId" entry), ", total: $", show fromMaybe (error "missing") (Map.lookup "orderTotal" entry), ") paired with", show fromMaybe (error "missing") (Map.lookup "pairedCustomerName" entry)])) result
    |                                                                                                                                                                                                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

tests/vm/valid/cross_join.hs.out:188:295: error:
    • Couldn't match expected type: t3 -> Maybe AnyValue -> String
                  with actual type: [Char]
    • The function ‘show’ is applied to three value arguments,
        but its type ‘(a3 -> Maybe a3 -> a3) -> [Char]’ has only one
      In the expression:
        show
          fromMaybe (error "missing") (Map.lookup "pairedCustomerName" entry)
      In the first argument of ‘unwords’, namely
        ‘["Order",
          show fromMaybe (error "missing") (Map.lookup "orderId" entry),
          "(customerId:",
          show
            fromMaybe (error "missing") (Map.lookup "orderCustomerId" entry),
          ....]’
    |
188 |   mapM_ (\entry -> putStrLn (unwords ["Order", show fromMaybe (error "missing") (Map.lookup "orderId" entry), "(customerId:", show fromMaybe (error "missing") (Map.lookup "orderCustomerId" entry), ", total: $", show fromMaybe (error "missing") (Map.lookup "orderTotal" entry), ") paired with", show fromMaybe (error "missing") (Map.lookup "pairedCustomerName" entry)])) result
    |                                                                                                                                                                                                                                                                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

```

## tests/vm/valid/cross_join_filter.mochi

```
runhaskell error: exit status 1

tests/vm/valid/cross_join_filter.hs.out:188:35: error:
    • Couldn't match expected type: t0 -> Maybe AnyValue -> String
                  with actual type: [Char]
    • The function ‘show’ is applied to three value arguments,
        but its type ‘(a0 -> Maybe a0 -> a0) -> [Char]’ has only one
      In the expression:
        show fromMaybe (error "missing") (Map.lookup "n" p)
      In the first argument of ‘unwords’, namely
        ‘[show fromMaybe (error "missing") (Map.lookup "n" p),
          show fromMaybe (error "missing") (Map.lookup "l" p)]’
    |
188 |   mapM_ (\p -> putStrLn (unwords [show fromMaybe (error "missing") (Map.lookup "n" p), show fromMaybe (error "missing") (Map.lookup "l" p)])) pairs
    |                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

tests/vm/valid/cross_join_filter.hs.out:188:88: error:
    • Couldn't match expected type: t1 -> Maybe AnyValue -> String
                  with actual type: [Char]
    • The function ‘show’ is applied to three value arguments,
        but its type ‘(a1 -> Maybe a1 -> a1) -> [Char]’ has only one
      In the expression:
        show fromMaybe (error "missing") (Map.lookup "l" p)
      In the first argument of ‘unwords’, namely
        ‘[show fromMaybe (error "missing") (Map.lookup "n" p),
          show fromMaybe (error "missing") (Map.lookup "l" p)]’
    |
188 |   mapM_ (\p -> putStrLn (unwords [show fromMaybe (error "missing") (Map.lookup "n" p), show fromMaybe (error "missing") (Map.lookup "l" p)])) pairs
    |                                                                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

```

## tests/vm/valid/cross_join_triple.mochi

```
runhaskell error: exit status 1

tests/vm/valid/cross_join_triple.hs.out:190:35: error:
    • Couldn't match expected type: t0 -> Maybe AnyValue -> String
                  with actual type: [Char]
    • The function ‘show’ is applied to three value arguments,
        but its type ‘(a0 -> Maybe a0 -> a0) -> [Char]’ has only one
      In the expression:
        show fromMaybe (error "missing") (Map.lookup "n" c)
      In the first argument of ‘unwords’, namely
        ‘[show fromMaybe (error "missing") (Map.lookup "n" c),
          show fromMaybe (error "missing") (Map.lookup "l" c),
          show fromMaybe (error "missing") (Map.lookup "b" c)]’
    |
190 |   mapM_ (\c -> putStrLn (unwords [show fromMaybe (error "missing") (Map.lookup "n" c), show fromMaybe (error "missing") (Map.lookup "l" c), show fromMaybe (error "missing") (Map.lookup "b" c)])) combos
    |                                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

tests/vm/valid/cross_join_triple.hs.out:190:88: error:
    • Couldn't match expected type: t1 -> Maybe AnyValue -> String
                  with actual type: [Char]
    • The function ‘show’ is applied to three value arguments,
        but its type ‘(a1 -> Maybe a1 -> a1) -> [Char]’ has only one
      In the expression:
        show fromMaybe (error "missing") (Map.lookup "l" c)
      In the first argument of ‘unwords’, namely
        ‘[show fromMaybe (error "missing") (Map.lookup "n" c),
          show fromMaybe (error "missing") (Map.lookup "l" c),
          show fromMaybe (error "missing") (Map.lookup "b" c)]’
    |
190 |   mapM_ (\c -> putStrLn (unwords [show fromMaybe (error "missing") (Map.lookup "n" c), show fromMaybe (error "missing") (Map.lookup "l" c), show fromMaybe (error "missing") (Map.lookup "b" c)])) combos
    |                                                                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

tests/vm/valid/cross_join_triple.hs.out:190:141: error:
    • Couldn't match expected type: t2 -> Maybe AnyValue -> String
                  with actual type: [Char]
    • The function ‘show’ is applied to three value arguments,
        but its type ‘(a2 -> Maybe a2 -> a2) -> [Char]’ has only one
      In the expression:
        show fromMaybe (error "missing") (Map.lookup "b" c)
      In the first argument of ‘unwords’, namely
        ‘[show fromMaybe (error "missing") (Map.lookup "n" c),
          show fromMaybe (error "missing") (Map.lookup "l" c),
          show fromMaybe (error "missing") (Map.lookup "b" c)]’
    |
190 |   mapM_ (\c -> putStrLn (unwords [show fromMaybe (error "missing") (Map.lookup "n" c), show fromMaybe (error "missing") (Map.lookup "l" c), show fromMaybe (error "missing") (Map.lookup "b" c)])) combos
    |                                                                                                                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

```

## tests/vm/valid/dataset_sort_take_limit.mochi

```
runhaskell error: exit status 1

tests/vm/valid/dataset_sort_take_limit.hs.out:181:13: error:
    • Couldn't match expected type: t3
                                    -> ((a3 -> b0) -> [a3] -> [b0])
                                    -> ((a4, b1) -> b1)
                                    -> [(AnyValue, Map.Map String AnyValue)]
                                    -> t
                  with actual type: [a6]
    • The function ‘take’ is applied to six value arguments,
        but its type ‘Int -> [a6] -> [a6]’ has only two
      In the expression:
        take
          3 drop 1 map snd
          (List.sortOn
             fst
             [((- fromMaybe (error "missing") (Map.lookup "price" (p))), p) |
                p <- products])
      In an equation for ‘expensive’:
          expensive
            = take
                3 drop 1 map snd
                (List.sortOn
                   fst
                   [((- fromMaybe (error "missing") (Map.lookup "price" (p))), p) |
                      p <- products])
    • Relevant bindings include
        expensive :: t
          (bound at tests/vm/valid/dataset_sort_take_limit.hs.out:181:1)
    |
181 | expensive = take 3 drop 1 map snd (List.sortOn fst [((-fromMaybe (error "missing") (Map.lookup "price" (p))), p) | p <- products])
    |             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

tests/vm/valid/dataset_sort_take_limit.hs.out:181:20: error:
    • Couldn't match expected type: [a6]
                  with actual type: Int -> [a5] -> [a5]
    • Probable cause: ‘drop’ is applied to too few arguments
      In the second argument of ‘take’, namely ‘drop’
      In the expression:
        take
          3 drop 1 map snd
          (List.sortOn
             fst
             [((- fromMaybe (error "missing") (Map.lookup "price" (p))), p) |
                p <- products])
      In an equation for ‘expensive’:
          expensive
            = take
                3 drop 1 map snd
                (List.sortOn
                   fst
                   [((- fromMaybe (error "missing") (Map.lookup "price" (p))), p) |
                      p <- products])
    |
181 | expensive = take 3 drop 1 map snd (List.sortOn fst [((-fromMaybe (error "missing") (Map.lookup "price" (p))), p) | p <- products])
    |                    ^^^^

tests/vm/valid/dataset_sort_take_limit.hs.out:186:38: error:
    • Couldn't match expected type: t0 -> Maybe a0 -> String
                  with actual type: [Char]
    • The function ‘show’ is applied to three value arguments,
        but its type ‘(a1 -> Maybe a1 -> a1) -> [Char]’ has only one
      In the expression:
        show fromMaybe (error "missing") (Map.lookup "name" item)
      In the first argument of ‘unwords’, namely
        ‘[show fromMaybe (error "missing") (Map.lookup "name" item),
          "costs $",
          show fromMaybe (error "missing") (Map.lookup "price" item)]’
    • Relevant bindings include
        item :: Map.Map String a0
          (bound at tests/vm/valid/dataset_sort_take_limit.hs.out:186:11)
    |
186 |   mapM_ (\item -> putStrLn (unwords [show fromMaybe (error "missing") (Map.lookup "name" item), "costs $", show fromMaybe (error "missing") (Map.lookup "price" item)])) expensive
    |                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

tests/vm/valid/dataset_sort_take_limit.hs.out:186:108: error:
    • Couldn't match expected type: t1 -> Maybe a0 -> String
                  with actual type: [Char]
    • The function ‘show’ is applied to three value arguments,
        but its type ‘(a2 -> Maybe a2 -> a2) -> [Char]’ has only one
      In the expression:
        show fromMaybe (error "missing") (Map.lookup "price" item)
      In the first argument of ‘unwords’, namely
        ‘[show fromMaybe (error "missing") (Map.lookup "name" item),
          "costs $",
          show fromMaybe (error "missing") (Map.lookup "price" item)]’
    • Relevant bindings include
        item :: Map.Map String a0
          (bound at tests/vm/valid/dataset_sort_take_limit.hs.out:186:11)
    |
186 |   mapM_ (\item -> putStrLn (unwords [show fromMaybe (error "missing") (Map.lookup "name" item), "costs $", show fromMaybe (error "missing") (Map.lookup "price" item)])) expensive
    |                                                                                                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

```

## tests/vm/valid/dataset_where_filter.mochi

```
runhaskell error: exit status 1

tests/vm/valid/dataset_where_filter.hs.out:181:90: error:
    • Couldn't match type ‘AnyValue’ with ‘[Char]’
      Expected: Map.Map String String
        Actual: Map.Map String AnyValue
    • In the second argument of ‘Map.lookup’, namely ‘person’
      In the second argument of ‘fromMaybe’, namely
        ‘(Map.lookup "name" person)’
      In the first argument of ‘VString’, namely
        ‘(fromMaybe (error "missing") (Map.lookup "name" person))’
    |
181 | adults = [Map.fromList [("name", VString (fromMaybe (error "missing") (Map.lookup "name" person))), ("age", VString (fromMaybe (error "missing") (Map.lookup "age" person))), ("is_senior", VBool ((fromMaybe (error "missing") (Map.lookup "age" person) >= 60)))] | person <- filter (\person -> (fromMaybe (error "missing") (Map.lookup "age" person) >= 18)) people]
    |                                                                                          ^^^^^^

tests/vm/valid/dataset_where_filter.hs.out:181:164: error:
    • Couldn't match type ‘AnyValue’ with ‘[Char]’
      Expected: Map.Map String String
        Actual: Map.Map String AnyValue
    • In the second argument of ‘Map.lookup’, namely ‘person’
      In the second argument of ‘fromMaybe’, namely
        ‘(Map.lookup "age" person)’
      In the first argument of ‘VString’, namely
        ‘(fromMaybe (error "missing") (Map.lookup "age" person))’
    |
181 | adults = [Map.fromList [("name", VString (fromMaybe (error "missing") (Map.lookup "name" person))), ("age", VString (fromMaybe (error "missing") (Map.lookup "age" person))), ("is_senior", VBool ((fromMaybe (error "missing") (Map.lookup "age" person) >= 60)))] | person <- filter (\person -> (fromMaybe (error "missing") (Map.lookup "age" person) >= 18)) people]
    |                                                                                                                                                                    ^^^^^^

tests/vm/valid/dataset_where_filter.hs.out:186:40: error:
    • Couldn't match expected type: t0 -> Maybe AnyValue -> String
                  with actual type: [Char]
    • The function ‘show’ is applied to three value arguments,
        but its type ‘(a0 -> Maybe a0 -> a0) -> [Char]’ has only one
      In the expression:
        show fromMaybe (error "missing") (Map.lookup "name" person)
      In the first argument of ‘unwords’, namely
        ‘[show fromMaybe (error "missing") (Map.lookup "name" person),
          "is", show fromMaybe (error "missing") (Map.lookup "age" person),
          0]’
    |
186 |   mapM_ (\person -> putStrLn (unwords [show fromMaybe (error "missing") (Map.lookup "name" person), "is", show fromMaybe (error "missing") (Map.lookup "age" person), 0])) adults
    |                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

tests/vm/valid/dataset_where_filter.hs.out:186:107: error:
    • Couldn't match expected type: t1 -> Maybe AnyValue -> String
                  with actual type: [Char]
    • The function ‘show’ is applied to three value arguments,
        but its type ‘(a1 -> Maybe a1 -> a1) -> [Char]’ has only one
      In the expression:
        show fromMaybe (error "missing") (Map.lookup "age" person)
      In the first argument of ‘unwords’, namely
        ‘[show fromMaybe (error "missing") (Map.lookup "name" person),
          "is", show fromMaybe (error "missing") (Map.lookup "age" person),
          0]’
    |
186 |   mapM_ (\person -> putStrLn (unwords [show fromMaybe (error "missing") (Map.lookup "name" person), "is", show fromMaybe (error "missing") (Map.lookup "age" person), 0])) adults
    |                                                                                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

```

## tests/vm/valid/exists_builtin.mochi

```
runhaskell error: exit status 1

tests/vm/valid/exists_builtin.hs.out:172:6: error:
    parse error on input ‘=’
    |
172 | data = [1, 2]
    |      ^

```

