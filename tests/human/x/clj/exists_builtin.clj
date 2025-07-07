(ns exists-builtin)

(def data [1 2])
(def flag (boolean (some #{1} data)))
(println flag)
