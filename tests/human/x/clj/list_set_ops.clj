(ns list-set-ops
  (:require [clojure.set :as set]))

(println (vec (set/union #{1 2} #{2 3})))
(println (vec (set/difference #{1 2 3} #{2})))
(println (vec (set/intersection #{1 2 3} #{2 4})))
(println (count (concat [1 2] [2 3])))
