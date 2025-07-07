(ns string-in-operator
  (:require [clojure.string :as str]))

(def s "catch")
(println (str/includes? s "cat"))
(println (str/includes? s "dog"))
