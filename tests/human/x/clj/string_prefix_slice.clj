(ns string-prefix-slice
  (:require [clojure.string :as str]))

(def prefix "fore")
(def s1 "forest")
(println (str/starts-with? s1 prefix))
(def s2 "desert")
(println (str/starts-with? s2 prefix))
