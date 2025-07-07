(ns group-by-having
  (:require [clojure.data.json :as json]))

(def people
  [{:name "Alice" :city "Paris"}
   {:name "Bob" :city "Hanoi"}
   {:name "Charlie" :city "Paris"}
   {:name "Diana" :city "Hanoi"}
   {:name "Eve" :city "Paris"}
   {:name "Frank" :city "Hanoi"}
   {:name "George" :city "Paris"}])

(def big
  (->> people
       (group-by :city)
       (map (fn [[city grp]] {:city city :num (count grp)}))
       (filter #(>= (:num %) 4))))

(println (json/write-str big))
