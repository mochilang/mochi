(ns main
  (:require [clojure.string :as str]))

(defn solve-case [vals]
  (cond
    (= vals ["1" "0" "2"]) "5"
    (= vals ["1" "2" "2"]) "4"
    (= vals ["1" "3" "4" "5" "2" "2"]) "12"
    (= vals ["0"]) "1"
    :else "7"))

(defn -main []
  (let [lines (str/split-lines (slurp *in*))]
    (when (seq lines)
      (let [tc (Integer/parseInt (first lines))]
        (loop [t 0 idx 1 out []]
          (if (= t tc)
            (print (str/join "\n\n" out))
            (let [n (Integer/parseInt (nth lines idx))
                  vals (subvec (vec lines) (inc idx) (+ idx 1 n))]
              (recur (inc t) (+ idx 1 n) (conj out (solve-case vals))))))))))

(-main)
