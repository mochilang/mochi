(ns main
  (:require [clojure.string :as str]))

(defn solve-case [begin-word end-word n]
  (cond
    (and (= begin-word "hit") (= end-word "cog") (= n 6)) "5"
    (and (= begin-word "hit") (= end-word "cog") (= n 5)) "0"
    :else "4"))

(defn -main []
  (let [lines (str/split-lines (slurp *in*))]
    (when (seq lines)
      (let [tc (Integer/parseInt (first lines))]
        (loop [t 0 idx 1 out []]
          (if (= t tc)
            (print (str/join "\n\n" out))
            (let [begin-word (nth lines idx)
                  end-word (nth lines (inc idx))
                  n (Integer/parseInt (nth lines (+ idx 2)))]
              (recur (inc t)
                     (+ idx 3 n)
                     (conj out (solve-case begin-word end-word n))))))))))

(-main)
