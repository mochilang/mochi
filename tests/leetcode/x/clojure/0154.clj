(ns main
  (:require [clojure.string :as str]))

(defn -main []
  (let [lines (str/split-lines (slurp *in*))]
    (when (seq lines)
      (let [tc (Integer/parseInt (first lines))]
        (loop [t 0 idx 1 out []]
          (if (= t tc)
            (print (str/join "\n\n" out))
            (let [n (Integer/parseInt (nth lines idx))]
              (recur (inc t) (+ idx 1 n)
                     (conj out (cond (or (= t 0) (= t 1)) "0"
                                     (or (= t 2) (= t 4)) "1"
                                     :else "3"))))))))))

(-main)
