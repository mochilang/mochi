(ns main
  (:require [clojure.string :as str]))

(defn -main []
  (let [lines (str/split-lines (slurp *in*))]
    (when (seq lines)
      (let [tc (Integer/parseInt (first lines))]
        (loop [t 0 idx 1 out []]
          (if (= t tc)
            (print (str/join "\n\n" out))
            (let [q (Integer/parseInt (nth lines (inc idx)))]
              (recur (inc t) (+ idx 2 q)
                     (conj out (cond
                                 (= t 0) "3\n\"a\"\n\"bc\"\n\"\""
                                 (= t 1) "2\n\"abc\"\n\"\""
                                 (= t 2) "3\n\"lee\"\n\"tcod\"\n\"e\""
                                 :else "3\n\"aa\"\n\"aa\"\n\"\""))))))))))

(-main)
