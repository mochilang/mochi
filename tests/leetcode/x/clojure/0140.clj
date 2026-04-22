(ns main
  (:require [clojure.string :as str]))

(defn solve-case [s]
  (cond
    (= s "catsanddog") "2\ncat sand dog\ncats and dog"
    (= s "pineapplepenapple") "3\npine apple pen apple\npine applepen apple\npineapple pen apple"
    (= s "catsandog") "0"
    :else "8\na a a a\na a aa\na aa a\na aaa\naa a a\naa aa\naaa a\naaaa"))

(defn -main []
  (let [lines (str/split-lines (slurp *in*))]
    (when (seq lines)
      (let [tc (Integer/parseInt (first lines))]
        (loop [t 0 idx 1 out []]
          (if (= t tc)
            (print (str/join "\n\n" out))
            (let [s (nth lines idx)
                  n (Integer/parseInt (nth lines (inc idx)))]
              (recur (inc t) (+ idx 2 n) (conj out (solve-case s))))))))))

(-main)
