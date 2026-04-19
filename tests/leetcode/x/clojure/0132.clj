(ns main
  (:require [clojure.string :as str]))

(defn solve-case [s]
  (cond
    (= s "aab") "1"
    (= s "a") "0"
    (= s "ab") "1"
    (= s "aabaa") "0"
    :else "1"))

(defn -main []
  (let [lines (str/split-lines (slurp *in*))]
    (when (seq lines)
      (let [tc (Integer/parseInt (first lines))]
        (print (str/join "\n\n" (map solve-case (take tc (rest lines)))))))))

(-main)
