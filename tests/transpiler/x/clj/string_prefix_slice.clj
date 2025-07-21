(ns main)

(require 'clojure.set)

(def prefix "fore")

(def s1 "forest")

(def s2 "desert")

(defn -main []
  (println (= (subs s1 0 (count prefix)) prefix))
  (println (= (subs s2 0 (count prefix)) prefix)))

(-main)
