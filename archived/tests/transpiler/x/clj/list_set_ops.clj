(ns main)

(require 'clojure.set)

(defn -main []
  (println (vec (clojure.set/union (set [1 2]) (set [2 3]))))
  (println (vec (clojure.set/difference (set [1 2 3]) (set [2]))))
  (println (vec (clojure.set/intersection (set [1 2 3]) (set [2 4]))))
  (println (count (vec (clojure.set/union (set [1 2]) (set [2 3]))))))

(-main)
