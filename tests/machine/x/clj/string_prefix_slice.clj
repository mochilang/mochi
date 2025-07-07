(ns main)

(declare prefix s1 s2)

(defn -main []
  (def prefix "fore") ;; string
  (def s1 "forest") ;; string
  (println (= (subs s1 0 (count prefix)) prefix))
  (def s2 "desert") ;; string
  (println (= (subs s2 0 (count prefix)) prefix))
)

(-main)
