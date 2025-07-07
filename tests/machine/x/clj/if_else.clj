(ns main)

(declare x)

(defn -main []
  (def x 5) ;; int
  (if (> x 3)
    (do
      (println "big")
    )
  
  (do
    (println "small")
  )
  )
)

(-main)
