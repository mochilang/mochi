(ns main)

(declare r area root sin45 log_e)

(defn -main []
  (def r 3.0) ;; float
  (def area (* Math/PI (Math/pow r 2.0))) ;; float
  (def root (Math/sqrt 49.0)) ;; float
  (def sin45 (Math/sin (/ Math/PI 4.0))) ;; float
  (def log_e (Math/log Math/E)) ;; float
  (println "Circle area with r =" r "=>" area)
  (println "Square root of 49:" root)
  (println "sin(π/4):" sin45)
  (println "log(e):" log_e)
)

(-main)
