(ns main)

(require 'clojure.set)

(def math {:sqrt (fn [x] (Math/sqrt x)) :pow (fn [x y] (Math/pow x y)) :sin (fn [x] (Math/sin x)) :log (fn [x] (Math/log x)) :pi Math/PI :e Math/E})

(def r 3)

(def area (* (:pi math) ((:pow math) r 2)))

(def root ((:sqrt math) 49))

(def sin45 ((:sin math) (/ (:pi math) 4)))

(def log_e ((:log math) (:e math)))

(defn -main []
  (println "Circle area with r =" r "=>" area)
  (println "Square root of 49:" root)
  (println "sin(π/4):" sin45)
  (println "log(e):" log_e))

(-main)
