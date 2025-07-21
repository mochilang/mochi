(ns main)

(require 'clojure.set)

(def math {:sqrt (fn [x] (Math/sqrt x)) :pow (fn [x y] (Math/pow x y)) :sin (fn [x] (Math/sin x)) :log (fn [x] (Math/log x)) :pi Math/PI :e Math/E})

(defn -main []
  (println ((:sqrt math) 16))
  (println (:pi math)))

(-main)
