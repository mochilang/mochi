(ns main (:refer-clojure :exclude [classify]))

(require 'clojure.set)

(def x 2)

(def label (cond (= x 1) "one" (= x 2) "two" (= x 3) "three" true "unknown"))

(def day "sun")

(def mood (cond (= day "mon") "tired" (= day "fri") "excited" (= day "sun") "relaxed" true "normal"))

(def ok true)

(def status (cond (= ok true) "confirmed" (= ok false) "denied"))

(defn classify [n]
  (cond (= n 0) "zero" (= n 1) "one" true "many"))

(defn -main []
  (println label)
  (println mood)
  (println status)
  (println (classify 0))
  (println (classify 5)))

(-main)
