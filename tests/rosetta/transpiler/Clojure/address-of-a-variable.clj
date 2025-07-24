(ns main)

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def myVar 3.14)

(defn -main []
  (println "value as float:" myVar)
  (println "address: <not available>"))

(-main)
