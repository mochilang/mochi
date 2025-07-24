(ns main)

(require 'clojure.set)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def res ((:LookupHost net) "www.kame.net"))

(def addrs (nth res 0))

(def err (nth res 1))

(defn -main []
  (if (= err nil) (println (str addrs)) (println err)))

(-main)
