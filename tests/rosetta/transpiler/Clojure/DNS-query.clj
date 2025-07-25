(ns main)

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def net {:LookupHost (fn [host] [["2001:2f0:0:8800:226:2dff:fe0b:4311" "2001:2f0:0:8800::1:1" "210.155.141.200"] nil])})

(def res ((:LookupHost net) "www.kame.net"))

(def addrs (nth res 0))

(def err (nth res 1))

(defn -main []
  (if (= err nil) (println (str addrs)) (println err)))

(-main)
