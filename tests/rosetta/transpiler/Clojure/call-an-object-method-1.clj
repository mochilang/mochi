(ns main)

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare main_myPointer main_myValue)

(def main_myValue {})

(def main_myPointer {})

(defn -main []
  ((:ValueMethod main_myValue) 0)
  ((:PointerMethod main_myPointer) 0)
  ((:ValueMethod main_myPointer) 0)
  ((:PointerMethod main_myValue) 0)
  ((:ValueMethod main_myValue) 0)
  ((:PointerMethod main_myPointer) 0))

(-main)
