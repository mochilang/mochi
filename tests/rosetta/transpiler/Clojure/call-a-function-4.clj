(ns main (:refer-clojure :exclude [gifEncode main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare gifEncode main)

(declare main_opts)

(defn gifEncode [gifEncode_out gifEncode_img gifEncode_opts]
  (do))

(defn main []
  (do (def main_opts {}) (def main_opts (assoc main_opts "NumColors" 16)) (gifEncode nil nil main_opts)))

(defn -main []
  (main))

(-main)
