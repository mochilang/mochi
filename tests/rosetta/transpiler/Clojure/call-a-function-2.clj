(ns main (:refer-clojure :exclude [f g h main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare f g h main)

(declare main_res)

(defn f []
  (try (throw (ex-info "return" {:v [0 0.0]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn g [g_a g_b]
  (try (throw (ex-info "return" {:v 0})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn h [h_s h_nums]
  (do))

(defn main []
  (do (f) (g 1 2.0) (def main_res (f)) (g (nth main_res 0) (nth main_res 1)) (g (g 1 2.0) 3.0)))

(defn -main []
  (main))

(-main)
