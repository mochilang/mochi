(ns main (:refer-clojure :exclude [f g h main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare f g h main)

(declare main_list)

(defn f []
  (try (throw (ex-info "return" {:v [0 0.0]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn g [g_a g_b]
  (try (throw (ex-info "return" {:v 0})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn h [h_s h_nums]
  (do))

(defn main []
  (do (h "ex1" []) (h "ex2" [1 2]) (h "ex3" [1 2 3 4]) (def main_list [1 2 3 4]) (h "ex4" main_list)))

(defn -main []
  (main))

(-main)
