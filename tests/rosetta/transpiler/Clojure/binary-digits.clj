(ns main (:refer-clojure :exclude [toBin]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare toBin)

(defn toBin [n]
  (try (do (when (= n 0) (throw (ex-info "return" {:v "0"}))) (def bits "") (def x n) (while (> x 0) (do (def bits (str (str (mod x 2)) bits)) (def x (int (/ x 2))))) (throw (ex-info "return" {:v bits}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (dotimes [i 16] (println (toBin i))))

(-main)
