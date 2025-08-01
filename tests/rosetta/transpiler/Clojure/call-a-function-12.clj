(ns main (:refer-clojure :exclude [mkAdd mysum partialSum main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare mkAdd mysum partialSum main)

(declare main_add2 main_add3 main_partial)

(defn mkAdd [mkAdd_a]
  (try (throw (ex-info "return" {:v (fn [b] (+ mkAdd_a mkAdd_b))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mysum [mysum_x mysum_y]
  (try (throw (ex-info "return" {:v (+ mysum_x mysum_y)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn partialSum [partialSum_x]
  (try (throw (ex-info "return" {:v (fn [y] (mysum partialSum_x partialSum_y))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def main_add2 (mkAdd 2)) (def main_add3 (mkAdd 3)) (println (str (str (str (add2 5)) " ") (str (add3 6)))) (def main_partial (partialSum 13)) (println (str (partial 5)))))

(defn -main []
  (main))

(-main)
