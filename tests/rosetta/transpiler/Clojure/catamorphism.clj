(ns main (:refer-clojure :exclude [add sub mul fold]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare add sub mul fold)

(declare fold_i fold_r n)

(defn add [add_a add_b]
  (try (throw (ex-info "return" {:v (+ add_a add_b)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sub [sub_a sub_b]
  (try (throw (ex-info "return" {:v (- sub_a sub_b)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mul [mul_a mul_b]
  (try (throw (ex-info "return" {:v (* mul_a mul_b)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fold [fold_f fold_xs]
  (try (do (def fold_r (nth fold_xs 0)) (def fold_i 1) (while (< fold_i (count fold_xs)) (do (def fold_r (f fold_r (nth fold_xs fold_i))) (def fold_i (+ fold_i 1)))) (throw (ex-info "return" {:v fold_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def n [1 2 3 4 5])

(defn -main []
  (println (fold (fn [a b] (add a b)) n))
  (println (fold (fn [a b] (sub a b)) n))
  (println (fold (fn [a b] (mul a b)) n)))

(-main)
