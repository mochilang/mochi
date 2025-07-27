(ns main (:refer-clojure :exclude [flood]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare flood)

(defn ff [x y repl px py]
  (try (do (when (or (or (or (< px 0) (< py 0)) (>= py (count grid))) (>= px (count (nth grid 0)))) (throw (ex-info "return" {:v nil}))) (when (not= (nth (nth grid py) px) target) (throw (ex-info "return" {:v nil}))) (def grid (assoc-in grid [py px] repl)) (ff (- px 1) py) (ff (+ px 1) py) (ff px (- py 1)) (ff px (+ py 1))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn flood [x y repl]
  (try (do (def target (nth (nth grid y) x)) (when (= target repl) (throw (ex-info "return" {:v nil}))) (ff x y repl x y)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (def grid [["." "." "." "." "."] ["." "#" "#" "#" "."] ["." "#" "." "#" "."] ["." "#" "#" "#" "."] ["." "." "." "." "."]])
  (flood 2 2 "o")
  (doseq [row grid] (do (def line "") (doseq [ch row] (def line (+ line ch))) (println line))))

(-main)
