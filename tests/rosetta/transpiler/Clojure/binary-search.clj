(ns main (:refer-clojure :exclude [bsearch bsearchRec main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare bsearch bsearchRec main)

(defn bsearch [arr x]
  (try (do (def low 0) (def high (- (count arr) 1)) (while (<= low high) (do (def mid (/ (+ low high) 2)) (if (> (nth arr mid) x) (def high (- mid 1)) (if (< (nth arr mid) x) (def low (+ mid 1)) (throw (ex-info "return" {:v mid})))))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bsearchRec [arr x low high]
  (try (do (when (< high low) (throw (ex-info "return" {:v (- 1)}))) (def mid (/ (+ low high) 2)) (if (> (nth arr mid) x) (throw (ex-info "return" {:v (bsearchRec arr x low (- mid 1))})) (when (< (nth arr mid) x) (throw (ex-info "return" {:v (bsearchRec arr x (+ mid 1) high)})))) (throw (ex-info "return" {:v mid}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def nums [(- 31) 0 1 2 2 4 65 83 99 782]) (def x 2) (def idx (bsearch nums x)) (if (>= idx 0) (println (str (str (str (str x) " is at index ") (str idx)) ".")) (println (str (str x) " is not found."))) (def x 5) (def idx (bsearchRec nums x 0 (- (count nums) 1))) (if (>= idx 0) (println (str (str (str (str x) " is at index ") (str idx)) ".")) (println (str (str x) " is not found.")))))

(defn -main []
  (main))

(-main)
