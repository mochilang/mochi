(ns main (:refer-clojure :exclude [bernoulli binom coeff main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare bernoulli binom coeff main)

(declare a base c exp i j kk line m p res)

(defn bernoulli [n]
  (try (do (def a []) (def m 0) (while (<= m n) (do (def a (conj a (/ (/ (bigint 1) 1) (/ (bigint (+ m 1)) 1)))) (def j m) (while (>= j 1) (do (def a (assoc a (- j 1) (* (/ (bigint j) 1) (- (nth a (- j 1)) (nth a j))))) (def j (- j 1)))) (def m (+ m 1)))) (throw (ex-info "return" {:v (nth a 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn binom [n k]
  (try (do (when (or (< k 0) (> k n)) (throw (ex-info "return" {:v (bigint 0)}))) (def kk k) (when (> kk (- n kk)) (def kk (- n kk))) (def res 1) (def i 0) (while (< i kk) (do (def res (* res (bigint (- n i)))) (def i (+ i 1)) (def res (/ res (bigint i))))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn coeff [p j]
  (try (do (def base (/ (/ (bigint 1) 1) (/ (bigint (+ p 1)) 1))) (def c base) (when (= (mod j 2) 1) (def c (- c))) (def c (* c (/ (bigint (binom (+ p 1) j)) 1))) (def c (* c (bernoulli j))) (throw (ex-info "return" {:v c}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def p 0) (while (< p 10) (do (def line (str (str p) " :")) (def j 0) (while (<= j p) (do (def c (coeff p j)) (when (not= (str c) "0/1") (do (def line (str (str (str line " ") (str c)) "×n")) (def exp (- (+ p 1) j)) (when (> exp 1) (def line (str (str line "^") (str exp)))))) (def j (+ j 1)))) (println line) (def p (+ p 1))))))

(defn -main []
  (main))

(-main)
