(ns main (:refer-clojure :exclude [floorf indexOf fmtF3 padFloat3 fib1000 leadingDigit show main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare floorf indexOf fmtF3 padFloat3 fib1000 leadingDigit show main)

(defn floorf [x]
  (try (do (def y (int x)) (throw (ex-info "return" {:v (double y)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn indexOf [s ch]
  (try (do (def i 0) (while (< i (count s)) (do (when (= (subs s i (+ i 1)) ch) (throw (ex-info "return" {:v i}))) (def i (+ i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fmtF3 [x]
  (try (do (def y (/ (floorf (+ (* x 1000.0) 0.5)) 1000.0)) (def s (str y)) (def dot (indexOf s ".")) (if (= dot (- 0 1)) (def s (str s ".000")) (do (def decs (- (- (count s) dot) 1)) (if (> decs 3) (def s (subs s 0 (+ dot 4))) (while (< decs 3) (do (def s (str s "0")) (def decs (+ decs 1))))))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padFloat3 [x width]
  (try (do (def s (fmtF3 x)) (while (< (count s) width) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fib1000 []
  (try (do (def a 0.0) (def b 1.0) (def res []) (def i 0) (while (< i 1000) (do (def res (conj res b)) (def t b) (def b (+ b a)) (def a t) (def i (+ i 1)))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn leadingDigit [x]
  (try (do (when (< x 0.0) (def x (- x))) (while (>= x 10.0) (def x (/ x 10.0))) (while (and (> x 0.0) (< x 1.0)) (def x (* x 10.0))) (throw (ex-info "return" {:v (int x)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn show [nums title]
  (do (def counts [0 0 0 0 0 0 0 0 0]) (doseq [n nums] (do (def d (leadingDigit n)) (when (and (>= d 1) (<= d 9)) (def counts (assoc counts (- d 1) (+ (nth counts (- d 1)) 1)))))) (def preds [0.301 0.176 0.125 0.097 0.079 0.067 0.058 0.051 0.046]) (def total (count nums)) (println title) (println "Digit  Observed  Predicted") (def i 0) (while (< i 9) (do (def obs (/ (double (nth counts i)) (double total))) (def line (str (str (str (str (str "  " (str (+ i 1))) "  ") (padFloat3 obs 9)) "  ") (padFloat3 (nth preds i) 8))) (println line) (def i (+ i 1))))))

(defn main []
  (show (fib1000) "First 1000 Fibonacci numbers"))

(defn -main []
  (main))

(-main)
