(ns main (:refer-clojure :exclude [sinApprox cosApprox atanApprox atan2Approx digit parseTwo parseSec pad meanTime main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sinApprox cosApprox atanApprox atan2Approx digit parseTwo parseSec pad meanTime main)

(defn sinApprox [x]
  (try (do (def term x) (def sum x) (def n 1) (while (<= n 8) (do (def denom (double (* (* 2 n) (+ (* 2 n) 1)))) (def term (/ (* (* (- term) x) x) denom)) (def sum (+ sum term)) (def n (+ n 1)))) (throw (ex-info "return" {:v sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn cosApprox [x]
  (try (do (def term 1.0) (def sum 1.0) (def n 1) (while (<= n 8) (do (def denom (double (* (- (* 2 n) 1) (* 2 n)))) (def term (/ (* (* (- term) x) x) denom)) (def sum (+ sum term)) (def n (+ n 1)))) (throw (ex-info "return" {:v sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn atanApprox [x]
  (try (do (when (> x 1.0) (throw (ex-info "return" {:v (- (/ PI 2.0) (/ x (+ (* x x) 0.28)))}))) (if (< x (- 1.0)) (- (/ (- PI) 2.0) (/ x (+ (* x x) 0.28))) (/ x (+ 1.0 (* (* 0.28 x) x))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn atan2Approx [y x]
  (try (do (when (> x 0.0) (throw (ex-info "return" {:v (atanApprox (/ y x))}))) (when (< x 0.0) (do (when (>= y 0.0) (throw (ex-info "return" {:v (+ (atanApprox (/ y x)) PI)}))) (throw (ex-info "return" {:v (- (atanApprox (/ y x)) PI)})))) (when (> y 0.0) (throw (ex-info "return" {:v (/ PI 2.0)}))) (if (< y 0.0) (/ (- PI) 2.0) 0.0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn digit [ch]
  (try (do (def digits "0123456789") (def i 0) (while (< i (count digits)) (do (when (= (subs digits i (+ i 1)) ch) (throw (ex-info "return" {:v i}))) (def i (+ i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseTwo [s idx]
  (try (throw (ex-info "return" {:v (+ (* (digit (subs s idx (+ idx 1))) 10) (digit (subs s (+ idx 1) (+ idx 2))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseSec [s]
  (try (do (def h (parseTwo s 0)) (def m (parseTwo s 3)) (def sec (parseTwo s 6)) (def tmp (+ (* (+ (* h 60) m) 60) sec)) (throw (ex-info "return" {:v (double tmp)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [n]
  (try (if (< n 10) (str "0" (str n)) (str n)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn meanTime [times]
  (try (do (def ssum 0.0) (def csum 0.0) (def i 0) (while (< i (count times)) (do (def sec (parseSec (nth times i))) (def ang (/ (* (* sec 2.0) PI) 86400.0)) (def ssum (+ ssum (sinApprox ang))) (def csum (+ csum (cosApprox ang))) (def i (+ i 1)))) (def theta (atan2Approx ssum csum)) (def frac (/ theta (* 2.0 PI))) (while (< frac 0.0) (def frac (+ frac 1.0))) (def total (* frac 86400.0)) (def si (int total)) (def h (int (/ si 3600))) (def m (int (/ (mod si 3600) 60))) (def s (int (mod si 60))) (throw (ex-info "return" {:v (str (str (str (str (pad h) ":") (pad m)) ":") (pad s))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def inputs ["23:00:17" "23:40:20" "00:12:45" "00:17:19"]) (println (meanTime inputs))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def PI 3.141592653589793)
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
