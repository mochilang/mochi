(ns main (:refer-clojure :exclude [floorf frac sinApprox sqrtApprox]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare floorf frac sinApprox sqrtApprox)

(declare PI ci denom f fx fy guess h i n nframes rem sum term total value w x y)

(def PI 3.141592653589793)

(defn floorf [x]
  (try (do (def i (int x)) (when (> (double i) x) (def i (- i 1))) (throw (ex-info "return" {:v (double i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn frac [x]
  (try (throw (ex-info "return" {:v (- x (floorf x))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sinApprox [x]
  (try (do (def term x) (def sum x) (def n 1) (while (<= n 10) (do (def denom (double (* (* 2 n) (+' (* 2 n) 1)))) (def term (/ (* (* (- term) x) x) denom)) (def sum (+' sum term)) (def n (+' n 1)))) (throw (ex-info "return" {:v sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [x]
  (try (do (when (<= x 0) (throw (ex-info "return" {:v 0.0}))) (def guess x) (def i 0) (while (< i 10) (do (def guess (/ (+' guess (/ x guess)) 2.0)) (def i (+' i 1)))) (throw (ex-info "return" {:v guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def nframes 10)

(def w 32)

(def h 32)

(def total 0)

(def f 1)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (<= f nframes) (do (def y 0) (while (< y h) (do (def x 0) (while (< x w) (do (def fx (double x)) (def fy (double y)) (def value (sinApprox (/ fx 16.0))) (def value (+' value (sinApprox (/ fy 8.0)))) (def value (+' value (sinApprox (/ (+' fx fy) 16.0)))) (def value (+' value (sinApprox (/ (sqrtApprox (+' (* fx fx) (* fy fy))) 8.0)))) (def value (+' value 4.0)) (def value (/ value 8.0)) (def rem (frac (+' value (/ (double f) (double nframes))))) (def ci (+' (int (* (double nframes) rem)) 1)) (def total (+' total ci)) (def x (+' x 1)))) (def y (+' y 1)))) (def f (+' f 1))))
      (println total)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
