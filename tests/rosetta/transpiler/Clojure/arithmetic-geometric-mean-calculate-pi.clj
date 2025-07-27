(ns main (:refer-clojure :exclude [abs sqrtApprox agmPi main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare abs sqrtApprox agmPi main)

(defn abs [x]
  (try (if (< x 0.0) (- x) x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [x]
  (try (do (def guess x) (def i 0) (while (< i 20) (do (def guess (/ (+ guess (/ x guess)) 2.0)) (def i (+ i 1)))) (throw (ex-info "return" {:v guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn agmPi []
  (try (do (def a 1.0) (def g (/ 1.0 (sqrtApprox 2.0))) (def sum 0.0) (def pow 2.0) (while (> (abs (- a g)) 0.000000000000001) (do (def t (/ (+ a g) 2.0)) (def u (sqrtApprox (* a g))) (def a t) (def g u) (def pow (* pow 2.0)) (def diff (- (* a a) (* g g))) (def sum (+ sum (* diff pow))))) (def pi (/ (* (* 4.0 a) a) (- 1.0 sum))) (throw (ex-info "return" {:v pi}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (println (str (agmPi))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
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
