(ns main (:refer-clojure :exclude [abs sqrtApprox agm main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare abs sqrtApprox agm main)

(defn abs [x_v0]
  (try (if (< x_v0 0.0) (- x_v0) x_v0) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [x_v0]
  (try (do (def guess_v1 x_v0) (def i_v2 0) (while (< i_v2 20) (do (def guess_v1 (/ (+ guess_v1 (/ x_v0 guess_v1)) 2.0)) (def i_v2 (+ i_v2 1)))) (throw (ex-info "return" {:v guess_v1}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn agm [a_v0 g_v1]
  (try (do (def eps_v2 0.00000000000001) (while (> (abs (- a_v0 g_v1)) (* (abs a_v0) eps_v2)) (do (def newA_v3 (/ (+ a_v0 g_v1) 2.0)) (def newG_v4 (sqrtApprox (* a_v0 g_v1))) (def a_v0 newA_v3) (def g_v1 newG_v4))) (throw (ex-info "return" {:v a_v0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (println (str (agm 1.0 (/ 1.0 (sqrtApprox 2.0))))))

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
