(ns main (:refer-clojure :exclude [expApprox tangent_hyperbolic main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare expApprox tangent_hyperbolic main)

(def ^:dynamic expApprox_n nil)

(def ^:dynamic expApprox_neg nil)

(def ^:dynamic expApprox_sum nil)

(def ^:dynamic expApprox_term nil)

(def ^:dynamic expApprox_y nil)

(def ^:dynamic main_v1 nil)

(def ^:dynamic main_v2 nil)

(def ^:dynamic tangent_hyperbolic_i nil)

(def ^:dynamic tangent_hyperbolic_result nil)

(def ^:dynamic tangent_hyperbolic_t nil)

(def ^:dynamic tangent_hyperbolic_x nil)

(defn expApprox [expApprox_x]
  (binding [expApprox_n nil expApprox_neg nil expApprox_sum nil expApprox_term nil expApprox_y nil] (try (do (set! expApprox_neg false) (set! expApprox_y expApprox_x) (when (< expApprox_x 0.0) (do (set! expApprox_neg true) (set! expApprox_y (- expApprox_x)))) (set! expApprox_term 1.0) (set! expApprox_sum 1.0) (set! expApprox_n 1) (while (< expApprox_n 30) (do (set! expApprox_term (quot (* expApprox_term expApprox_y) (double expApprox_n))) (set! expApprox_sum (+ expApprox_sum expApprox_term)) (set! expApprox_n (+ expApprox_n 1)))) (if expApprox_neg (/ 1.0 expApprox_sum) expApprox_sum)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn tangent_hyperbolic [tangent_hyperbolic_vector]
  (binding [tangent_hyperbolic_i nil tangent_hyperbolic_result nil tangent_hyperbolic_t nil tangent_hyperbolic_x nil] (try (do (set! tangent_hyperbolic_result []) (set! tangent_hyperbolic_i 0) (while (< tangent_hyperbolic_i (count tangent_hyperbolic_vector)) (do (set! tangent_hyperbolic_x (nth tangent_hyperbolic_vector tangent_hyperbolic_i)) (set! tangent_hyperbolic_t (- (/ 2.0 (+ 1.0 (expApprox (* (- 2.0) tangent_hyperbolic_x)))) 1.0)) (set! tangent_hyperbolic_result (conj tangent_hyperbolic_result tangent_hyperbolic_t)) (set! tangent_hyperbolic_i (+ tangent_hyperbolic_i 1)))) (throw (ex-info "return" {:v tangent_hyperbolic_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_v1 nil main_v2 nil] (do (set! main_v1 [1.0 5.0 6.0 (- 0.67)]) (set! main_v2 [8.0 10.0 2.0 (- 0.98) 13.0]) (println (str (tangent_hyperbolic main_v1))) (println (str (tangent_hyperbolic main_v2))))))

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
