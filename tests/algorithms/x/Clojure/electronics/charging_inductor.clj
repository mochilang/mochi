(ns main (:refer-clojure :exclude [expApprox floor pow10 round charging_inductor]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare expApprox floor pow10 round charging_inductor)

(def ^:dynamic charging_inductor_current nil)

(def ^:dynamic charging_inductor_exponent nil)

(def ^:dynamic expApprox_half nil)

(def ^:dynamic expApprox_n nil)

(def ^:dynamic expApprox_sum nil)

(def ^:dynamic expApprox_term nil)

(def ^:dynamic floor_i nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_result nil)

(def ^:dynamic round_m nil)

(defn expApprox [expApprox_x]
  (binding [expApprox_half nil expApprox_n nil expApprox_sum nil expApprox_term nil] (try (do (when (< expApprox_x 0.0) (throw (ex-info "return" {:v (/ 1.0 (expApprox (- expApprox_x)))}))) (when (> expApprox_x 1.0) (do (set! expApprox_half (expApprox (/ expApprox_x 2.0))) (throw (ex-info "return" {:v (* expApprox_half expApprox_half)})))) (set! expApprox_sum 1.0) (set! expApprox_term 1.0) (set! expApprox_n 1) (while (< expApprox_n 20) (do (set! expApprox_term (quot (* expApprox_term expApprox_x) (double expApprox_n))) (set! expApprox_sum (+ expApprox_sum expApprox_term)) (set! expApprox_n (+ expApprox_n 1)))) (throw (ex-info "return" {:v expApprox_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_result nil] (try (do (set! pow10_result 1.0) (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_result (* pow10_result 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round [round_x round_n]
  (binding [round_m nil] (try (do (set! round_m (pow10 round_n)) (throw (ex-info "return" {:v (quot (floor (+ (* round_x round_m) 0.5)) round_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn charging_inductor [charging_inductor_source_voltage charging_inductor_resistance charging_inductor_inductance charging_inductor_time]
  (binding [charging_inductor_current nil charging_inductor_exponent nil] (try (do (when (<= charging_inductor_source_voltage 0.0) (throw (Exception. "Source voltage must be positive."))) (when (<= charging_inductor_resistance 0.0) (throw (Exception. "Resistance must be positive."))) (when (<= charging_inductor_inductance 0.0) (throw (Exception. "Inductance must be positive."))) (set! charging_inductor_exponent (quot (* (- charging_inductor_time) charging_inductor_resistance) charging_inductor_inductance)) (set! charging_inductor_current (* (quot charging_inductor_source_voltage charging_inductor_resistance) (- 1.0 (expApprox charging_inductor_exponent)))) (throw (ex-info "return" {:v (round charging_inductor_current 3)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (charging_inductor 5.8 1.5 2.3 2.0))
      (println (charging_inductor 8.0 5.0 3.0 2.0))
      (println (charging_inductor 8.0 (* 5.0 (pow10 2)) 3.0 2.0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
