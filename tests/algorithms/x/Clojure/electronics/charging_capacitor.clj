(ns main (:refer-clojure :exclude [expApprox round3 charging_capacitor]))

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

(declare expApprox round3 charging_capacitor)

(def ^:dynamic charging_capacitor_exponent nil)

(def ^:dynamic charging_capacitor_voltage nil)

(def ^:dynamic expApprox_is_neg nil)

(def ^:dynamic expApprox_n nil)

(def ^:dynamic expApprox_sum nil)

(def ^:dynamic expApprox_term nil)

(def ^:dynamic expApprox_y nil)

(def ^:dynamic round3_scaled nil)

(def ^:dynamic round3_scaled_int nil)

(defn expApprox [expApprox_x]
  (binding [expApprox_is_neg nil expApprox_n nil expApprox_sum nil expApprox_term nil expApprox_y nil] (try (do (set! expApprox_y expApprox_x) (set! expApprox_is_neg false) (when (< expApprox_x 0.0) (do (set! expApprox_is_neg true) (set! expApprox_y (- expApprox_x)))) (set! expApprox_term 1.0) (set! expApprox_sum 1.0) (set! expApprox_n 1) (while (< expApprox_n 30) (do (set! expApprox_term (quot (* expApprox_term expApprox_y) (double expApprox_n))) (set! expApprox_sum (+ expApprox_sum expApprox_term)) (set! expApprox_n (+ expApprox_n 1)))) (if expApprox_is_neg (/ 1.0 expApprox_sum) expApprox_sum)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round3 [round3_x]
  (binding [round3_scaled nil round3_scaled_int nil] (try (do (set! round3_scaled (* round3_x 1000.0)) (if (>= round3_scaled 0.0) (set! round3_scaled (+ round3_scaled 0.5)) (set! round3_scaled (- round3_scaled 0.5))) (set! round3_scaled_int (long round3_scaled)) (throw (ex-info "return" {:v (/ (double round3_scaled_int) 1000.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn charging_capacitor [charging_capacitor_source_voltage charging_capacitor_resistance charging_capacitor_capacitance charging_capacitor_time_sec]
  (binding [charging_capacitor_exponent nil charging_capacitor_voltage nil] (try (do (when (<= charging_capacitor_source_voltage 0.0) (throw (Exception. "Source voltage must be positive."))) (when (<= charging_capacitor_resistance 0.0) (throw (Exception. "Resistance must be positive."))) (when (<= charging_capacitor_capacitance 0.0) (throw (Exception. "Capacitance must be positive."))) (set! charging_capacitor_exponent (quot (- charging_capacitor_time_sec) (* charging_capacitor_resistance charging_capacitor_capacitance))) (set! charging_capacitor_voltage (* charging_capacitor_source_voltage (- 1.0 (expApprox charging_capacitor_exponent)))) (throw (ex-info "return" {:v (round3 charging_capacitor_voltage)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (charging_capacitor 0.2 0.9 8.4 0.5))
      (println (charging_capacitor 2.2 3.5 2.4 9.0))
      (println (charging_capacitor 15.0 200.0 20.0 2.0))
      (println (charging_capacitor 20.0 2000.0 0.0003 4.0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
