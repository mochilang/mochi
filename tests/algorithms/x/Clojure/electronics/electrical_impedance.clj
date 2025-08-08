(ns main (:refer-clojure :exclude [sqrtApprox electrical_impedance]))

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

(declare sqrtApprox electrical_impedance)

(def ^:dynamic electrical_impedance_value nil)

(def ^:dynamic electrical_impedance_zero_count nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn electrical_impedance [electrical_impedance_resistance electrical_impedance_reactance electrical_impedance_impedance]
  (binding [electrical_impedance_value nil electrical_impedance_zero_count nil] (try (do (set! electrical_impedance_zero_count 0) (when (= electrical_impedance_resistance 0.0) (set! electrical_impedance_zero_count (+ electrical_impedance_zero_count 1))) (when (= electrical_impedance_reactance 0.0) (set! electrical_impedance_zero_count (+ electrical_impedance_zero_count 1))) (when (= electrical_impedance_impedance 0.0) (set! electrical_impedance_zero_count (+ electrical_impedance_zero_count 1))) (when (not= electrical_impedance_zero_count 1) (throw (Exception. "One and only one argument must be 0"))) (if (= electrical_impedance_resistance 0.0) (do (set! electrical_impedance_value (sqrtApprox (- (* electrical_impedance_impedance electrical_impedance_impedance) (* electrical_impedance_reactance electrical_impedance_reactance)))) (throw (ex-info "return" {:v {"resistance" electrical_impedance_value}}))) (if (= electrical_impedance_reactance 0.0) (do (set! electrical_impedance_value (sqrtApprox (- (* electrical_impedance_impedance electrical_impedance_impedance) (* electrical_impedance_resistance electrical_impedance_resistance)))) (throw (ex-info "return" {:v {"reactance" electrical_impedance_value}}))) (if (= electrical_impedance_impedance 0.0) (do (set! electrical_impedance_value (sqrtApprox (+ (* electrical_impedance_resistance electrical_impedance_resistance) (* electrical_impedance_reactance electrical_impedance_reactance)))) (throw (ex-info "return" {:v {"impedance" electrical_impedance_value}}))) (throw (Exception. "Exactly one argument must be 0")))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (electrical_impedance 3.0 4.0 0.0))
      (println (electrical_impedance 0.0 4.0 5.0))
      (println (electrical_impedance 3.0 0.0 5.0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
