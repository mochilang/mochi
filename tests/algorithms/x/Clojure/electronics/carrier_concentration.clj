(ns main (:refer-clojure :exclude [sqrtApprox carrier_concentration]))

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

(declare sqrtApprox carrier_concentration)

(def ^:dynamic carrier_concentration_zero_count nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn carrier_concentration [carrier_concentration_electron_conc carrier_concentration_hole_conc carrier_concentration_intrinsic_conc]
  (binding [carrier_concentration_zero_count nil] (try (do (set! carrier_concentration_zero_count 0) (when (= carrier_concentration_electron_conc 0.0) (set! carrier_concentration_zero_count (+ carrier_concentration_zero_count 1))) (when (= carrier_concentration_hole_conc 0.0) (set! carrier_concentration_zero_count (+ carrier_concentration_zero_count 1))) (when (= carrier_concentration_intrinsic_conc 0.0) (set! carrier_concentration_zero_count (+ carrier_concentration_zero_count 1))) (when (not= carrier_concentration_zero_count 1) (throw (Exception. "You cannot supply more or less than 2 values"))) (when (< carrier_concentration_electron_conc 0.0) (throw (Exception. "Electron concentration cannot be negative in a semiconductor"))) (when (< carrier_concentration_hole_conc 0.0) (throw (Exception. "Hole concentration cannot be negative in a semiconductor"))) (when (< carrier_concentration_intrinsic_conc 0.0) (throw (Exception. "Intrinsic concentration cannot be negative in a semiconductor"))) (when (= carrier_concentration_electron_conc 0.0) (throw (ex-info "return" {:v {:name "electron_conc" :value (quot (* carrier_concentration_intrinsic_conc carrier_concentration_intrinsic_conc) carrier_concentration_hole_conc)}}))) (when (= carrier_concentration_hole_conc 0.0) (throw (ex-info "return" {:v {:name "hole_conc" :value (quot (* carrier_concentration_intrinsic_conc carrier_concentration_intrinsic_conc) carrier_concentration_electron_conc)}}))) (if (= carrier_concentration_intrinsic_conc 0.0) {:name "intrinsic_conc" :value (sqrtApprox (* carrier_concentration_electron_conc carrier_concentration_hole_conc))} {:name "" :value (- 1.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_r1 (carrier_concentration 25.0 100.0 0.0))

(def ^:dynamic main_r2 (carrier_concentration 0.0 1600.0 200.0))

(def ^:dynamic main_r3 (carrier_concentration 1000.0 0.0 1200.0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (str (:name main_r1) ", ") (str (:value main_r1))))
      (println (str (str (:name main_r2) ", ") (str (:value main_r2))))
      (println (str (str (:name main_r3) ", ") (str (:value main_r3))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
