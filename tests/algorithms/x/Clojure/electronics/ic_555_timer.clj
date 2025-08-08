(ns main (:refer-clojure :exclude [astable_frequency astable_duty_cycle]))

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

(declare astable_frequency astable_duty_cycle)

(defn astable_frequency [astable_frequency_resistance_1 astable_frequency_resistance_2 astable_frequency_capacitance]
  (try (do (when (or (or (<= astable_frequency_resistance_1 0.0) (<= astable_frequency_resistance_2 0.0)) (<= astable_frequency_capacitance 0.0)) (throw (Exception. "All values must be positive"))) (throw (ex-info "return" {:v (* (/ 1.44 (* (+ astable_frequency_resistance_1 (* 2.0 astable_frequency_resistance_2)) astable_frequency_capacitance)) 1000000.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn astable_duty_cycle [astable_duty_cycle_resistance_1 astable_duty_cycle_resistance_2]
  (try (do (when (or (<= astable_duty_cycle_resistance_1 0.0) (<= astable_duty_cycle_resistance_2 0.0)) (throw (Exception. "All values must be positive"))) (throw (ex-info "return" {:v (* (quot (+ astable_duty_cycle_resistance_1 astable_duty_cycle_resistance_2) (+ astable_duty_cycle_resistance_1 (* 2.0 astable_duty_cycle_resistance_2))) 100.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (astable_frequency 45.0 45.0 7.0))
      (println (astable_duty_cycle 45.0 45.0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
