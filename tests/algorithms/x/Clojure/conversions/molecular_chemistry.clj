(ns main (:refer-clojure :exclude [round_to_int molarity_to_normality moles_to_pressure moles_to_volume pressure_and_volume_to_temperature]))

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

(declare round_to_int molarity_to_normality moles_to_pressure moles_to_volume pressure_and_volume_to_temperature)

(defn round_to_int [round_to_int_x]
  (try (if (>= round_to_int_x 0.0) (Integer/parseInt (+ round_to_int_x 0.5)) (Integer/parseInt (- round_to_int_x 0.5))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn molarity_to_normality [molarity_to_normality_nfactor molarity_to_normality_moles molarity_to_normality_volume]
  (try (throw (ex-info "return" {:v (round_to_int (* (/ molarity_to_normality_moles molarity_to_normality_volume) molarity_to_normality_nfactor))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn moles_to_pressure [moles_to_pressure_volume moles_to_pressure_moles moles_to_pressure_temperature]
  (try (throw (ex-info "return" {:v (round_to_int (/ (* (* moles_to_pressure_moles 0.0821) moles_to_pressure_temperature) moles_to_pressure_volume))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn moles_to_volume [moles_to_volume_pressure moles_to_volume_moles moles_to_volume_temperature]
  (try (throw (ex-info "return" {:v (round_to_int (/ (* (* moles_to_volume_moles 0.0821) moles_to_volume_temperature) moles_to_volume_pressure))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pressure_and_volume_to_temperature [pressure_and_volume_to_temperature_pressure pressure_and_volume_to_temperature_moles pressure_and_volume_to_temperature_volume]
  (try (throw (ex-info "return" {:v (round_to_int (/ (* pressure_and_volume_to_temperature_pressure pressure_and_volume_to_temperature_volume) (* 0.0821 pressure_and_volume_to_temperature_moles)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (molarity_to_normality 2.0 3.1 0.31)))
      (println (str (molarity_to_normality 4.0 11.4 5.7)))
      (println (str (moles_to_pressure 0.82 3.0 300.0)))
      (println (str (moles_to_pressure 8.2 5.0 200.0)))
      (println (str (moles_to_volume 0.82 3.0 300.0)))
      (println (str (moles_to_volume 8.2 5.0 200.0)))
      (println (str (pressure_and_volume_to_temperature 0.82 1.0 2.0)))
      (println (str (pressure_and_volume_to_temperature 8.2 5.0 3.0)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
