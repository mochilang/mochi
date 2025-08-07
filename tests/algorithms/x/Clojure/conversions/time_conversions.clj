(ns main (:refer-clojure :exclude [contains convert_time]))

(require 'clojure.set)

(defrecord TimeChartInverse [seconds minutes hours days weeks months years])

(defrecord TimeChart [seconds minutes hours days weeks months years])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def ^:dynamic contains_i nil)

(def ^:dynamic convert_time_converted nil)

(def ^:dynamic convert_time_from nil)

(def ^:dynamic convert_time_int_part nil)

(def ^:dynamic convert_time_invalid_unit nil)

(def ^:dynamic convert_time_scaled nil)

(def ^:dynamic convert_time_seconds nil)

(def ^:dynamic convert_time_to nil)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare contains convert_time)

(def ^:dynamic main_time_chart {"seconds" 1.0 "minutes" 60.0 "hours" 3600.0 "days" 86400.0 "weeks" 604800.0 "months" 2629800.0 "years" 31557600.0})

(def ^:dynamic main_time_chart_inverse {"seconds" 1.0 "minutes" (/ 1.0 60.0) "hours" (/ 1.0 3600.0) "days" (/ 1.0 86400.0) "weeks" (/ 1.0 604800.0) "months" (/ 1.0 2629800.0) "years" (/ 1.0 31557600.0)})

(def ^:dynamic main_units ["seconds" "minutes" "hours" "days" "weeks" "months" "years"])

(def ^:dynamic main_units_str "seconds, minutes, hours, days, weeks, months, years")

(defn contains [contains_arr contains_t]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_arr)) (do (when (= (nth contains_arr contains_i) contains_t) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn convert_time [convert_time_time_value convert_time_unit_from convert_time_unit_to]
  (binding [convert_time_converted nil convert_time_from nil convert_time_int_part nil convert_time_invalid_unit nil convert_time_scaled nil convert_time_seconds nil convert_time_to nil] (try (do (when (< convert_time_time_value 0.0) (throw (Exception. "'time_value' must be a non-negative number."))) (set! convert_time_from (clojure.string/lower-case convert_time_unit_from)) (set! convert_time_to (clojure.string/lower-case convert_time_unit_to)) (when (or (not (contains main_units convert_time_from)) (not (contains main_units convert_time_to))) (do (set! convert_time_invalid_unit convert_time_from) (when (contains main_units convert_time_from) (set! convert_time_invalid_unit convert_time_to)) (throw (Exception. (str (str (str (str "Invalid unit " convert_time_invalid_unit) " is not in ") main_units_str) "."))))) (set! convert_time_seconds (* convert_time_time_value (nth main_time_chart convert_time_from))) (set! convert_time_converted (* convert_time_seconds (nth main_time_chart_inverse convert_time_to))) (set! convert_time_scaled (* convert_time_converted 1000.0)) (set! convert_time_int_part (Integer/parseInt (+ convert_time_scaled 0.5))) (throw (ex-info "return" {:v (/ (+ convert_time_int_part 0.0) 1000.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (convert_time 3600.0 "seconds" "hours"))
      (println (convert_time 360.0 "days" "months"))
      (println (convert_time 360.0 "months" "years"))
      (println (convert_time 1.0 "years" "seconds"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
