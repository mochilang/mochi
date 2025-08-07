(ns main (:refer-clojure :exclude [index_of units_string round3 convert_speed]))

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

(declare index_of units_string round3 convert_speed)

(def ^:dynamic convert_speed_from_index nil)

(def ^:dynamic convert_speed_msg nil)

(def ^:dynamic convert_speed_r nil)

(def ^:dynamic convert_speed_result nil)

(def ^:dynamic convert_speed_to_index nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic round3_y nil)

(def ^:dynamic round3_z nil)

(def ^:dynamic round3_zf nil)

(def ^:dynamic units_string_i nil)

(def ^:dynamic units_string_s nil)

(def ^:dynamic main_units ["km/h" "m/s" "mph" "knot"])

(def ^:dynamic main_speed_chart [1.0 3.6 1.609344 1.852])

(def ^:dynamic main_speed_chart_inverse [1.0 0.277777778 0.621371192 0.539956803])

(defn index_of [index_of_arr index_of_value]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_arr)) (do (when (= (nth index_of_arr index_of_i) index_of_value) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn units_string [units_string_arr]
  (binding [units_string_i nil units_string_s nil] (try (do (set! units_string_s "") (set! units_string_i 0) (while (< units_string_i (count units_string_arr)) (do (when (> units_string_i 0) (set! units_string_s (str units_string_s ", "))) (set! units_string_s (str units_string_s (nth units_string_arr units_string_i))) (set! units_string_i (+ units_string_i 1)))) (throw (ex-info "return" {:v units_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round3 [round3_x]
  (binding [round3_y nil round3_z nil round3_zf nil] (try (do (set! round3_y (+ (* round3_x 1000.0) 0.5)) (set! round3_z (long round3_y)) (set! round3_zf (double round3_z)) (throw (ex-info "return" {:v (/ round3_zf 1000.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn convert_speed [convert_speed_speed convert_speed_unit_from convert_speed_unit_to]
  (binding [convert_speed_from_index nil convert_speed_msg nil convert_speed_r nil convert_speed_result nil convert_speed_to_index nil] (try (do (set! convert_speed_from_index (index_of main_units convert_speed_unit_from)) (set! convert_speed_to_index (index_of main_units convert_speed_unit_to)) (when (or (< convert_speed_from_index 0) (< convert_speed_to_index 0)) (do (set! convert_speed_msg (str (str (str (str (str "Incorrect 'from_type' or 'to_type' value: " convert_speed_unit_from) ", ") convert_speed_unit_to) "\nValid values are: ") (units_string main_units))) (throw (Exception. convert_speed_msg)))) (set! convert_speed_result (* (* convert_speed_speed (nth main_speed_chart convert_speed_from_index)) (nth main_speed_chart_inverse convert_speed_to_index))) (set! convert_speed_r (round3 convert_speed_result)) (throw (ex-info "return" {:v convert_speed_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (convert_speed 100.0 "km/h" "m/s")))
      (println (str (convert_speed 100.0 "km/h" "mph")))
      (println (str (convert_speed 100.0 "km/h" "knot")))
      (println (str (convert_speed 100.0 "m/s" "km/h")))
      (println (str (convert_speed 100.0 "m/s" "mph")))
      (println (str (convert_speed 100.0 "m/s" "knot")))
      (println (str (convert_speed 100.0 "mph" "km/h")))
      (println (str (convert_speed 100.0 "mph" "m/s")))
      (println (str (convert_speed 100.0 "mph" "knot")))
      (println (str (convert_speed 100.0 "knot" "km/h")))
      (println (str (convert_speed 100.0 "knot" "m/s")))
      (println (str (convert_speed 100.0 "knot" "mph")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
