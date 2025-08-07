(ns main (:refer-clojure :exclude [supported_values find_index get_from_factor get_to_factor volume_conversion]))

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

(declare supported_values find_index get_from_factor get_to_factor volume_conversion)

(def ^:dynamic find_index_i nil)

(def ^:dynamic get_from_factor_idx nil)

(def ^:dynamic get_to_factor_idx nil)

(def ^:dynamic supported_values_i nil)

(def ^:dynamic supported_values_result nil)

(def ^:dynamic volume_conversion_from_factor nil)

(def ^:dynamic volume_conversion_to_factor nil)

(def ^:dynamic main_units ["cubic meter" "litre" "kilolitre" "gallon" "cubic yard" "cubic foot" "cup"])

(def ^:dynamic main_from_factors [1.0 0.001 1.0 0.00454 0.76455 0.028 0.000236588])

(def ^:dynamic main_to_factors [1.0 1000.0 1.0 264.172 1.30795 35.3147 4226.75])

(defn supported_values []
  (binding [supported_values_i nil supported_values_result nil] (try (do (set! supported_values_result (nth main_units 0)) (set! supported_values_i 1) (while (< supported_values_i (count main_units)) (do (set! supported_values_result (str (str supported_values_result ", ") (nth main_units supported_values_i))) (set! supported_values_i (+ supported_values_i 1)))) (throw (ex-info "return" {:v supported_values_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_index [find_index_name]
  (binding [find_index_i nil] (try (do (set! find_index_i 0) (while (< find_index_i (count main_units)) (do (when (= (nth main_units find_index_i) find_index_name) (throw (ex-info "return" {:v find_index_i}))) (set! find_index_i (+ find_index_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_from_factor [get_from_factor_name]
  (binding [get_from_factor_idx nil] (try (do (set! get_from_factor_idx (find_index get_from_factor_name)) (when (< get_from_factor_idx 0) (throw (Exception. (str (str (str "Invalid 'from_type' value: '" get_from_factor_name) "' Supported values are: ") (supported_values))))) (throw (ex-info "return" {:v (nth main_from_factors get_from_factor_idx)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_to_factor [get_to_factor_name]
  (binding [get_to_factor_idx nil] (try (do (set! get_to_factor_idx (find_index get_to_factor_name)) (when (< get_to_factor_idx 0) (throw (Exception. (str (str (str "Invalid 'to_type' value: '" get_to_factor_name) "' Supported values are: ") (supported_values))))) (throw (ex-info "return" {:v (nth main_to_factors get_to_factor_idx)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn volume_conversion [volume_conversion_value volume_conversion_from_type volume_conversion_to_type]
  (binding [volume_conversion_from_factor nil volume_conversion_to_factor nil] (try (do (set! volume_conversion_from_factor (get_from_factor volume_conversion_from_type)) (set! volume_conversion_to_factor (get_to_factor volume_conversion_to_type)) (throw (ex-info "return" {:v (* (* volume_conversion_value volume_conversion_from_factor) volume_conversion_to_factor)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (volume_conversion 4.0 "cubic meter" "litre")))
      (println (str (volume_conversion 1.0 "litre" "gallon")))
      (println (str (volume_conversion 1.0 "kilolitre" "cubic meter")))
      (println (str (volume_conversion 3.0 "gallon" "cubic yard")))
      (println (str (volume_conversion 2.0 "cubic yard" "litre")))
      (println (str (volume_conversion 4.0 "cubic foot" "cup")))
      (println (str (volume_conversion 1.0 "cup" "kilolitre")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
