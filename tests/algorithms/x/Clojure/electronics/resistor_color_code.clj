(ns main (:refer-clojure :exclude [contains get_significant_digits get_multiplier get_tolerance get_temperature_coeffecient get_band_type_count check_validity calculate_resistance]))

(require 'clojure.set)

(defrecord TemperatureCoeffecientColorValues [Black Brown Red Orange Yellow Green Blue Violet Grey])

(defrecord ToleranceColorValues [Brown Red Orange Yellow Green Blue Violet Grey Gold Silver])

(defrecord MultiplierColorValues [Black Brown Red Orange Yellow Green Blue Violet Grey White Gold Silver])

(defrecord SignificantFiguresColorValues [Black Brown Red Orange Yellow Green Blue Violet Grey White])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(def ^:dynamic calculate_resistance_answer nil)

(def ^:dynamic calculate_resistance_multiplier nil)

(def ^:dynamic calculate_resistance_multiplier_color nil)

(def ^:dynamic calculate_resistance_resistance_str nil)

(def ^:dynamic calculate_resistance_resistance_value nil)

(def ^:dynamic calculate_resistance_sig_count nil)

(def ^:dynamic calculate_resistance_significant_colors nil)

(def ^:dynamic calculate_resistance_significant_digits nil)

(def ^:dynamic calculate_resistance_temp_coeff nil)

(def ^:dynamic calculate_resistance_temp_color nil)

(def ^:dynamic calculate_resistance_tolerance nil)

(def ^:dynamic calculate_resistance_tolerance_color nil)

(def ^:dynamic check_validity_color nil)

(def ^:dynamic contains_c nil)

(def ^:dynamic get_significant_digits_color nil)

(def ^:dynamic get_significant_digits_digit nil)

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare contains get_significant_digits get_multiplier get_tolerance get_temperature_coeffecient get_band_type_count check_validity calculate_resistance)

(declare _read_file)

(def ^:dynamic main_valid_colors nil)

(def ^:dynamic main_significant_figures_color_values nil)

(def ^:dynamic main_multiplier_color_values nil)

(def ^:dynamic main_tolerance_color_values nil)

(def ^:dynamic main_temperature_coeffecient_color_values nil)

(defn contains [contains_list contains_value]
  (binding [contains_c nil] (try (do (doseq [contains_c contains_list] (when (= contains_c contains_value) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_significant_digits [get_significant_digits_colors]
  (binding [get_significant_digits_color nil get_significant_digits_digit nil] (try (do (set! get_significant_digits_digit 0) (doseq [get_significant_digits_color get_significant_digits_colors] (do (when (not (in get_significant_digits_color main_significant_figures_color_values)) (throw (Exception. (str get_significant_digits_color " is not a valid color for significant figure bands")))) (set! get_significant_digits_digit (+ (* get_significant_digits_digit 10) (get main_significant_figures_color_values get_significant_digits_color))))) (throw (ex-info "return" {:v get_significant_digits_digit}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_multiplier [get_multiplier_color]
  (try (do (when (not (in get_multiplier_color main_multiplier_color_values)) (throw (Exception. (str get_multiplier_color " is not a valid color for multiplier band")))) (throw (ex-info "return" {:v (get main_multiplier_color_values get_multiplier_color)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_tolerance [get_tolerance_color]
  (try (do (when (not (in get_tolerance_color main_tolerance_color_values)) (throw (Exception. (str get_tolerance_color " is not a valid color for tolerance band")))) (throw (ex-info "return" {:v (get main_tolerance_color_values get_tolerance_color)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_temperature_coeffecient [get_temperature_coeffecient_color]
  (try (do (when (not (in get_temperature_coeffecient_color main_temperature_coeffecient_color_values)) (throw (Exception. (str get_temperature_coeffecient_color " is not a valid color for temperature coeffecient band")))) (throw (ex-info "return" {:v (get main_temperature_coeffecient_color_values get_temperature_coeffecient_color)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_band_type_count [get_band_type_count_total get_band_type_count_typ]
  (try (if (= get_band_type_count_total 3) (do (when (= get_band_type_count_typ "significant") (throw (ex-info "return" {:v 2}))) (when (= get_band_type_count_typ "multiplier") (throw (ex-info "return" {:v 1}))) (throw (Exception. (str get_band_type_count_typ " is not valid for a 3 band resistor")))) (if (= get_band_type_count_total 4) (do (when (= get_band_type_count_typ "significant") (throw (ex-info "return" {:v 2}))) (when (= get_band_type_count_typ "multiplier") (throw (ex-info "return" {:v 1}))) (when (= get_band_type_count_typ "tolerance") (throw (ex-info "return" {:v 1}))) (throw (Exception. (str get_band_type_count_typ " is not valid for a 4 band resistor")))) (if (= get_band_type_count_total 5) (do (when (= get_band_type_count_typ "significant") (throw (ex-info "return" {:v 3}))) (when (= get_band_type_count_typ "multiplier") (throw (ex-info "return" {:v 1}))) (when (= get_band_type_count_typ "tolerance") (throw (ex-info "return" {:v 1}))) (throw (Exception. (str get_band_type_count_typ " is not valid for a 5 band resistor")))) (if (= get_band_type_count_total 6) (do (when (= get_band_type_count_typ "significant") (throw (ex-info "return" {:v 3}))) (when (= get_band_type_count_typ "multiplier") (throw (ex-info "return" {:v 1}))) (when (= get_band_type_count_typ "tolerance") (throw (ex-info "return" {:v 1}))) (when (= get_band_type_count_typ "temp_coeffecient") (throw (ex-info "return" {:v 1}))) (throw (Exception. (str get_band_type_count_typ " is not valid for a 6 band resistor")))) (throw (Exception. (str (mochi_str get_band_type_count_total) " is not a valid number of bands"))))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn check_validity [check_validity_number_of_bands check_validity_colors]
  (binding [check_validity_color nil] (try (do (when (or (< check_validity_number_of_bands 3) (> check_validity_number_of_bands 6)) (throw (Exception. "Invalid number of bands. Resistor bands must be 3 to 6"))) (when (not= check_validity_number_of_bands (count check_validity_colors)) (throw (Exception. (str (str (str (str "Expecting " (mochi_str check_validity_number_of_bands)) " colors, provided ") (mochi_str (count check_validity_colors))) " colors")))) (doseq [check_validity_color check_validity_colors] (when (not (contains main_valid_colors check_validity_color)) (throw (Exception. (str check_validity_color " is not a valid color"))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calculate_resistance [calculate_resistance_number_of_bands calculate_resistance_color_code_list]
  (binding [calculate_resistance_answer nil calculate_resistance_multiplier nil calculate_resistance_multiplier_color nil calculate_resistance_resistance_str nil calculate_resistance_resistance_value nil calculate_resistance_sig_count nil calculate_resistance_significant_colors nil calculate_resistance_significant_digits nil calculate_resistance_temp_coeff nil calculate_resistance_temp_color nil calculate_resistance_tolerance nil calculate_resistance_tolerance_color nil] (try (do (check_validity calculate_resistance_number_of_bands calculate_resistance_color_code_list) (set! calculate_resistance_sig_count (get_band_type_count calculate_resistance_number_of_bands "significant")) (set! calculate_resistance_significant_colors (subvec calculate_resistance_color_code_list 0 calculate_resistance_sig_count)) (set! calculate_resistance_significant_digits (get_significant_digits calculate_resistance_significant_colors)) (set! calculate_resistance_multiplier_color (nth calculate_resistance_color_code_list calculate_resistance_sig_count)) (set! calculate_resistance_multiplier (get_multiplier calculate_resistance_multiplier_color)) (set! calculate_resistance_tolerance 20.0) (when (>= calculate_resistance_number_of_bands 4) (do (set! calculate_resistance_tolerance_color (nth calculate_resistance_color_code_list (+ calculate_resistance_sig_count 1))) (set! calculate_resistance_tolerance (get_tolerance calculate_resistance_tolerance_color)))) (set! calculate_resistance_temp_coeff 0) (when (= calculate_resistance_number_of_bands 6) (do (set! calculate_resistance_temp_color (nth calculate_resistance_color_code_list (+ calculate_resistance_sig_count 2))) (set! calculate_resistance_temp_coeff (get_temperature_coeffecient calculate_resistance_temp_color)))) (set! calculate_resistance_resistance_value (* calculate_resistance_multiplier calculate_resistance_significant_digits)) (set! calculate_resistance_resistance_str (mochi_str calculate_resistance_resistance_value)) (when (= calculate_resistance_resistance_value (toi calculate_resistance_resistance_value)) (set! calculate_resistance_resistance_str (mochi_str (toi calculate_resistance_resistance_value)))) (set! calculate_resistance_answer (str (str (str calculate_resistance_resistance_str "Ω ±") (mochi_str calculate_resistance_tolerance)) "% ")) (when (not= calculate_resistance_temp_coeff 0) (set! calculate_resistance_answer (str (str calculate_resistance_answer (mochi_str calculate_resistance_temp_coeff)) " ppm/K"))) (throw (ex-info "return" {:v calculate_resistance_answer}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_valid_colors) (constantly ["Black" "Brown" "Red" "Orange" "Yellow" "Green" "Blue" "Violet" "Grey" "White" "Gold" "Silver"]))
      (alter-var-root (var main_significant_figures_color_values) (constantly {"Black" 0 "Blue" 6 "Brown" 1 "Green" 5 "Grey" 8 "Orange" 3 "Red" 2 "Violet" 7 "White" 9 "Yellow" 4}))
      (alter-var-root (var main_multiplier_color_values) (constantly {"Black" 1.0 "Blue" 1000000.0 "Brown" 10.0 "Gold" 0.1 "Green" 100000.0 "Grey" 100000000.0 "Orange" 1000.0 "Red" 100.0 "Silver" 0.01 "Violet" 10000000.0 "White" 1000000000.0 "Yellow" 10000.0}))
      (alter-var-root (var main_tolerance_color_values) (constantly {"Blue" 0.25 "Brown" 1.0 "Gold" 5.0 "Green" 0.5 "Grey" 0.01 "Orange" 0.05 "Red" 2.0 "Silver" 10.0 "Violet" 0.1 "Yellow" 0.02}))
      (alter-var-root (var main_temperature_coeffecient_color_values) (constantly {"Black" 250 "Blue" 10 "Brown" 100 "Green" 20 "Grey" 1 "Orange" 15 "Red" 50 "Violet" 5 "Yellow" 25}))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
