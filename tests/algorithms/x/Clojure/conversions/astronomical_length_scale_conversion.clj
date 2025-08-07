(ns main (:refer-clojure :exclude [sanitize pow10 length_conversion]))

(require 'clojure.set)

(defrecord METRICCONVERSION [m km Mm Gm Tm Pm Em Zm Ym])

(defrecord UNITSYMBOL [meter kilometer megametre gigametre terametre petametre exametre zettametre yottametre])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def ^:dynamic length_conversion_exponent nil)

(def ^:dynamic length_conversion_from_exp nil)

(def ^:dynamic length_conversion_from_sanitized nil)

(def ^:dynamic length_conversion_to_exp nil)

(def ^:dynamic length_conversion_to_sanitized nil)

(def ^:dynamic pow10_e nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_res nil)

(def ^:dynamic sanitize_last nil)

(def ^:dynamic sanitize_res nil)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sanitize pow10 length_conversion)

(def ^:dynamic main_UNIT_SYMBOL {"meter" "m" "kilometer" "km" "megametre" "Mm" "gigametre" "Gm" "terametre" "Tm" "petametre" "Pm" "exametre" "Em" "zettametre" "Zm" "yottametre" "Ym"})

(def ^:dynamic main_METRIC_CONVERSION {"m" 0 "km" 3 "Mm" 6 "Gm" 9 "Tm" 12 "Pm" 15 "Em" 18 "Zm" 21 "Ym" 24})

(def ^:dynamic main_ABBREVIATIONS "m, km, Mm, Gm, Tm, Pm, Em, Zm, Ym")

(defn sanitize [sanitize_unit]
  (binding [sanitize_last nil sanitize_res nil] (try (do (set! sanitize_res (clojure.string/lower-case sanitize_unit)) (when (> (count sanitize_res) 0) (do (set! sanitize_last (subs sanitize_res (- (count sanitize_res) 1) (min (count sanitize_res) (count sanitize_res)))) (when (= sanitize_last "s") (set! sanitize_res (subs sanitize_res 0 (min (- (count sanitize_res) 1) (count sanitize_res))))))) (if (in sanitize_res main_UNIT_SYMBOL) (get main_UNIT_SYMBOL sanitize_res) sanitize_res)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow10 [pow10_exp]
  (binding [pow10_e nil pow10_i nil pow10_res nil] (try (do (when (= pow10_exp 0) (throw (ex-info "return" {:v 1.0}))) (set! pow10_e pow10_exp) (set! pow10_res 1.0) (when (< pow10_e 0) (set! pow10_e (- pow10_e))) (set! pow10_i 0) (while (< pow10_i pow10_e) (do (set! pow10_res (* pow10_res 10.0)) (set! pow10_i (+ pow10_i 1)))) (if (< pow10_exp 0) (/ 1.0 pow10_res) pow10_res)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn length_conversion [length_conversion_value length_conversion_from_type length_conversion_to_type]
  (binding [length_conversion_exponent nil length_conversion_from_exp nil length_conversion_from_sanitized nil length_conversion_to_exp nil length_conversion_to_sanitized nil] (try (do (set! length_conversion_from_sanitized (sanitize length_conversion_from_type)) (set! length_conversion_to_sanitized (sanitize length_conversion_to_type)) (when (not (in length_conversion_from_sanitized main_METRIC_CONVERSION)) (throw (Exception. (str (str (str "Invalid 'from_type' value: '" length_conversion_from_type) "'.\nConversion abbreviations are: ") main_ABBREVIATIONS)))) (when (not (in length_conversion_to_sanitized main_METRIC_CONVERSION)) (throw (Exception. (str (str (str "Invalid 'to_type' value: '" length_conversion_to_type) "'.\nConversion abbreviations are: ") main_ABBREVIATIONS)))) (set! length_conversion_from_exp (get main_METRIC_CONVERSION length_conversion_from_sanitized)) (set! length_conversion_to_exp (get main_METRIC_CONVERSION length_conversion_to_sanitized)) (set! length_conversion_exponent 0) (if (> length_conversion_from_exp length_conversion_to_exp) (set! length_conversion_exponent (- length_conversion_from_exp length_conversion_to_exp)) (set! length_conversion_exponent (- (- length_conversion_to_exp length_conversion_from_exp)))) (throw (ex-info "return" {:v (* length_conversion_value (pow10 length_conversion_exponent))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (length_conversion 1.0 "meter" "kilometer")))
      (println (str (length_conversion 1.0 "meter" "megametre")))
      (println (str (length_conversion 1.0 "gigametre" "meter")))
      (println (str (length_conversion 1.0 "terametre" "zettametre")))
      (println (str (length_conversion 1.0 "yottametre" "zettametre")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
