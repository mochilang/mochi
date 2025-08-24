(ns main (:refer-clojure :exclude [pow10 weight_conversion]))

(require 'clojure.set)

(defrecord WEIGHTTYPECHART [kilogram gram milligram metric-ton long-ton short-ton pound stone ounce carrat atomic-mass-unit])

(defrecord KILOGRAMCHART [kilogram gram milligram metric-ton long-ton short-ton pound stone ounce carrat atomic-mass-unit])

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_result nil)

(def ^:dynamic weight_conversion_has_from nil)

(def ^:dynamic weight_conversion_has_to nil)

(declare pow10 weight_conversion)

(declare _read_file)

(defn pow10 [pow10_exp]
  (binding [pow10_i nil pow10_result nil] (try (do (set! pow10_result 1.0) (if (>= pow10_exp 0) (do (set! pow10_i 0) (while (< pow10_i pow10_exp) (do (set! pow10_result (* pow10_result 10.0)) (set! pow10_i (+ pow10_i 1))))) (do (set! pow10_i 0) (while (< pow10_i (- pow10_exp)) (do (set! pow10_result (/ pow10_result 10.0)) (set! pow10_i (+ pow10_i 1)))))) (throw (ex-info "return" {:v pow10_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_KILOGRAM_CHART nil)

(def ^:dynamic main_WEIGHT_TYPE_CHART nil)

(defn weight_conversion [weight_conversion_from_type weight_conversion_to_type weight_conversion_value]
  (binding [weight_conversion_has_from nil weight_conversion_has_to nil] (try (do (set! weight_conversion_has_to (in weight_conversion_to_type main_KILOGRAM_CHART)) (set! weight_conversion_has_from (in weight_conversion_from_type main_WEIGHT_TYPE_CHART)) (when (and weight_conversion_has_to weight_conversion_has_from) (throw (ex-info "return" {:v (* (* weight_conversion_value (get main_KILOGRAM_CHART weight_conversion_to_type)) (get main_WEIGHT_TYPE_CHART weight_conversion_from_type))}))) (println "Invalid 'from_type' or 'to_type'") (throw (ex-info "return" {:v 0.0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_KILOGRAM_CHART) (constantly {"atomic-mass-unit" (* 6.022136652 (pow10 26)) "carrat" 5000.0 "gram" 1000.0 "kilogram" 1.0 "long-ton" 0.0009842073 "metric-ton" 0.001 "milligram" 1000000.0 "ounce" 35.273990723 "pound" 2.2046244202 "short-ton" 0.0011023122 "stone" 0.1574731728}))
      (alter-var-root (var main_WEIGHT_TYPE_CHART) (constantly {"atomic-mass-unit" (* 1.660540199 (pow10 (- 27))) "carrat" 0.0002 "gram" 0.001 "kilogram" 1.0 "long-ton" 1016.04608 "metric-ton" 1000.0 "milligram" 0.000001 "ounce" 0.0283495 "pound" 0.453592 "short-ton" 907.184 "stone" 6.35029}))
      (println (weight_conversion "kilogram" "gram" 1.0))
      (println (weight_conversion "gram" "pound" 3.0))
      (println (weight_conversion "ounce" "kilogram" 3.0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
