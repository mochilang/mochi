(ns main (:refer-clojure :exclude [pressure_conversion]))

(require 'clojure.set)

(defrecord PRESSURECONVERSION [atm pascal bar kilopascal megapascal psi inHg torr])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def ^:dynamic pressure_conversion_from nil)

(def ^:dynamic pressure_conversion_keys nil)

(def ^:dynamic pressure_conversion_to nil)

(declare pressure_conversion)

(def ^:dynamic main_PRESSURE_CONVERSION {"atm" {:from_factor 1.0 :to_factor 1.0} "pascal" {:from_factor 0.0000098 :to_factor 101325.0} "bar" {:from_factor 0.986923 :to_factor 1.01325} "kilopascal" {:from_factor 0.00986923 :to_factor 101.325} "megapascal" {:from_factor 9.86923 :to_factor 0.101325} "psi" {:from_factor 0.068046 :to_factor 14.6959} "inHg" {:from_factor 0.0334211 :to_factor 29.9213} "torr" {:from_factor 0.00131579 :to_factor 760.0}})

(defn pressure_conversion [pressure_conversion_value pressure_conversion_from_type pressure_conversion_to_type]
  (binding [pressure_conversion_from nil pressure_conversion_keys nil pressure_conversion_to nil] (try (do (when (not (in pressure_conversion_from_type main_PRESSURE_CONVERSION)) (do (set! pressure_conversion_keys ((:join ((:keys Object) main_PRESSURE_CONVERSION)) ", ")) (throw (Exception. (str (str (str "Invalid 'from_type' value: '" pressure_conversion_from_type) "'  Supported values are:\n") pressure_conversion_keys))))) (when (not (in pressure_conversion_to_type main_PRESSURE_CONVERSION)) (do (set! pressure_conversion_keys ((:join ((:keys Object) main_PRESSURE_CONVERSION)) ", ")) (throw (Exception. (str (str (str "Invalid 'to_type' value: '" pressure_conversion_to_type) ".  Supported values are:\n") pressure_conversion_keys))))) (set! pressure_conversion_from (get main_PRESSURE_CONVERSION pressure_conversion_from_type)) (set! pressure_conversion_to (get main_PRESSURE_CONVERSION pressure_conversion_to_type)) (throw (ex-info "return" {:v (* (* pressure_conversion_value (:from_factor pressure_conversion_from)) (:to_factor pressure_conversion_to))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (pressure_conversion 4.0 "atm" "pascal"))
      (println (pressure_conversion 1.0 "pascal" "psi"))
      (println (pressure_conversion 1.0 "bar" "atm"))
      (println (pressure_conversion 3.0 "kilopascal" "bar"))
      (println (pressure_conversion 2.0 "megapascal" "psi"))
      (println (pressure_conversion 4.0 "psi" "torr"))
      (println (pressure_conversion 1.0 "inHg" "atm"))
      (println (pressure_conversion 1.0 "torr" "psi"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
