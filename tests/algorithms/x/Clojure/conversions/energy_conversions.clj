(ns main (:refer-clojure :exclude [energy_conversion]))

(require 'clojure.set)

(defrecord ENERGYCONVERSION [joule kilojoule megajoule gigajoule wattsecond watthour kilowatthour newtonmeter calorie_nutr kilocalorie_nutr electronvolt britishthermalunit_it footpound])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare energy_conversion)

(def ^:dynamic main_ENERGY_CONVERSION {"joule" 1.0 "kilojoule" 1000.0 "megajoule" 1000000.0 "gigajoule" 1000000000.0 "wattsecond" 1.0 "watthour" 3600.0 "kilowatthour" 3600000.0 "newtonmeter" 1.0 "calorie_nutr" 4186.8 "kilocalorie_nutr" 4186800.0 "electronvolt" 0.0000000000000000001602176634 "britishthermalunit_it" 1055.05585 "footpound" 1.355818})

(defn energy_conversion [energy_conversion_from_type energy_conversion_to_type energy_conversion_value]
  (try (do (when (or (= (in energy_conversion_from_type main_ENERGY_CONVERSION) false) (= (in energy_conversion_to_type main_ENERGY_CONVERSION) false)) (throw (Exception. "Incorrect 'from_type' or 'to_type'"))) (throw (ex-info "return" {:v (/ (* energy_conversion_value (get main_ENERGY_CONVERSION energy_conversion_from_type)) (get main_ENERGY_CONVERSION energy_conversion_to_type))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (energy_conversion "joule" "kilojoule" 1.0)))
      (println (str (energy_conversion "kilowatthour" "joule" 10.0)))
      (println (str (energy_conversion "britishthermalunit_it" "footpound" 1.0)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
