(ns main (:refer-clojure :exclude [pressure_of_gas_system volume_of_gas_system temperature_of_gas_system moles_of_gas_system]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pressure_of_gas_system volume_of_gas_system temperature_of_gas_system moles_of_gas_system)

(def ^:dynamic main_UNIVERSAL_GAS_CONSTANT 8.314462)

(defn pressure_of_gas_system [pressure_of_gas_system_moles pressure_of_gas_system_kelvin pressure_of_gas_system_volume]
  (try (do (when (or (or (< pressure_of_gas_system_moles 0) (< pressure_of_gas_system_kelvin 0)) (< pressure_of_gas_system_volume 0)) (throw (Exception. "Invalid inputs. Enter positive value."))) (throw (ex-info "return" {:v (/ (* (* pressure_of_gas_system_moles pressure_of_gas_system_kelvin) main_UNIVERSAL_GAS_CONSTANT) pressure_of_gas_system_volume)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn volume_of_gas_system [volume_of_gas_system_moles volume_of_gas_system_kelvin volume_of_gas_system_pressure]
  (try (do (when (or (or (< volume_of_gas_system_moles 0) (< volume_of_gas_system_kelvin 0)) (< volume_of_gas_system_pressure 0)) (throw (Exception. "Invalid inputs. Enter positive value."))) (throw (ex-info "return" {:v (/ (* (* volume_of_gas_system_moles volume_of_gas_system_kelvin) main_UNIVERSAL_GAS_CONSTANT) volume_of_gas_system_pressure)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn temperature_of_gas_system [temperature_of_gas_system_moles temperature_of_gas_system_volume temperature_of_gas_system_pressure]
  (try (do (when (or (or (< temperature_of_gas_system_moles 0) (< temperature_of_gas_system_volume 0)) (< temperature_of_gas_system_pressure 0)) (throw (Exception. "Invalid inputs. Enter positive value."))) (throw (ex-info "return" {:v (/ (* temperature_of_gas_system_pressure temperature_of_gas_system_volume) (* temperature_of_gas_system_moles main_UNIVERSAL_GAS_CONSTANT))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn moles_of_gas_system [moles_of_gas_system_kelvin moles_of_gas_system_volume moles_of_gas_system_pressure]
  (try (do (when (or (or (< moles_of_gas_system_kelvin 0) (< moles_of_gas_system_volume 0)) (< moles_of_gas_system_pressure 0)) (throw (Exception. "Invalid inputs. Enter positive value."))) (throw (ex-info "return" {:v (/ (* moles_of_gas_system_pressure moles_of_gas_system_volume) (* moles_of_gas_system_kelvin main_UNIVERSAL_GAS_CONSTANT))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (pressure_of_gas_system 2.0 100.0 5.0))
      (println (volume_of_gas_system 0.5 273.0 0.004))
      (println (temperature_of_gas_system 2.0 100.0 5.0))
      (println (moles_of_gas_system 100.0 5.0 10.0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
