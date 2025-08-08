(ns main (:refer-clojure :exclude [resistor_parallel resistor_series main]))

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

(declare resistor_parallel resistor_series main)

(def ^:dynamic main_resistors nil)

(def ^:dynamic resistor_parallel_i nil)

(def ^:dynamic resistor_parallel_r nil)

(def ^:dynamic resistor_parallel_sum nil)

(def ^:dynamic resistor_series_i nil)

(def ^:dynamic resistor_series_r nil)

(def ^:dynamic resistor_series_sum nil)

(defn resistor_parallel [resistor_parallel_resistors]
  (binding [resistor_parallel_i nil resistor_parallel_r nil resistor_parallel_sum nil] (try (do (set! resistor_parallel_sum 0.0) (set! resistor_parallel_i 0) (while (< resistor_parallel_i (count resistor_parallel_resistors)) (do (set! resistor_parallel_r (nth resistor_parallel_resistors resistor_parallel_i)) (when (<= resistor_parallel_r 0.0) (throw (Exception. (str (str "Resistor at index " (str resistor_parallel_i)) " has a negative or zero value!")))) (set! resistor_parallel_sum (+ resistor_parallel_sum (/ 1.0 resistor_parallel_r))) (set! resistor_parallel_i (+ resistor_parallel_i 1)))) (throw (ex-info "return" {:v (/ 1.0 resistor_parallel_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn resistor_series [resistor_series_resistors]
  (binding [resistor_series_i nil resistor_series_r nil resistor_series_sum nil] (try (do (set! resistor_series_sum 0.0) (set! resistor_series_i 0) (while (< resistor_series_i (count resistor_series_resistors)) (do (set! resistor_series_r (nth resistor_series_resistors resistor_series_i)) (when (< resistor_series_r 0.0) (throw (Exception. (str (str "Resistor at index " (str resistor_series_i)) " has a negative value!")))) (set! resistor_series_sum (+ resistor_series_sum resistor_series_r)) (set! resistor_series_i (+ resistor_series_i 1)))) (throw (ex-info "return" {:v resistor_series_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_resistors nil] (do (set! main_resistors [3.21389 2.0 3.0]) (println (str "Parallel: " (str (resistor_parallel main_resistors)))) (println (str "Series: " (str (resistor_series main_resistors)))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
