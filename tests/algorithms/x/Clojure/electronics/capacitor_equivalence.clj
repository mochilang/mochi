(ns main (:refer-clojure :exclude [capacitor_parallel capacitor_series main]))

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

(declare capacitor_parallel capacitor_series main)

(def ^:dynamic capacitor_parallel_c nil)

(def ^:dynamic capacitor_parallel_i nil)

(def ^:dynamic capacitor_parallel_sum_c nil)

(def ^:dynamic capacitor_series_c nil)

(def ^:dynamic capacitor_series_first_sum nil)

(def ^:dynamic capacitor_series_i nil)

(def ^:dynamic main_parallel nil)

(def ^:dynamic main_series nil)

(defn capacitor_parallel [capacitor_parallel_capacitors]
  (binding [capacitor_parallel_c nil capacitor_parallel_i nil capacitor_parallel_sum_c nil] (try (do (set! capacitor_parallel_sum_c 0.0) (set! capacitor_parallel_i 0) (while (< capacitor_parallel_i (count capacitor_parallel_capacitors)) (do (set! capacitor_parallel_c (nth capacitor_parallel_capacitors capacitor_parallel_i)) (when (< capacitor_parallel_c 0.0) (do (throw (Exception. (str (str "Capacitor at index " (str capacitor_parallel_i)) " has a negative value!"))) (throw (ex-info "return" {:v 0.0})))) (set! capacitor_parallel_sum_c (+ capacitor_parallel_sum_c capacitor_parallel_c)) (set! capacitor_parallel_i (+ capacitor_parallel_i 1)))) (throw (ex-info "return" {:v capacitor_parallel_sum_c}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn capacitor_series [capacitor_series_capacitors]
  (binding [capacitor_series_c nil capacitor_series_first_sum nil capacitor_series_i nil] (try (do (set! capacitor_series_first_sum 0.0) (set! capacitor_series_i 0) (while (< capacitor_series_i (count capacitor_series_capacitors)) (do (set! capacitor_series_c (nth capacitor_series_capacitors capacitor_series_i)) (when (<= capacitor_series_c 0.0) (do (throw (Exception. (str (str "Capacitor at index " (str capacitor_series_i)) " has a negative or zero value!"))) (throw (ex-info "return" {:v 0.0})))) (set! capacitor_series_first_sum (+ capacitor_series_first_sum (/ 1.0 capacitor_series_c))) (set! capacitor_series_i (+ capacitor_series_i 1)))) (throw (ex-info "return" {:v (/ 1.0 capacitor_series_first_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_parallel nil main_series nil] (do (set! main_parallel (capacitor_parallel [5.71389 12.0 3.0])) (set! main_series (capacitor_series [5.71389 12.0 3.0])) (println (str main_parallel)) (println (str main_series)))))

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
