(ns main (:refer-clojure :exclude [absf pow10 round_to electric_power str_result]))

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

(declare absf pow10 round_to electric_power str_result)

(def ^:dynamic electric_power_p nil)

(def ^:dynamic electric_power_zeros nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_p nil)

(def ^:dynamic round_to_m nil)

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (- absf_x) absf_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_p nil] (try (do (set! pow10_p 1.0) (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_p (* pow10_p 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round_to [round_to_x round_to_n]
  (binding [round_to_m nil] (try (do (set! round_to_m (pow10 round_to_n)) (throw (ex-info "return" {:v (/ (Math/floor (+ (* round_to_x round_to_m) 0.5)) round_to_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn electric_power [electric_power_voltage electric_power_current electric_power_power]
  (binding [electric_power_p nil electric_power_zeros nil] (try (do (set! electric_power_zeros 0) (when (= electric_power_voltage 0.0) (set! electric_power_zeros (+ electric_power_zeros 1))) (when (= electric_power_current 0.0) (set! electric_power_zeros (+ electric_power_zeros 1))) (when (= electric_power_power 0.0) (set! electric_power_zeros (+ electric_power_zeros 1))) (if (not= electric_power_zeros 1) (throw (Exception. "Exactly one argument must be 0")) (if (< electric_power_power 0.0) (throw (Exception. "Power cannot be negative in any electrical/electronics system")) (if (= electric_power_voltage 0.0) (throw (ex-info "return" {:v {:name "voltage" :value (/ electric_power_power electric_power_current)}})) (if (= electric_power_current 0.0) (throw (ex-info "return" {:v {:name "current" :value (/ electric_power_power electric_power_voltage)}})) (if (= electric_power_power 0.0) (do (set! electric_power_p (absf (* electric_power_voltage electric_power_current))) (throw (ex-info "return" {:v {:name "power" :value (round_to electric_power_p 2)}}))) (throw (Exception. "Unhandled case")))))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn str_result [str_result_r]
  (try (throw (ex-info "return" {:v (str (str (str (str "Result(name='" (:name str_result_r)) "', value=") (str (:value str_result_r))) ")")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_r1 nil)

(def ^:dynamic main_r2 nil)

(def ^:dynamic main_r3 nil)

(def ^:dynamic main_r4 nil)

(def ^:dynamic main_r5 nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_r1) (constantly (electric_power 0.0 2.0 5.0)))
      (println (str_result main_r1))
      (alter-var-root (var main_r2) (constantly (electric_power 2.0 2.0 0.0)))
      (println (str_result main_r2))
      (alter-var-root (var main_r3) (constantly (electric_power (- 2.0) 3.0 0.0)))
      (println (str_result main_r3))
      (alter-var-root (var main_r4) (constantly (electric_power 2.2 2.2 0.0)))
      (println (str_result main_r4))
      (alter-var-root (var main_r5) (constantly (electric_power 2.0 0.0 6.0)))
      (println (str_result main_r5))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
