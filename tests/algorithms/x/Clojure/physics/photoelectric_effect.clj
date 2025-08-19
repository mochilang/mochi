(ns main (:refer-clojure :exclude [pow10 maximum_kinetic_energy]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pow10 maximum_kinetic_energy)

(def ^:dynamic maximum_kinetic_energy_energy nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_result nil)

(defn pow10 [pow10_exp]
  (binding [pow10_i nil pow10_result nil] (try (do (set! pow10_result 1.0) (set! pow10_i 0) (while (< pow10_i pow10_exp) (do (set! pow10_result (* pow10_result 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_PLANCK_CONSTANT_JS nil)

(def ^:dynamic main_PLANCK_CONSTANT_EVS nil)

(defn maximum_kinetic_energy [maximum_kinetic_energy_frequency maximum_kinetic_energy_work_function maximum_kinetic_energy_in_ev]
  (binding [maximum_kinetic_energy_energy nil] (try (do (when (< maximum_kinetic_energy_frequency 0.0) (throw (Exception. "Frequency can't be negative."))) (set! maximum_kinetic_energy_energy (if maximum_kinetic_energy_in_ev (- (* main_PLANCK_CONSTANT_EVS maximum_kinetic_energy_frequency) maximum_kinetic_energy_work_function) (- (* main_PLANCK_CONSTANT_JS maximum_kinetic_energy_frequency) maximum_kinetic_energy_work_function))) (if (> maximum_kinetic_energy_energy 0.0) maximum_kinetic_energy_energy 0.0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_PLANCK_CONSTANT_JS) (constantly (/ 6.6261 (pow10 34))))
      (alter-var-root (var main_PLANCK_CONSTANT_EVS) (constantly (/ 4.1357 (pow10 15))))
      (println (mochi_str (maximum_kinetic_energy 1000000.0 2.0 false)))
      (println (mochi_str (maximum_kinetic_energy 1000000.0 2.0 true)))
      (println (mochi_str (maximum_kinetic_energy 10000000000000000.0 2.0 true)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
