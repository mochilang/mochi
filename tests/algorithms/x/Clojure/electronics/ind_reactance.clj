(ns main (:refer-clojure :exclude [ind_reactance]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare ind_reactance)

(declare _read_file)

(def ^:dynamic ind_reactance_zero_count nil)

(def ^:dynamic main_PI nil)

(defn ind_reactance [ind_reactance_inductance ind_reactance_frequency ind_reactance_reactance]
  (binding [ind_reactance_zero_count nil] (try (do (set! ind_reactance_zero_count 0) (when (= ind_reactance_inductance 0.0) (set! ind_reactance_zero_count (+ ind_reactance_zero_count 1))) (when (= ind_reactance_frequency 0.0) (set! ind_reactance_zero_count (+ ind_reactance_zero_count 1))) (when (= ind_reactance_reactance 0.0) (set! ind_reactance_zero_count (+ ind_reactance_zero_count 1))) (when (not= ind_reactance_zero_count 1) (throw (Exception. "One and only one argument must be 0"))) (when (< ind_reactance_inductance 0.0) (throw (Exception. "Inductance cannot be negative"))) (when (< ind_reactance_frequency 0.0) (throw (Exception. "Frequency cannot be negative"))) (when (< ind_reactance_reactance 0.0) (throw (Exception. "Inductive reactance cannot be negative"))) (when (= ind_reactance_inductance 0.0) (throw (ex-info "return" {:v {"inductance" (/ ind_reactance_reactance (* (* 2.0 main_PI) ind_reactance_frequency))}}))) (if (= ind_reactance_frequency 0.0) {"frequency" (/ ind_reactance_reactance (* (* 2.0 main_PI) ind_reactance_inductance))} {"reactance" (* (* (* 2.0 main_PI) ind_reactance_frequency) ind_reactance_inductance)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_PI) (constantly 3.141592653589793))
      (println (ind_reactance 0.0 10000.0 50.0))
      (println (ind_reactance 0.035 0.0 50.0))
      (println (ind_reactance 0.000035 1000.0 0.0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
