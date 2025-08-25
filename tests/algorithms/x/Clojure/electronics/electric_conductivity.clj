(ns main (:refer-clojure :exclude [electric_conductivity]))

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

(declare electric_conductivity)

(declare _read_file)

(def ^:dynamic electric_conductivity_zero_count nil)

(def ^:dynamic main_ELECTRON_CHARGE nil)

(defn electric_conductivity [electric_conductivity_conductivity electric_conductivity_electron_conc electric_conductivity_mobility]
  (binding [electric_conductivity_zero_count nil] (try (do (set! electric_conductivity_zero_count 0) (when (= electric_conductivity_conductivity 0.0) (set! electric_conductivity_zero_count (+ electric_conductivity_zero_count 1))) (when (= electric_conductivity_electron_conc 0.0) (set! electric_conductivity_zero_count (+ electric_conductivity_zero_count 1))) (when (= electric_conductivity_mobility 0.0) (set! electric_conductivity_zero_count (+ electric_conductivity_zero_count 1))) (when (not= electric_conductivity_zero_count 1) (throw (Exception. "You cannot supply more or less than 2 values"))) (when (< electric_conductivity_conductivity 0.0) (throw (Exception. "Conductivity cannot be negative"))) (when (< electric_conductivity_electron_conc 0.0) (throw (Exception. "Electron concentration cannot be negative"))) (when (< electric_conductivity_mobility 0.0) (throw (Exception. "mobility cannot be negative"))) (when (= electric_conductivity_conductivity 0.0) (throw (ex-info "return" {:v {:kind "conductivity" :value (* (* electric_conductivity_mobility electric_conductivity_electron_conc) main_ELECTRON_CHARGE)}}))) (if (= electric_conductivity_electron_conc 0.0) {:kind "electron_conc" :value (/ electric_conductivity_conductivity (* electric_conductivity_mobility main_ELECTRON_CHARGE))} {:kind "mobility" :value (/ electric_conductivity_conductivity (* electric_conductivity_electron_conc main_ELECTRON_CHARGE))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_r1 nil)

(def ^:dynamic main_r2 nil)

(def ^:dynamic main_r3 nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_ELECTRON_CHARGE) (constantly 0.00000000000000000016021))
      (alter-var-root (var main_r1) (constantly (electric_conductivity 25.0 100.0 0.0)))
      (alter-var-root (var main_r2) (constantly (electric_conductivity 0.0 1600.0 200.0)))
      (alter-var-root (var main_r3) (constantly (electric_conductivity 1000.0 0.0 1200.0)))
      (println (str (str (:kind main_r1) " ") (mochi_str (:value main_r1))))
      (println (str (str (:kind main_r2) " ") (mochi_str (:value main_r2))))
      (println (str (str (:kind main_r3) " ") (mochi_str (:value main_r3))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
