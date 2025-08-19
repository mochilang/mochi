(ns main (:refer-clojure :exclude [shear_stress str_result]))

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

(declare shear_stress str_result)

(def ^:dynamic shear_stress_zeros nil)

(defn shear_stress [shear_stress_stress shear_stress_tangential_force shear_stress_area]
  (binding [shear_stress_zeros nil] (try (do (set! shear_stress_zeros 0) (when (= shear_stress_stress 0.0) (set! shear_stress_zeros (+ shear_stress_zeros 1))) (when (= shear_stress_tangential_force 0.0) (set! shear_stress_zeros (+ shear_stress_zeros 1))) (when (= shear_stress_area 0.0) (set! shear_stress_zeros (+ shear_stress_zeros 1))) (if (not= shear_stress_zeros 1) (throw (Exception. "You cannot supply more or less than 2 values")) (if (< shear_stress_stress 0.0) (throw (Exception. "Stress cannot be negative")) (if (< shear_stress_tangential_force 0.0) (throw (Exception. "Tangential Force cannot be negative")) (if (< shear_stress_area 0.0) (throw (Exception. "Area cannot be negative")) (if (= shear_stress_stress 0.0) (throw (ex-info "return" {:v {:name "stress" :value (quot shear_stress_tangential_force shear_stress_area)}})) (if (= shear_stress_tangential_force 0.0) (throw (ex-info "return" {:v {:name "tangential_force" :value (* shear_stress_stress shear_stress_area)}})) (throw (ex-info "return" {:v {:name "area" :value (quot shear_stress_tangential_force shear_stress_stress)}}))))))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn str_result [str_result_r]
  (try (throw (ex-info "return" {:v (str (str (str (str "Result(name='" (:name str_result_r)) "', value=") (mochi_str (:value str_result_r))) ")")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_r1 nil)

(def ^:dynamic main_r2 nil)

(def ^:dynamic main_r3 nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_r1) (constantly (shear_stress 25.0 100.0 0.0)))
      (println (str_result main_r1))
      (alter-var-root (var main_r2) (constantly (shear_stress 0.0 1600.0 200.0)))
      (println (str_result main_r2))
      (alter-var-root (var main_r3) (constantly (shear_stress 1000.0 0.0 1200.0)))
      (println (str_result main_r3))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
