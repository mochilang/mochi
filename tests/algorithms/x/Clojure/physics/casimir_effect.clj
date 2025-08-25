(ns main (:refer-clojure :exclude [sqrtApprox casimir_force main]))

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

(declare sqrtApprox casimir_force main)

(declare _read_file)

(def ^:dynamic casimir_force_a nil)

(def ^:dynamic casimir_force_d nil)

(def ^:dynamic casimir_force_den nil)

(def ^:dynamic casimir_force_f nil)

(def ^:dynamic casimir_force_inner nil)

(def ^:dynamic casimir_force_num nil)

(def ^:dynamic casimir_force_zero_count nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic main_PI nil)

(def ^:dynamic main_REDUCED_PLANCK_CONSTANT nil)

(def ^:dynamic main_SPEED_OF_LIGHT nil)

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 100) (do (set! sqrtApprox_guess (/ (+' sqrtApprox_guess (/ sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+' sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn casimir_force [casimir_force_force casimir_force_area casimir_force_distance]
  (binding [casimir_force_a nil casimir_force_d nil casimir_force_den nil casimir_force_f nil casimir_force_inner nil casimir_force_num nil casimir_force_zero_count nil] (try (do (set! casimir_force_zero_count 0) (when (= casimir_force_force 0.0) (set! casimir_force_zero_count (+' casimir_force_zero_count 1))) (when (= casimir_force_area 0.0) (set! casimir_force_zero_count (+' casimir_force_zero_count 1))) (when (= casimir_force_distance 0.0) (set! casimir_force_zero_count (+' casimir_force_zero_count 1))) (when (not= casimir_force_zero_count 1) (throw (Exception. "One and only one argument must be 0"))) (when (< casimir_force_force 0.0) (throw (Exception. "Magnitude of force can not be negative"))) (when (< casimir_force_distance 0.0) (throw (Exception. "Distance can not be negative"))) (when (< casimir_force_area 0.0) (throw (Exception. "Area can not be negative"))) (when (= casimir_force_force 0.0) (do (set! casimir_force_num (*' (*' (*' (*' main_REDUCED_PLANCK_CONSTANT main_SPEED_OF_LIGHT) main_PI) main_PI) casimir_force_area)) (set! casimir_force_den (*' (*' (*' (*' 240.0 casimir_force_distance) casimir_force_distance) casimir_force_distance) casimir_force_distance)) (set! casimir_force_f (/ casimir_force_num casimir_force_den)) (throw (ex-info "return" {:v {"force" casimir_force_f}})))) (when (= casimir_force_area 0.0) (do (set! casimir_force_num (*' (*' (*' (*' (*' 240.0 casimir_force_force) casimir_force_distance) casimir_force_distance) casimir_force_distance) casimir_force_distance)) (set! casimir_force_den (*' (*' (*' main_REDUCED_PLANCK_CONSTANT main_SPEED_OF_LIGHT) main_PI) main_PI)) (set! casimir_force_a (/ casimir_force_num casimir_force_den)) (throw (ex-info "return" {:v {"area" casimir_force_a}})))) (set! casimir_force_num (*' (*' (*' (*' main_REDUCED_PLANCK_CONSTANT main_SPEED_OF_LIGHT) main_PI) main_PI) casimir_force_area)) (set! casimir_force_den (*' 240.0 casimir_force_force)) (set! casimir_force_inner (/ casimir_force_num casimir_force_den)) (set! casimir_force_d (sqrtApprox (sqrtApprox casimir_force_inner))) (throw (ex-info "return" {:v {"distance" casimir_force_d}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (mochi_str (casimir_force 0.0 4.0 0.03))) (println (mochi_str (casimir_force 0.0000000002635 0.0023 0.0))) (println (mochi_str (casimir_force 0.000000000000000002737 0.0 0.0023746)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_PI) (constantly 3.141592653589793))
      (alter-var-root (var main_REDUCED_PLANCK_CONSTANT) (constantly 0.0000000000000000000000000000000001054571817))
      (alter-var-root (var main_SPEED_OF_LIGHT) (constantly 300000000.0))
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
