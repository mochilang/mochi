(ns main (:refer-clojure :exclude [pow sqrt_approx hubble_parameter test_hubble_parameter main]))

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

(declare pow sqrt_approx hubble_parameter test_hubble_parameter main)

(def ^:dynamic hubble_parameter_curvature nil)

(def ^:dynamic hubble_parameter_e2 nil)

(def ^:dynamic hubble_parameter_i nil)

(def ^:dynamic hubble_parameter_parameters nil)

(def ^:dynamic hubble_parameter_zp1 nil)

(def ^:dynamic pow_i nil)

(def ^:dynamic pow_result nil)

(def ^:dynamic sqrt_approx_guess nil)

(def ^:dynamic sqrt_approx_i nil)

(def ^:dynamic test_hubble_parameter_h nil)

(defn pow [pow_base pow_exp]
  (binding [pow_i nil pow_result nil] (try (do (set! pow_result 1.0) (set! pow_i 0) (while (< pow_i pow_exp) (do (set! pow_result (* pow_result pow_base)) (set! pow_i (+ pow_i 1)))) (throw (ex-info "return" {:v pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sqrt_approx [sqrt_approx_x]
  (binding [sqrt_approx_guess nil sqrt_approx_i nil] (try (do (when (= sqrt_approx_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_approx_guess (/ sqrt_approx_x 2.0)) (set! sqrt_approx_i 0) (while (< sqrt_approx_i 20) (do (set! sqrt_approx_guess (/ (+ sqrt_approx_guess (quot sqrt_approx_x sqrt_approx_guess)) 2.0)) (set! sqrt_approx_i (+ sqrt_approx_i 1)))) (throw (ex-info "return" {:v sqrt_approx_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hubble_parameter [hubble_parameter_hubble_constant hubble_parameter_radiation_density hubble_parameter_matter_density hubble_parameter_dark_energy hubble_parameter_redshift]
  (binding [hubble_parameter_curvature nil hubble_parameter_e2 nil hubble_parameter_i nil hubble_parameter_parameters nil hubble_parameter_zp1 nil] (try (do (set! hubble_parameter_parameters [hubble_parameter_redshift hubble_parameter_radiation_density hubble_parameter_matter_density hubble_parameter_dark_energy]) (set! hubble_parameter_i 0) (while (< hubble_parameter_i (count hubble_parameter_parameters)) (do (when (< (nth hubble_parameter_parameters hubble_parameter_i) 0.0) (throw (Exception. "All input parameters must be positive"))) (set! hubble_parameter_i (+ hubble_parameter_i 1)))) (set! hubble_parameter_i 1) (while (< hubble_parameter_i 4) (do (when (> (nth hubble_parameter_parameters hubble_parameter_i) 1.0) (throw (Exception. "Relative densities cannot be greater than one"))) (set! hubble_parameter_i (+ hubble_parameter_i 1)))) (set! hubble_parameter_curvature (- 1.0 (+ (+ hubble_parameter_matter_density hubble_parameter_radiation_density) hubble_parameter_dark_energy))) (set! hubble_parameter_zp1 (+ hubble_parameter_redshift 1.0)) (set! hubble_parameter_e2 (+ (+ (+ (* hubble_parameter_radiation_density (pow hubble_parameter_zp1 4)) (* hubble_parameter_matter_density (pow hubble_parameter_zp1 3))) (* hubble_parameter_curvature (pow hubble_parameter_zp1 2))) hubble_parameter_dark_energy)) (throw (ex-info "return" {:v (* hubble_parameter_hubble_constant (sqrt_approx hubble_parameter_e2))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_hubble_parameter []
  (binding [test_hubble_parameter_h nil] (do (set! test_hubble_parameter_h (hubble_parameter 68.3 0.0001 0.3 0.7 0.0)) (when (or (< test_hubble_parameter_h 68.2999) (> test_hubble_parameter_h 68.3001)) (throw (Exception. "hubble_parameter test failed"))))))

(defn main []
  (do (test_hubble_parameter) (println (hubble_parameter 68.3 0.0001 0.3 0.7 0.0))))

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
