(ns main (:refer-clojure :exclude [exp_approx ln_series ln powf rainfall_intensity]))

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

(declare exp_approx ln_series ln powf rainfall_intensity)

(def ^:dynamic exp_approx_is_neg nil)

(def ^:dynamic exp_approx_n nil)

(def ^:dynamic exp_approx_sum nil)

(def ^:dynamic exp_approx_term nil)

(def ^:dynamic exp_approx_y nil)

(def ^:dynamic ln_k nil)

(def ^:dynamic ln_series_n nil)

(def ^:dynamic ln_series_sum nil)

(def ^:dynamic ln_series_t nil)

(def ^:dynamic ln_series_term nil)

(def ^:dynamic ln_y nil)

(def ^:dynamic rainfall_intensity_denominator nil)

(def ^:dynamic rainfall_intensity_numerator nil)

(defn exp_approx [exp_approx_x]
  (binding [exp_approx_is_neg nil exp_approx_n nil exp_approx_sum nil exp_approx_term nil exp_approx_y nil] (try (do (set! exp_approx_y exp_approx_x) (set! exp_approx_is_neg false) (when (< exp_approx_x 0.0) (do (set! exp_approx_is_neg true) (set! exp_approx_y (- exp_approx_x)))) (set! exp_approx_term 1.0) (set! exp_approx_sum 1.0) (set! exp_approx_n 1) (while (< exp_approx_n 30) (do (set! exp_approx_term (/ (* exp_approx_term exp_approx_y) (double exp_approx_n))) (set! exp_approx_sum (+ exp_approx_sum exp_approx_term)) (set! exp_approx_n (+ exp_approx_n 1)))) (if exp_approx_is_neg (/ 1.0 exp_approx_sum) exp_approx_sum)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ln_series [ln_series_x]
  (binding [ln_series_n nil ln_series_sum nil ln_series_t nil ln_series_term nil] (try (do (set! ln_series_t (/ (- ln_series_x 1.0) (+ ln_series_x 1.0))) (set! ln_series_term ln_series_t) (set! ln_series_sum 0.0) (set! ln_series_n 1) (while (<= ln_series_n 19) (do (set! ln_series_sum (+ ln_series_sum (/ ln_series_term (double ln_series_n)))) (set! ln_series_term (* (* ln_series_term ln_series_t) ln_series_t)) (set! ln_series_n (+ ln_series_n 2)))) (throw (ex-info "return" {:v (* 2.0 ln_series_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ln [ln_x]
  (binding [ln_k nil ln_y nil] (try (do (set! ln_y ln_x) (set! ln_k 0) (while (>= ln_y 10.0) (do (set! ln_y (/ ln_y 10.0)) (set! ln_k (+ ln_k 1)))) (while (< ln_y 1.0) (do (set! ln_y (* ln_y 10.0)) (set! ln_k (- ln_k 1)))) (throw (ex-info "return" {:v (+ (ln_series ln_y) (* (double ln_k) (ln_series 10.0)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn powf [powf_base powf_exponent]
  (try (throw (ex-info "return" {:v (exp_approx (* powf_exponent (ln powf_base)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rainfall_intensity [rainfall_intensity_coefficient_k rainfall_intensity_coefficient_a rainfall_intensity_coefficient_b rainfall_intensity_coefficient_c rainfall_intensity_return_period rainfall_intensity_duration]
  (binding [rainfall_intensity_denominator nil rainfall_intensity_numerator nil] (try (do (when (<= rainfall_intensity_coefficient_k 0.0) (throw (Exception. "All parameters must be positive."))) (when (<= rainfall_intensity_coefficient_a 0.0) (throw (Exception. "All parameters must be positive."))) (when (<= rainfall_intensity_coefficient_b 0.0) (throw (Exception. "All parameters must be positive."))) (when (<= rainfall_intensity_coefficient_c 0.0) (throw (Exception. "All parameters must be positive."))) (when (<= rainfall_intensity_return_period 0.0) (throw (Exception. "All parameters must be positive."))) (when (<= rainfall_intensity_duration 0.0) (throw (Exception. "All parameters must be positive."))) (set! rainfall_intensity_numerator (* rainfall_intensity_coefficient_k (powf rainfall_intensity_return_period rainfall_intensity_coefficient_a))) (set! rainfall_intensity_denominator (powf (+ rainfall_intensity_duration rainfall_intensity_coefficient_b) rainfall_intensity_coefficient_c)) (throw (ex-info "return" {:v (/ rainfall_intensity_numerator rainfall_intensity_denominator)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_r1 (rainfall_intensity 1000.0 0.2 11.6 0.81 10.0 60.0))

(def ^:dynamic main_r2 (rainfall_intensity 1000.0 0.2 11.6 0.81 10.0 30.0))

(def ^:dynamic main_r3 (rainfall_intensity 1000.0 0.2 11.6 0.81 5.0 60.0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_r1))
      (println (str main_r2))
      (println (str main_r3))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
