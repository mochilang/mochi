(ns main (:refer-clojure :exclude [abs sqrtApprox ln log10 peak_signal_to_noise_ratio]))

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

(declare abs sqrtApprox ln log10 peak_signal_to_noise_ratio)

(def ^:dynamic ln_n nil)

(def ^:dynamic ln_sum nil)

(def ^:dynamic ln_t nil)

(def ^:dynamic ln_term nil)

(def ^:dynamic peak_signal_to_noise_ratio_PIXEL_MAX nil)

(def ^:dynamic peak_signal_to_noise_ratio_diff nil)

(def ^:dynamic peak_signal_to_noise_ratio_i nil)

(def ^:dynamic peak_signal_to_noise_ratio_j nil)

(def ^:dynamic peak_signal_to_noise_ratio_mse nil)

(def ^:dynamic peak_signal_to_noise_ratio_size nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(defn abs [abs_x]
  (try (if (< abs_x 0.0) (- abs_x) abs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 10) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (/ sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ln [ln_x]
  (binding [ln_n nil ln_sum nil ln_t nil ln_term nil] (try (do (set! ln_t (/ (- ln_x 1.0) (+ ln_x 1.0))) (set! ln_term ln_t) (set! ln_sum 0.0) (set! ln_n 1) (while (<= ln_n 19) (do (set! ln_sum (+ ln_sum (/ ln_term (double ln_n)))) (set! ln_term (* (* ln_term ln_t) ln_t)) (set! ln_n (+ ln_n 2)))) (throw (ex-info "return" {:v (* 2.0 ln_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn log10 [log10_x]
  (try (throw (ex-info "return" {:v (/ (ln log10_x) (ln 10.0))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn peak_signal_to_noise_ratio [peak_signal_to_noise_ratio_original peak_signal_to_noise_ratio_contrast]
  (binding [peak_signal_to_noise_ratio_PIXEL_MAX nil peak_signal_to_noise_ratio_diff nil peak_signal_to_noise_ratio_i nil peak_signal_to_noise_ratio_j nil peak_signal_to_noise_ratio_mse nil peak_signal_to_noise_ratio_size nil] (try (do (set! peak_signal_to_noise_ratio_mse 0.0) (set! peak_signal_to_noise_ratio_i 0) (while (< peak_signal_to_noise_ratio_i (count peak_signal_to_noise_ratio_original)) (do (set! peak_signal_to_noise_ratio_j 0) (while (< peak_signal_to_noise_ratio_j (count (nth peak_signal_to_noise_ratio_original peak_signal_to_noise_ratio_i))) (do (set! peak_signal_to_noise_ratio_diff (double (- (nth (nth peak_signal_to_noise_ratio_original peak_signal_to_noise_ratio_i) peak_signal_to_noise_ratio_j) (nth (nth peak_signal_to_noise_ratio_contrast peak_signal_to_noise_ratio_i) peak_signal_to_noise_ratio_j)))) (set! peak_signal_to_noise_ratio_mse (+ peak_signal_to_noise_ratio_mse (* peak_signal_to_noise_ratio_diff peak_signal_to_noise_ratio_diff))) (set! peak_signal_to_noise_ratio_j (+ peak_signal_to_noise_ratio_j 1)))) (set! peak_signal_to_noise_ratio_i (+ peak_signal_to_noise_ratio_i 1)))) (set! peak_signal_to_noise_ratio_size (double (* (count peak_signal_to_noise_ratio_original) (count (nth peak_signal_to_noise_ratio_original 0))))) (set! peak_signal_to_noise_ratio_mse (/ peak_signal_to_noise_ratio_mse peak_signal_to_noise_ratio_size)) (when (= peak_signal_to_noise_ratio_mse 0.0) (throw (ex-info "return" {:v 100.0}))) (set! peak_signal_to_noise_ratio_PIXEL_MAX 255.0) (throw (ex-info "return" {:v (* 20.0 (log10 (/ peak_signal_to_noise_ratio_PIXEL_MAX (sqrtApprox peak_signal_to_noise_ratio_mse))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
