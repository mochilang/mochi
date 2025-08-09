(ns main (:refer-clojure :exclude [sinApprox cosApprox expApprox ln log10 sqrtApprox absf normalize dft triangular_filters dot discrete_cosine_transform mfcc]))

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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sinApprox cosApprox expApprox ln log10 sqrtApprox absf normalize dft triangular_filters dot discrete_cosine_transform mfcc)

(def ^:dynamic cosApprox_denom nil)

(def ^:dynamic cosApprox_n nil)

(def ^:dynamic cosApprox_sum nil)

(def ^:dynamic cosApprox_term nil)

(def ^:dynamic dft_N nil)

(def ^:dynamic dft_angle nil)

(def ^:dynamic dft_imag nil)

(def ^:dynamic dft_k nil)

(def ^:dynamic dft_n nil)

(def ^:dynamic dft_real nil)

(def ^:dynamic dft_spec nil)

(def ^:dynamic discrete_cosine_transform_angle nil)

(def ^:dynamic discrete_cosine_transform_basis nil)

(def ^:dynamic discrete_cosine_transform_i nil)

(def ^:dynamic discrete_cosine_transform_j nil)

(def ^:dynamic discrete_cosine_transform_row nil)

(def ^:dynamic dot_i nil)

(def ^:dynamic dot_j nil)

(def ^:dynamic dot_res nil)

(def ^:dynamic dot_sum nil)

(def ^:dynamic expApprox_n nil)

(def ^:dynamic expApprox_sum nil)

(def ^:dynamic expApprox_term nil)

(def ^:dynamic ln_n nil)

(def ^:dynamic ln_sum nil)

(def ^:dynamic ln_t nil)

(def ^:dynamic ln_term nil)

(def ^:dynamic main_audio nil)

(def ^:dynamic main_n nil)

(def ^:dynamic mfcc_dct_basis nil)

(def ^:dynamic mfcc_energies nil)

(def ^:dynamic mfcc_filters nil)

(def ^:dynamic mfcc_i nil)

(def ^:dynamic mfcc_logfb nil)

(def ^:dynamic mfcc_norm nil)

(def ^:dynamic mfcc_res nil)

(def ^:dynamic mfcc_spec nil)

(def ^:dynamic normalize_i nil)

(def ^:dynamic normalize_max_val nil)

(def ^:dynamic normalize_res nil)

(def ^:dynamic normalize_v nil)

(def ^:dynamic sinApprox_denom nil)

(def ^:dynamic sinApprox_n nil)

(def ^:dynamic sinApprox_sum nil)

(def ^:dynamic sinApprox_term nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic triangular_filters_b nil)

(def ^:dynamic triangular_filters_center nil)

(def ^:dynamic triangular_filters_filt nil)

(def ^:dynamic triangular_filters_filters nil)

(def ^:dynamic triangular_filters_i nil)

(def ^:dynamic triangular_filters_v nil)

(def ^:dynamic main_PI 3.141592653589793)

(defn sinApprox [sinApprox_x]
  (binding [sinApprox_denom nil sinApprox_n nil sinApprox_sum nil sinApprox_term nil] (try (do (set! sinApprox_term sinApprox_x) (set! sinApprox_sum sinApprox_x) (set! sinApprox_n 1) (while (<= sinApprox_n 10) (do (set! sinApprox_denom (double (* (* 2 sinApprox_n) (+ (* 2 sinApprox_n) 1)))) (set! sinApprox_term (quot (* (* (- sinApprox_term) sinApprox_x) sinApprox_x) sinApprox_denom)) (set! sinApprox_sum (+ sinApprox_sum sinApprox_term)) (set! sinApprox_n (+ sinApprox_n 1)))) (throw (ex-info "return" {:v sinApprox_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cosApprox [cosApprox_x]
  (binding [cosApprox_denom nil cosApprox_n nil cosApprox_sum nil cosApprox_term nil] (try (do (set! cosApprox_term 1.0) (set! cosApprox_sum 1.0) (set! cosApprox_n 1) (while (<= cosApprox_n 10) (do (set! cosApprox_denom (double (* (- (* 2 cosApprox_n) 1) (* 2 cosApprox_n)))) (set! cosApprox_term (quot (* (* (- cosApprox_term) cosApprox_x) cosApprox_x) cosApprox_denom)) (set! cosApprox_sum (+ cosApprox_sum cosApprox_term)) (set! cosApprox_n (+ cosApprox_n 1)))) (throw (ex-info "return" {:v cosApprox_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn expApprox [expApprox_x]
  (binding [expApprox_n nil expApprox_sum nil expApprox_term nil] (try (do (set! expApprox_sum 1.0) (set! expApprox_term 1.0) (set! expApprox_n 1) (while (< expApprox_n 10) (do (set! expApprox_term (quot (* expApprox_term expApprox_x) (double expApprox_n))) (set! expApprox_sum (+ expApprox_sum expApprox_term)) (set! expApprox_n (+ expApprox_n 1)))) (throw (ex-info "return" {:v expApprox_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ln [ln_x]
  (binding [ln_n nil ln_sum nil ln_t nil ln_term nil] (try (do (set! ln_t (quot (- ln_x 1.0) (+ ln_x 1.0))) (set! ln_term ln_t) (set! ln_sum 0.0) (set! ln_n 1) (while (<= ln_n 19) (do (set! ln_sum (+ ln_sum (quot ln_term (double ln_n)))) (set! ln_term (* (* ln_term ln_t) ln_t)) (set! ln_n (+ ln_n 2)))) (throw (ex-info "return" {:v (* 2.0 ln_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn log10 [log10_x]
  (try (throw (ex-info "return" {:v (quot (ln log10_x) (ln 10.0))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 10) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (- absf_x) absf_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn normalize [normalize_audio]
  (binding [normalize_i nil normalize_max_val nil normalize_res nil normalize_v nil] (try (do (set! normalize_max_val 0.0) (set! normalize_i 0) (while (< normalize_i (count normalize_audio)) (do (set! normalize_v (absf (nth normalize_audio normalize_i))) (when (> normalize_v normalize_max_val) (set! normalize_max_val normalize_v)) (set! normalize_i (+ normalize_i 1)))) (set! normalize_res []) (set! normalize_i 0) (while (< normalize_i (count normalize_audio)) (do (set! normalize_res (conj normalize_res (quot (nth normalize_audio normalize_i) normalize_max_val))) (set! normalize_i (+ normalize_i 1)))) (throw (ex-info "return" {:v normalize_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dft [dft_frame dft_bins]
  (binding [dft_N nil dft_angle nil dft_imag nil dft_k nil dft_n nil dft_real nil dft_spec nil] (try (do (set! dft_N (count dft_frame)) (set! dft_spec []) (set! dft_k 0) (while (< dft_k dft_bins) (do (set! dft_real 0.0) (set! dft_imag 0.0) (set! dft_n 0) (while (< dft_n dft_N) (do (set! dft_angle (quot (* (* (* (- 2.0) main_PI) (double dft_k)) (double dft_n)) (double dft_N))) (set! dft_real (+ dft_real (* (nth dft_frame dft_n) (cosApprox dft_angle)))) (set! dft_imag (+ dft_imag (* (nth dft_frame dft_n) (sinApprox dft_angle)))) (set! dft_n (+ dft_n 1)))) (set! dft_spec (conj dft_spec (+ (* dft_real dft_real) (* dft_imag dft_imag)))) (set! dft_k (+ dft_k 1)))) (throw (ex-info "return" {:v dft_spec}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn triangular_filters [triangular_filters_bins triangular_filters_spectrum_size]
  (binding [triangular_filters_b nil triangular_filters_center nil triangular_filters_filt nil triangular_filters_filters nil triangular_filters_i nil triangular_filters_v nil] (try (do (set! triangular_filters_filters []) (set! triangular_filters_b 0) (while (< triangular_filters_b triangular_filters_bins) (do (set! triangular_filters_center (quot (* (+ triangular_filters_b 1) triangular_filters_spectrum_size) (+ triangular_filters_bins 1))) (set! triangular_filters_filt []) (set! triangular_filters_i 0) (while (< triangular_filters_i triangular_filters_spectrum_size) (do (set! triangular_filters_v 0.0) (if (<= triangular_filters_i triangular_filters_center) (set! triangular_filters_v (quot (double triangular_filters_i) (double triangular_filters_center))) (set! triangular_filters_v (quot (double (- triangular_filters_spectrum_size triangular_filters_i)) (double (- triangular_filters_spectrum_size triangular_filters_center))))) (set! triangular_filters_filt (conj triangular_filters_filt triangular_filters_v)) (set! triangular_filters_i (+ triangular_filters_i 1)))) (set! triangular_filters_filters (conj triangular_filters_filters triangular_filters_filt)) (set! triangular_filters_b (+ triangular_filters_b 1)))) (throw (ex-info "return" {:v triangular_filters_filters}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dot [dot_mat dot_vec]
  (binding [dot_i nil dot_j nil dot_res nil dot_sum nil] (try (do (set! dot_res []) (set! dot_i 0) (while (< dot_i (count dot_mat)) (do (set! dot_sum 0.0) (set! dot_j 0) (while (< dot_j (count dot_vec)) (do (set! dot_sum (+ dot_sum (* (nth (nth dot_mat dot_i) dot_j) (nth dot_vec dot_j)))) (set! dot_j (+ dot_j 1)))) (set! dot_res (conj dot_res dot_sum)) (set! dot_i (+ dot_i 1)))) (throw (ex-info "return" {:v dot_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn discrete_cosine_transform [discrete_cosine_transform_dct_filter_num discrete_cosine_transform_filter_num]
  (binding [discrete_cosine_transform_angle nil discrete_cosine_transform_basis nil discrete_cosine_transform_i nil discrete_cosine_transform_j nil discrete_cosine_transform_row nil] (try (do (set! discrete_cosine_transform_basis []) (set! discrete_cosine_transform_i 0) (while (< discrete_cosine_transform_i discrete_cosine_transform_dct_filter_num) (do (set! discrete_cosine_transform_row []) (set! discrete_cosine_transform_j 0) (while (< discrete_cosine_transform_j discrete_cosine_transform_filter_num) (do (if (= discrete_cosine_transform_i 0) (set! discrete_cosine_transform_row (conj discrete_cosine_transform_row (/ 1.0 (sqrtApprox (double discrete_cosine_transform_filter_num))))) (do (set! discrete_cosine_transform_angle (quot (* (* (double (+ (* 2 discrete_cosine_transform_j) 1)) (double discrete_cosine_transform_i)) main_PI) (* 2.0 (double discrete_cosine_transform_filter_num)))) (set! discrete_cosine_transform_row (conj discrete_cosine_transform_row (* (cosApprox discrete_cosine_transform_angle) (sqrtApprox (/ 2.0 (double discrete_cosine_transform_filter_num)))))))) (set! discrete_cosine_transform_j (+ discrete_cosine_transform_j 1)))) (set! discrete_cosine_transform_basis (conj discrete_cosine_transform_basis discrete_cosine_transform_row)) (set! discrete_cosine_transform_i (+ discrete_cosine_transform_i 1)))) (throw (ex-info "return" {:v discrete_cosine_transform_basis}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mfcc [mfcc_audio mfcc_bins mfcc_dct_num]
  (binding [mfcc_dct_basis nil mfcc_energies nil mfcc_filters nil mfcc_i nil mfcc_logfb nil mfcc_norm nil mfcc_res nil mfcc_spec nil] (try (do (set! mfcc_norm (normalize mfcc_audio)) (set! mfcc_spec (dft mfcc_norm (+ mfcc_bins 2))) (set! mfcc_filters (triangular_filters mfcc_bins (count mfcc_spec))) (set! mfcc_energies (dot mfcc_filters mfcc_spec)) (set! mfcc_logfb []) (set! mfcc_i 0) (while (< mfcc_i (count mfcc_energies)) (do (set! mfcc_logfb (conj mfcc_logfb (* 10.0 (log10 (+ (nth mfcc_energies mfcc_i) 0.0000000001))))) (set! mfcc_i (+ mfcc_i 1)))) (set! mfcc_dct_basis (discrete_cosine_transform mfcc_dct_num mfcc_bins)) (set! mfcc_res (dot mfcc_dct_basis mfcc_logfb)) (when (= (count mfcc_res) 0) (set! mfcc_res [0.0 0.0 0.0])) (throw (ex-info "return" {:v mfcc_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_sample_rate 8000)

(def ^:dynamic main_size 16)

(def ^:dynamic main_audio [])

(def ^:dynamic main_n 0)

(def ^:dynamic main_coeffs (mfcc main_audio 5 3))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_n main_size) (do (def ^:dynamic main_t (quot (double main_n) (double main_sample_rate))) (def main_audio (conj main_audio (sinApprox (* (* (* 2.0 main_PI) 440.0) main_t)))) (def main_n (+ main_n 1))))
      (doseq [c main_coeffs] (println main_c))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
