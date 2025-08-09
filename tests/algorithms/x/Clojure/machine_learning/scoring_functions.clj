(ns main (:refer-clojure :exclude [absf sqrtApprox ln_series ln mae mse rmse rmsle mbd manual_accuracy main]))

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

(declare absf sqrtApprox ln_series ln mae mse rmse rmsle mbd manual_accuracy main)

(def ^:dynamic ln_k nil)

(def ^:dynamic ln_series_n nil)

(def ^:dynamic ln_series_sum nil)

(def ^:dynamic ln_series_t nil)

(def ^:dynamic ln_series_term nil)

(def ^:dynamic ln_y nil)

(def ^:dynamic mae_diff nil)

(def ^:dynamic mae_i nil)

(def ^:dynamic mae_sum nil)

(def ^:dynamic main_actual nil)

(def ^:dynamic main_predict nil)

(def ^:dynamic manual_accuracy_correct nil)

(def ^:dynamic manual_accuracy_i nil)

(def ^:dynamic mbd_actual_sum nil)

(def ^:dynamic mbd_denominator nil)

(def ^:dynamic mbd_diff_sum nil)

(def ^:dynamic mbd_i nil)

(def ^:dynamic mbd_n nil)

(def ^:dynamic mbd_numerator nil)

(def ^:dynamic mse_diff nil)

(def ^:dynamic mse_i nil)

(def ^:dynamic mse_sum nil)

(def ^:dynamic rmsle_diff nil)

(def ^:dynamic rmsle_i nil)

(def ^:dynamic rmsle_la nil)

(def ^:dynamic rmsle_lp nil)

(def ^:dynamic rmsle_sum nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (- 0.0 absf_x) absf_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ln_series [ln_series_x]
  (binding [ln_series_n nil ln_series_sum nil ln_series_t nil ln_series_term nil] (try (do (set! ln_series_t (quot (- ln_series_x 1.0) (+ ln_series_x 1.0))) (set! ln_series_term ln_series_t) (set! ln_series_sum 0.0) (set! ln_series_n 1) (while (<= ln_series_n 19) (do (set! ln_series_sum (+ ln_series_sum (quot ln_series_term (double ln_series_n)))) (set! ln_series_term (* (* ln_series_term ln_series_t) ln_series_t)) (set! ln_series_n (+ ln_series_n 2)))) (throw (ex-info "return" {:v (* 2.0 ln_series_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ln [ln_x]
  (binding [ln_k nil ln_y nil] (try (do (set! ln_y ln_x) (set! ln_k 0) (while (>= ln_y 10.0) (do (set! ln_y (/ ln_y 10.0)) (set! ln_k (+ ln_k 1)))) (while (< ln_y 1.0) (do (set! ln_y (* ln_y 10.0)) (set! ln_k (- ln_k 1)))) (throw (ex-info "return" {:v (+ (ln_series ln_y) (* (double ln_k) (ln_series 10.0)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mae [mae_predict mae_actual]
  (binding [mae_diff nil mae_i nil mae_sum nil] (try (do (set! mae_sum 0.0) (set! mae_i 0) (while (< mae_i (count mae_predict)) (do (set! mae_diff (- (nth mae_predict mae_i) (nth mae_actual mae_i))) (set! mae_sum (+ mae_sum (absf mae_diff))) (set! mae_i (+ mae_i 1)))) (throw (ex-info "return" {:v (quot mae_sum (double (count mae_predict)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mse [mse_predict mse_actual]
  (binding [mse_diff nil mse_i nil mse_sum nil] (try (do (set! mse_sum 0.0) (set! mse_i 0) (while (< mse_i (count mse_predict)) (do (set! mse_diff (- (nth mse_predict mse_i) (nth mse_actual mse_i))) (set! mse_sum (+ mse_sum (* mse_diff mse_diff))) (set! mse_i (+ mse_i 1)))) (throw (ex-info "return" {:v (quot mse_sum (double (count mse_predict)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rmse [rmse_predict rmse_actual]
  (try (throw (ex-info "return" {:v (sqrtApprox (mse rmse_predict rmse_actual))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rmsle [rmsle_predict rmsle_actual]
  (binding [rmsle_diff nil rmsle_i nil rmsle_la nil rmsle_lp nil rmsle_sum nil] (try (do (set! rmsle_sum 0.0) (set! rmsle_i 0) (while (< rmsle_i (count rmsle_predict)) (do (set! rmsle_lp (ln (+ (nth rmsle_predict rmsle_i) 1.0))) (set! rmsle_la (ln (+ (nth rmsle_actual rmsle_i) 1.0))) (set! rmsle_diff (- rmsle_lp rmsle_la)) (set! rmsle_sum (+ rmsle_sum (* rmsle_diff rmsle_diff))) (set! rmsle_i (+ rmsle_i 1)))) (throw (ex-info "return" {:v (sqrtApprox (quot rmsle_sum (double (count rmsle_predict))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mbd [mbd_predict mbd_actual]
  (binding [mbd_actual_sum nil mbd_denominator nil mbd_diff_sum nil mbd_i nil mbd_n nil mbd_numerator nil] (try (do (set! mbd_diff_sum 0.0) (set! mbd_actual_sum 0.0) (set! mbd_i 0) (while (< mbd_i (count mbd_predict)) (do (set! mbd_diff_sum (+ mbd_diff_sum (- (nth mbd_predict mbd_i) (nth mbd_actual mbd_i)))) (set! mbd_actual_sum (+ mbd_actual_sum (nth mbd_actual mbd_i))) (set! mbd_i (+ mbd_i 1)))) (set! mbd_n (double (count mbd_predict))) (set! mbd_numerator (quot mbd_diff_sum mbd_n)) (set! mbd_denominator (quot mbd_actual_sum mbd_n)) (throw (ex-info "return" {:v (* (quot mbd_numerator mbd_denominator) 100.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn manual_accuracy [manual_accuracy_predict manual_accuracy_actual]
  (binding [manual_accuracy_correct nil manual_accuracy_i nil] (try (do (set! manual_accuracy_correct 0) (set! manual_accuracy_i 0) (while (< manual_accuracy_i (count manual_accuracy_predict)) (do (when (= (nth manual_accuracy_predict manual_accuracy_i) (nth manual_accuracy_actual manual_accuracy_i)) (set! manual_accuracy_correct (+ manual_accuracy_correct 1))) (set! manual_accuracy_i (+ manual_accuracy_i 1)))) (throw (ex-info "return" {:v (quot (double manual_accuracy_correct) (double (count manual_accuracy_predict)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_actual nil main_predict nil] (do (set! main_actual [1.0 2.0 3.0]) (set! main_predict [1.0 4.0 3.0]) (println (str (mae main_predict main_actual))) (println (str (mse main_predict main_actual))) (println (str (rmse main_predict main_actual))) (println (str (rmsle [10.0 2.0 30.0] [10.0 10.0 30.0]))) (println (str (mbd [2.0 3.0 4.0] [1.0 2.0 3.0]))) (println (str (mbd [0.0 1.0 1.0] [1.0 2.0 3.0]))) (println (str (manual_accuracy main_predict main_actual))))))

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
