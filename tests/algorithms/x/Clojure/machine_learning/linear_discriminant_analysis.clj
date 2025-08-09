(ns main (:refer-clojure :exclude [rand random _mod cos sqrtApprox ln gaussian_distribution y_generator calculate_mean calculate_probabilities calculate_variance predict_y_values accuracy main]))

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

(declare rand random _mod cos sqrtApprox ln gaussian_distribution y_generator calculate_mean calculate_probabilities calculate_variance predict_y_values accuracy main)

(def ^:dynamic accuracy_correct nil)

(def ^:dynamic accuracy_i nil)

(def ^:dynamic calculate_mean_i nil)

(def ^:dynamic calculate_mean_total nil)

(def ^:dynamic calculate_variance_diff nil)

(def ^:dynamic calculate_variance_i nil)

(def ^:dynamic calculate_variance_j nil)

(def ^:dynamic calculate_variance_k nil)

(def ^:dynamic calculate_variance_n_classes nil)

(def ^:dynamic calculate_variance_squared_diff nil)

(def ^:dynamic calculate_variance_sum_sq nil)

(def ^:dynamic cos_y nil)

(def ^:dynamic cos_y2 nil)

(def ^:dynamic cos_y4 nil)

(def ^:dynamic cos_y6 nil)

(def ^:dynamic gaussian_distribution_i nil)

(def ^:dynamic gaussian_distribution_r nil)

(def ^:dynamic gaussian_distribution_res nil)

(def ^:dynamic gaussian_distribution_theta nil)

(def ^:dynamic gaussian_distribution_u1 nil)

(def ^:dynamic gaussian_distribution_u2 nil)

(def ^:dynamic gaussian_distribution_z nil)

(def ^:dynamic ln_n nil)

(def ^:dynamic ln_sum nil)

(def ^:dynamic ln_t nil)

(def ^:dynamic ln_term nil)

(def ^:dynamic main_actual_means nil)

(def ^:dynamic main_counts nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_means nil)

(def ^:dynamic main_predicted nil)

(def ^:dynamic main_probabilities nil)

(def ^:dynamic main_std_dev nil)

(def ^:dynamic main_total_count nil)

(def ^:dynamic main_variance nil)

(def ^:dynamic main_x nil)

(def ^:dynamic main_y nil)

(def ^:dynamic predict_y_values_discr nil)

(def ^:dynamic predict_y_values_i nil)

(def ^:dynamic predict_y_values_j nil)

(def ^:dynamic predict_y_values_k nil)

(def ^:dynamic predict_y_values_max_idx nil)

(def ^:dynamic predict_y_values_max_val nil)

(def ^:dynamic predict_y_values_results nil)

(def ^:dynamic predict_y_values_t nil)

(def ^:dynamic predict_y_values_temp nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic y_generator_i nil)

(def ^:dynamic y_generator_k nil)

(def ^:dynamic y_generator_res nil)

(def ^:dynamic main_PI 3.141592653589793)

(def ^:dynamic main_TWO_PI 6.283185307179586)

(def ^:dynamic main_seed 1)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn random []
  (try (throw (ex-info "return" {:v (/ (double (rand)) 2147483648.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn _mod [_mod_x _mod_m]
  (try (throw (ex-info "return" {:v (- _mod_x (* (double (int (quot _mod_x _mod_m))) _mod_m))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn cos [cos_x]
  (binding [cos_y nil cos_y2 nil cos_y4 nil cos_y6 nil] (try (do (set! cos_y (- (_mod (+ cos_x main_PI) main_TWO_PI) main_PI)) (set! cos_y2 (* cos_y cos_y)) (set! cos_y4 (* cos_y2 cos_y2)) (set! cos_y6 (* cos_y4 cos_y2)) (throw (ex-info "return" {:v (- (+ (- 1.0 (/ cos_y2 2.0)) (/ cos_y4 24.0)) (/ cos_y6 720.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 10) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ln [ln_x]
  (binding [ln_n nil ln_sum nil ln_t nil ln_term nil] (try (do (set! ln_t (quot (- ln_x 1.0) (+ ln_x 1.0))) (set! ln_term ln_t) (set! ln_sum 0.0) (set! ln_n 1) (while (<= ln_n 19) (do (set! ln_sum (+ ln_sum (quot ln_term (double ln_n)))) (set! ln_term (* (* ln_term ln_t) ln_t)) (set! ln_n (+ ln_n 2)))) (throw (ex-info "return" {:v (* 2.0 ln_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gaussian_distribution [gaussian_distribution_mean gaussian_distribution_std_dev gaussian_distribution_instance_count]
  (binding [gaussian_distribution_i nil gaussian_distribution_r nil gaussian_distribution_res nil gaussian_distribution_theta nil gaussian_distribution_u1 nil gaussian_distribution_u2 nil gaussian_distribution_z nil] (try (do (set! gaussian_distribution_res []) (set! gaussian_distribution_i 0) (while (< gaussian_distribution_i gaussian_distribution_instance_count) (do (set! gaussian_distribution_u1 (random)) (set! gaussian_distribution_u2 (random)) (set! gaussian_distribution_r (sqrtApprox (* (- 2.0) (ln gaussian_distribution_u1)))) (set! gaussian_distribution_theta (* main_TWO_PI gaussian_distribution_u2)) (set! gaussian_distribution_z (* gaussian_distribution_r (cos gaussian_distribution_theta))) (set! gaussian_distribution_res (conj gaussian_distribution_res (+ gaussian_distribution_mean (* gaussian_distribution_z gaussian_distribution_std_dev)))) (set! gaussian_distribution_i (+ gaussian_distribution_i 1)))) (throw (ex-info "return" {:v gaussian_distribution_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn y_generator [y_generator_class_count y_generator_instance_count]
  (binding [y_generator_i nil y_generator_k nil y_generator_res nil] (try (do (set! y_generator_res []) (set! y_generator_k 0) (while (< y_generator_k y_generator_class_count) (do (set! y_generator_i 0) (while (< y_generator_i (nth y_generator_instance_count y_generator_k)) (do (set! y_generator_res (conj y_generator_res y_generator_k)) (set! y_generator_i (+ y_generator_i 1)))) (set! y_generator_k (+ y_generator_k 1)))) (throw (ex-info "return" {:v y_generator_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calculate_mean [calculate_mean_instance_count calculate_mean_items]
  (binding [calculate_mean_i nil calculate_mean_total nil] (try (do (set! calculate_mean_total 0.0) (set! calculate_mean_i 0) (while (< calculate_mean_i calculate_mean_instance_count) (do (set! calculate_mean_total (+ calculate_mean_total (nth calculate_mean_items calculate_mean_i))) (set! calculate_mean_i (+ calculate_mean_i 1)))) (throw (ex-info "return" {:v (quot calculate_mean_total (double calculate_mean_instance_count))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calculate_probabilities [calculate_probabilities_instance_count calculate_probabilities_total_count]
  (try (throw (ex-info "return" {:v (quot (double calculate_probabilities_instance_count) (double calculate_probabilities_total_count))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn calculate_variance [calculate_variance_items calculate_variance_means calculate_variance_total_count]
  (binding [calculate_variance_diff nil calculate_variance_i nil calculate_variance_j nil calculate_variance_k nil calculate_variance_n_classes nil calculate_variance_squared_diff nil calculate_variance_sum_sq nil] (try (do (set! calculate_variance_squared_diff []) (set! calculate_variance_i 0) (while (< calculate_variance_i (count calculate_variance_items)) (do (set! calculate_variance_j 0) (while (< calculate_variance_j (count (nth calculate_variance_items calculate_variance_i))) (do (set! calculate_variance_diff (- (nth (nth calculate_variance_items calculate_variance_i) calculate_variance_j) (nth calculate_variance_means calculate_variance_i))) (set! calculate_variance_squared_diff (conj calculate_variance_squared_diff (* calculate_variance_diff calculate_variance_diff))) (set! calculate_variance_j (+ calculate_variance_j 1)))) (set! calculate_variance_i (+ calculate_variance_i 1)))) (set! calculate_variance_sum_sq 0.0) (set! calculate_variance_k 0) (while (< calculate_variance_k (count calculate_variance_squared_diff)) (do (set! calculate_variance_sum_sq (+ calculate_variance_sum_sq (nth calculate_variance_squared_diff calculate_variance_k))) (set! calculate_variance_k (+ calculate_variance_k 1)))) (set! calculate_variance_n_classes (count calculate_variance_means)) (throw (ex-info "return" {:v (* (/ 1.0 (double (- calculate_variance_total_count calculate_variance_n_classes))) calculate_variance_sum_sq)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn predict_y_values [predict_y_values_x_items predict_y_values_means predict_y_values_variance predict_y_values_probabilities]
  (binding [predict_y_values_discr nil predict_y_values_i nil predict_y_values_j nil predict_y_values_k nil predict_y_values_max_idx nil predict_y_values_max_val nil predict_y_values_results nil predict_y_values_t nil predict_y_values_temp nil] (try (do (set! predict_y_values_results []) (set! predict_y_values_i 0) (while (< predict_y_values_i (count predict_y_values_x_items)) (do (set! predict_y_values_j 0) (while (< predict_y_values_j (count (nth predict_y_values_x_items predict_y_values_i))) (do (set! predict_y_values_temp []) (set! predict_y_values_k 0) (while (< predict_y_values_k (count predict_y_values_x_items)) (do (set! predict_y_values_discr (+ (- (* (nth (nth predict_y_values_x_items predict_y_values_i) predict_y_values_j) (quot (nth predict_y_values_means predict_y_values_k) predict_y_values_variance)) (quot (* (nth predict_y_values_means predict_y_values_k) (nth predict_y_values_means predict_y_values_k)) (* 2.0 predict_y_values_variance))) (ln (nth predict_y_values_probabilities predict_y_values_k)))) (set! predict_y_values_temp (conj predict_y_values_temp predict_y_values_discr)) (set! predict_y_values_k (+ predict_y_values_k 1)))) (set! predict_y_values_max_idx 0) (set! predict_y_values_max_val (nth predict_y_values_temp 0)) (set! predict_y_values_t 1) (while (< predict_y_values_t (count predict_y_values_temp)) (do (when (> (nth predict_y_values_temp predict_y_values_t) predict_y_values_max_val) (do (set! predict_y_values_max_val (nth predict_y_values_temp predict_y_values_t)) (set! predict_y_values_max_idx predict_y_values_t))) (set! predict_y_values_t (+ predict_y_values_t 1)))) (set! predict_y_values_results (conj predict_y_values_results predict_y_values_max_idx)) (set! predict_y_values_j (+ predict_y_values_j 1)))) (set! predict_y_values_i (+ predict_y_values_i 1)))) (throw (ex-info "return" {:v predict_y_values_results}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn accuracy [accuracy_actual_y accuracy_predicted_y]
  (binding [accuracy_correct nil accuracy_i nil] (try (do (set! accuracy_correct 0) (set! accuracy_i 0) (while (< accuracy_i (count accuracy_actual_y)) (do (when (= (nth accuracy_actual_y accuracy_i) (nth accuracy_predicted_y accuracy_i)) (set! accuracy_correct (+ accuracy_correct 1))) (set! accuracy_i (+ accuracy_i 1)))) (throw (ex-info "return" {:v (* (quot (double accuracy_correct) (double (count accuracy_actual_y))) 100.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_actual_means nil main_counts nil main_i nil main_means nil main_predicted nil main_probabilities nil main_std_dev nil main_total_count nil main_variance nil main_x nil main_y nil] (do (alter-var-root (var main_seed) (fn [_] 1)) (set! main_counts [20 20 20]) (set! main_means [5.0 10.0 15.0]) (set! main_std_dev 1.0) (set! main_x []) (set! main_i 0) (while (< main_i (count main_counts)) (do (set! main_x (conj main_x (gaussian_distribution (nth main_means main_i) main_std_dev (nth main_counts main_i)))) (set! main_i (+ main_i 1)))) (set! main_y (y_generator (count main_counts) main_counts)) (set! main_actual_means []) (set! main_i 0) (while (< main_i (count main_counts)) (do (set! main_actual_means (conj main_actual_means (calculate_mean (nth main_counts main_i) (nth main_x main_i)))) (set! main_i (+ main_i 1)))) (set! main_total_count 0) (set! main_i 0) (while (< main_i (count main_counts)) (do (set! main_total_count (+ main_total_count (nth main_counts main_i))) (set! main_i (+ main_i 1)))) (set! main_probabilities []) (set! main_i 0) (while (< main_i (count main_counts)) (do (set! main_probabilities (conj main_probabilities (calculate_probabilities (nth main_counts main_i) main_total_count))) (set! main_i (+ main_i 1)))) (set! main_variance (calculate_variance main_x main_actual_means main_total_count)) (set! main_predicted (predict_y_values main_x main_actual_means main_variance main_probabilities)) (println main_predicted) (println (accuracy main_y main_predicted)))))

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
