(ns main (:refer-clojure :exclude [absf maxf minf clip to_float powf ln exp mean binary_cross_entropy binary_focal_cross_entropy categorical_cross_entropy categorical_focal_cross_entropy hinge_loss huber_loss mean_squared_error mean_absolute_error mean_squared_logarithmic_error mean_absolute_percentage_error perplexity_loss smooth_l1_loss kullback_leibler_divergence main]))

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

(declare absf maxf minf clip to_float powf ln exp mean binary_cross_entropy binary_focal_cross_entropy categorical_cross_entropy categorical_focal_cross_entropy hinge_loss huber_loss mean_squared_error mean_absolute_error mean_squared_logarithmic_error mean_absolute_percentage_error perplexity_loss smooth_l1_loss kullback_leibler_divergence main)

(def ^:dynamic binary_cross_entropy_i nil)

(def ^:dynamic binary_cross_entropy_loss nil)

(def ^:dynamic binary_cross_entropy_losses nil)

(def ^:dynamic binary_cross_entropy_yp nil)

(def ^:dynamic binary_cross_entropy_yt nil)

(def ^:dynamic binary_focal_cross_entropy_i nil)

(def ^:dynamic binary_focal_cross_entropy_losses nil)

(def ^:dynamic binary_focal_cross_entropy_term1 nil)

(def ^:dynamic binary_focal_cross_entropy_term2 nil)

(def ^:dynamic binary_focal_cross_entropy_yp nil)

(def ^:dynamic binary_focal_cross_entropy_yt nil)

(def ^:dynamic categorical_cross_entropy_i nil)

(def ^:dynamic categorical_cross_entropy_j nil)

(def ^:dynamic categorical_cross_entropy_rows nil)

(def ^:dynamic categorical_cross_entropy_sum_pred nil)

(def ^:dynamic categorical_cross_entropy_sum_true nil)

(def ^:dynamic categorical_cross_entropy_total nil)

(def ^:dynamic categorical_cross_entropy_yp nil)

(def ^:dynamic categorical_cross_entropy_yt nil)

(def ^:dynamic categorical_focal_cross_entropy_a nil)

(def ^:dynamic categorical_focal_cross_entropy_cols nil)

(def ^:dynamic categorical_focal_cross_entropy_i nil)

(def ^:dynamic categorical_focal_cross_entropy_j nil)

(def ^:dynamic categorical_focal_cross_entropy_row_loss nil)

(def ^:dynamic categorical_focal_cross_entropy_rows nil)

(def ^:dynamic categorical_focal_cross_entropy_sum_pred nil)

(def ^:dynamic categorical_focal_cross_entropy_sum_true nil)

(def ^:dynamic categorical_focal_cross_entropy_tmp nil)

(def ^:dynamic categorical_focal_cross_entropy_total nil)

(def ^:dynamic categorical_focal_cross_entropy_yp nil)

(def ^:dynamic categorical_focal_cross_entropy_yt nil)

(def ^:dynamic exp_n nil)

(def ^:dynamic exp_sum nil)

(def ^:dynamic exp_term nil)

(def ^:dynamic hinge_loss_i nil)

(def ^:dynamic hinge_loss_l nil)

(def ^:dynamic hinge_loss_losses nil)

(def ^:dynamic hinge_loss_pred nil)

(def ^:dynamic hinge_loss_yt nil)

(def ^:dynamic huber_loss_adiff nil)

(def ^:dynamic huber_loss_diff nil)

(def ^:dynamic huber_loss_i nil)

(def ^:dynamic huber_loss_total nil)

(def ^:dynamic kullback_leibler_divergence_i nil)

(def ^:dynamic kullback_leibler_divergence_total nil)

(def ^:dynamic ln_denom nil)

(def ^:dynamic ln_k nil)

(def ^:dynamic ln_sum nil)

(def ^:dynamic ln_term nil)

(def ^:dynamic ln_y nil)

(def ^:dynamic ln_y2 nil)

(def ^:dynamic main_alpha nil)

(def ^:dynamic main_y_pred_bc nil)

(def ^:dynamic main_y_pred_cce nil)

(def ^:dynamic main_y_pred_hinge nil)

(def ^:dynamic main_y_pred_huber nil)

(def ^:dynamic main_y_pred_kl nil)

(def ^:dynamic main_y_pred_mape nil)

(def ^:dynamic main_y_pred_perp nil)

(def ^:dynamic main_y_pred_smooth nil)

(def ^:dynamic main_y_true_bc nil)

(def ^:dynamic main_y_true_cce nil)

(def ^:dynamic main_y_true_hinge nil)

(def ^:dynamic main_y_true_huber nil)

(def ^:dynamic main_y_true_kl nil)

(def ^:dynamic main_y_true_mape nil)

(def ^:dynamic main_y_true_perp nil)

(def ^:dynamic main_y_true_smooth nil)

(def ^:dynamic mean_absolute_error_i nil)

(def ^:dynamic mean_absolute_error_total nil)

(def ^:dynamic mean_absolute_percentage_error_i nil)

(def ^:dynamic mean_absolute_percentage_error_total nil)

(def ^:dynamic mean_absolute_percentage_error_yt nil)

(def ^:dynamic mean_i nil)

(def ^:dynamic mean_squared_error_diff nil)

(def ^:dynamic mean_squared_error_i nil)

(def ^:dynamic mean_squared_error_losses nil)

(def ^:dynamic mean_squared_logarithmic_error_a nil)

(def ^:dynamic mean_squared_logarithmic_error_b nil)

(def ^:dynamic mean_squared_logarithmic_error_diff nil)

(def ^:dynamic mean_squared_logarithmic_error_i nil)

(def ^:dynamic mean_squared_logarithmic_error_total nil)

(def ^:dynamic mean_total nil)

(def ^:dynamic perplexity_loss_b nil)

(def ^:dynamic perplexity_loss_batch nil)

(def ^:dynamic perplexity_loss_j nil)

(def ^:dynamic perplexity_loss_label nil)

(def ^:dynamic perplexity_loss_mean_log nil)

(def ^:dynamic perplexity_loss_perp nil)

(def ^:dynamic perplexity_loss_prob nil)

(def ^:dynamic perplexity_loss_sentence_len nil)

(def ^:dynamic perplexity_loss_sum_log nil)

(def ^:dynamic perplexity_loss_total_perp nil)

(def ^:dynamic perplexity_loss_vocab_size nil)

(def ^:dynamic powf_i nil)

(def ^:dynamic powf_n nil)

(def ^:dynamic powf_result nil)

(def ^:dynamic smooth_l1_loss_diff nil)

(def ^:dynamic smooth_l1_loss_i nil)

(def ^:dynamic smooth_l1_loss_total nil)

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (- absf_x) absf_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn maxf [maxf_a maxf_b]
  (try (if (> maxf_a maxf_b) maxf_a maxf_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn minf [minf_a minf_b]
  (try (if (< minf_a minf_b) minf_a minf_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn clip [clip_x clip_lo clip_hi]
  (try (throw (ex-info "return" {:v (maxf clip_lo (minf clip_x clip_hi))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_float [to_float_x]
  (try (throw (ex-info "return" {:v (* to_float_x 1.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn powf [powf_base exp_v]
  (binding [powf_i nil powf_n nil powf_result nil] (try (do (set! powf_result 1.0) (set! powf_i 0) (set! powf_n (int exp_v)) (while (< powf_i powf_n) (do (set! powf_result (* powf_result powf_base)) (set! powf_i (+ powf_i 1)))) (throw (ex-info "return" {:v powf_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ln [ln_x]
  (binding [ln_denom nil ln_k nil ln_sum nil ln_term nil ln_y nil ln_y2 nil] (try (do (when (<= ln_x 0.0) (throw (Exception. "ln domain error"))) (set! ln_y (quot (- ln_x 1.0) (+ ln_x 1.0))) (set! ln_y2 (* ln_y ln_y)) (set! ln_term ln_y) (set! ln_sum 0.0) (set! ln_k 0) (while (< ln_k 10) (do (set! ln_denom (to_float (+ (* 2 ln_k) 1))) (set! ln_sum (+ ln_sum (quot ln_term ln_denom))) (set! ln_term (* ln_term ln_y2)) (set! ln_k (+ ln_k 1)))) (throw (ex-info "return" {:v (* 2.0 ln_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn exp [exp_x]
  (binding [exp_n nil exp_sum nil exp_term nil] (try (do (set! exp_term 1.0) (set! exp_sum 1.0) (set! exp_n 1) (while (< exp_n 20) (do (set! exp_term (quot (* exp_term exp_x) (to_float exp_n))) (set! exp_sum (+ exp_sum exp_term)) (set! exp_n (+ exp_n 1)))) (throw (ex-info "return" {:v exp_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mean [mean_v]
  (binding [mean_i nil mean_total nil] (try (do (set! mean_total 0.0) (set! mean_i 0) (while (< mean_i (count mean_v)) (do (set! mean_total (+ mean_total (nth mean_v mean_i))) (set! mean_i (+ mean_i 1)))) (throw (ex-info "return" {:v (quot mean_total (to_float (count mean_v)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_cross_entropy [binary_cross_entropy_y_true binary_cross_entropy_y_pred binary_cross_entropy_epsilon]
  (binding [binary_cross_entropy_i nil binary_cross_entropy_loss nil binary_cross_entropy_losses nil binary_cross_entropy_yp nil binary_cross_entropy_yt nil] (try (do (when (not= (count binary_cross_entropy_y_true) (count binary_cross_entropy_y_pred)) (throw (Exception. "Input arrays must have the same length."))) (set! binary_cross_entropy_losses []) (set! binary_cross_entropy_i 0) (while (< binary_cross_entropy_i (count binary_cross_entropy_y_true)) (do (set! binary_cross_entropy_yt (nth binary_cross_entropy_y_true binary_cross_entropy_i)) (set! binary_cross_entropy_yp (clip (nth binary_cross_entropy_y_pred binary_cross_entropy_i) binary_cross_entropy_epsilon (- 1.0 binary_cross_entropy_epsilon))) (set! binary_cross_entropy_loss (- (+ (* binary_cross_entropy_yt (ln binary_cross_entropy_yp)) (* (- 1.0 binary_cross_entropy_yt) (ln (- 1.0 binary_cross_entropy_yp)))))) (set! binary_cross_entropy_losses (conj binary_cross_entropy_losses binary_cross_entropy_loss)) (set! binary_cross_entropy_i (+ binary_cross_entropy_i 1)))) (throw (ex-info "return" {:v (mean binary_cross_entropy_losses)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_focal_cross_entropy [binary_focal_cross_entropy_y_true binary_focal_cross_entropy_y_pred binary_focal_cross_entropy_gamma binary_focal_cross_entropy_alpha binary_focal_cross_entropy_epsilon]
  (binding [binary_focal_cross_entropy_i nil binary_focal_cross_entropy_losses nil binary_focal_cross_entropy_term1 nil binary_focal_cross_entropy_term2 nil binary_focal_cross_entropy_yp nil binary_focal_cross_entropy_yt nil] (try (do (when (not= (count binary_focal_cross_entropy_y_true) (count binary_focal_cross_entropy_y_pred)) (throw (Exception. "Input arrays must have the same length."))) (set! binary_focal_cross_entropy_losses []) (set! binary_focal_cross_entropy_i 0) (while (< binary_focal_cross_entropy_i (count binary_focal_cross_entropy_y_true)) (do (set! binary_focal_cross_entropy_yt (nth binary_focal_cross_entropy_y_true binary_focal_cross_entropy_i)) (set! binary_focal_cross_entropy_yp (clip (nth binary_focal_cross_entropy_y_pred binary_focal_cross_entropy_i) binary_focal_cross_entropy_epsilon (- 1.0 binary_focal_cross_entropy_epsilon))) (set! binary_focal_cross_entropy_term1 (* (* (* binary_focal_cross_entropy_alpha (powf (- 1.0 binary_focal_cross_entropy_yp) binary_focal_cross_entropy_gamma)) binary_focal_cross_entropy_yt) (ln binary_focal_cross_entropy_yp))) (set! binary_focal_cross_entropy_term2 (* (* (* (- 1.0 binary_focal_cross_entropy_alpha) (powf binary_focal_cross_entropy_yp binary_focal_cross_entropy_gamma)) (- 1.0 binary_focal_cross_entropy_yt)) (ln (- 1.0 binary_focal_cross_entropy_yp)))) (set! binary_focal_cross_entropy_losses (conj binary_focal_cross_entropy_losses (- (+ binary_focal_cross_entropy_term1 binary_focal_cross_entropy_term2)))) (set! binary_focal_cross_entropy_i (+ binary_focal_cross_entropy_i 1)))) (throw (ex-info "return" {:v (mean binary_focal_cross_entropy_losses)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn categorical_cross_entropy [categorical_cross_entropy_y_true categorical_cross_entropy_y_pred categorical_cross_entropy_epsilon]
  (binding [categorical_cross_entropy_i nil categorical_cross_entropy_j nil categorical_cross_entropy_rows nil categorical_cross_entropy_sum_pred nil categorical_cross_entropy_sum_true nil categorical_cross_entropy_total nil categorical_cross_entropy_yp nil categorical_cross_entropy_yt nil] (try (do (when (not= (count categorical_cross_entropy_y_true) (count categorical_cross_entropy_y_pred)) (throw (Exception. "Input arrays must have the same shape."))) (set! categorical_cross_entropy_rows (count categorical_cross_entropy_y_true)) (set! categorical_cross_entropy_total 0.0) (set! categorical_cross_entropy_i 0) (while (< categorical_cross_entropy_i categorical_cross_entropy_rows) (do (when (not= (count (nth categorical_cross_entropy_y_true categorical_cross_entropy_i)) (count (nth categorical_cross_entropy_y_pred categorical_cross_entropy_i))) (throw (Exception. "Input arrays must have the same shape."))) (set! categorical_cross_entropy_sum_true 0.0) (set! categorical_cross_entropy_sum_pred 0.0) (set! categorical_cross_entropy_j 0) (while (< categorical_cross_entropy_j (count (nth categorical_cross_entropy_y_true categorical_cross_entropy_i))) (do (set! categorical_cross_entropy_yt (nth (nth categorical_cross_entropy_y_true categorical_cross_entropy_i) categorical_cross_entropy_j)) (set! categorical_cross_entropy_yp (nth (nth categorical_cross_entropy_y_pred categorical_cross_entropy_i) categorical_cross_entropy_j)) (when (and (not= categorical_cross_entropy_yt 0.0) (not= categorical_cross_entropy_yt 1.0)) (throw (Exception. "y_true must be one-hot encoded."))) (set! categorical_cross_entropy_sum_true (+ categorical_cross_entropy_sum_true categorical_cross_entropy_yt)) (set! categorical_cross_entropy_sum_pred (+ categorical_cross_entropy_sum_pred categorical_cross_entropy_yp)) (set! categorical_cross_entropy_j (+ categorical_cross_entropy_j 1)))) (when (not= categorical_cross_entropy_sum_true 1.0) (throw (Exception. "y_true must be one-hot encoded."))) (when (> (absf (- categorical_cross_entropy_sum_pred 1.0)) categorical_cross_entropy_epsilon) (throw (Exception. "Predicted probabilities must sum to approximately 1."))) (set! categorical_cross_entropy_j 0) (while (< categorical_cross_entropy_j (count (nth categorical_cross_entropy_y_true categorical_cross_entropy_i))) (do (set! categorical_cross_entropy_yp (clip (nth (nth categorical_cross_entropy_y_pred categorical_cross_entropy_i) categorical_cross_entropy_j) categorical_cross_entropy_epsilon 1.0)) (set! categorical_cross_entropy_total (- categorical_cross_entropy_total (* (nth (nth categorical_cross_entropy_y_true categorical_cross_entropy_i) categorical_cross_entropy_j) (ln categorical_cross_entropy_yp)))) (set! categorical_cross_entropy_j (+ categorical_cross_entropy_j 1)))) (set! categorical_cross_entropy_i (+ categorical_cross_entropy_i 1)))) (throw (ex-info "return" {:v categorical_cross_entropy_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn categorical_focal_cross_entropy [categorical_focal_cross_entropy_y_true categorical_focal_cross_entropy_y_pred categorical_focal_cross_entropy_alpha categorical_focal_cross_entropy_gamma categorical_focal_cross_entropy_epsilon]
  (binding [categorical_focal_cross_entropy_a nil categorical_focal_cross_entropy_cols nil categorical_focal_cross_entropy_i nil categorical_focal_cross_entropy_j nil categorical_focal_cross_entropy_row_loss nil categorical_focal_cross_entropy_rows nil categorical_focal_cross_entropy_sum_pred nil categorical_focal_cross_entropy_sum_true nil categorical_focal_cross_entropy_tmp nil categorical_focal_cross_entropy_total nil categorical_focal_cross_entropy_yp nil categorical_focal_cross_entropy_yt nil] (try (do (when (not= (count categorical_focal_cross_entropy_y_true) (count categorical_focal_cross_entropy_y_pred)) (throw (Exception. "Shape of y_true and y_pred must be the same."))) (set! categorical_focal_cross_entropy_rows (count categorical_focal_cross_entropy_y_true)) (set! categorical_focal_cross_entropy_cols (count (nth categorical_focal_cross_entropy_y_true 0))) (set! categorical_focal_cross_entropy_a categorical_focal_cross_entropy_alpha) (when (= (count categorical_focal_cross_entropy_a) 0) (do (set! categorical_focal_cross_entropy_tmp []) (set! categorical_focal_cross_entropy_j 0) (while (< categorical_focal_cross_entropy_j categorical_focal_cross_entropy_cols) (do (set! categorical_focal_cross_entropy_tmp (conj categorical_focal_cross_entropy_tmp 1.0)) (set! categorical_focal_cross_entropy_j (+ categorical_focal_cross_entropy_j 1)))) (set! categorical_focal_cross_entropy_a categorical_focal_cross_entropy_tmp))) (when (not= (count categorical_focal_cross_entropy_a) categorical_focal_cross_entropy_cols) (throw (Exception. "Length of alpha must match the number of classes."))) (set! categorical_focal_cross_entropy_total 0.0) (set! categorical_focal_cross_entropy_i 0) (while (< categorical_focal_cross_entropy_i categorical_focal_cross_entropy_rows) (do (when (or (not= (count (nth categorical_focal_cross_entropy_y_true categorical_focal_cross_entropy_i)) categorical_focal_cross_entropy_cols) (not= (count (nth categorical_focal_cross_entropy_y_pred categorical_focal_cross_entropy_i)) categorical_focal_cross_entropy_cols)) (throw (Exception. "Shape of y_true and y_pred must be the same."))) (set! categorical_focal_cross_entropy_sum_true 0.0) (set! categorical_focal_cross_entropy_sum_pred 0.0) (set! categorical_focal_cross_entropy_j 0) (while (< categorical_focal_cross_entropy_j categorical_focal_cross_entropy_cols) (do (set! categorical_focal_cross_entropy_yt (nth (nth categorical_focal_cross_entropy_y_true categorical_focal_cross_entropy_i) categorical_focal_cross_entropy_j)) (set! categorical_focal_cross_entropy_yp (nth (nth categorical_focal_cross_entropy_y_pred categorical_focal_cross_entropy_i) categorical_focal_cross_entropy_j)) (when (and (not= categorical_focal_cross_entropy_yt 0.0) (not= categorical_focal_cross_entropy_yt 1.0)) (throw (Exception. "y_true must be one-hot encoded."))) (set! categorical_focal_cross_entropy_sum_true (+ categorical_focal_cross_entropy_sum_true categorical_focal_cross_entropy_yt)) (set! categorical_focal_cross_entropy_sum_pred (+ categorical_focal_cross_entropy_sum_pred categorical_focal_cross_entropy_yp)) (set! categorical_focal_cross_entropy_j (+ categorical_focal_cross_entropy_j 1)))) (when (not= categorical_focal_cross_entropy_sum_true 1.0) (throw (Exception. "y_true must be one-hot encoded."))) (when (> (absf (- categorical_focal_cross_entropy_sum_pred 1.0)) categorical_focal_cross_entropy_epsilon) (throw (Exception. "Predicted probabilities must sum to approximately 1."))) (set! categorical_focal_cross_entropy_row_loss 0.0) (set! categorical_focal_cross_entropy_j 0) (while (< categorical_focal_cross_entropy_j categorical_focal_cross_entropy_cols) (do (set! categorical_focal_cross_entropy_yp (clip (nth (nth categorical_focal_cross_entropy_y_pred categorical_focal_cross_entropy_i) categorical_focal_cross_entropy_j) categorical_focal_cross_entropy_epsilon 1.0)) (set! categorical_focal_cross_entropy_row_loss (+ categorical_focal_cross_entropy_row_loss (* (* (* (nth categorical_focal_cross_entropy_a categorical_focal_cross_entropy_j) (powf (- 1.0 categorical_focal_cross_entropy_yp) categorical_focal_cross_entropy_gamma)) (nth (nth categorical_focal_cross_entropy_y_true categorical_focal_cross_entropy_i) categorical_focal_cross_entropy_j)) (ln categorical_focal_cross_entropy_yp)))) (set! categorical_focal_cross_entropy_j (+ categorical_focal_cross_entropy_j 1)))) (set! categorical_focal_cross_entropy_total (- categorical_focal_cross_entropy_total categorical_focal_cross_entropy_row_loss)) (set! categorical_focal_cross_entropy_i (+ categorical_focal_cross_entropy_i 1)))) (throw (ex-info "return" {:v (quot categorical_focal_cross_entropy_total (to_float categorical_focal_cross_entropy_rows))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hinge_loss [hinge_loss_y_true hinge_loss_y_pred]
  (binding [hinge_loss_i nil hinge_loss_l nil hinge_loss_losses nil hinge_loss_pred nil hinge_loss_yt nil] (try (do (when (not= (count hinge_loss_y_true) (count hinge_loss_y_pred)) (throw (Exception. "Length of predicted and actual array must be same."))) (set! hinge_loss_losses []) (set! hinge_loss_i 0) (while (< hinge_loss_i (count hinge_loss_y_true)) (do (set! hinge_loss_yt (nth hinge_loss_y_true hinge_loss_i)) (when (and (not= hinge_loss_yt (- 1.0)) (not= hinge_loss_yt 1.0)) (throw (Exception. "y_true can have values -1 or 1 only."))) (set! hinge_loss_pred (nth hinge_loss_y_pred hinge_loss_i)) (set! hinge_loss_l (maxf 0.0 (- 1.0 (* hinge_loss_yt hinge_loss_pred)))) (set! hinge_loss_losses (conj hinge_loss_losses hinge_loss_l)) (set! hinge_loss_i (+ hinge_loss_i 1)))) (throw (ex-info "return" {:v (mean hinge_loss_losses)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn huber_loss [huber_loss_y_true huber_loss_y_pred huber_loss_delta]
  (binding [huber_loss_adiff nil huber_loss_diff nil huber_loss_i nil huber_loss_total nil] (try (do (when (not= (count huber_loss_y_true) (count huber_loss_y_pred)) (throw (Exception. "Input arrays must have the same length."))) (set! huber_loss_total 0.0) (set! huber_loss_i 0) (while (< huber_loss_i (count huber_loss_y_true)) (do (set! huber_loss_diff (- (nth huber_loss_y_true huber_loss_i) (nth huber_loss_y_pred huber_loss_i))) (set! huber_loss_adiff (absf huber_loss_diff)) (if (<= huber_loss_adiff huber_loss_delta) (set! huber_loss_total (+ huber_loss_total (* (* 0.5 huber_loss_diff) huber_loss_diff))) (set! huber_loss_total (+ huber_loss_total (* huber_loss_delta (- huber_loss_adiff (* 0.5 huber_loss_delta)))))) (set! huber_loss_i (+ huber_loss_i 1)))) (throw (ex-info "return" {:v (quot huber_loss_total (to_float (count huber_loss_y_true)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mean_squared_error [mean_squared_error_y_true mean_squared_error_y_pred]
  (binding [mean_squared_error_diff nil mean_squared_error_i nil mean_squared_error_losses nil] (try (do (when (not= (count mean_squared_error_y_true) (count mean_squared_error_y_pred)) (throw (Exception. "Input arrays must have the same length."))) (set! mean_squared_error_losses []) (set! mean_squared_error_i 0) (while (< mean_squared_error_i (count mean_squared_error_y_true)) (do (set! mean_squared_error_diff (- (nth mean_squared_error_y_true mean_squared_error_i) (nth mean_squared_error_y_pred mean_squared_error_i))) (set! mean_squared_error_losses (conj mean_squared_error_losses (* mean_squared_error_diff mean_squared_error_diff))) (set! mean_squared_error_i (+ mean_squared_error_i 1)))) (throw (ex-info "return" {:v (mean mean_squared_error_losses)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mean_absolute_error [mean_absolute_error_y_true mean_absolute_error_y_pred]
  (binding [mean_absolute_error_i nil mean_absolute_error_total nil] (try (do (when (not= (count mean_absolute_error_y_true) (count mean_absolute_error_y_pred)) (throw (Exception. "Input arrays must have the same length."))) (set! mean_absolute_error_total 0.0) (set! mean_absolute_error_i 0) (while (< mean_absolute_error_i (count mean_absolute_error_y_true)) (do (set! mean_absolute_error_total (+ mean_absolute_error_total (absf (- (nth mean_absolute_error_y_true mean_absolute_error_i) (nth mean_absolute_error_y_pred mean_absolute_error_i))))) (set! mean_absolute_error_i (+ mean_absolute_error_i 1)))) (throw (ex-info "return" {:v (quot mean_absolute_error_total (to_float (count mean_absolute_error_y_true)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mean_squared_logarithmic_error [mean_squared_logarithmic_error_y_true mean_squared_logarithmic_error_y_pred]
  (binding [mean_squared_logarithmic_error_a nil mean_squared_logarithmic_error_b nil mean_squared_logarithmic_error_diff nil mean_squared_logarithmic_error_i nil mean_squared_logarithmic_error_total nil] (try (do (when (not= (count mean_squared_logarithmic_error_y_true) (count mean_squared_logarithmic_error_y_pred)) (throw (Exception. "Input arrays must have the same length."))) (set! mean_squared_logarithmic_error_total 0.0) (set! mean_squared_logarithmic_error_i 0) (while (< mean_squared_logarithmic_error_i (count mean_squared_logarithmic_error_y_true)) (do (set! mean_squared_logarithmic_error_a (ln (+ 1.0 (nth mean_squared_logarithmic_error_y_true mean_squared_logarithmic_error_i)))) (set! mean_squared_logarithmic_error_b (ln (+ 1.0 (nth mean_squared_logarithmic_error_y_pred mean_squared_logarithmic_error_i)))) (set! mean_squared_logarithmic_error_diff (- mean_squared_logarithmic_error_a mean_squared_logarithmic_error_b)) (set! mean_squared_logarithmic_error_total (+ mean_squared_logarithmic_error_total (* mean_squared_logarithmic_error_diff mean_squared_logarithmic_error_diff))) (set! mean_squared_logarithmic_error_i (+ mean_squared_logarithmic_error_i 1)))) (throw (ex-info "return" {:v (quot mean_squared_logarithmic_error_total (to_float (count mean_squared_logarithmic_error_y_true)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mean_absolute_percentage_error [mean_absolute_percentage_error_y_true mean_absolute_percentage_error_y_pred mean_absolute_percentage_error_epsilon]
  (binding [mean_absolute_percentage_error_i nil mean_absolute_percentage_error_total nil mean_absolute_percentage_error_yt nil] (try (do (when (not= (count mean_absolute_percentage_error_y_true) (count mean_absolute_percentage_error_y_pred)) (throw (Exception. "The length of the two arrays should be the same."))) (set! mean_absolute_percentage_error_total 0.0) (set! mean_absolute_percentage_error_i 0) (while (< mean_absolute_percentage_error_i (count mean_absolute_percentage_error_y_true)) (do (set! mean_absolute_percentage_error_yt (nth mean_absolute_percentage_error_y_true mean_absolute_percentage_error_i)) (when (= mean_absolute_percentage_error_yt 0.0) (set! mean_absolute_percentage_error_yt mean_absolute_percentage_error_epsilon)) (set! mean_absolute_percentage_error_total (+ mean_absolute_percentage_error_total (absf (quot (- mean_absolute_percentage_error_yt (nth mean_absolute_percentage_error_y_pred mean_absolute_percentage_error_i)) mean_absolute_percentage_error_yt)))) (set! mean_absolute_percentage_error_i (+ mean_absolute_percentage_error_i 1)))) (throw (ex-info "return" {:v (quot mean_absolute_percentage_error_total (to_float (count mean_absolute_percentage_error_y_true)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn perplexity_loss [perplexity_loss_y_true perplexity_loss_y_pred perplexity_loss_epsilon]
  (binding [perplexity_loss_b nil perplexity_loss_batch nil perplexity_loss_j nil perplexity_loss_label nil perplexity_loss_mean_log nil perplexity_loss_perp nil perplexity_loss_prob nil perplexity_loss_sentence_len nil perplexity_loss_sum_log nil perplexity_loss_total_perp nil perplexity_loss_vocab_size nil] (try (do (set! perplexity_loss_batch (count perplexity_loss_y_true)) (when (not= perplexity_loss_batch (count perplexity_loss_y_pred)) (throw (Exception. "Batch size of y_true and y_pred must be equal."))) (set! perplexity_loss_sentence_len (count (nth perplexity_loss_y_true 0))) (when (not= perplexity_loss_sentence_len (count (nth perplexity_loss_y_pred 0))) (throw (Exception. "Sentence length of y_true and y_pred must be equal."))) (set! perplexity_loss_vocab_size (count (nth (nth perplexity_loss_y_pred 0) 0))) (set! perplexity_loss_b 0) (set! perplexity_loss_total_perp 0.0) (while (< perplexity_loss_b perplexity_loss_batch) (do (when (or (not= (count (nth perplexity_loss_y_true perplexity_loss_b)) perplexity_loss_sentence_len) (not= (count (nth perplexity_loss_y_pred perplexity_loss_b)) perplexity_loss_sentence_len)) (throw (Exception. "Sentence length of y_true and y_pred must be equal."))) (set! perplexity_loss_sum_log 0.0) (set! perplexity_loss_j 0) (while (< perplexity_loss_j perplexity_loss_sentence_len) (do (set! perplexity_loss_label (nth (nth perplexity_loss_y_true perplexity_loss_b) perplexity_loss_j)) (when (>= perplexity_loss_label perplexity_loss_vocab_size) (throw (Exception. "Label value must not be greater than vocabulary size."))) (set! perplexity_loss_prob (clip (nth (nth (nth perplexity_loss_y_pred perplexity_loss_b) perplexity_loss_j) perplexity_loss_label) perplexity_loss_epsilon 1.0)) (set! perplexity_loss_sum_log (+ perplexity_loss_sum_log (ln perplexity_loss_prob))) (set! perplexity_loss_j (+ perplexity_loss_j 1)))) (set! perplexity_loss_mean_log (quot perplexity_loss_sum_log (to_float perplexity_loss_sentence_len))) (set! perplexity_loss_perp (exp (- perplexity_loss_mean_log))) (set! perplexity_loss_total_perp (+ perplexity_loss_total_perp perplexity_loss_perp)) (set! perplexity_loss_b (+ perplexity_loss_b 1)))) (throw (ex-info "return" {:v (quot perplexity_loss_total_perp (to_float perplexity_loss_batch))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn smooth_l1_loss [smooth_l1_loss_y_true smooth_l1_loss_y_pred smooth_l1_loss_beta]
  (binding [smooth_l1_loss_diff nil smooth_l1_loss_i nil smooth_l1_loss_total nil] (try (do (when (not= (count smooth_l1_loss_y_true) (count smooth_l1_loss_y_pred)) (throw (Exception. "The length of the two arrays should be the same."))) (set! smooth_l1_loss_total 0.0) (set! smooth_l1_loss_i 0) (while (< smooth_l1_loss_i (count smooth_l1_loss_y_true)) (do (set! smooth_l1_loss_diff (absf (- (nth smooth_l1_loss_y_true smooth_l1_loss_i) (nth smooth_l1_loss_y_pred smooth_l1_loss_i)))) (if (< smooth_l1_loss_diff smooth_l1_loss_beta) (set! smooth_l1_loss_total (+ smooth_l1_loss_total (quot (* (* 0.5 smooth_l1_loss_diff) smooth_l1_loss_diff) smooth_l1_loss_beta))) (set! smooth_l1_loss_total (- (+ smooth_l1_loss_total smooth_l1_loss_diff) (* 0.5 smooth_l1_loss_beta)))) (set! smooth_l1_loss_i (+ smooth_l1_loss_i 1)))) (throw (ex-info "return" {:v (quot smooth_l1_loss_total (to_float (count smooth_l1_loss_y_true)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn kullback_leibler_divergence [kullback_leibler_divergence_y_true kullback_leibler_divergence_y_pred]
  (binding [kullback_leibler_divergence_i nil kullback_leibler_divergence_total nil] (try (do (when (not= (count kullback_leibler_divergence_y_true) (count kullback_leibler_divergence_y_pred)) (throw (Exception. "Input arrays must have the same length."))) (set! kullback_leibler_divergence_total 0.0) (set! kullback_leibler_divergence_i 0) (while (< kullback_leibler_divergence_i (count kullback_leibler_divergence_y_true)) (do (set! kullback_leibler_divergence_total (+ kullback_leibler_divergence_total (* (nth kullback_leibler_divergence_y_true kullback_leibler_divergence_i) (ln (quot (nth kullback_leibler_divergence_y_true kullback_leibler_divergence_i) (nth kullback_leibler_divergence_y_pred kullback_leibler_divergence_i)))))) (set! kullback_leibler_divergence_i (+ kullback_leibler_divergence_i 1)))) (throw (ex-info "return" {:v kullback_leibler_divergence_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_alpha nil main_y_pred_bc nil main_y_pred_cce nil main_y_pred_hinge nil main_y_pred_huber nil main_y_pred_kl nil main_y_pred_mape nil main_y_pred_perp nil main_y_pred_smooth nil main_y_true_bc nil main_y_true_cce nil main_y_true_hinge nil main_y_true_huber nil main_y_true_kl nil main_y_true_mape nil main_y_true_perp nil main_y_true_smooth nil] (do (set! main_y_true_bc [0.0 1.0 1.0 0.0 1.0]) (set! main_y_pred_bc [0.2 0.7 0.9 0.3 0.8]) (println (binary_cross_entropy main_y_true_bc main_y_pred_bc 0.000000000000001)) (println (binary_focal_cross_entropy main_y_true_bc main_y_pred_bc 2.0 0.25 0.000000000000001)) (set! main_y_true_cce [[1.0 0.0 0.0] [0.0 1.0 0.0] [0.0 0.0 1.0]]) (set! main_y_pred_cce [[0.9 0.1 0.0] [0.2 0.7 0.1] [0.0 0.1 0.9]]) (println (categorical_cross_entropy main_y_true_cce main_y_pred_cce 0.000000000000001)) (set! main_alpha [0.6 0.2 0.7]) (println (categorical_focal_cross_entropy main_y_true_cce main_y_pred_cce main_alpha 2.0 0.000000000000001)) (set! main_y_true_hinge [(- 1.0) 1.0 1.0 (- 1.0) 1.0]) (set! main_y_pred_hinge [(- 4.0) (- 0.3) 0.7 5.0 10.0]) (println (hinge_loss main_y_true_hinge main_y_pred_hinge)) (set! main_y_true_huber [0.9 10.0 2.0 1.0 5.2]) (set! main_y_pred_huber [0.8 2.1 2.9 4.2 5.2]) (println (huber_loss main_y_true_huber main_y_pred_huber 1.0)) (println (mean_squared_error main_y_true_huber main_y_pred_huber)) (println (mean_absolute_error main_y_true_huber main_y_pred_huber)) (println (mean_squared_logarithmic_error main_y_true_huber main_y_pred_huber)) (set! main_y_true_mape [10.0 20.0 30.0 40.0]) (set! main_y_pred_mape [12.0 18.0 33.0 45.0]) (println (mean_absolute_percentage_error main_y_true_mape main_y_pred_mape 0.000000000000001)) (set! main_y_true_perp [[1 4] [2 3]]) (set! main_y_pred_perp [[[0.28 0.19 0.21 0.15 0.17] [0.24 0.19 0.09 0.18 0.3]] [[0.03 0.26 0.21 0.18 0.32] [0.28 0.1 0.33 0.15 0.14]]]) (println (perplexity_loss main_y_true_perp main_y_pred_perp 0.0000001)) (set! main_y_true_smooth [3.0 5.0 2.0 7.0]) (set! main_y_pred_smooth [2.9 4.8 2.1 7.2]) (println (smooth_l1_loss main_y_true_smooth main_y_pred_smooth 1.0)) (set! main_y_true_kl [0.2 0.3 0.5]) (set! main_y_pred_kl [0.3 0.3 0.4]) (println (kullback_leibler_divergence main_y_true_kl main_y_pred_kl)))))

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
