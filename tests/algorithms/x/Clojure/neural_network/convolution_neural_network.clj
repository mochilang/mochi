(ns main (:refer-clojure :exclude [random sigmoid to_float exp convolve average_pool flatten vec_mul_mat matT_vec_mul vec_add vec_sub vec_mul vec_map_sig new_cnn forward train main]))

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

(declare random sigmoid to_float exp convolve average_pool flatten vec_mul_mat matT_vec_mul vec_add vec_sub vec_mul vec_map_sig new_cnn forward train main)

(declare _read_file)

(def ^:dynamic average_pool_a nil)

(def ^:dynamic average_pool_b nil)

(def ^:dynamic average_pool_i nil)

(def ^:dynamic average_pool_j nil)

(def ^:dynamic average_pool_out nil)

(def ^:dynamic average_pool_row nil)

(def ^:dynamic average_pool_sum nil)

(def ^:dynamic convolve_a nil)

(def ^:dynamic convolve_b nil)

(def ^:dynamic convolve_i nil)

(def ^:dynamic convolve_j nil)

(def ^:dynamic convolve_out nil)

(def ^:dynamic convolve_row nil)

(def ^:dynamic convolve_size_data nil)

(def ^:dynamic convolve_size_kernel nil)

(def ^:dynamic convolve_sum nil)

(def ^:dynamic exp_n nil)

(def ^:dynamic exp_sum nil)

(def ^:dynamic exp_term nil)

(def ^:dynamic flatten_i nil)

(def ^:dynamic flatten_j nil)

(def ^:dynamic flatten_k nil)

(def ^:dynamic flatten_out nil)

(def ^:dynamic forward_conv_map nil)

(def ^:dynamic forward_flat nil)

(def ^:dynamic forward_hidden_net nil)

(def ^:dynamic forward_hidden_out nil)

(def ^:dynamic forward_i nil)

(def ^:dynamic forward_maps nil)

(def ^:dynamic forward_out nil)

(def ^:dynamic forward_out_net nil)

(def ^:dynamic forward_pooled nil)

(def ^:dynamic main_cnn nil)

(def ^:dynamic main_image nil)

(def ^:dynamic main_sample nil)

(def ^:dynamic main_trained nil)

(def ^:dynamic matT_vec_mul_i nil)

(def ^:dynamic matT_vec_mul_j nil)

(def ^:dynamic matT_vec_mul_res nil)

(def ^:dynamic matT_vec_mul_sum nil)

(def ^:dynamic new_cnn_b_hidden nil)

(def ^:dynamic new_cnn_b_out nil)

(def ^:dynamic new_cnn_conv_bias nil)

(def ^:dynamic new_cnn_conv_kernels nil)

(def ^:dynamic new_cnn_conv_step nil)

(def ^:dynamic new_cnn_hidden_size nil)

(def ^:dynamic new_cnn_i nil)

(def ^:dynamic new_cnn_input_size nil)

(def ^:dynamic new_cnn_j nil)

(def ^:dynamic new_cnn_k1 nil)

(def ^:dynamic new_cnn_k2 nil)

(def ^:dynamic new_cnn_output_size nil)

(def ^:dynamic new_cnn_pool_size nil)

(def ^:dynamic new_cnn_row nil)

(def ^:dynamic new_cnn_w_hidden nil)

(def ^:dynamic new_cnn_w_out nil)

(def ^:dynamic train_b_hidden nil)

(def ^:dynamic train_b_out nil)

(def ^:dynamic train_conv_map nil)

(def ^:dynamic train_data nil)

(def ^:dynamic train_e nil)

(def ^:dynamic train_error_hidden nil)

(def ^:dynamic train_error_out nil)

(def ^:dynamic train_flat nil)

(def ^:dynamic train_hidden_net nil)

(def ^:dynamic train_hidden_out nil)

(def ^:dynamic train_i nil)

(def ^:dynamic train_i_h nil)

(def ^:dynamic train_j nil)

(def ^:dynamic train_j_h nil)

(def ^:dynamic train_k nil)

(def ^:dynamic train_maps nil)

(def ^:dynamic train_out nil)

(def ^:dynamic train_out_net nil)

(def ^:dynamic train_pd_hidden nil)

(def ^:dynamic train_pd_out nil)

(def ^:dynamic train_pooled nil)

(def ^:dynamic train_s nil)

(def ^:dynamic train_target nil)

(def ^:dynamic train_w_hidden nil)

(def ^:dynamic train_w_out nil)

(def ^:dynamic vec_add_i nil)

(def ^:dynamic vec_add_res nil)

(def ^:dynamic vec_map_sig_i nil)

(def ^:dynamic vec_map_sig_res nil)

(def ^:dynamic vec_mul_i nil)

(def ^:dynamic vec_mul_mat_cols nil)

(def ^:dynamic vec_mul_mat_i nil)

(def ^:dynamic vec_mul_mat_j nil)

(def ^:dynamic vec_mul_mat_res nil)

(def ^:dynamic vec_mul_mat_sum nil)

(def ^:dynamic vec_mul_res nil)

(def ^:dynamic vec_sub_i nil)

(def ^:dynamic vec_sub_res nil)

(def ^:dynamic main_seed nil)

(defn random []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+' (*' main_seed 13) 7) 100))) (throw (ex-info "return" {:v (/ (double main_seed) 100.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sigmoid [sigmoid_x]
  (try (throw (ex-info "return" {:v (/ 1.0 (+' 1.0 (exp (- sigmoid_x))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_float [to_float_x]
  (try (throw (ex-info "return" {:v (*' to_float_x 1.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn exp [exp_x]
  (binding [exp_n nil exp_sum nil exp_term nil] (try (do (set! exp_term 1.0) (set! exp_sum 1.0) (set! exp_n 1) (while (< exp_n 20) (do (set! exp_term (/ (*' exp_term exp_x) (to_float exp_n))) (set! exp_sum (+' exp_sum exp_term)) (set! exp_n (+' exp_n 1)))) (throw (ex-info "return" {:v exp_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn convolve [convolve_data convolve_kernel convolve_step convolve_bias]
  (binding [convolve_a nil convolve_b nil convolve_i nil convolve_j nil convolve_out nil convolve_row nil convolve_size_data nil convolve_size_kernel nil convolve_sum nil] (try (do (set! convolve_size_data (count convolve_data)) (set! convolve_size_kernel (count convolve_kernel)) (set! convolve_out []) (set! convolve_i 0) (while (<= convolve_i (- convolve_size_data convolve_size_kernel)) (do (set! convolve_row []) (set! convolve_j 0) (while (<= convolve_j (- convolve_size_data convolve_size_kernel)) (do (set! convolve_sum 0.0) (set! convolve_a 0) (while (< convolve_a convolve_size_kernel) (do (set! convolve_b 0) (while (< convolve_b convolve_size_kernel) (do (set! convolve_sum (+' convolve_sum (*' (nth (nth convolve_data (+' convolve_i convolve_a)) (+' convolve_j convolve_b)) (nth (nth convolve_kernel convolve_a) convolve_b)))) (set! convolve_b (+' convolve_b 1)))) (set! convolve_a (+' convolve_a 1)))) (set! convolve_row (conj convolve_row (sigmoid (- convolve_sum convolve_bias)))) (set! convolve_j (+' convolve_j convolve_step)))) (set! convolve_out (conj convolve_out convolve_row)) (set! convolve_i (+' convolve_i convolve_step)))) (throw (ex-info "return" {:v convolve_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn average_pool [average_pool_map average_pool_size]
  (binding [average_pool_a nil average_pool_b nil average_pool_i nil average_pool_j nil average_pool_out nil average_pool_row nil average_pool_sum nil] (try (do (set! average_pool_out []) (set! average_pool_i 0) (while (< average_pool_i (count average_pool_map)) (do (set! average_pool_row []) (set! average_pool_j 0) (while (< average_pool_j (count (nth average_pool_map average_pool_i))) (do (set! average_pool_sum 0.0) (set! average_pool_a 0) (while (< average_pool_a average_pool_size) (do (set! average_pool_b 0) (while (< average_pool_b average_pool_size) (do (set! average_pool_sum (+' average_pool_sum (nth (nth average_pool_map (+' average_pool_i average_pool_a)) (+' average_pool_j average_pool_b)))) (set! average_pool_b (+' average_pool_b 1)))) (set! average_pool_a (+' average_pool_a 1)))) (set! average_pool_row (conj average_pool_row (/ average_pool_sum (double (*' average_pool_size average_pool_size))))) (set! average_pool_j (+' average_pool_j average_pool_size)))) (set! average_pool_out (conj average_pool_out average_pool_row)) (set! average_pool_i (+' average_pool_i average_pool_size)))) (throw (ex-info "return" {:v average_pool_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn flatten [flatten_maps]
  (binding [flatten_i nil flatten_j nil flatten_k nil flatten_out nil] (try (do (set! flatten_out []) (set! flatten_i 0) (while (< flatten_i (count flatten_maps)) (do (set! flatten_j 0) (while (< flatten_j (count (nth flatten_maps flatten_i))) (do (set! flatten_k 0) (while (< flatten_k (count (nth (nth flatten_maps flatten_i) flatten_j))) (do (set! flatten_out (conj flatten_out (nth (nth (nth flatten_maps flatten_i) flatten_j) flatten_k))) (set! flatten_k (+' flatten_k 1)))) (set! flatten_j (+' flatten_j 1)))) (set! flatten_i (+' flatten_i 1)))) (throw (ex-info "return" {:v flatten_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vec_mul_mat [vec_mul_mat_v vec_mul_mat_m]
  (binding [vec_mul_mat_cols nil vec_mul_mat_i nil vec_mul_mat_j nil vec_mul_mat_res nil vec_mul_mat_sum nil] (try (do (set! vec_mul_mat_cols (count (nth vec_mul_mat_m 0))) (set! vec_mul_mat_res []) (set! vec_mul_mat_j 0) (while (< vec_mul_mat_j vec_mul_mat_cols) (do (set! vec_mul_mat_sum 0.0) (set! vec_mul_mat_i 0) (while (< vec_mul_mat_i (count vec_mul_mat_v)) (do (set! vec_mul_mat_sum (+' vec_mul_mat_sum (*' (nth vec_mul_mat_v vec_mul_mat_i) (nth (nth vec_mul_mat_m vec_mul_mat_i) vec_mul_mat_j)))) (set! vec_mul_mat_i (+' vec_mul_mat_i 1)))) (set! vec_mul_mat_res (conj vec_mul_mat_res vec_mul_mat_sum)) (set! vec_mul_mat_j (+' vec_mul_mat_j 1)))) (throw (ex-info "return" {:v vec_mul_mat_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matT_vec_mul [matT_vec_mul_m matT_vec_mul_v]
  (binding [matT_vec_mul_i nil matT_vec_mul_j nil matT_vec_mul_res nil matT_vec_mul_sum nil] (try (do (set! matT_vec_mul_res []) (set! matT_vec_mul_i 0) (while (< matT_vec_mul_i (count matT_vec_mul_m)) (do (set! matT_vec_mul_sum 0.0) (set! matT_vec_mul_j 0) (while (< matT_vec_mul_j (count (nth matT_vec_mul_m matT_vec_mul_i))) (do (set! matT_vec_mul_sum (+' matT_vec_mul_sum (*' (nth (nth matT_vec_mul_m matT_vec_mul_i) matT_vec_mul_j) (nth matT_vec_mul_v matT_vec_mul_j)))) (set! matT_vec_mul_j (+' matT_vec_mul_j 1)))) (set! matT_vec_mul_res (conj matT_vec_mul_res matT_vec_mul_sum)) (set! matT_vec_mul_i (+' matT_vec_mul_i 1)))) (throw (ex-info "return" {:v matT_vec_mul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vec_add [vec_add_a vec_add_b]
  (binding [vec_add_i nil vec_add_res nil] (try (do (set! vec_add_res []) (set! vec_add_i 0) (while (< vec_add_i (count vec_add_a)) (do (set! vec_add_res (conj vec_add_res (+' (nth vec_add_a vec_add_i) (nth vec_add_b vec_add_i)))) (set! vec_add_i (+' vec_add_i 1)))) (throw (ex-info "return" {:v vec_add_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vec_sub [vec_sub_a vec_sub_b]
  (binding [vec_sub_i nil vec_sub_res nil] (try (do (set! vec_sub_res []) (set! vec_sub_i 0) (while (< vec_sub_i (count vec_sub_a)) (do (set! vec_sub_res (conj vec_sub_res (- (nth vec_sub_a vec_sub_i) (nth vec_sub_b vec_sub_i)))) (set! vec_sub_i (+' vec_sub_i 1)))) (throw (ex-info "return" {:v vec_sub_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vec_mul [vec_mul_a vec_mul_b]
  (binding [vec_mul_i nil vec_mul_res nil] (try (do (set! vec_mul_res []) (set! vec_mul_i 0) (while (< vec_mul_i (count vec_mul_a)) (do (set! vec_mul_res (conj vec_mul_res (*' (nth vec_mul_a vec_mul_i) (nth vec_mul_b vec_mul_i)))) (set! vec_mul_i (+' vec_mul_i 1)))) (throw (ex-info "return" {:v vec_mul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vec_map_sig [vec_map_sig_v]
  (binding [vec_map_sig_i nil vec_map_sig_res nil] (try (do (set! vec_map_sig_res []) (set! vec_map_sig_i 0) (while (< vec_map_sig_i (count vec_map_sig_v)) (do (set! vec_map_sig_res (conj vec_map_sig_res (sigmoid (nth vec_map_sig_v vec_map_sig_i)))) (set! vec_map_sig_i (+' vec_map_sig_i 1)))) (throw (ex-info "return" {:v vec_map_sig_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn new_cnn []
  (binding [new_cnn_b_hidden nil new_cnn_b_out nil new_cnn_conv_bias nil new_cnn_conv_kernels nil new_cnn_conv_step nil new_cnn_hidden_size nil new_cnn_i nil new_cnn_input_size nil new_cnn_j nil new_cnn_k1 nil new_cnn_k2 nil new_cnn_output_size nil new_cnn_pool_size nil new_cnn_row nil new_cnn_w_hidden nil new_cnn_w_out nil] (try (do (set! new_cnn_k1 [[1.0 0.0] [0.0 1.0]]) (set! new_cnn_k2 [[0.0 1.0] [1.0 0.0]]) (set! new_cnn_conv_kernels [new_cnn_k1 new_cnn_k2]) (set! new_cnn_conv_bias [0.0 0.0]) (set! new_cnn_conv_step 2) (set! new_cnn_pool_size 2) (set! new_cnn_input_size 2) (set! new_cnn_hidden_size 2) (set! new_cnn_output_size 2) (set! new_cnn_w_hidden []) (set! new_cnn_i 0) (while (< new_cnn_i new_cnn_input_size) (do (set! new_cnn_row []) (set! new_cnn_j 0) (while (< new_cnn_j new_cnn_hidden_size) (do (set! new_cnn_row (conj new_cnn_row (- (random) 0.5))) (set! new_cnn_j (+' new_cnn_j 1)))) (set! new_cnn_w_hidden (conj new_cnn_w_hidden new_cnn_row)) (set! new_cnn_i (+' new_cnn_i 1)))) (set! new_cnn_w_out []) (set! new_cnn_i 0) (while (< new_cnn_i new_cnn_hidden_size) (do (set! new_cnn_row []) (set! new_cnn_j 0) (while (< new_cnn_j new_cnn_output_size) (do (set! new_cnn_row (conj new_cnn_row (- (random) 0.5))) (set! new_cnn_j (+' new_cnn_j 1)))) (set! new_cnn_w_out (conj new_cnn_w_out new_cnn_row)) (set! new_cnn_i (+' new_cnn_i 1)))) (set! new_cnn_b_hidden [0.0 0.0]) (set! new_cnn_b_out [0.0 0.0]) (throw (ex-info "return" {:v {:b_hidden new_cnn_b_hidden :b_out new_cnn_b_out :conv_bias new_cnn_conv_bias :conv_kernels new_cnn_conv_kernels :conv_step new_cnn_conv_step :pool_size new_cnn_pool_size :rate_bias 0.2 :rate_weight 0.2 :w_hidden new_cnn_w_hidden :w_out new_cnn_w_out}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn forward [forward_cnn forward_data]
  (binding [forward_conv_map nil forward_flat nil forward_hidden_net nil forward_hidden_out nil forward_i nil forward_maps nil forward_out nil forward_out_net nil forward_pooled nil] (try (do (set! forward_maps []) (set! forward_i 0) (while (< forward_i (count (:conv_kernels forward_cnn))) (do (set! forward_conv_map (convolve forward_data (get (:conv_kernels forward_cnn) forward_i) (:conv_step forward_cnn) (get (:conv_bias forward_cnn) forward_i))) (set! forward_pooled (average_pool forward_conv_map (:pool_size forward_cnn))) (set! forward_maps (conj forward_maps forward_pooled)) (set! forward_i (+' forward_i 1)))) (set! forward_flat (flatten forward_maps)) (set! forward_hidden_net (vec_add (vec_mul_mat forward_flat (:w_hidden forward_cnn)) (:b_hidden forward_cnn))) (set! forward_hidden_out (vec_map_sig forward_hidden_net)) (set! forward_out_net (vec_add (vec_mul_mat forward_hidden_out (:w_out forward_cnn)) (:b_out forward_cnn))) (set! forward_out (vec_map_sig forward_out_net)) (throw (ex-info "return" {:v forward_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn train [train_cnn train_samples train_epochs]
  (binding [train_b_hidden nil train_b_out nil train_conv_map nil train_data nil train_e nil train_error_hidden nil train_error_out nil train_flat nil train_hidden_net nil train_hidden_out nil train_i nil train_i_h nil train_j nil train_j_h nil train_k nil train_maps nil train_out nil train_out_net nil train_pd_hidden nil train_pd_out nil train_pooled nil train_s nil train_target nil train_w_hidden nil train_w_out nil] (try (do (set! train_w_out (:w_out train_cnn)) (set! train_b_out (:b_out train_cnn)) (set! train_w_hidden (:w_hidden train_cnn)) (set! train_b_hidden (:b_hidden train_cnn)) (set! train_e 0) (while (< train_e train_epochs) (do (set! train_s 0) (while (< train_s (count train_samples)) (do (set! train_data (:image (nth train_samples train_s))) (set! train_target (:target (nth train_samples train_s))) (set! train_maps []) (set! train_i 0) (while (< train_i (count (:conv_kernels train_cnn))) (do (set! train_conv_map (convolve train_data (get (:conv_kernels train_cnn) train_i) (:conv_step train_cnn) (get (:conv_bias train_cnn) train_i))) (set! train_pooled (average_pool train_conv_map (:pool_size train_cnn))) (set! train_maps (conj train_maps train_pooled)) (set! train_i (+' train_i 1)))) (set! train_flat (flatten train_maps)) (set! train_hidden_net (vec_add (vec_mul_mat train_flat train_w_hidden) train_b_hidden)) (set! train_hidden_out (vec_map_sig train_hidden_net)) (set! train_out_net (vec_add (vec_mul_mat train_hidden_out train_w_out) train_b_out)) (set! train_out (vec_map_sig train_out_net)) (set! train_error_out (vec_sub train_target train_out)) (set! train_pd_out (vec_mul train_error_out (vec_mul train_out (vec_sub [1.0 1.0] train_out)))) (set! train_error_hidden (matT_vec_mul train_w_out train_pd_out)) (set! train_pd_hidden (vec_mul train_error_hidden (vec_mul train_hidden_out (vec_sub [1.0 1.0] train_hidden_out)))) (set! train_j 0) (while (< train_j (count train_w_out)) (do (set! train_k 0) (while (< train_k (count (get train_w_out train_j))) (do (set! train_w_out (assoc-in train_w_out [train_j train_k] (+' (get (get train_w_out train_j) train_k) (*' (*' (:rate_weight train_cnn) (nth train_hidden_out train_j)) (nth train_pd_out train_k))))) (set! train_k (+' train_k 1)))) (set! train_j (+' train_j 1)))) (set! train_j 0) (while (< train_j (count train_b_out)) (do (set! train_b_out (assoc train_b_out train_j (- (get train_b_out train_j) (*' (:rate_bias train_cnn) (nth train_pd_out train_j))))) (set! train_j (+' train_j 1)))) (set! train_i_h 0) (while (< train_i_h (count train_w_hidden)) (do (set! train_j_h 0) (while (< train_j_h (count (get train_w_hidden train_i_h))) (do (set! train_w_hidden (assoc-in train_w_hidden [train_i_h train_j_h] (+' (get (get train_w_hidden train_i_h) train_j_h) (*' (*' (:rate_weight train_cnn) (nth train_flat train_i_h)) (nth train_pd_hidden train_j_h))))) (set! train_j_h (+' train_j_h 1)))) (set! train_i_h (+' train_i_h 1)))) (set! train_j 0) (while (< train_j (count train_b_hidden)) (do (set! train_b_hidden (assoc train_b_hidden train_j (- (get train_b_hidden train_j) (*' (:rate_bias train_cnn) (nth train_pd_hidden train_j))))) (set! train_j (+' train_j 1)))) (set! train_s (+' train_s 1)))) (set! train_e (+' train_e 1)))) (throw (ex-info "return" {:v {:b_hidden train_b_hidden :b_out train_b_out :conv_bias (:conv_bias train_cnn) :conv_kernels (:conv_kernels train_cnn) :conv_step (:conv_step train_cnn) :pool_size (:pool_size train_cnn) :rate_bias (:rate_bias train_cnn) :rate_weight (:rate_weight train_cnn) :w_hidden train_w_hidden :w_out train_w_out}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_cnn nil main_image nil main_sample nil main_trained nil] (do (set! main_cnn (new_cnn)) (set! main_image [[1.0 0.0 1.0 0.0] [0.0 1.0 0.0 1.0] [1.0 0.0 1.0 0.0] [0.0 1.0 0.0 1.0]]) (set! main_sample {:image main_image :target [1.0 0.0]}) (println "Before training:" (forward main_cnn main_image)) (set! main_trained (train main_cnn [main_sample] 50)) (println "After training:" (forward main_trained main_image)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_seed) (constantly 1))
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
