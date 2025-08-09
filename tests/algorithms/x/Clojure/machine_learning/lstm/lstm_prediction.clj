(ns main (:refer-clojure :exclude [exp_approx sigmoid tanh_approx forward backward make_samples init_weights train predict]))

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

(declare exp_approx sigmoid tanh_approx forward backward make_samples init_weights train predict)

(def ^:dynamic backward_T nil)

(def ^:dynamic backward_c_prev nil)

(def ^:dynamic backward_c_t nil)

(def ^:dynamic backward_da_f nil)

(def ^:dynamic backward_da_g nil)

(def ^:dynamic backward_da_i nil)

(def ^:dynamic backward_da_o nil)

(def ^:dynamic backward_db_c nil)

(def ^:dynamic backward_db_f nil)

(def ^:dynamic backward_db_i nil)

(def ^:dynamic backward_db_o nil)

(def ^:dynamic backward_db_y nil)

(def ^:dynamic backward_dc nil)

(def ^:dynamic backward_dc_next nil)

(def ^:dynamic backward_df_t nil)

(def ^:dynamic backward_dg_t nil)

(def ^:dynamic backward_dh_next nil)

(def ^:dynamic backward_di_t nil)

(def ^:dynamic backward_do_t nil)

(def ^:dynamic backward_du_c nil)

(def ^:dynamic backward_du_f nil)

(def ^:dynamic backward_du_i nil)

(def ^:dynamic backward_du_o nil)

(def ^:dynamic backward_dw_c nil)

(def ^:dynamic backward_dw_f nil)

(def ^:dynamic backward_dw_i nil)

(def ^:dynamic backward_dw_o nil)

(def ^:dynamic backward_dw_y nil)

(def ^:dynamic backward_dy nil)

(def ^:dynamic backward_f_t nil)

(def ^:dynamic backward_g_t nil)

(def ^:dynamic backward_h_last nil)

(def ^:dynamic backward_h_prev nil)

(def ^:dynamic backward_i_t nil)

(def ^:dynamic backward_o_t nil)

(def ^:dynamic backward_t nil)

(def ^:dynamic backward_tanh_c nil)

(def ^:dynamic backward_w nil)

(def ^:dynamic backward_y nil)

(def ^:dynamic exp_approx_n nil)

(def ^:dynamic exp_approx_sum nil)

(def ^:dynamic exp_approx_term nil)

(def ^:dynamic forward_c_arr nil)

(def ^:dynamic forward_c_prev nil)

(def ^:dynamic forward_c_t nil)

(def ^:dynamic forward_f_arr nil)

(def ^:dynamic forward_f_t nil)

(def ^:dynamic forward_g_arr nil)

(def ^:dynamic forward_g_t nil)

(def ^:dynamic forward_h_arr nil)

(def ^:dynamic forward_h_prev nil)

(def ^:dynamic forward_h_t nil)

(def ^:dynamic forward_i_arr nil)

(def ^:dynamic forward_i_t nil)

(def ^:dynamic forward_o_arr nil)

(def ^:dynamic forward_o_t nil)

(def ^:dynamic forward_t nil)

(def ^:dynamic forward_x nil)

(def ^:dynamic make_samples_X nil)

(def ^:dynamic make_samples_Y nil)

(def ^:dynamic make_samples_i nil)

(def ^:dynamic make_samples_seq nil)

(def ^:dynamic predict_h_last nil)

(def ^:dynamic predict_state nil)

(def ^:dynamic tanh_approx_e nil)

(def ^:dynamic train_ep nil)

(def ^:dynamic train_j nil)

(def ^:dynamic train_samples nil)

(def ^:dynamic train_seq nil)

(def ^:dynamic train_state nil)

(def ^:dynamic train_target nil)

(def ^:dynamic train_w nil)

(defn exp_approx [exp_approx_x]
  (binding [exp_approx_n nil exp_approx_sum nil exp_approx_term nil] (try (do (set! exp_approx_sum 1.0) (set! exp_approx_term 1.0) (set! exp_approx_n 1) (while (< exp_approx_n 20) (do (set! exp_approx_term (quot (* exp_approx_term exp_approx_x) (double exp_approx_n))) (set! exp_approx_sum (+ exp_approx_sum exp_approx_term)) (set! exp_approx_n (+ exp_approx_n 1)))) (throw (ex-info "return" {:v exp_approx_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sigmoid [sigmoid_x]
  (try (throw (ex-info "return" {:v (/ 1.0 (+ 1.0 (exp_approx (- sigmoid_x))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn tanh_approx [tanh_approx_x]
  (binding [tanh_approx_e nil] (try (do (set! tanh_approx_e (exp_approx (* 2.0 tanh_approx_x))) (throw (ex-info "return" {:v (quot (- tanh_approx_e 1.0) (+ tanh_approx_e 1.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn forward [forward_seq forward_w]
  (binding [forward_c_arr nil forward_c_prev nil forward_c_t nil forward_f_arr nil forward_f_t nil forward_g_arr nil forward_g_t nil forward_h_arr nil forward_h_prev nil forward_h_t nil forward_i_arr nil forward_i_t nil forward_o_arr nil forward_o_t nil forward_t nil forward_x nil] (try (do (set! forward_i_arr []) (set! forward_f_arr []) (set! forward_o_arr []) (set! forward_g_arr []) (set! forward_c_arr [0.0]) (set! forward_h_arr [0.0]) (set! forward_t 0) (while (< forward_t (count forward_seq)) (do (set! forward_x (nth forward_seq forward_t)) (set! forward_h_prev (nth forward_h_arr forward_t)) (set! forward_c_prev (nth forward_c_arr forward_t)) (set! forward_i_t (sigmoid (+ (+ (* (:w_i forward_w) forward_x) (* (:u_i forward_w) forward_h_prev)) (:b_i forward_w)))) (set! forward_f_t (sigmoid (+ (+ (* (:w_f forward_w) forward_x) (* (:u_f forward_w) forward_h_prev)) (:b_f forward_w)))) (set! forward_o_t (sigmoid (+ (+ (* (:w_o forward_w) forward_x) (* (:u_o forward_w) forward_h_prev)) (:b_o forward_w)))) (set! forward_g_t (tanh_approx (+ (+ (* (:w_c forward_w) forward_x) (* (:u_c forward_w) forward_h_prev)) (:b_c forward_w)))) (set! forward_c_t (+ (* forward_f_t forward_c_prev) (* forward_i_t forward_g_t))) (set! forward_h_t (* forward_o_t (tanh_approx forward_c_t))) (set! forward_i_arr (conj forward_i_arr forward_i_t)) (set! forward_f_arr (conj forward_f_arr forward_f_t)) (set! forward_o_arr (conj forward_o_arr forward_o_t)) (set! forward_g_arr (conj forward_g_arr forward_g_t)) (set! forward_c_arr (conj forward_c_arr forward_c_t)) (set! forward_h_arr (conj forward_h_arr forward_h_t)) (set! forward_t (+ forward_t 1)))) (throw (ex-info "return" {:v {:c forward_c_arr :f forward_f_arr :g forward_g_arr :h forward_h_arr :i forward_i_arr :o forward_o_arr}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn backward [backward_seq backward_target backward_w_p backward_s backward_lr]
  (binding [backward_T nil backward_c_prev nil backward_c_t nil backward_da_f nil backward_da_g nil backward_da_i nil backward_da_o nil backward_db_c nil backward_db_f nil backward_db_i nil backward_db_o nil backward_db_y nil backward_dc nil backward_dc_next nil backward_df_t nil backward_dg_t nil backward_dh_next nil backward_di_t nil backward_do_t nil backward_du_c nil backward_du_f nil backward_du_i nil backward_du_o nil backward_dw_c nil backward_dw_f nil backward_dw_i nil backward_dw_o nil backward_dw_y nil backward_dy nil backward_f_t nil backward_g_t nil backward_h_last nil backward_h_prev nil backward_i_t nil backward_o_t nil backward_t nil backward_tanh_c nil backward_w nil backward_y nil] (try (do (set! backward_w backward_w_p) (set! backward_dw_i 0.0) (set! backward_du_i 0.0) (set! backward_db_i 0.0) (set! backward_dw_f 0.0) (set! backward_du_f 0.0) (set! backward_db_f 0.0) (set! backward_dw_o 0.0) (set! backward_du_o 0.0) (set! backward_db_o 0.0) (set! backward_dw_c 0.0) (set! backward_du_c 0.0) (set! backward_db_c 0.0) (set! backward_dw_y 0.0) (set! backward_db_y 0.0) (set! backward_T (count backward_seq)) (set! backward_h_last (get (:h backward_s) backward_T)) (set! backward_y (+ (* (:w_y backward_w) backward_h_last) (:b_y backward_w))) (set! backward_dy (- backward_y backward_target)) (set! backward_dw_y (* backward_dy backward_h_last)) (set! backward_db_y backward_dy) (set! backward_dh_next (* backward_dy (:w_y backward_w))) (set! backward_dc_next 0.0) (set! backward_t (- backward_T 1)) (while (>= backward_t 0) (do (set! backward_i_t (get (:i backward_s) backward_t)) (set! backward_f_t (get (:f backward_s) backward_t)) (set! backward_o_t (get (:o backward_s) backward_t)) (set! backward_g_t (get (:g backward_s) backward_t)) (set! backward_c_t (get (:c backward_s) (+ backward_t 1))) (set! backward_c_prev (get (:c backward_s) backward_t)) (set! backward_h_prev (get (:h backward_s) backward_t)) (set! backward_tanh_c (tanh_approx backward_c_t)) (set! backward_do_t (* backward_dh_next backward_tanh_c)) (set! backward_da_o (* (* backward_do_t backward_o_t) (- 1.0 backward_o_t))) (set! backward_dc (+ (* (* backward_dh_next backward_o_t) (- 1.0 (* backward_tanh_c backward_tanh_c))) backward_dc_next)) (set! backward_di_t (* backward_dc backward_g_t)) (set! backward_da_i (* (* backward_di_t backward_i_t) (- 1.0 backward_i_t))) (set! backward_dg_t (* backward_dc backward_i_t)) (set! backward_da_g (* backward_dg_t (- 1.0 (* backward_g_t backward_g_t)))) (set! backward_df_t (* backward_dc backward_c_prev)) (set! backward_da_f (* (* backward_df_t backward_f_t) (- 1.0 backward_f_t))) (set! backward_dw_i (+ backward_dw_i (* backward_da_i (nth backward_seq backward_t)))) (set! backward_du_i (+ backward_du_i (* backward_da_i backward_h_prev))) (set! backward_db_i (+ backward_db_i backward_da_i)) (set! backward_dw_f (+ backward_dw_f (* backward_da_f (nth backward_seq backward_t)))) (set! backward_du_f (+ backward_du_f (* backward_da_f backward_h_prev))) (set! backward_db_f (+ backward_db_f backward_da_f)) (set! backward_dw_o (+ backward_dw_o (* backward_da_o (nth backward_seq backward_t)))) (set! backward_du_o (+ backward_du_o (* backward_da_o backward_h_prev))) (set! backward_db_o (+ backward_db_o backward_da_o)) (set! backward_dw_c (+ backward_dw_c (* backward_da_g (nth backward_seq backward_t)))) (set! backward_du_c (+ backward_du_c (* backward_da_g backward_h_prev))) (set! backward_db_c (+ backward_db_c backward_da_g)) (set! backward_dh_next (+ (+ (+ (* backward_da_i (:u_i backward_w)) (* backward_da_f (:u_f backward_w))) (* backward_da_o (:u_o backward_w))) (* backward_da_g (:u_c backward_w)))) (set! backward_dc_next (* backward_dc backward_f_t)) (set! backward_t (- backward_t 1)))) (set! backward_w (assoc backward_w :w_y (- (:w_y backward_w) (* backward_lr backward_dw_y)))) (set! backward_w (assoc backward_w :b_y (- (:b_y backward_w) (* backward_lr backward_db_y)))) (set! backward_w (assoc backward_w :w_i (- (:w_i backward_w) (* backward_lr backward_dw_i)))) (set! backward_w (assoc backward_w :u_i (- (:u_i backward_w) (* backward_lr backward_du_i)))) (set! backward_w (assoc backward_w :b_i (- (:b_i backward_w) (* backward_lr backward_db_i)))) (set! backward_w (assoc backward_w :w_f (- (:w_f backward_w) (* backward_lr backward_dw_f)))) (set! backward_w (assoc backward_w :u_f (- (:u_f backward_w) (* backward_lr backward_du_f)))) (set! backward_w (assoc backward_w :b_f (- (:b_f backward_w) (* backward_lr backward_db_f)))) (set! backward_w (assoc backward_w :w_o (- (:w_o backward_w) (* backward_lr backward_dw_o)))) (set! backward_w (assoc backward_w :u_o (- (:u_o backward_w) (* backward_lr backward_du_o)))) (set! backward_w (assoc backward_w :b_o (- (:b_o backward_w) (* backward_lr backward_db_o)))) (set! backward_w (assoc backward_w :w_c (- (:w_c backward_w) (* backward_lr backward_dw_c)))) (set! backward_w (assoc backward_w :u_c (- (:u_c backward_w) (* backward_lr backward_du_c)))) (set! backward_w (assoc backward_w :b_c (- (:b_c backward_w) (* backward_lr backward_db_c)))) (throw (ex-info "return" {:v backward_w}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_samples [make_samples_data make_samples_look_back]
  (binding [make_samples_X nil make_samples_Y nil make_samples_i nil make_samples_seq nil] (try (do (set! make_samples_X []) (set! make_samples_Y []) (set! make_samples_i 0) (while (< (+ make_samples_i make_samples_look_back) (count make_samples_data)) (do (set! make_samples_seq (subvec make_samples_data make_samples_i (min (+ make_samples_i make_samples_look_back) (count make_samples_data)))) (set! make_samples_X (conj make_samples_X make_samples_seq)) (set! make_samples_Y (conj make_samples_Y (nth make_samples_data (+ make_samples_i make_samples_look_back)))) (set! make_samples_i (+ make_samples_i 1)))) (throw (ex-info "return" {:v {:x make_samples_X :y make_samples_Y}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn init_weights []
  (try (throw (ex-info "return" {:v {:b_c 0.0 :b_f 0.0 :b_i 0.0 :b_o 0.0 :b_y 0.0 :u_c 0.2 :u_f 0.2 :u_i 0.2 :u_o 0.2 :w_c 0.1 :w_f 0.1 :w_i 0.1 :w_o 0.1 :w_y 0.1}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn train [train_data train_look_back train_epochs train_lr]
  (binding [train_ep nil train_j nil train_samples nil train_seq nil train_state nil train_target nil train_w nil] (try (do (set! train_samples (make_samples train_data train_look_back)) (set! train_w (init_weights)) (set! train_ep 0) (while (< train_ep train_epochs) (do (set! train_j 0) (while (< train_j (count (:x train_samples))) (do (set! train_seq (get (:x train_samples) train_j)) (set! train_target (get (:y train_samples) train_j)) (set! train_state (forward train_seq train_w)) (set! train_w (backward train_seq train_target train_w train_state train_lr)) (set! train_j (+ train_j 1)))) (set! train_ep (+ train_ep 1)))) (throw (ex-info "return" {:v train_w}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn predict [predict_seq predict_w]
  (binding [predict_h_last nil predict_state nil] (try (do (set! predict_state (forward predict_seq predict_w)) (set! predict_h_last (get (:h predict_state) (- (count (:h predict_state)) 1))) (throw (ex-info "return" {:v (+ (* (:w_y predict_w) predict_h_last) (:b_y predict_w))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_data [0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8])

(def ^:dynamic main_look_back 3)

(def ^:dynamic main_epochs 200)

(def ^:dynamic main_lr 0.1)

(def ^:dynamic main_w (train main_data main_look_back main_epochs main_lr))

(def ^:dynamic main_test_seq [0.6 0.7 0.8])

(def ^:dynamic main_pred (predict main_test_seq main_w))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "Predicted value: " (str main_pred)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
