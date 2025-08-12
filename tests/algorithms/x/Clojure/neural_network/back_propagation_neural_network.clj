(ns main (:refer-clojure :exclude [rand random expApprox sigmoid sigmoid_vec sigmoid_derivative random_vector random_matrix matvec matTvec vec_sub vec_mul vec_scalar_mul outer mat_scalar_mul mat_sub init_layer forward backward calc_loss calc_gradient train create_data main]))

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

(declare rand random expApprox sigmoid sigmoid_vec sigmoid_derivative random_vector random_matrix matvec matTvec vec_sub vec_mul vec_scalar_mul outer mat_scalar_mul mat_sub init_layer forward backward calc_loss calc_gradient train create_data main)

(def ^:dynamic backward_delta nil)

(def ^:dynamic backward_deriv nil)

(def ^:dynamic backward_g nil)

(def ^:dynamic backward_grad_w nil)

(def ^:dynamic backward_i nil)

(def ^:dynamic backward_layer nil)

(def ^:dynamic backward_layers nil)

(def ^:dynamic calc_gradient_g nil)

(def ^:dynamic calc_gradient_i nil)

(def ^:dynamic calc_loss_d nil)

(def ^:dynamic calc_loss_i nil)

(def ^:dynamic calc_loss_s nil)

(def ^:dynamic create_data_i nil)

(def ^:dynamic create_data_x nil)

(def ^:dynamic create_data_y nil)

(def ^:dynamic expApprox_is_neg nil)

(def ^:dynamic expApprox_n nil)

(def ^:dynamic expApprox_sum nil)

(def ^:dynamic expApprox_term nil)

(def ^:dynamic expApprox_y nil)

(def ^:dynamic forward_data nil)

(def ^:dynamic forward_i nil)

(def ^:dynamic forward_layer nil)

(def ^:dynamic forward_layers nil)

(def ^:dynamic forward_z nil)

(def ^:dynamic main_data nil)

(def ^:dynamic main_final_mse nil)

(def ^:dynamic main_layers nil)

(def ^:dynamic main_x nil)

(def ^:dynamic main_y nil)

(def ^:dynamic matTvec_cols nil)

(def ^:dynamic matTvec_i nil)

(def ^:dynamic matTvec_j nil)

(def ^:dynamic matTvec_res nil)

(def ^:dynamic matTvec_s nil)

(def ^:dynamic mat_scalar_mul_i nil)

(def ^:dynamic mat_scalar_mul_j nil)

(def ^:dynamic mat_scalar_mul_res nil)

(def ^:dynamic mat_scalar_mul_row nil)

(def ^:dynamic mat_sub_i nil)

(def ^:dynamic mat_sub_j nil)

(def ^:dynamic mat_sub_res nil)

(def ^:dynamic mat_sub_row nil)

(def ^:dynamic matvec_i nil)

(def ^:dynamic matvec_j nil)

(def ^:dynamic matvec_res nil)

(def ^:dynamic matvec_s nil)

(def ^:dynamic outer_i nil)

(def ^:dynamic outer_j nil)

(def ^:dynamic outer_res nil)

(def ^:dynamic outer_row nil)

(def ^:dynamic random_matrix_i nil)

(def ^:dynamic random_matrix_m nil)

(def ^:dynamic random_vector_i nil)

(def ^:dynamic random_vector_v nil)

(def ^:dynamic sigmoid_derivative_i nil)

(def ^:dynamic sigmoid_derivative_res nil)

(def ^:dynamic sigmoid_derivative_val nil)

(def ^:dynamic sigmoid_vec_i nil)

(def ^:dynamic sigmoid_vec_res nil)

(def ^:dynamic train_grad nil)

(def ^:dynamic train_i nil)

(def ^:dynamic train_layers nil)

(def ^:dynamic train_out nil)

(def ^:dynamic train_r nil)

(def ^:dynamic vec_mul_i nil)

(def ^:dynamic vec_mul_res nil)

(def ^:dynamic vec_scalar_mul_i nil)

(def ^:dynamic vec_scalar_mul_res nil)

(def ^:dynamic vec_sub_i nil)

(def ^:dynamic vec_sub_res nil)

(def ^:dynamic main_seed 1)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn random []
  (try (throw (ex-info "return" {:v (/ (* 1.0 (rand)) 2147483648.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn expApprox [expApprox_x]
  (binding [expApprox_is_neg nil expApprox_n nil expApprox_sum nil expApprox_term nil expApprox_y nil] (try (do (set! expApprox_y expApprox_x) (set! expApprox_is_neg false) (when (< expApprox_x 0.0) (do (set! expApprox_is_neg true) (set! expApprox_y (- expApprox_x)))) (set! expApprox_term 1.0) (set! expApprox_sum 1.0) (set! expApprox_n 1) (while (< expApprox_n 30) (do (set! expApprox_term (/ (* expApprox_term expApprox_y) (double expApprox_n))) (set! expApprox_sum (+ expApprox_sum expApprox_term)) (set! expApprox_n (+ expApprox_n 1)))) (if expApprox_is_neg (/ 1.0 expApprox_sum) expApprox_sum)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sigmoid [sigmoid_z]
  (try (throw (ex-info "return" {:v (/ 1.0 (+ 1.0 (expApprox (- sigmoid_z))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sigmoid_vec [sigmoid_vec_v]
  (binding [sigmoid_vec_i nil sigmoid_vec_res nil] (try (do (set! sigmoid_vec_res []) (set! sigmoid_vec_i 0) (while (< sigmoid_vec_i (count sigmoid_vec_v)) (do (set! sigmoid_vec_res (conj sigmoid_vec_res (sigmoid (nth sigmoid_vec_v sigmoid_vec_i)))) (set! sigmoid_vec_i (+ sigmoid_vec_i 1)))) (throw (ex-info "return" {:v sigmoid_vec_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sigmoid_derivative [sigmoid_derivative_out]
  (binding [sigmoid_derivative_i nil sigmoid_derivative_res nil sigmoid_derivative_val nil] (try (do (set! sigmoid_derivative_res []) (set! sigmoid_derivative_i 0) (while (< sigmoid_derivative_i (count sigmoid_derivative_out)) (do (set! sigmoid_derivative_val (nth sigmoid_derivative_out sigmoid_derivative_i)) (set! sigmoid_derivative_res (conj sigmoid_derivative_res (* sigmoid_derivative_val (- 1.0 sigmoid_derivative_val)))) (set! sigmoid_derivative_i (+ sigmoid_derivative_i 1)))) (throw (ex-info "return" {:v sigmoid_derivative_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn random_vector [random_vector_n]
  (binding [random_vector_i nil random_vector_v nil] (try (do (set! random_vector_v []) (set! random_vector_i 0) (while (< random_vector_i random_vector_n) (do (set! random_vector_v (conj random_vector_v (- (random) 0.5))) (set! random_vector_i (+ random_vector_i 1)))) (throw (ex-info "return" {:v random_vector_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn random_matrix [random_matrix_r random_matrix_c]
  (binding [random_matrix_i nil random_matrix_m nil] (try (do (set! random_matrix_m []) (set! random_matrix_i 0) (while (< random_matrix_i random_matrix_r) (do (set! random_matrix_m (conj random_matrix_m (random_vector random_matrix_c))) (set! random_matrix_i (+ random_matrix_i 1)))) (throw (ex-info "return" {:v random_matrix_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matvec [matvec_mat matvec_vec]
  (binding [matvec_i nil matvec_j nil matvec_res nil matvec_s nil] (try (do (set! matvec_res []) (set! matvec_i 0) (while (< matvec_i (count matvec_mat)) (do (set! matvec_s 0.0) (set! matvec_j 0) (while (< matvec_j (count matvec_vec)) (do (set! matvec_s (+ matvec_s (* (nth (nth matvec_mat matvec_i) matvec_j) (nth matvec_vec matvec_j)))) (set! matvec_j (+ matvec_j 1)))) (set! matvec_res (conj matvec_res matvec_s)) (set! matvec_i (+ matvec_i 1)))) (throw (ex-info "return" {:v matvec_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matTvec [matTvec_mat matTvec_vec]
  (binding [matTvec_cols nil matTvec_i nil matTvec_j nil matTvec_res nil matTvec_s nil] (try (do (set! matTvec_cols (count (nth matTvec_mat 0))) (set! matTvec_res []) (set! matTvec_j 0) (while (< matTvec_j matTvec_cols) (do (set! matTvec_s 0.0) (set! matTvec_i 0) (while (< matTvec_i (count matTvec_mat)) (do (set! matTvec_s (+ matTvec_s (* (nth (nth matTvec_mat matTvec_i) matTvec_j) (nth matTvec_vec matTvec_i)))) (set! matTvec_i (+ matTvec_i 1)))) (set! matTvec_res (conj matTvec_res matTvec_s)) (set! matTvec_j (+ matTvec_j 1)))) (throw (ex-info "return" {:v matTvec_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vec_sub [vec_sub_a vec_sub_b]
  (binding [vec_sub_i nil vec_sub_res nil] (try (do (set! vec_sub_res []) (set! vec_sub_i 0) (while (< vec_sub_i (count vec_sub_a)) (do (set! vec_sub_res (conj vec_sub_res (- (nth vec_sub_a vec_sub_i) (nth vec_sub_b vec_sub_i)))) (set! vec_sub_i (+ vec_sub_i 1)))) (throw (ex-info "return" {:v vec_sub_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vec_mul [vec_mul_a vec_mul_b]
  (binding [vec_mul_i nil vec_mul_res nil] (try (do (set! vec_mul_res []) (set! vec_mul_i 0) (while (< vec_mul_i (count vec_mul_a)) (do (set! vec_mul_res (conj vec_mul_res (* (nth vec_mul_a vec_mul_i) (nth vec_mul_b vec_mul_i)))) (set! vec_mul_i (+ vec_mul_i 1)))) (throw (ex-info "return" {:v vec_mul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vec_scalar_mul [vec_scalar_mul_v vec_scalar_mul_s]
  (binding [vec_scalar_mul_i nil vec_scalar_mul_res nil] (try (do (set! vec_scalar_mul_res []) (set! vec_scalar_mul_i 0) (while (< vec_scalar_mul_i (count vec_scalar_mul_v)) (do (set! vec_scalar_mul_res (conj vec_scalar_mul_res (* (nth vec_scalar_mul_v vec_scalar_mul_i) vec_scalar_mul_s))) (set! vec_scalar_mul_i (+ vec_scalar_mul_i 1)))) (throw (ex-info "return" {:v vec_scalar_mul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn outer [outer_a outer_b]
  (binding [outer_i nil outer_j nil outer_res nil outer_row nil] (try (do (set! outer_res []) (set! outer_i 0) (while (< outer_i (count outer_a)) (do (set! outer_row []) (set! outer_j 0) (while (< outer_j (count outer_b)) (do (set! outer_row (conj outer_row (* (nth outer_a outer_i) (nth outer_b outer_j)))) (set! outer_j (+ outer_j 1)))) (set! outer_res (conj outer_res outer_row)) (set! outer_i (+ outer_i 1)))) (throw (ex-info "return" {:v outer_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mat_scalar_mul [mat_scalar_mul_mat mat_scalar_mul_s]
  (binding [mat_scalar_mul_i nil mat_scalar_mul_j nil mat_scalar_mul_res nil mat_scalar_mul_row nil] (try (do (set! mat_scalar_mul_res []) (set! mat_scalar_mul_i 0) (while (< mat_scalar_mul_i (count mat_scalar_mul_mat)) (do (set! mat_scalar_mul_row []) (set! mat_scalar_mul_j 0) (while (< mat_scalar_mul_j (count (nth mat_scalar_mul_mat mat_scalar_mul_i))) (do (set! mat_scalar_mul_row (conj mat_scalar_mul_row (* (nth (nth mat_scalar_mul_mat mat_scalar_mul_i) mat_scalar_mul_j) mat_scalar_mul_s))) (set! mat_scalar_mul_j (+ mat_scalar_mul_j 1)))) (set! mat_scalar_mul_res (conj mat_scalar_mul_res mat_scalar_mul_row)) (set! mat_scalar_mul_i (+ mat_scalar_mul_i 1)))) (throw (ex-info "return" {:v mat_scalar_mul_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mat_sub [mat_sub_a mat_sub_b]
  (binding [mat_sub_i nil mat_sub_j nil mat_sub_res nil mat_sub_row nil] (try (do (set! mat_sub_res []) (set! mat_sub_i 0) (while (< mat_sub_i (count mat_sub_a)) (do (set! mat_sub_row []) (set! mat_sub_j 0) (while (< mat_sub_j (count (nth mat_sub_a mat_sub_i))) (do (set! mat_sub_row (conj mat_sub_row (- (nth (nth mat_sub_a mat_sub_i) mat_sub_j) (nth (nth mat_sub_b mat_sub_i) mat_sub_j)))) (set! mat_sub_j (+ mat_sub_j 1)))) (set! mat_sub_res (conj mat_sub_res mat_sub_row)) (set! mat_sub_i (+ mat_sub_i 1)))) (throw (ex-info "return" {:v mat_sub_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn init_layer [init_layer_units init_layer_back_units init_layer_lr]
  (try (throw (ex-info "return" {:v {:bias (random_vector init_layer_units) :learn_rate init_layer_lr :output [] :units init_layer_units :weight (random_matrix init_layer_units init_layer_back_units) :xdata []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn forward [forward_layers_p forward_x]
  (binding [forward_data nil forward_i nil forward_layer nil forward_layers nil forward_z nil] (try (do (set! forward_layers forward_layers_p) (set! forward_data forward_x) (set! forward_i 0) (while (< forward_i (count forward_layers)) (do (set! forward_layer (nth forward_layers forward_i)) (set! forward_layer (assoc forward_layer :xdata forward_data)) (if (= forward_i 0) (set! forward_layer (assoc forward_layer :output forward_data)) (do (set! forward_z (vec_sub (matvec (:weight forward_layer) forward_data) (:bias forward_layer))) (set! forward_layer (assoc forward_layer :output (sigmoid_vec forward_z))) (set! forward_data (:output forward_layer)))) (set! forward_layers (assoc forward_layers forward_i forward_layer)) (set! forward_i (+ forward_i 1)))) (throw (ex-info "return" {:v forward_layers}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn backward [backward_layers_p backward_grad]
  (binding [backward_delta nil backward_deriv nil backward_g nil backward_grad_w nil backward_i nil backward_layer nil backward_layers nil] (try (do (set! backward_layers backward_layers_p) (set! backward_g backward_grad) (set! backward_i (- (count backward_layers) 1)) (while (> backward_i 0) (do (set! backward_layer (nth backward_layers backward_i)) (set! backward_deriv (sigmoid_derivative (:output backward_layer))) (set! backward_delta (vec_mul backward_g backward_deriv)) (set! backward_grad_w (outer backward_delta (:xdata backward_layer))) (set! backward_layer (assoc backward_layer :weight (mat_sub (:weight backward_layer) (mat_scalar_mul backward_grad_w (:learn_rate backward_layer))))) (set! backward_layer (assoc backward_layer :bias (vec_sub (:bias backward_layer) (vec_scalar_mul backward_delta (:learn_rate backward_layer))))) (set! backward_g (matTvec (:weight backward_layer) backward_delta)) (set! backward_layers (assoc backward_layers backward_i backward_layer)) (set! backward_i (- backward_i 1)))) (throw (ex-info "return" {:v backward_layers}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calc_loss [calc_loss_y calc_loss_yhat]
  (binding [calc_loss_d nil calc_loss_i nil calc_loss_s nil] (try (do (set! calc_loss_s 0.0) (set! calc_loss_i 0) (while (< calc_loss_i (count calc_loss_y)) (do (set! calc_loss_d (- (nth calc_loss_y calc_loss_i) (nth calc_loss_yhat calc_loss_i))) (set! calc_loss_s (+ calc_loss_s (* calc_loss_d calc_loss_d))) (set! calc_loss_i (+ calc_loss_i 1)))) (throw (ex-info "return" {:v calc_loss_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calc_gradient [calc_gradient_y calc_gradient_yhat]
  (binding [calc_gradient_g nil calc_gradient_i nil] (try (do (set! calc_gradient_g []) (set! calc_gradient_i 0) (while (< calc_gradient_i (count calc_gradient_y)) (do (set! calc_gradient_g (conj calc_gradient_g (* 2.0 (- (nth calc_gradient_yhat calc_gradient_i) (nth calc_gradient_y calc_gradient_i))))) (set! calc_gradient_i (+ calc_gradient_i 1)))) (throw (ex-info "return" {:v calc_gradient_g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn train [train_layers_p train_xdata train_ydata train_rounds train_acc]
  (binding [train_grad nil train_i nil train_layers nil train_out nil train_r nil] (try (do (set! train_layers train_layers_p) (set! train_r 0) (while (< train_r train_rounds) (do (set! train_i 0) (while (< train_i (count train_xdata)) (do (set! train_layers (forward train_layers (nth train_xdata train_i))) (set! train_out (:output (nth train_layers (- (count train_layers) 1)))) (set! train_grad (calc_gradient (nth train_ydata train_i) train_out)) (set! train_layers (backward train_layers train_grad)) (set! train_i (+ train_i 1)))) (set! train_r (+ train_r 1)))) (throw (ex-info "return" {:v 0.0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn create_data []
  (binding [create_data_i nil create_data_x nil create_data_y nil] (try (do (set! create_data_x []) (set! create_data_i 0) (while (< create_data_i 10) (do (set! create_data_x (conj create_data_x (random_vector 10))) (set! create_data_i (+ create_data_i 1)))) (set! create_data_y [[0.8 0.4] [0.4 0.3] [0.34 0.45] [0.67 0.32] [0.88 0.67] [0.78 0.77] [0.55 0.66] [0.55 0.43] [0.54 0.1] [0.1 0.5]]) (throw (ex-info "return" {:v {:x create_data_x :y create_data_y}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_data nil main_final_mse nil main_layers nil main_x nil main_y nil] (do (set! main_data (create_data)) (set! main_x (:x main_data)) (set! main_y (:y main_data)) (set! main_layers []) (set! main_layers (conj main_layers (init_layer 10 0 0.3))) (set! main_layers (conj main_layers (init_layer 20 10 0.3))) (set! main_layers (conj main_layers (init_layer 30 20 0.3))) (set! main_layers (conj main_layers (init_layer 2 30 0.3))) (set! main_final_mse (train main_layers main_x main_y 100 0.01)) (println main_final_mse))))

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
