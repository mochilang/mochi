(ns main (:refer-clojure :exclude [_mod sin rand mean mean_squared_error train_tree predict main]))

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

(declare _mod sin rand mean mean_squared_error train_tree predict main)

(def ^:dynamic main_avg_error nil)

(def ^:dynamic main_diff nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_predictions nil)

(def ^:dynamic main_sum_err nil)

(def ^:dynamic main_test_cases nil)

(def ^:dynamic main_tree nil)

(def ^:dynamic main_v nil)

(def ^:dynamic main_x nil)

(def ^:dynamic main_y nil)

(def ^:dynamic mean_i nil)

(def ^:dynamic mean_squared_error_diff nil)

(def ^:dynamic mean_squared_error_i nil)

(def ^:dynamic mean_squared_error_total nil)

(def ^:dynamic mean_sum nil)

(def ^:dynamic sin_y nil)

(def ^:dynamic sin_y2 nil)

(def ^:dynamic sin_y3 nil)

(def ^:dynamic sin_y5 nil)

(def ^:dynamic sin_y7 nil)

(def ^:dynamic train_tree_best_split nil)

(def ^:dynamic train_tree_boundary nil)

(def ^:dynamic train_tree_err nil)

(def ^:dynamic train_tree_err_left nil)

(def ^:dynamic train_tree_err_right nil)

(def ^:dynamic train_tree_i nil)

(def ^:dynamic train_tree_left_tree nil)

(def ^:dynamic train_tree_left_x nil)

(def ^:dynamic train_tree_left_y nil)

(def ^:dynamic train_tree_min_error nil)

(def ^:dynamic train_tree_right_tree nil)

(def ^:dynamic train_tree_right_x nil)

(def ^:dynamic train_tree_right_y nil)

(def ^:dynamic main_PI 3.141592653589793)

(def ^:dynamic main_TWO_PI 6.283185307179586)

(defn _mod [_mod_x _mod_m]
  (try (throw (ex-info "return" {:v (- _mod_x (* (double (int (quot _mod_x _mod_m))) _mod_m))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sin [sin_x]
  (binding [sin_y nil sin_y2 nil sin_y3 nil sin_y5 nil sin_y7 nil] (try (do (set! sin_y (- (_mod (+ sin_x main_PI) main_TWO_PI) main_PI)) (set! sin_y2 (* sin_y sin_y)) (set! sin_y3 (* sin_y2 sin_y)) (set! sin_y5 (* sin_y3 sin_y2)) (set! sin_y7 (* sin_y5 sin_y2)) (throw (ex-info "return" {:v (- (+ (- sin_y (/ sin_y3 6.0)) (/ sin_y5 120.0)) (/ sin_y7 5040.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_seed 123456789)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* 1103515245 main_seed) 12345) 2147483648))) (throw (ex-info "return" {:v (/ (double main_seed) 2147483648.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mean [mean_vals]
  (binding [mean_i nil mean_sum nil] (try (do (set! mean_sum 0.0) (set! mean_i 0) (while (< mean_i (count mean_vals)) (do (set! mean_sum (+ mean_sum (nth mean_vals mean_i))) (set! mean_i (+ mean_i 1)))) (throw (ex-info "return" {:v (quot mean_sum (count mean_vals))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mean_squared_error [mean_squared_error_labels mean_squared_error_prediction]
  (binding [mean_squared_error_diff nil mean_squared_error_i nil mean_squared_error_total nil] (try (do (set! mean_squared_error_total 0.0) (set! mean_squared_error_i 0) (while (< mean_squared_error_i (count mean_squared_error_labels)) (do (set! mean_squared_error_diff (- (nth mean_squared_error_labels mean_squared_error_i) mean_squared_error_prediction)) (set! mean_squared_error_total (+ mean_squared_error_total (* mean_squared_error_diff mean_squared_error_diff))) (set! mean_squared_error_i (+ mean_squared_error_i 1)))) (throw (ex-info "return" {:v (quot mean_squared_error_total (count mean_squared_error_labels))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn train_tree [train_tree_x train_tree_y train_tree_depth train_tree_min_leaf_size]
  (binding [train_tree_best_split nil train_tree_boundary nil train_tree_err nil train_tree_err_left nil train_tree_err_right nil train_tree_i nil train_tree_left_tree nil train_tree_left_x nil train_tree_left_y nil train_tree_min_error nil train_tree_right_tree nil train_tree_right_x nil train_tree_right_y nil] (try (do (when (< (count train_tree_x) (* 2 train_tree_min_leaf_size)) (throw (ex-info "return" {:v {:__tag "Leaf" :prediction (mean train_tree_y)}}))) (when (= train_tree_depth 1) (throw (ex-info "return" {:v {:__tag "Leaf" :prediction (mean train_tree_y)}}))) (set! train_tree_best_split 0) (set! train_tree_min_error (* (mean_squared_error train_tree_x (mean train_tree_y)) 2.0)) (set! train_tree_i 0) (while (< train_tree_i (count train_tree_x)) (do (if (< (count (subvec train_tree_x 0 train_tree_i)) train_tree_min_leaf_size) (set! train_tree_i train_tree_i) (if (< (count (subvec train_tree_x train_tree_i (count train_tree_x))) train_tree_min_leaf_size) (set! train_tree_i train_tree_i) (do (set! train_tree_err_left (mean_squared_error (subvec train_tree_x 0 train_tree_i) (mean (subvec train_tree_y 0 train_tree_i)))) (set! train_tree_err_right (mean_squared_error (subvec train_tree_x train_tree_i (count train_tree_x)) (mean (subvec train_tree_y train_tree_i (count train_tree_y))))) (set! train_tree_err (+ train_tree_err_left train_tree_err_right)) (when (< train_tree_err train_tree_min_error) (do (set! train_tree_best_split train_tree_i) (set! train_tree_min_error train_tree_err)))))) (set! train_tree_i (+ train_tree_i 1)))) (when (not= train_tree_best_split 0) (do (set! train_tree_left_x (subvec train_tree_x 0 train_tree_best_split)) (set! train_tree_left_y (subvec train_tree_y 0 train_tree_best_split)) (set! train_tree_right_x (subvec train_tree_x train_tree_best_split (count train_tree_x))) (set! train_tree_right_y (subvec train_tree_y train_tree_best_split (count train_tree_y))) (set! train_tree_boundary (nth train_tree_x train_tree_best_split)) (set! train_tree_left_tree (train_tree train_tree_left_x train_tree_left_y (- train_tree_depth 1) train_tree_min_leaf_size)) (set! train_tree_right_tree (train_tree train_tree_right_x train_tree_right_y (- train_tree_depth 1) train_tree_min_leaf_size)) (throw (ex-info "return" {:v {:__tag "Branch" :decision_boundary train_tree_boundary :left train_tree_left_tree :right train_tree_right_tree}})))) (throw (ex-info "return" {:v {:__tag "Leaf" :prediction (mean train_tree_y)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn predict [predict_tree predict_value]
  (try (throw (ex-info "return" {:v (cond (and (map? predict_tree) (= (:__tag predict_tree) "Leaf") (contains? predict_tree :prediction)) (let [predict_p (:prediction predict_tree)] predict_p) (and (map? predict_tree) (= (:__tag predict_tree) "Branch") (contains? predict_tree :decision_boundary) (contains? predict_tree :left) (contains? predict_tree :right)) (let [predict_b (:decision_boundary predict_tree) predict_l (:left predict_tree) predict_r (:right predict_tree)] (if (>= predict_value predict_b) (predict predict_r predict_value) (predict predict_l predict_value))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_avg_error nil main_diff nil main_i nil main_predictions nil main_sum_err nil main_test_cases nil main_tree nil main_v nil main_x nil main_y nil] (do (set! main_x []) (set! main_v (- 1.0)) (while (< main_v 1.0) (do (set! main_x (conj main_x main_v)) (set! main_v (+ main_v 0.005)))) (set! main_y []) (set! main_i 0) (while (< main_i (count main_x)) (do (set! main_y (conj main_y (sin (nth main_x main_i)))) (set! main_i (+ main_i 1)))) (set! main_tree (train_tree main_x main_y 10 10)) (set! main_test_cases []) (set! main_i 0) (while (< main_i 10) (do (set! main_test_cases (conj main_test_cases (- (* (rand) 2.0) 1.0))) (set! main_i (+ main_i 1)))) (set! main_predictions []) (set! main_i 0) (while (< main_i (count main_test_cases)) (do (set! main_predictions (conj main_predictions (predict main_tree (nth main_test_cases main_i)))) (set! main_i (+ main_i 1)))) (set! main_sum_err 0.0) (set! main_i 0) (while (< main_i (count main_test_cases)) (do (set! main_diff (- (nth main_predictions main_i) (nth main_test_cases main_i))) (set! main_sum_err (+ main_sum_err (* main_diff main_diff))) (set! main_i (+ main_i 1)))) (set! main_avg_error (quot main_sum_err (count main_test_cases))) (println (str "Test values: " (str main_test_cases))) (println (str "Predictions: " (str main_predictions))) (println (str "Average error: " (str main_avg_error))))))

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
