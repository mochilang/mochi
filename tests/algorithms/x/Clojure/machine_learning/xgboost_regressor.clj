(ns main (:refer-clojure :exclude [data_handling xgboost mean_absolute_error mean_squared_error main]))

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

(declare data_handling xgboost mean_absolute_error mean_squared_error main)

(def ^:dynamic main_california nil)

(def ^:dynamic main_ds nil)

(def ^:dynamic main_predictions nil)

(def ^:dynamic main_x_test nil)

(def ^:dynamic main_x_train nil)

(def ^:dynamic main_y_test nil)

(def ^:dynamic main_y_train nil)

(def ^:dynamic mean_absolute_error_diff nil)

(def ^:dynamic mean_absolute_error_i nil)

(def ^:dynamic mean_absolute_error_sum nil)

(def ^:dynamic mean_squared_error_diff nil)

(def ^:dynamic mean_squared_error_i nil)

(def ^:dynamic mean_squared_error_sum nil)

(def ^:dynamic xgboost_est nil)

(def ^:dynamic xgboost_i nil)

(def ^:dynamic xgboost_j nil)

(def ^:dynamic xgboost_k nil)

(def ^:dynamic xgboost_learning_rate nil)

(def ^:dynamic xgboost_left_count nil)

(def ^:dynamic xgboost_left_sum nil)

(def ^:dynamic xgboost_left_value nil)

(def ^:dynamic xgboost_n_estimators nil)

(def ^:dynamic xgboost_pred nil)

(def ^:dynamic xgboost_predictions nil)

(def ^:dynamic xgboost_preds nil)

(def ^:dynamic xgboost_residuals nil)

(def ^:dynamic xgboost_right_count nil)

(def ^:dynamic xgboost_right_sum nil)

(def ^:dynamic xgboost_right_value nil)

(def ^:dynamic xgboost_sum_feat nil)

(def ^:dynamic xgboost_t nil)

(def ^:dynamic xgboost_threshold nil)

(def ^:dynamic xgboost_trees nil)

(defn data_handling [data_handling_dataset]
  (try (throw (ex-info "return" {:v data_handling_dataset})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn xgboost [xgboost_features xgboost_target xgboost_test_features]
  (binding [xgboost_est nil xgboost_i nil xgboost_j nil xgboost_k nil xgboost_learning_rate nil xgboost_left_count nil xgboost_left_sum nil xgboost_left_value nil xgboost_n_estimators nil xgboost_pred nil xgboost_predictions nil xgboost_preds nil xgboost_residuals nil xgboost_right_count nil xgboost_right_sum nil xgboost_right_value nil xgboost_sum_feat nil xgboost_t nil xgboost_threshold nil xgboost_trees nil] (try (do (set! xgboost_learning_rate 0.5) (set! xgboost_n_estimators 3) (set! xgboost_trees []) (set! xgboost_predictions []) (set! xgboost_i 0) (while (< xgboost_i (count xgboost_target)) (do (set! xgboost_predictions (conj xgboost_predictions 0.0)) (set! xgboost_i (+ xgboost_i 1)))) (set! xgboost_est 0) (while (< xgboost_est xgboost_n_estimators) (do (set! xgboost_residuals []) (set! xgboost_j 0) (while (< xgboost_j (count xgboost_target)) (do (set! xgboost_residuals (conj xgboost_residuals (- (nth xgboost_target xgboost_j) (nth xgboost_predictions xgboost_j)))) (set! xgboost_j (+ xgboost_j 1)))) (set! xgboost_sum_feat 0.0) (set! xgboost_j 0) (while (< xgboost_j (count xgboost_features)) (do (set! xgboost_sum_feat (+ xgboost_sum_feat (nth (nth xgboost_features xgboost_j) 0))) (set! xgboost_j (+ xgboost_j 1)))) (set! xgboost_threshold (quot xgboost_sum_feat (double (count xgboost_features)))) (set! xgboost_left_sum 0.0) (set! xgboost_left_count 0) (set! xgboost_right_sum 0.0) (set! xgboost_right_count 0) (set! xgboost_j 0) (while (< xgboost_j (count xgboost_features)) (do (if (<= (nth (nth xgboost_features xgboost_j) 0) xgboost_threshold) (do (set! xgboost_left_sum (+ xgboost_left_sum (nth xgboost_residuals xgboost_j))) (set! xgboost_left_count (+ xgboost_left_count 1))) (do (set! xgboost_right_sum (+ xgboost_right_sum (nth xgboost_residuals xgboost_j))) (set! xgboost_right_count (+ xgboost_right_count 1)))) (set! xgboost_j (+ xgboost_j 1)))) (set! xgboost_left_value 0.0) (when (> xgboost_left_count 0) (set! xgboost_left_value (quot xgboost_left_sum (double xgboost_left_count)))) (set! xgboost_right_value 0.0) (when (> xgboost_right_count 0) (set! xgboost_right_value (quot xgboost_right_sum (double xgboost_right_count)))) (set! xgboost_j 0) (while (< xgboost_j (count xgboost_features)) (do (if (<= (nth (nth xgboost_features xgboost_j) 0) xgboost_threshold) (set! xgboost_predictions (assoc xgboost_predictions xgboost_j (+ (nth xgboost_predictions xgboost_j) (* xgboost_learning_rate xgboost_left_value)))) (set! xgboost_predictions (assoc xgboost_predictions xgboost_j (+ (nth xgboost_predictions xgboost_j) (* xgboost_learning_rate xgboost_right_value))))) (set! xgboost_j (+ xgboost_j 1)))) (set! xgboost_trees (conj xgboost_trees {:left_value xgboost_left_value :right_value xgboost_right_value :threshold xgboost_threshold})) (set! xgboost_est (+ xgboost_est 1)))) (set! xgboost_preds []) (set! xgboost_t 0) (while (< xgboost_t (count xgboost_test_features)) (do (set! xgboost_pred 0.0) (set! xgboost_k 0) (while (< xgboost_k (count xgboost_trees)) (do (if (<= (nth (nth xgboost_test_features xgboost_t) 0) (:threshold (nth xgboost_trees xgboost_k))) (set! xgboost_pred (+ xgboost_pred (* xgboost_learning_rate (:left_value (nth xgboost_trees xgboost_k))))) (set! xgboost_pred (+ xgboost_pred (* xgboost_learning_rate (:right_value (nth xgboost_trees xgboost_k)))))) (set! xgboost_k (+ xgboost_k 1)))) (set! xgboost_preds (conj xgboost_preds xgboost_pred)) (set! xgboost_t (+ xgboost_t 1)))) (throw (ex-info "return" {:v xgboost_preds}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mean_absolute_error [mean_absolute_error_y_true mean_absolute_error_y_pred]
  (binding [mean_absolute_error_diff nil mean_absolute_error_i nil mean_absolute_error_sum nil] (try (do (set! mean_absolute_error_sum 0.0) (set! mean_absolute_error_i 0) (while (< mean_absolute_error_i (count mean_absolute_error_y_true)) (do (set! mean_absolute_error_diff (- (nth mean_absolute_error_y_true mean_absolute_error_i) (nth mean_absolute_error_y_pred mean_absolute_error_i))) (when (< mean_absolute_error_diff 0.0) (set! mean_absolute_error_diff (- mean_absolute_error_diff))) (set! mean_absolute_error_sum (+ mean_absolute_error_sum mean_absolute_error_diff)) (set! mean_absolute_error_i (+ mean_absolute_error_i 1)))) (throw (ex-info "return" {:v (quot mean_absolute_error_sum (double (count mean_absolute_error_y_true)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mean_squared_error [mean_squared_error_y_true mean_squared_error_y_pred]
  (binding [mean_squared_error_diff nil mean_squared_error_i nil mean_squared_error_sum nil] (try (do (set! mean_squared_error_sum 0.0) (set! mean_squared_error_i 0) (while (< mean_squared_error_i (count mean_squared_error_y_true)) (do (set! mean_squared_error_diff (- (nth mean_squared_error_y_true mean_squared_error_i) (nth mean_squared_error_y_pred mean_squared_error_i))) (set! mean_squared_error_sum (+ mean_squared_error_sum (* mean_squared_error_diff mean_squared_error_diff))) (set! mean_squared_error_i (+ mean_squared_error_i 1)))) (throw (ex-info "return" {:v (quot mean_squared_error_sum (double (count mean_squared_error_y_true)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_california nil main_ds nil main_predictions nil main_x_test nil main_x_train nil main_y_test nil main_y_train nil] (do (set! main_california {:data [[1.0] [2.0] [3.0] [4.0]] :target [2.0 3.0 4.0 5.0]}) (set! main_ds (data_handling main_california)) (set! main_x_train (:data main_ds)) (set! main_y_train (:target main_ds)) (set! main_x_test [[1.5] [3.5]]) (set! main_y_test [2.5 4.5]) (set! main_predictions (xgboost main_x_train main_y_train main_x_test)) (println "Predictions:") (println main_predictions) (println "Mean Absolute Error:") (println (mean_absolute_error main_y_test main_predictions)) (println "Mean Square Error:") (println (mean_squared_error main_y_test main_predictions)))))

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
