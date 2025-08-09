(ns main (:refer-clojure :exclude [mean stump_predict train_stump boost predict main]))

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

(declare mean stump_predict train_stump boost predict main)

(def ^:dynamic boost_i nil)

(def ^:dynamic boost_j nil)

(def ^:dynamic boost_model nil)

(def ^:dynamic boost_preds nil)

(def ^:dynamic boost_r nil)

(def ^:dynamic boost_residuals nil)

(def ^:dynamic boost_stump nil)

(def ^:dynamic main_features nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_label nil)

(def ^:dynamic main_model nil)

(def ^:dynamic main_out nil)

(def ^:dynamic main_s nil)

(def ^:dynamic main_targets nil)

(def ^:dynamic mean_i nil)

(def ^:dynamic mean_sum nil)

(def ^:dynamic predict_i nil)

(def ^:dynamic predict_s nil)

(def ^:dynamic predict_score nil)

(def ^:dynamic train_stump_best_error nil)

(def ^:dynamic train_stump_best_feature nil)

(def ^:dynamic train_stump_best_left nil)

(def ^:dynamic train_stump_best_right nil)

(def ^:dynamic train_stump_best_threshold nil)

(def ^:dynamic train_stump_diff nil)

(def ^:dynamic train_stump_err nil)

(def ^:dynamic train_stump_f nil)

(def ^:dynamic train_stump_i nil)

(def ^:dynamic train_stump_j nil)

(def ^:dynamic train_stump_left nil)

(def ^:dynamic train_stump_left_mean nil)

(def ^:dynamic train_stump_num_features nil)

(def ^:dynamic train_stump_pred nil)

(def ^:dynamic train_stump_right nil)

(def ^:dynamic train_stump_right_mean nil)

(def ^:dynamic train_stump_threshold nil)

(defn mean [mean_xs]
  (binding [mean_i nil mean_sum nil] (try (do (set! mean_sum 0.0) (set! mean_i 0) (while (< mean_i (count mean_xs)) (do (set! mean_sum (+ mean_sum (nth mean_xs mean_i))) (set! mean_i (+ mean_i 1)))) (throw (ex-info "return" {:v (quot mean_sum (* (count mean_xs) 1.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn stump_predict [stump_predict_s stump_predict_x]
  (try (if (< (nth stump_predict_x (:feature stump_predict_s)) (:threshold stump_predict_s)) (:left stump_predict_s) (:right stump_predict_s)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn train_stump [train_stump_features train_stump_residuals]
  (binding [train_stump_best_error nil train_stump_best_feature nil train_stump_best_left nil train_stump_best_right nil train_stump_best_threshold nil train_stump_diff nil train_stump_err nil train_stump_f nil train_stump_i nil train_stump_j nil train_stump_left nil train_stump_left_mean nil train_stump_num_features nil train_stump_pred nil train_stump_right nil train_stump_right_mean nil train_stump_threshold nil] (try (do (set! train_stump_best_feature 0) (set! train_stump_best_threshold 0.0) (set! train_stump_best_error 1000000000.0) (set! train_stump_best_left 0.0) (set! train_stump_best_right 0.0) (set! train_stump_num_features (count (nth train_stump_features 0))) (set! train_stump_f 0) (while (< train_stump_f train_stump_num_features) (do (set! train_stump_i 0) (while (< train_stump_i (count train_stump_features)) (do (set! train_stump_threshold (nth (nth train_stump_features train_stump_i) train_stump_f)) (set! train_stump_left []) (set! train_stump_right []) (set! train_stump_j 0) (while (< train_stump_j (count train_stump_features)) (do (if (< (nth (nth train_stump_features train_stump_j) train_stump_f) train_stump_threshold) (set! train_stump_left (concat train_stump_left [(nth train_stump_residuals train_stump_j)])) (set! train_stump_right (concat train_stump_right [(nth train_stump_residuals train_stump_j)]))) (set! train_stump_j (+ train_stump_j 1)))) (when (and (not= (count train_stump_left) 0) (not= (count train_stump_right) 0)) (do (set! train_stump_left_mean (mean train_stump_left)) (set! train_stump_right_mean (mean train_stump_right)) (set! train_stump_err 0.0) (set! train_stump_j 0) (while (< train_stump_j (count train_stump_features)) (do (set! train_stump_pred (if (< (nth (nth train_stump_features train_stump_j) train_stump_f) train_stump_threshold) train_stump_left_mean train_stump_right_mean)) (set! train_stump_diff (- (nth train_stump_residuals train_stump_j) train_stump_pred)) (set! train_stump_err (+ train_stump_err (* train_stump_diff train_stump_diff))) (set! train_stump_j (+ train_stump_j 1)))) (when (< train_stump_err train_stump_best_error) (do (set! train_stump_best_error train_stump_err) (set! train_stump_best_feature train_stump_f) (set! train_stump_best_threshold train_stump_threshold) (set! train_stump_best_left train_stump_left_mean) (set! train_stump_best_right train_stump_right_mean))))) (set! train_stump_i (+ train_stump_i 1)))) (set! train_stump_f (+ train_stump_f 1)))) (throw (ex-info "return" {:v {:feature train_stump_best_feature :left train_stump_best_left :right train_stump_best_right :threshold train_stump_best_threshold}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn boost [boost_features boost_targets boost_rounds]
  (binding [boost_i nil boost_j nil boost_model nil boost_preds nil boost_r nil boost_residuals nil boost_stump nil] (try (do (set! boost_model []) (set! boost_preds []) (set! boost_i 0) (while (< boost_i (count boost_targets)) (do (set! boost_preds (concat boost_preds [0.0])) (set! boost_i (+ boost_i 1)))) (set! boost_r 0) (while (< boost_r boost_rounds) (do (set! boost_residuals []) (set! boost_j 0) (while (< boost_j (count boost_targets)) (do (set! boost_residuals (concat boost_residuals [(- (nth boost_targets boost_j) (nth boost_preds boost_j))])) (set! boost_j (+ boost_j 1)))) (set! boost_stump (train_stump boost_features boost_residuals)) (set! boost_model (concat boost_model [boost_stump])) (set! boost_j 0) (while (< boost_j (count boost_preds)) (do (set! boost_preds (assoc boost_preds boost_j (+ (nth boost_preds boost_j) (stump_predict boost_stump (nth boost_features boost_j))))) (set! boost_j (+ boost_j 1)))) (set! boost_r (+ boost_r 1)))) (throw (ex-info "return" {:v boost_model}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn predict [predict_model predict_x]
  (binding [predict_i nil predict_s nil predict_score nil] (try (do (set! predict_score 0.0) (set! predict_i 0) (while (< predict_i (count predict_model)) (do (set! predict_s (nth predict_model predict_i)) (if (< (nth predict_x (:feature predict_s)) (:threshold predict_s)) (set! predict_score (+ predict_score (:left predict_s))) (set! predict_score (+ predict_score (:right predict_s)))) (set! predict_i (+ predict_i 1)))) (throw (ex-info "return" {:v predict_score}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_features nil main_i nil main_label nil main_model nil main_out nil main_s nil main_targets nil] (do (set! main_features [[5.1 3.5] [4.9 3.0] [6.2 3.4] [5.9 3.0]]) (set! main_targets [0 0 1 1]) (set! main_model (boost main_features main_targets 3)) (set! main_out "") (set! main_i 0) (while (< main_i (count main_features)) (do (set! main_s (predict main_model (nth main_features main_i))) (set! main_label (if (>= main_s 0.5) 1 0)) (if (= main_i 0) (set! main_out (str main_label)) (set! main_out (str (str main_out " ") (str main_label)))) (set! main_i (+ main_i 1)))) (println main_out))))

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
