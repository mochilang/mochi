(ns main (:refer-clojure :exclude [sqrtApprox make_knn euclidean_distance classify]))

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

(declare sqrtApprox make_knn euclidean_distance classify)

(def ^:dynamic classify_d nil)

(def ^:dynamic classify_distances nil)

(def ^:dynamic classify_i nil)

(def ^:dynamic classify_j nil)

(def ^:dynamic classify_lbl nil)

(def ^:dynamic classify_m nil)

(def ^:dynamic classify_max_idx nil)

(def ^:dynamic classify_min_index nil)

(def ^:dynamic classify_t nil)

(def ^:dynamic classify_tally nil)

(def ^:dynamic classify_v nil)

(def ^:dynamic classify_votes nil)

(def ^:dynamic count_v nil)

(def ^:dynamic euclidean_distance_diff nil)

(def ^:dynamic euclidean_distance_i nil)

(def ^:dynamic euclidean_distance_sum nil)

(def ^:dynamic make_knn_i nil)

(def ^:dynamic make_knn_items nil)

(def ^:dynamic make_knn_pl nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_knn [make_knn_train_data make_knn_train_target make_knn_class_labels]
  (binding [make_knn_i nil make_knn_items nil make_knn_pl nil] (try (do (set! make_knn_items []) (set! make_knn_i 0) (while (< make_knn_i (count make_knn_train_data)) (do (set! make_knn_pl {:label (nth make_knn_train_target make_knn_i) :point (nth make_knn_train_data make_knn_i)}) (set! make_knn_items (conj make_knn_items make_knn_pl)) (set! make_knn_i (+ make_knn_i 1)))) (throw (ex-info "return" {:v {:data make_knn_items :labels make_knn_class_labels}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn euclidean_distance [euclidean_distance_a euclidean_distance_b]
  (binding [euclidean_distance_diff nil euclidean_distance_i nil euclidean_distance_sum nil] (try (do (set! euclidean_distance_sum 0.0) (set! euclidean_distance_i 0) (while (< euclidean_distance_i (count euclidean_distance_a)) (do (set! euclidean_distance_diff (- (nth euclidean_distance_a euclidean_distance_i) (nth euclidean_distance_b euclidean_distance_i))) (set! euclidean_distance_sum (+ euclidean_distance_sum (* euclidean_distance_diff euclidean_distance_diff))) (set! euclidean_distance_i (+ euclidean_distance_i 1)))) (throw (ex-info "return" {:v (sqrtApprox euclidean_distance_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn classify [classify_knn classify_pred_point classify_k]
  (binding [classify_d nil classify_distances nil classify_i nil classify_j nil classify_lbl nil classify_m nil classify_max_idx nil classify_min_index nil classify_t nil classify_tally nil classify_v nil classify_votes nil count_v nil] (try (do (set! classify_distances []) (set! classify_i 0) (while (< classify_i (count (:data classify_knn))) (do (set! classify_d (euclidean_distance (:point (get (:data classify_knn) classify_i)) classify_pred_point)) (set! classify_distances (conj classify_distances {:dist classify_d :label (:label (get (:data classify_knn) classify_i))})) (set! classify_i (+ classify_i 1)))) (set! classify_votes []) (set! count_v 0) (while (< count_v classify_k) (do (set! classify_min_index 0) (set! classify_j 1) (while (< classify_j (count classify_distances)) (do (when (< (:dist (nth classify_distances classify_j)) (:dist (nth classify_distances classify_min_index))) (set! classify_min_index classify_j)) (set! classify_j (+ classify_j 1)))) (set! classify_votes (conj classify_votes (:label (nth classify_distances classify_min_index)))) (set! classify_distances (assoc-in classify_distances [classify_min_index :dist] 1000000000000000000.0)) (set! count_v (+ count_v 1)))) (set! classify_tally []) (set! classify_t 0) (while (< classify_t (count (:labels classify_knn))) (do (set! classify_tally (conj classify_tally 0)) (set! classify_t (+ classify_t 1)))) (set! classify_v 0) (while (< classify_v (count classify_votes)) (do (set! classify_lbl (nth classify_votes classify_v)) (set! classify_tally (assoc classify_tally classify_lbl (+ (nth classify_tally classify_lbl) 1))) (set! classify_v (+ classify_v 1)))) (set! classify_max_idx 0) (set! classify_m 1) (while (< classify_m (count classify_tally)) (do (when (> (nth classify_tally classify_m) (nth classify_tally classify_max_idx)) (set! classify_max_idx classify_m)) (set! classify_m (+ classify_m 1)))) (throw (ex-info "return" {:v (get (:labels classify_knn) classify_max_idx)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_train_X [[0.0 0.0] [1.0 0.0] [0.0 1.0] [0.5 0.5] [3.0 3.0] [2.0 3.0] [3.0 2.0]])

(def ^:dynamic main_train_y [0 0 0 0 1 1 1])

(def ^:dynamic main_classes ["A" "B"])

(def ^:dynamic main_knn (make_knn main_train_X main_train_y main_classes))

(def ^:dynamic main_point [1.2 1.2])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (classify main_knn main_point 5))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
