(ns main (:refer-clojure :exclude [distance_sq assign_clusters revise_centroids compute_heterogeneity lists_equal kmeans]))

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

(declare distance_sq assign_clusters revise_centroids compute_heterogeneity lists_equal kmeans)

(def ^:dynamic assign_clusters_assignments nil)

(def ^:dynamic assign_clusters_best nil)

(def ^:dynamic assign_clusters_best_idx nil)

(def ^:dynamic assign_clusters_dist nil)

(def ^:dynamic compute_heterogeneity_c nil)

(def ^:dynamic compute_heterogeneity_total nil)

(def ^:dynamic distance_sq_diff nil)

(def ^:dynamic distance_sq_sum nil)

(def ^:dynamic kmeans_assignment nil)

(def ^:dynamic kmeans_centroids nil)

(def ^:dynamic kmeans_h nil)

(def ^:dynamic kmeans_heterogeneity nil)

(def ^:dynamic kmeans_iter nil)

(def ^:dynamic kmeans_prev nil)

(def ^:dynamic revise_centroids_c nil)

(def ^:dynamic revise_centroids_centroids nil)

(def ^:dynamic revise_centroids_counts nil)

(def ^:dynamic revise_centroids_dim nil)

(def ^:dynamic revise_centroids_row nil)

(def ^:dynamic revise_centroids_sums nil)

(defn distance_sq [distance_sq_a distance_sq_b]
  (binding [distance_sq_diff nil distance_sq_sum nil] (try (do (set! distance_sq_sum 0.0) (dotimes [i (count distance_sq_a)] (do (set! distance_sq_diff (- (nth distance_sq_a i) (nth distance_sq_b i))) (set! distance_sq_sum (+ distance_sq_sum (* distance_sq_diff distance_sq_diff))))) (throw (ex-info "return" {:v distance_sq_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn assign_clusters [assign_clusters_data assign_clusters_centroids]
  (binding [assign_clusters_assignments nil assign_clusters_best nil assign_clusters_best_idx nil assign_clusters_dist nil] (try (do (set! assign_clusters_assignments []) (dotimes [i (count assign_clusters_data)] (do (set! assign_clusters_best_idx 0) (set! assign_clusters_best (distance_sq (nth assign_clusters_data i) (nth assign_clusters_centroids 0))) (doseq [j (range 1 (count assign_clusters_centroids))] (do (set! assign_clusters_dist (distance_sq (nth assign_clusters_data i) (nth assign_clusters_centroids j))) (when (< assign_clusters_dist assign_clusters_best) (do (set! assign_clusters_best assign_clusters_dist) (set! assign_clusters_best_idx j))))) (set! assign_clusters_assignments (conj assign_clusters_assignments assign_clusters_best_idx)))) (throw (ex-info "return" {:v assign_clusters_assignments}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn revise_centroids [revise_centroids_data revise_centroids_k revise_centroids_assignment]
  (binding [revise_centroids_c nil revise_centroids_centroids nil revise_centroids_counts nil revise_centroids_dim nil revise_centroids_row nil revise_centroids_sums nil] (try (do (set! revise_centroids_dim (count (nth revise_centroids_data 0))) (set! revise_centroids_sums []) (set! revise_centroids_counts []) (dotimes [i revise_centroids_k] (do (set! revise_centroids_row []) (dotimes [j revise_centroids_dim] (set! revise_centroids_row (conj revise_centroids_row 0.0))) (set! revise_centroids_sums (conj revise_centroids_sums revise_centroids_row)) (set! revise_centroids_counts (conj revise_centroids_counts 0)))) (dotimes [i (count revise_centroids_data)] (do (set! revise_centroids_c (nth revise_centroids_assignment i)) (set! revise_centroids_counts (assoc revise_centroids_counts revise_centroids_c (+ (nth revise_centroids_counts revise_centroids_c) 1))) (dotimes [j revise_centroids_dim] (set! revise_centroids_sums (assoc-in revise_centroids_sums [revise_centroids_c j] (+ (nth (nth revise_centroids_sums revise_centroids_c) j) (nth (nth revise_centroids_data i) j))))))) (set! revise_centroids_centroids []) (dotimes [i revise_centroids_k] (do (set! revise_centroids_row []) (if (> (nth revise_centroids_counts i) 0) (dotimes [j revise_centroids_dim] (set! revise_centroids_row (conj revise_centroids_row (quot (nth (nth revise_centroids_sums i) j) (double (nth revise_centroids_counts i)))))) (dotimes [j revise_centroids_dim] (set! revise_centroids_row (conj revise_centroids_row 0.0)))) (set! revise_centroids_centroids (conj revise_centroids_centroids revise_centroids_row)))) (throw (ex-info "return" {:v revise_centroids_centroids}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn compute_heterogeneity [compute_heterogeneity_data compute_heterogeneity_centroids compute_heterogeneity_assignment]
  (binding [compute_heterogeneity_c nil compute_heterogeneity_total nil] (try (do (set! compute_heterogeneity_total 0.0) (dotimes [i (count compute_heterogeneity_data)] (do (set! compute_heterogeneity_c (nth compute_heterogeneity_assignment i)) (set! compute_heterogeneity_total (+ compute_heterogeneity_total (distance_sq (nth compute_heterogeneity_data i) (nth compute_heterogeneity_centroids compute_heterogeneity_c)))))) (throw (ex-info "return" {:v compute_heterogeneity_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lists_equal [lists_equal_a lists_equal_b]
  (try (do (when (not= (count lists_equal_a) (count lists_equal_b)) (throw (ex-info "return" {:v false}))) (dotimes [i (count lists_equal_a)] (when (not= (nth lists_equal_a i) (nth lists_equal_b i)) (throw (ex-info "return" {:v false})))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn kmeans [kmeans_data kmeans_k kmeans_initial_centroids kmeans_max_iter]
  (binding [kmeans_assignment nil kmeans_centroids nil kmeans_h nil kmeans_heterogeneity nil kmeans_iter nil kmeans_prev nil] (try (do (set! kmeans_centroids kmeans_initial_centroids) (set! kmeans_assignment []) (set! kmeans_prev []) (set! kmeans_heterogeneity []) (set! kmeans_iter 0) (loop [while_flag_1 true] (when (and while_flag_1 (< kmeans_iter kmeans_max_iter)) (do (set! kmeans_assignment (assign_clusters kmeans_data kmeans_centroids)) (set! kmeans_centroids (revise_centroids kmeans_data kmeans_k kmeans_assignment)) (set! kmeans_h (compute_heterogeneity kmeans_data kmeans_centroids kmeans_assignment)) (set! kmeans_heterogeneity (conj kmeans_heterogeneity kmeans_h)) (cond (and (> kmeans_iter 0) (lists_equal kmeans_prev kmeans_assignment)) (recur false) :else (do (set! kmeans_prev kmeans_assignment) (set! kmeans_iter (+ kmeans_iter 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v {:assignments kmeans_assignment :centroids kmeans_centroids :heterogeneity kmeans_heterogeneity}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_data [[1.0 2.0] [1.5 1.8] [5.0 8.0] [8.0 8.0] [1.0 0.6] [9.0 11.0]])

(def ^:dynamic main_k 3)

(def ^:dynamic main_initial_centroids [(nth main_data 0) (nth main_data 2) (nth main_data 5)])

(def ^:dynamic main_result (kmeans main_data main_k main_initial_centroids 10))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (:centroids main_result)))
      (println (str (:assignments main_result)))
      (println (str (:heterogeneity main_result)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
