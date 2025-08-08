(ns main (:refer-clojure :exclude [distance_sq mean k_means main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare distance_sq mean k_means main)

(def ^:dynamic distance_sq_diff nil)

(def ^:dynamic distance_sq_i nil)

(def ^:dynamic distance_sq_sum nil)

(def ^:dynamic k_means_assignments nil)

(def ^:dynamic k_means_best nil)

(def ^:dynamic k_means_bestDist nil)

(def ^:dynamic k_means_c nil)

(def ^:dynamic k_means_cIdx nil)

(def ^:dynamic k_means_centroids nil)

(def ^:dynamic k_means_cluster nil)

(def ^:dynamic k_means_d nil)

(def ^:dynamic k_means_i nil)

(def ^:dynamic k_means_it nil)

(def ^:dynamic k_means_n nil)

(def ^:dynamic k_means_v nil)

(def ^:dynamic k_means_v2 nil)

(def ^:dynamic main_result nil)

(def ^:dynamic main_vectors nil)

(def ^:dynamic mean_dim nil)

(def ^:dynamic mean_i nil)

(def ^:dynamic mean_j nil)

(def ^:dynamic mean_res nil)

(def ^:dynamic mean_total nil)

(defn distance_sq [distance_sq_a distance_sq_b]
  (binding [distance_sq_diff nil distance_sq_i nil distance_sq_sum nil] (try (do (set! distance_sq_sum 0.0) (set! distance_sq_i 0) (while (< distance_sq_i (count distance_sq_a)) (do (set! distance_sq_diff (- (nth distance_sq_a distance_sq_i) (nth distance_sq_b distance_sq_i))) (set! distance_sq_sum (+ distance_sq_sum (* distance_sq_diff distance_sq_diff))) (set! distance_sq_i (+ distance_sq_i 1)))) (throw (ex-info "return" {:v distance_sq_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mean [mean_vectors]
  (binding [mean_dim nil mean_i nil mean_j nil mean_res nil mean_total nil] (try (do (set! mean_dim (count (nth mean_vectors 0))) (set! mean_res []) (set! mean_i 0) (while (< mean_i mean_dim) (do (set! mean_total 0.0) (set! mean_j 0) (while (< mean_j (count mean_vectors)) (do (set! mean_total (+ mean_total (nth (nth mean_vectors mean_j) mean_i))) (set! mean_j (+ mean_j 1)))) (set! mean_res (conj mean_res (quot mean_total (count mean_vectors)))) (set! mean_i (+ mean_i 1)))) (throw (ex-info "return" {:v mean_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn k_means [k_means_vectors k_means_k k_means_iterations]
  (binding [k_means_assignments nil k_means_best nil k_means_bestDist nil k_means_c nil k_means_cIdx nil k_means_centroids nil k_means_cluster nil k_means_d nil k_means_i nil k_means_it nil k_means_n nil k_means_v nil k_means_v2 nil] (try (do (set! k_means_centroids []) (set! k_means_i 0) (while (< k_means_i k_means_k) (do (set! k_means_centroids (conj k_means_centroids (nth k_means_vectors k_means_i))) (set! k_means_i (+ k_means_i 1)))) (set! k_means_assignments []) (set! k_means_n (count k_means_vectors)) (set! k_means_i 0) (while (< k_means_i k_means_n) (do (set! k_means_assignments (conj k_means_assignments 0)) (set! k_means_i (+ k_means_i 1)))) (set! k_means_it 0) (while (< k_means_it k_means_iterations) (do (set! k_means_v 0) (while (< k_means_v k_means_n) (do (set! k_means_best 0) (set! k_means_bestDist (distance_sq (nth k_means_vectors k_means_v) (nth k_means_centroids 0))) (set! k_means_c 1) (while (< k_means_c k_means_k) (do (set! k_means_d (distance_sq (nth k_means_vectors k_means_v) (nth k_means_centroids k_means_c))) (when (< k_means_d k_means_bestDist) (do (set! k_means_bestDist k_means_d) (set! k_means_best k_means_c))) (set! k_means_c (+ k_means_c 1)))) (set! k_means_assignments (assoc k_means_assignments k_means_v k_means_best)) (set! k_means_v (+ k_means_v 1)))) (set! k_means_cIdx 0) (while (< k_means_cIdx k_means_k) (do (set! k_means_cluster []) (set! k_means_v2 0) (while (< k_means_v2 k_means_n) (do (when (= (nth k_means_assignments k_means_v2) k_means_cIdx) (set! k_means_cluster (conj k_means_cluster (nth k_means_vectors k_means_v2)))) (set! k_means_v2 (+ k_means_v2 1)))) (when (> (count k_means_cluster) 0) (set! k_means_centroids (assoc k_means_centroids k_means_cIdx (mean k_means_cluster)))) (set! k_means_cIdx (+ k_means_cIdx 1)))) (set! k_means_it (+ k_means_it 1)))) (throw (ex-info "return" {:v {:assignments k_means_assignments :centroids k_means_centroids}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_result nil main_vectors nil] (do (set! main_vectors [[1.0 2.0] [1.5 1.8] [5.0 8.0] [8.0 8.0] [1.0 0.6] [9.0 11.0]]) (set! main_result (k_means main_vectors 2 5)) (println (str (:centroids main_result))) (println (str (:assignments main_result))))))

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
