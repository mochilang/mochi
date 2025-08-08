(ns main (:refer-clojure :exclude [sort_points build_kdtree]))

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

(declare sort_points build_kdtree)

(def ^:dynamic build_kdtree_axis nil)

(def ^:dynamic build_kdtree_idx nil)

(def ^:dynamic build_kdtree_k nil)

(def ^:dynamic build_kdtree_left_idx nil)

(def ^:dynamic build_kdtree_left_points nil)

(def ^:dynamic build_kdtree_median_idx nil)

(def ^:dynamic build_kdtree_node nil)

(def ^:dynamic build_kdtree_right_idx nil)

(def ^:dynamic build_kdtree_right_points nil)

(def ^:dynamic build_kdtree_sorted nil)

(def ^:dynamic sort_points_arr nil)

(def ^:dynamic sort_points_i nil)

(def ^:dynamic sort_points_j nil)

(def ^:dynamic sort_points_tmp nil)

(def ^:dynamic main_tree [])

(defn sort_points [sort_points_points sort_points_axis]
  (binding [sort_points_arr nil sort_points_i nil sort_points_j nil sort_points_tmp nil] (try (do (set! sort_points_arr sort_points_points) (set! sort_points_i 0) (while (< sort_points_i (count sort_points_arr)) (do (set! sort_points_j 0) (while (< sort_points_j (- (count sort_points_arr) 1)) (do (when (> (nth (nth sort_points_arr sort_points_j) sort_points_axis) (nth (nth sort_points_arr (+ sort_points_j 1)) sort_points_axis)) (do (set! sort_points_tmp (nth sort_points_arr sort_points_j)) (set! sort_points_arr (assoc sort_points_arr sort_points_j (nth sort_points_arr (+ sort_points_j 1)))) (set! sort_points_arr (assoc sort_points_arr (+ sort_points_j 1) sort_points_tmp)))) (set! sort_points_j (+ sort_points_j 1)))) (set! sort_points_i (+ sort_points_i 1)))) (throw (ex-info "return" {:v sort_points_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn build_kdtree [build_kdtree_points build_kdtree_depth]
  (binding [build_kdtree_axis nil build_kdtree_idx nil build_kdtree_k nil build_kdtree_left_idx nil build_kdtree_left_points nil build_kdtree_median_idx nil build_kdtree_node nil build_kdtree_right_idx nil build_kdtree_right_points nil build_kdtree_sorted nil] (try (do (when (= (count build_kdtree_points) 0) (throw (ex-info "return" {:v (- 0 1)}))) (set! build_kdtree_k (count (nth build_kdtree_points 0))) (set! build_kdtree_axis (mod build_kdtree_depth build_kdtree_k)) (set! build_kdtree_sorted (sort_points build_kdtree_points build_kdtree_axis)) (set! build_kdtree_median_idx (quot (count build_kdtree_sorted) 2)) (set! build_kdtree_left_points (subvec build_kdtree_sorted 0 build_kdtree_median_idx)) (set! build_kdtree_right_points (subvec build_kdtree_sorted (+ build_kdtree_median_idx 1) (count build_kdtree_sorted))) (set! build_kdtree_idx (count main_tree)) (alter-var-root (var main_tree) (fn [_] (conj main_tree {:left (- 0 1) :point (nth build_kdtree_sorted build_kdtree_median_idx) :right (- 0 1)}))) (set! build_kdtree_left_idx (build_kdtree build_kdtree_left_points (+ build_kdtree_depth 1))) (set! build_kdtree_right_idx (build_kdtree build_kdtree_right_points (+ build_kdtree_depth 1))) (set! build_kdtree_node (nth main_tree build_kdtree_idx)) (set! build_kdtree_node (assoc build_kdtree_node :left build_kdtree_left_idx)) (set! build_kdtree_node (assoc build_kdtree_node :right build_kdtree_right_idx)) (alter-var-root (var main_tree) (fn [_] (assoc main_tree build_kdtree_idx build_kdtree_node))) (throw (ex-info "return" {:v build_kdtree_idx}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_pts [[2.0 3.0] [5.0 4.0] [9.0 6.0] [4.0 7.0] [8.0 1.0] [7.0 2.0]])

(def ^:dynamic main_root (build_kdtree main_pts 0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_tree))
      (println main_root)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
