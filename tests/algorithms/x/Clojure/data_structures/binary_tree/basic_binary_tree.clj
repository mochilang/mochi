(ns main (:refer-clojure :exclude [inorder size depth is_full small_tree medium_tree]))

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

(declare inorder size depth is_full small_tree medium_tree)

(def ^:dynamic depth_left_depth nil)

(def ^:dynamic depth_node nil)

(def ^:dynamic depth_right_depth nil)

(def ^:dynamic inorder_node nil)

(def ^:dynamic inorder_res nil)

(def ^:dynamic is_full_node nil)

(def ^:dynamic medium_tree_arr nil)

(def ^:dynamic size_node nil)

(def ^:dynamic small_tree_arr nil)

(defn inorder [inorder_nodes inorder_index inorder_acc]
  (binding [inorder_node nil inorder_res nil] (try (do (when (= inorder_index (- 0 1)) (throw (ex-info "return" {:v inorder_acc}))) (set! inorder_node (nth inorder_nodes inorder_index)) (set! inorder_res (inorder inorder_nodes (:left inorder_node) inorder_acc)) (set! inorder_res (conj inorder_res (:data inorder_node))) (set! inorder_res (inorder inorder_nodes (:right inorder_node) inorder_res)) (throw (ex-info "return" {:v inorder_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn size [size_nodes size_index]
  (binding [size_node nil] (try (do (when (= size_index (- 0 1)) (throw (ex-info "return" {:v 0}))) (set! size_node (nth size_nodes size_index)) (throw (ex-info "return" {:v (+ (+ 1 (size size_nodes (:left size_node))) (size size_nodes (:right size_node)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn depth [depth_nodes depth_index]
  (binding [depth_left_depth nil depth_node nil depth_right_depth nil] (try (do (when (= depth_index (- 0 1)) (throw (ex-info "return" {:v 0}))) (set! depth_node (nth depth_nodes depth_index)) (set! depth_left_depth (depth depth_nodes (:left depth_node))) (set! depth_right_depth (depth depth_nodes (:right depth_node))) (if (> depth_left_depth depth_right_depth) (+ depth_left_depth 1) (+ depth_right_depth 1))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_full [is_full_nodes is_full_index]
  (binding [is_full_node nil] (try (do (when (= is_full_index (- 0 1)) (throw (ex-info "return" {:v true}))) (set! is_full_node (nth is_full_nodes is_full_index)) (when (and (= (:left is_full_node) (- 0 1)) (= (:right is_full_node) (- 0 1))) (throw (ex-info "return" {:v true}))) (if (and (not= (:left is_full_node) (- 0 1)) (not= (:right is_full_node) (- 0 1))) (and (is_full is_full_nodes (:left is_full_node)) (is_full is_full_nodes (:right is_full_node))) false)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn small_tree []
  (binding [small_tree_arr nil] (try (do (set! small_tree_arr []) (set! small_tree_arr (conj small_tree_arr {:data 2 :left 1 :right 2})) (set! small_tree_arr (conj small_tree_arr {:data 1 :left (- 0 1) :right (- 0 1)})) (set! small_tree_arr (conj small_tree_arr {:data 3 :left (- 0 1) :right (- 0 1)})) (throw (ex-info "return" {:v small_tree_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn medium_tree []
  (binding [medium_tree_arr nil] (try (do (set! medium_tree_arr []) (set! medium_tree_arr (conj medium_tree_arr {:data 4 :left 1 :right 4})) (set! medium_tree_arr (conj medium_tree_arr {:data 2 :left 2 :right 3})) (set! medium_tree_arr (conj medium_tree_arr {:data 1 :left (- 0 1) :right (- 0 1)})) (set! medium_tree_arr (conj medium_tree_arr {:data 3 :left (- 0 1) :right (- 0 1)})) (set! medium_tree_arr (conj medium_tree_arr {:data 5 :left (- 0 1) :right 5})) (set! medium_tree_arr (conj medium_tree_arr {:data 6 :left (- 0 1) :right 6})) (set! medium_tree_arr (conj medium_tree_arr {:data 7 :left (- 0 1) :right (- 0 1)})) (throw (ex-info "return" {:v medium_tree_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_small (small_tree))

(def ^:dynamic main_medium (medium_tree))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (size main_small 0))
      (println (inorder main_small 0 []))
      (println (depth main_small 0))
      (println (is_full main_small 0))
      (println (size main_medium 0))
      (println (inorder main_medium 0 []))
      (println (depth main_medium 0))
      (println (is_full main_medium 0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
