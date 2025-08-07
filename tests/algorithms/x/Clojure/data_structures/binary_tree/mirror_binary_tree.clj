(ns main (:refer-clojure :exclude [mirror_node mirror inorder make_tree_zero make_tree_seven make_tree_nine main]))

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

(declare mirror_node mirror inorder make_tree_zero make_tree_seven make_tree_nine main)

(def ^:dynamic inorder_left_vals nil)

(def ^:dynamic inorder_right_vals nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_mirrored nil)

(def ^:dynamic main_names nil)

(def ^:dynamic main_tree nil)

(def ^:dynamic main_trees nil)

(def ^:dynamic mirror_node_left nil)

(def ^:dynamic mirror_node_right nil)

(def ^:dynamic mirror_node_temp nil)

(defn mirror_node [mirror_node_left_p mirror_node_right_p mirror_node_idx]
  (binding [mirror_node_left nil mirror_node_right nil mirror_node_temp nil] (try (do (set! mirror_node_left mirror_node_left_p) (set! mirror_node_right mirror_node_right_p) (when (= mirror_node_idx (- 1)) (throw (ex-info "return" {:v nil}))) (set! mirror_node_temp (nth mirror_node_left mirror_node_idx)) (set! mirror_node_left (assoc mirror_node_left mirror_node_idx (nth mirror_node_right mirror_node_idx))) (set! mirror_node_right (assoc mirror_node_right mirror_node_idx mirror_node_temp)) (mirror_node mirror_node_left mirror_node_right (nth mirror_node_left mirror_node_idx)) (mirror_node mirror_node_left mirror_node_right (nth mirror_node_right mirror_node_idx))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mirror [mirror_tree]
  (try (do (mirror_node (:left mirror_tree) (:right mirror_tree) (:root mirror_tree)) (throw (ex-info "return" {:v mirror_tree}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn inorder [inorder_tree inorder_idx]
  (binding [inorder_left_vals nil inorder_right_vals nil] (try (do (when (= inorder_idx (- 1)) (throw (ex-info "return" {:v []}))) (set! inorder_left_vals (inorder inorder_tree (get (:left inorder_tree) inorder_idx))) (set! inorder_right_vals (inorder inorder_tree (get (:right inorder_tree) inorder_idx))) (throw (ex-info "return" {:v (concat (concat inorder_left_vals [(get (:values inorder_tree) inorder_idx)]) inorder_right_vals)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_tree_zero []
  (try (throw (ex-info "return" {:v {:values [0] :left [(- 1)] :right [(- 1)] :root 0}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn make_tree_seven []
  (try (throw (ex-info "return" {:v {:values [1 2 3 4 5 6 7] :left [1 3 5 (- 1) (- 1) (- 1) (- 1)] :right [2 4 6 (- 1) (- 1) (- 1) (- 1)] :root 0}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn make_tree_nine []
  (try (throw (ex-info "return" {:v {:values [1 2 3 4 5 6 7 8 9] :left [1 3 (- 1) 6 (- 1) (- 1) (- 1) (- 1) (- 1)] :right [2 4 5 7 8 (- 1) (- 1) (- 1) (- 1)] :root 0}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_i nil main_mirrored nil main_names nil main_tree nil main_trees nil] (do (set! main_names ["zero" "seven" "nine"]) (set! main_trees [(make_tree_zero) (make_tree_seven) (make_tree_nine)]) (set! main_i 0) (while (< main_i (count main_trees)) (do (set! main_tree (nth main_trees main_i)) (println (str (str (str "      The " (nth main_names main_i)) " tree: ") (str (inorder main_tree (:root main_tree))))) (set! main_mirrored (mirror main_tree)) (println (str (str (str "Mirror of " (nth main_names main_i)) " tree: ") (str (inorder main_mirrored (:root main_mirrored))))) (set! main_i (+ main_i 1)))))))

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
