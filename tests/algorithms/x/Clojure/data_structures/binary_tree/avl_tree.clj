(ns main (:refer-clojure :exclude [new_node get_height my_max update_height right_rotation left_rotation lr_rotation rl_rotation insert_node get_left_most del_node inorder main]))

(require 'clojure.set)

(defrecord Node [data left right height])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def ^:dynamic del_node_i nil)

(def ^:dynamic del_node_lh nil)

(def ^:dynamic del_node_nodes nil)

(def ^:dynamic del_node_rh nil)

(def ^:dynamic del_node_temp nil)

(def ^:dynamic get_left_most_cur nil)

(def ^:dynamic inorder_left nil)

(def ^:dynamic inorder_res nil)

(def ^:dynamic inorder_right nil)

(def ^:dynamic insert_node_i nil)

(def ^:dynamic insert_node_nodes nil)

(def ^:dynamic left_rotation_nodes nil)

(def ^:dynamic left_rotation_right nil)

(def ^:dynamic lr_rotation_nodes nil)

(def ^:dynamic main_nodes nil)

(def ^:dynamic main_root nil)

(def ^:dynamic new_node_node nil)

(def ^:dynamic new_node_nodes nil)

(def ^:dynamic right_rotation_left nil)

(def ^:dynamic right_rotation_nodes nil)

(def ^:dynamic rl_rotation_nodes nil)

(def ^:dynamic update_height_nodes nil)

(declare new_node get_height my_max update_height right_rotation left_rotation lr_rotation rl_rotation insert_node get_left_most del_node inorder main)

(def ^:dynamic main_NIL (- 0 1))

(def ^:dynamic main_nodes [])

(defn new_node [new_node_value]
  (binding [new_node_node nil new_node_nodes nil] (try (do (set! new_node_node {"data" new_node_value "left" main_NIL "right" main_NIL "height" 1}) (set! new_node_nodes (conj main_nodes new_node_node)) (throw (ex-info "return" {:v (- (count new_node_nodes) 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_height [get_height_i]
  (try (if (= get_height_i main_NIL) 0 (get (nth main_nodes get_height_i) "height")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn my_max [my_max_a my_max_b]
  (try (if (> my_max_a my_max_b) my_max_a my_max_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn update_height [update_height_i]
  (binding [update_height_nodes nil] (set! update_height_nodes (assoc-in update_height_nodes [update_height_i "height"] (+ (my_max (get_height (get (nth main_nodes update_height_i) "left")) (get_height (get (nth main_nodes update_height_i) "right"))) 1)))))

(defn right_rotation [right_rotation_i]
  (binding [right_rotation_left nil right_rotation_nodes nil] (try (do (set! right_rotation_left (get (nth main_nodes right_rotation_i) "left")) (set! right_rotation_nodes (assoc-in right_rotation_nodes [right_rotation_i "left"] (get (nth main_nodes right_rotation_left) "right"))) (set! right_rotation_nodes (assoc-in right_rotation_nodes [right_rotation_left "right"] right_rotation_i)) (update_height right_rotation_i) (update_height right_rotation_left) (throw (ex-info "return" {:v right_rotation_left}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn left_rotation [left_rotation_i]
  (binding [left_rotation_nodes nil left_rotation_right nil] (try (do (set! left_rotation_right (get (nth main_nodes left_rotation_i) "right")) (set! left_rotation_nodes (assoc-in left_rotation_nodes [left_rotation_i "right"] (get (nth main_nodes left_rotation_right) "left"))) (set! left_rotation_nodes (assoc-in left_rotation_nodes [left_rotation_right "left"] left_rotation_i)) (update_height left_rotation_i) (update_height left_rotation_right) (throw (ex-info "return" {:v left_rotation_right}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lr_rotation [lr_rotation_i]
  (binding [lr_rotation_nodes nil] (try (do (set! lr_rotation_nodes (assoc-in lr_rotation_nodes [lr_rotation_i "left"] (left_rotation (get (nth main_nodes lr_rotation_i) "left")))) (throw (ex-info "return" {:v (right_rotation lr_rotation_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rl_rotation [rl_rotation_i]
  (binding [rl_rotation_nodes nil] (try (do (set! rl_rotation_nodes (assoc-in rl_rotation_nodes [rl_rotation_i "right"] (right_rotation (get (nth main_nodes rl_rotation_i) "right")))) (throw (ex-info "return" {:v (left_rotation rl_rotation_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert_node [insert_node_i_p insert_node_value]
  (binding [insert_node_i nil insert_node_nodes nil] (try (do (set! insert_node_i insert_node_i_p) (when (= insert_node_i main_NIL) (throw (ex-info "return" {:v (new_node insert_node_value)}))) (if (< insert_node_value (get (nth main_nodes insert_node_i) "data")) (do (set! insert_node_nodes (assoc-in insert_node_nodes [insert_node_i "left"] (insert_node (get (nth main_nodes insert_node_i) "left") insert_node_value))) (when (= (- (get_height (get (nth insert_node_nodes insert_node_i) "left")) (get_height (get (nth insert_node_nodes insert_node_i) "right"))) 2) (if (< insert_node_value (get (nth insert_node_nodes (get (nth insert_node_nodes insert_node_i) "left")) "data")) (set! insert_node_i (right_rotation insert_node_i)) (set! insert_node_i (lr_rotation insert_node_i))))) (do (set! insert_node_nodes (assoc-in insert_node_nodes [insert_node_i "right"] (insert_node (get (nth insert_node_nodes insert_node_i) "right") insert_node_value))) (when (= (- (get_height (get (nth insert_node_nodes insert_node_i) "right")) (get_height (get (nth insert_node_nodes insert_node_i) "left"))) 2) (if (< insert_node_value (get (nth insert_node_nodes (get (nth insert_node_nodes insert_node_i) "right")) "data")) (set! insert_node_i (rl_rotation insert_node_i)) (set! insert_node_i (left_rotation insert_node_i)))))) (update_height insert_node_i) (throw (ex-info "return" {:v insert_node_i}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_left_most [get_left_most_i]
  (binding [get_left_most_cur nil] (try (do (set! get_left_most_cur get_left_most_i) (while (not= (get (nth main_nodes get_left_most_cur) "left") main_NIL) (set! get_left_most_cur (get (nth main_nodes get_left_most_cur) "left"))) (throw (ex-info "return" {:v (get (nth main_nodes get_left_most_cur) "data")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn del_node [del_node_i_p del_node_value]
  (binding [del_node_i nil del_node_lh nil del_node_nodes nil del_node_rh nil del_node_temp nil] (try (do (set! del_node_i del_node_i_p) (when (= del_node_i main_NIL) (throw (ex-info "return" {:v main_NIL}))) (if (< del_node_value (get (nth main_nodes del_node_i) "data")) (set! del_node_nodes (assoc-in del_node_nodes [del_node_i "left"] (del_node (get (nth main_nodes del_node_i) "left") del_node_value))) (if (> del_node_value (get (nth del_node_nodes del_node_i) "data")) (set! del_node_nodes (assoc-in del_node_nodes [del_node_i "right"] (del_node (get (nth del_node_nodes del_node_i) "right") del_node_value))) (if (and (not= (get (nth del_node_nodes del_node_i) "left") main_NIL) (not= (get (nth del_node_nodes del_node_i) "right") main_NIL)) (do (set! del_node_temp (get_left_most (get (nth del_node_nodes del_node_i) "right"))) (set! del_node_nodes (assoc-in del_node_nodes [del_node_i "data"] del_node_temp)) (set! del_node_nodes (assoc-in del_node_nodes [del_node_i "right"] (del_node (get (nth del_node_nodes del_node_i) "right") del_node_temp)))) (if (not= (get (nth del_node_nodes del_node_i) "left") main_NIL) (set! del_node_i (get (nth del_node_nodes del_node_i) "left")) (set! del_node_i (get (nth del_node_nodes del_node_i) "right")))))) (when (= del_node_i main_NIL) (throw (ex-info "return" {:v main_NIL}))) (set! del_node_lh (get_height (get (nth del_node_nodes del_node_i) "left"))) (set! del_node_rh (get_height (get (nth del_node_nodes del_node_i) "right"))) (if (= (- del_node_rh del_node_lh) 2) (if (> (get_height (get (nth del_node_nodes (get (nth del_node_nodes del_node_i) "right")) "right")) (get_height (get (nth del_node_nodes (get (nth del_node_nodes del_node_i) "right")) "left"))) (set! del_node_i (left_rotation del_node_i)) (set! del_node_i (rl_rotation del_node_i))) (when (= (- del_node_lh del_node_rh) 2) (if (> (get_height (get (nth del_node_nodes (get (nth del_node_nodes del_node_i) "left")) "left")) (get_height (get (nth del_node_nodes (get (nth del_node_nodes del_node_i) "left")) "right"))) (set! del_node_i (right_rotation del_node_i)) (set! del_node_i (lr_rotation del_node_i))))) (update_height del_node_i) (throw (ex-info "return" {:v del_node_i}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn inorder [inorder_i]
  (binding [inorder_left nil inorder_res nil inorder_right nil] (try (do (when (= inorder_i main_NIL) (throw (ex-info "return" {:v ""}))) (set! inorder_left (inorder (get (nth main_nodes inorder_i) "left"))) (set! inorder_right (inorder (get (nth main_nodes inorder_i) "right"))) (set! inorder_res (str (get (nth main_nodes inorder_i) "data"))) (when (not= inorder_left "") (set! inorder_res (str (str inorder_left " ") inorder_res))) (when (not= inorder_right "") (set! inorder_res (str (str inorder_res " ") inorder_right))) (throw (ex-info "return" {:v inorder_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_nodes nil main_root nil] (do (set! main_nodes []) (set! main_root main_NIL) (set! main_root (insert_node main_root 4)) (set! main_root (insert_node main_root 2)) (set! main_root (insert_node main_root 3)) (println (inorder main_root)) (println (str (get_height main_root))) (set! main_root (del_node main_root 3)) (println (inorder main_root)))))

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
