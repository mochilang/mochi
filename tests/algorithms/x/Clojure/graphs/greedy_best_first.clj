(ns main (:refer-clojure :exclude [abs manhattan clone_path make_node node_equal contains sort_nodes get_successors greedy_best_first print_grid main]))

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

(declare abs manhattan clone_path make_node node_equal contains sort_nodes get_successors greedy_best_first print_grid main)

(def ^:dynamic clone_path_i nil)

(def ^:dynamic clone_path_res nil)

(def ^:dynamic contains_i nil)

(def ^:dynamic get_successors_d nil)

(def ^:dynamic get_successors_i nil)

(def ^:dynamic get_successors_new_path nil)

(def ^:dynamic get_successors_pos_x nil)

(def ^:dynamic get_successors_pos_y nil)

(def ^:dynamic get_successors_res nil)

(def ^:dynamic greedy_best_first_child nil)

(def ^:dynamic greedy_best_first_closed_nodes nil)

(def ^:dynamic greedy_best_first_current nil)

(def ^:dynamic greedy_best_first_i nil)

(def ^:dynamic greedy_best_first_idx nil)

(def ^:dynamic greedy_best_first_new_open nil)

(def ^:dynamic greedy_best_first_open_nodes nil)

(def ^:dynamic greedy_best_first_r nil)

(def ^:dynamic greedy_best_first_start nil)

(def ^:dynamic greedy_best_first_start_path nil)

(def ^:dynamic greedy_best_first_successors nil)

(def ^:dynamic main_goal nil)

(def ^:dynamic main_grid nil)

(def ^:dynamic main_idx nil)

(def ^:dynamic main_init nil)

(def ^:dynamic main_j nil)

(def ^:dynamic main_p nil)

(def ^:dynamic main_path nil)

(def ^:dynamic make_node_f nil)

(def ^:dynamic print_grid_i nil)

(def ^:dynamic sort_nodes_arr nil)

(def ^:dynamic sort_nodes_i nil)

(def ^:dynamic sort_nodes_j nil)

(def ^:dynamic sort_nodes_key_node nil)

(def ^:dynamic sort_nodes_temp nil)

(defn abs [abs_x]
  (try (if (< abs_x 0) (- 0 abs_x) abs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn manhattan [manhattan_x1 manhattan_y1 manhattan_x2 manhattan_y2]
  (try (throw (ex-info "return" {:v (+ (abs (- manhattan_x1 manhattan_x2)) (abs (- manhattan_y1 manhattan_y2)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn clone_path [clone_path_p]
  (binding [clone_path_i nil clone_path_res nil] (try (do (set! clone_path_res []) (set! clone_path_i 0) (while (< clone_path_i (count clone_path_p)) (do (set! clone_path_res (conj clone_path_res (nth clone_path_p clone_path_i))) (set! clone_path_i (+ clone_path_i 1)))) (throw (ex-info "return" {:v clone_path_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_node [make_node_pos_x make_node_pos_y make_node_goal_x make_node_goal_y make_node_g_cost make_node_path]
  (binding [make_node_f nil] (try (do (set! make_node_f (manhattan make_node_pos_x make_node_pos_y make_node_goal_x make_node_goal_y)) (throw (ex-info "return" {:v {:f_cost make_node_f :g_cost make_node_g_cost :goal_x make_node_goal_x :goal_y make_node_goal_y :path make_node_path :pos_x make_node_pos_x :pos_y make_node_pos_y}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_delta [{:x 0 :y (- 1)} {:x (- 1) :y 0} {:x 0 :y 1} {:x 1 :y 0}])

(defn node_equal [node_equal_a node_equal_b]
  (try (throw (ex-info "return" {:v (and (= (:pos_x node_equal_a) (:pos_x node_equal_b)) (= (:pos_y node_equal_a) (:pos_y node_equal_b)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn contains [contains_nodes contains_node]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_nodes)) (do (when (node_equal (nth contains_nodes contains_i) contains_node) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_nodes [sort_nodes_nodes]
  (binding [sort_nodes_arr nil sort_nodes_i nil sort_nodes_j nil sort_nodes_key_node nil sort_nodes_temp nil] (try (do (set! sort_nodes_arr sort_nodes_nodes) (set! sort_nodes_i 1) (while (< sort_nodes_i (count sort_nodes_arr)) (do (set! sort_nodes_key_node (nth sort_nodes_arr sort_nodes_i)) (set! sort_nodes_j (- sort_nodes_i 1)) (loop [while_flag_1 true] (when (and while_flag_1 (>= sort_nodes_j 0)) (do (set! sort_nodes_temp (nth sort_nodes_arr sort_nodes_j)) (if (> (:f_cost sort_nodes_temp) (:f_cost sort_nodes_key_node)) (do (set! sort_nodes_arr (assoc sort_nodes_arr (+ sort_nodes_j 1) sort_nodes_temp)) (set! sort_nodes_j (- sort_nodes_j 1)) (recur while_flag_1)) (recur false))))) (set! sort_nodes_arr (assoc sort_nodes_arr (+ sort_nodes_j 1) sort_nodes_key_node)) (set! sort_nodes_i (+ sort_nodes_i 1)))) (throw (ex-info "return" {:v sort_nodes_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_successors [get_successors_grid get_successors_parent get_successors_target]
  (binding [get_successors_d nil get_successors_i nil get_successors_new_path nil get_successors_pos_x nil get_successors_pos_y nil get_successors_res nil] (try (do (set! get_successors_res []) (set! get_successors_i 0) (while (< get_successors_i (count main_delta)) (do (set! get_successors_d (nth main_delta get_successors_i)) (set! get_successors_pos_x (+ (:pos_x get_successors_parent) (:x get_successors_d))) (set! get_successors_pos_y (+ (:pos_y get_successors_parent) (:y get_successors_d))) (when (and (and (and (and (>= get_successors_pos_x 0) (< get_successors_pos_x (count (nth get_successors_grid 0)))) (>= get_successors_pos_y 0)) (< get_successors_pos_y (count get_successors_grid))) (= (nth (nth get_successors_grid get_successors_pos_y) get_successors_pos_x) 0)) (do (set! get_successors_new_path (clone_path (:path get_successors_parent))) (set! get_successors_new_path (conj get_successors_new_path {:x get_successors_pos_x :y get_successors_pos_y})) (set! get_successors_res (conj get_successors_res (make_node get_successors_pos_x get_successors_pos_y (:x get_successors_target) (:y get_successors_target) (+ (:g_cost get_successors_parent) 1) get_successors_new_path))))) (set! get_successors_i (+ get_successors_i 1)))) (throw (ex-info "return" {:v get_successors_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn greedy_best_first [greedy_best_first_grid greedy_best_first_init greedy_best_first_goal]
  (binding [greedy_best_first_child nil greedy_best_first_closed_nodes nil greedy_best_first_current nil greedy_best_first_i nil greedy_best_first_idx nil greedy_best_first_new_open nil greedy_best_first_open_nodes nil greedy_best_first_r nil greedy_best_first_start nil greedy_best_first_start_path nil greedy_best_first_successors nil] (try (do (set! greedy_best_first_start_path [greedy_best_first_init]) (set! greedy_best_first_start (make_node (:x greedy_best_first_init) (:y greedy_best_first_init) (:x greedy_best_first_goal) (:y greedy_best_first_goal) 0 greedy_best_first_start_path)) (set! greedy_best_first_open_nodes [greedy_best_first_start]) (set! greedy_best_first_closed_nodes []) (while (> (count greedy_best_first_open_nodes) 0) (do (set! greedy_best_first_open_nodes (sort_nodes greedy_best_first_open_nodes)) (set! greedy_best_first_current (nth greedy_best_first_open_nodes 0)) (set! greedy_best_first_new_open []) (set! greedy_best_first_idx 1) (while (< greedy_best_first_idx (count greedy_best_first_open_nodes)) (do (set! greedy_best_first_new_open (conj greedy_best_first_new_open (nth greedy_best_first_open_nodes greedy_best_first_idx))) (set! greedy_best_first_idx (+ greedy_best_first_idx 1)))) (set! greedy_best_first_open_nodes greedy_best_first_new_open) (when (and (= (:pos_x greedy_best_first_current) (:x greedy_best_first_goal)) (= (:pos_y greedy_best_first_current) (:y greedy_best_first_goal))) (throw (ex-info "return" {:v (:path greedy_best_first_current)}))) (set! greedy_best_first_closed_nodes (conj greedy_best_first_closed_nodes greedy_best_first_current)) (set! greedy_best_first_successors (get_successors greedy_best_first_grid greedy_best_first_current greedy_best_first_goal)) (set! greedy_best_first_i 0) (while (< greedy_best_first_i (count greedy_best_first_successors)) (do (set! greedy_best_first_child (nth greedy_best_first_successors greedy_best_first_i)) (when (and (not (contains greedy_best_first_closed_nodes greedy_best_first_child)) (not (contains greedy_best_first_open_nodes greedy_best_first_child))) (set! greedy_best_first_open_nodes (conj greedy_best_first_open_nodes greedy_best_first_child))) (set! greedy_best_first_i (+ greedy_best_first_i 1)))))) (set! greedy_best_first_r [greedy_best_first_init]) (throw (ex-info "return" {:v greedy_best_first_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_TEST_GRIDS [[[0 0 0 0 0 0 0] [0 1 0 0 0 0 0] [0 0 0 0 0 0 0] [0 0 1 0 0 0 0] [1 0 1 0 0 0 0] [0 0 0 0 0 0 0] [0 0 0 0 1 0 0]] [[0 0 0 1 1 0 0] [0 0 0 0 1 0 1] [0 0 0 1 1 0 0] [0 1 0 0 1 0 0] [1 0 0 1 1 0 1] [0 0 0 0 0 0 0]] [[0 0 1 0 0] [0 1 0 0 0] [0 0 1 0 1] [1 0 0 1 1] [0 0 0 0 0]]])

(defn print_grid [print_grid_grid]
  (binding [print_grid_i nil] (do (set! print_grid_i 0) (while (< print_grid_i (count print_grid_grid)) (do (println (str (nth print_grid_grid print_grid_i))) (set! print_grid_i (+ print_grid_i 1)))) print_grid_grid)))

(defn main []
  (binding [main_goal nil main_grid nil main_idx nil main_init nil main_j nil main_p nil main_path nil] (do (set! main_idx 0) (while (< main_idx (count main_TEST_GRIDS)) (do (println (str (str "==grid-" (str (+ main_idx 1))) "==")) (set! main_grid (nth main_TEST_GRIDS main_idx)) (set! main_init {:x 0 :y 0}) (set! main_goal {:x (- (count (nth main_grid 0)) 1) :y (- (count main_grid) 1)}) (set! main_grid (print_grid main_grid)) (println "------") (set! main_path (greedy_best_first main_grid main_init main_goal)) (set! main_j 0) (while (< main_j (count main_path)) (do (set! main_p (nth main_path main_j)) (set! main_grid (assoc-in main_grid [(:y main_p) (:x main_p)] 2)) (set! main_j (+ main_j 1)))) (set! main_grid (print_grid main_grid)) (set! main_idx (+ main_idx 1)))))))

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
