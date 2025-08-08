(ns main (:refer-clojure :exclude [expand_search construct_path reverse_list bidirectional_search is_edge path_exists print_path main]))

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

(declare expand_search construct_path reverse_list bidirectional_search is_edge path_exists print_path main)

(def ^:dynamic bidirectional_search_back_start nil)

(def ^:dynamic bidirectional_search_backward_head nil)

(def ^:dynamic bidirectional_search_backward_parents nil)

(def ^:dynamic bidirectional_search_backward_path nil)

(def ^:dynamic bidirectional_search_backward_queue nil)

(def ^:dynamic bidirectional_search_backward_visited nil)

(def ^:dynamic bidirectional_search_forward_head nil)

(def ^:dynamic bidirectional_search_forward_parents nil)

(def ^:dynamic bidirectional_search_forward_path nil)

(def ^:dynamic bidirectional_search_forward_queue nil)

(def ^:dynamic bidirectional_search_forward_visited nil)

(def ^:dynamic bidirectional_search_intersection nil)

(def ^:dynamic bidirectional_search_j nil)

(def ^:dynamic bidirectional_search_res nil)

(def ^:dynamic bidirectional_search_result nil)

(def ^:dynamic construct_path_node nil)

(def ^:dynamic construct_path_path nil)

(def ^:dynamic expand_search_current nil)

(def ^:dynamic expand_search_head nil)

(def ^:dynamic expand_search_i nil)

(def ^:dynamic expand_search_neighbor nil)

(def ^:dynamic expand_search_neighbors nil)

(def ^:dynamic expand_search_p nil)

(def ^:dynamic expand_search_q nil)

(def ^:dynamic expand_search_v nil)

(def ^:dynamic is_edge_i nil)

(def ^:dynamic is_edge_neighbors nil)

(def ^:dynamic main_disconnected nil)

(def ^:dynamic main_graph nil)

(def ^:dynamic path_exists_i nil)

(def ^:dynamic print_path_res nil)

(def ^:dynamic reverse_list_i nil)

(def ^:dynamic reverse_list_res nil)

(defn expand_search [expand_search_graph expand_search_queue expand_search_head_p expand_search_parents expand_search_visited expand_search_opposite_visited]
  (binding [expand_search_current nil expand_search_head nil expand_search_i nil expand_search_neighbor nil expand_search_neighbors nil expand_search_p nil expand_search_q nil expand_search_v nil] (try (do (set! expand_search_head expand_search_head_p) (when (>= expand_search_head (count expand_search_queue)) (throw (ex-info "return" {:v {:found false :head expand_search_head :intersection (- 0 1) :parents expand_search_parents :queue expand_search_queue :visited expand_search_visited}}))) (set! expand_search_current (nth expand_search_queue expand_search_head)) (set! expand_search_head (+ expand_search_head 1)) (set! expand_search_neighbors (nth expand_search_graph expand_search_current)) (set! expand_search_q expand_search_queue) (set! expand_search_p expand_search_parents) (set! expand_search_v expand_search_visited) (set! expand_search_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< expand_search_i (count expand_search_neighbors))) (do (set! expand_search_neighbor (nth expand_search_neighbors expand_search_i)) (cond (nth expand_search_v expand_search_neighbor) (do (set! expand_search_i (+ expand_search_i 1)) (recur true)) :else (do (set! expand_search_v (assoc expand_search_v expand_search_neighbor true)) (set! expand_search_p (assoc expand_search_p expand_search_neighbor expand_search_current)) (set! expand_search_q (conj expand_search_q expand_search_neighbor)) (when (nth expand_search_opposite_visited expand_search_neighbor) (throw (ex-info "return" {:v {:found true :head expand_search_head :intersection expand_search_neighbor :parents expand_search_p :queue expand_search_q :visited expand_search_v}}))) (set! expand_search_i (+ expand_search_i 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v {:found false :head expand_search_head :intersection (- 0 1) :parents expand_search_p :queue expand_search_q :visited expand_search_v}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn construct_path [construct_path_current construct_path_parents]
  (binding [construct_path_node nil construct_path_path nil] (try (do (set! construct_path_path []) (set! construct_path_node construct_path_current) (while (not= construct_path_node (- 0 1)) (do (set! construct_path_path (conj construct_path_path construct_path_node)) (set! construct_path_node (nth construct_path_parents construct_path_node)))) (throw (ex-info "return" {:v construct_path_path}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_list [reverse_list_xs]
  (binding [reverse_list_i nil reverse_list_res nil] (try (do (set! reverse_list_res []) (set! reverse_list_i (count reverse_list_xs)) (while (> reverse_list_i 0) (do (set! reverse_list_i (- reverse_list_i 1)) (set! reverse_list_res (conj reverse_list_res (nth reverse_list_xs reverse_list_i))))) (throw (ex-info "return" {:v reverse_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bidirectional_search [bidirectional_search_g bidirectional_search_start bidirectional_search_goal]
  (binding [bidirectional_search_back_start nil bidirectional_search_backward_head nil bidirectional_search_backward_parents nil bidirectional_search_backward_path nil bidirectional_search_backward_queue nil bidirectional_search_backward_visited nil bidirectional_search_forward_head nil bidirectional_search_forward_parents nil bidirectional_search_forward_path nil bidirectional_search_forward_queue nil bidirectional_search_forward_visited nil bidirectional_search_intersection nil bidirectional_search_j nil bidirectional_search_res nil bidirectional_search_result nil] (try (do (when (= bidirectional_search_start bidirectional_search_goal) (throw (ex-info "return" {:v {:ok true :path [bidirectional_search_start]}}))) (set! bidirectional_search_forward_parents {}) (set! bidirectional_search_forward_parents (assoc bidirectional_search_forward_parents bidirectional_search_start (- 0 1))) (set! bidirectional_search_backward_parents {}) (set! bidirectional_search_backward_parents (assoc bidirectional_search_backward_parents bidirectional_search_goal (- 0 1))) (set! bidirectional_search_forward_visited {}) (set! bidirectional_search_forward_visited (assoc bidirectional_search_forward_visited bidirectional_search_start true)) (set! bidirectional_search_backward_visited {}) (set! bidirectional_search_backward_visited (assoc bidirectional_search_backward_visited bidirectional_search_goal true)) (set! bidirectional_search_forward_queue [bidirectional_search_start]) (set! bidirectional_search_backward_queue [bidirectional_search_goal]) (set! bidirectional_search_forward_head 0) (set! bidirectional_search_backward_head 0) (set! bidirectional_search_intersection (- 0 1)) (loop [while_flag_2 true] (when (and while_flag_2 (and (and (< bidirectional_search_forward_head (count bidirectional_search_forward_queue)) (< bidirectional_search_backward_head (count bidirectional_search_backward_queue))) (= bidirectional_search_intersection (- 0 1)))) (do (set! bidirectional_search_res (expand_search bidirectional_search_g bidirectional_search_forward_queue bidirectional_search_forward_head bidirectional_search_forward_parents bidirectional_search_forward_visited bidirectional_search_backward_visited)) (set! bidirectional_search_forward_queue (:queue bidirectional_search_res)) (set! bidirectional_search_forward_head (:head bidirectional_search_res)) (set! bidirectional_search_forward_parents (:parents bidirectional_search_res)) (set! bidirectional_search_forward_visited (:visited bidirectional_search_res)) (cond (:found bidirectional_search_res) (do (set! bidirectional_search_intersection (:intersection bidirectional_search_res)) (recur false)) (:found bidirectional_search_res) (do (set! bidirectional_search_intersection (:intersection bidirectional_search_res)) (recur false)) :else (do (set! bidirectional_search_res (expand_search bidirectional_search_g bidirectional_search_backward_queue bidirectional_search_backward_head bidirectional_search_backward_parents bidirectional_search_backward_visited bidirectional_search_forward_visited)) (set! bidirectional_search_backward_queue (:queue bidirectional_search_res)) (set! bidirectional_search_backward_head (:head bidirectional_search_res)) (set! bidirectional_search_backward_parents (:parents bidirectional_search_res)) (set! bidirectional_search_backward_visited (:visited bidirectional_search_res)) (recur while_flag_2)))))) (when (= bidirectional_search_intersection (- 0 1)) (throw (ex-info "return" {:v {:ok false :path []}}))) (set! bidirectional_search_forward_path (construct_path bidirectional_search_intersection bidirectional_search_forward_parents)) (set! bidirectional_search_forward_path (reverse_list bidirectional_search_forward_path)) (set! bidirectional_search_back_start (nth bidirectional_search_backward_parents bidirectional_search_intersection)) (set! bidirectional_search_backward_path (construct_path bidirectional_search_back_start bidirectional_search_backward_parents)) (set! bidirectional_search_result bidirectional_search_forward_path) (set! bidirectional_search_j 0) (while (< bidirectional_search_j (count bidirectional_search_backward_path)) (do (set! bidirectional_search_result (conj bidirectional_search_result (nth bidirectional_search_backward_path bidirectional_search_j))) (set! bidirectional_search_j (+ bidirectional_search_j 1)))) (throw (ex-info "return" {:v {:ok true :path bidirectional_search_result}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_edge [is_edge_g is_edge_u is_edge_v]
  (binding [is_edge_i nil is_edge_neighbors nil] (try (do (set! is_edge_neighbors (nth is_edge_g is_edge_u)) (set! is_edge_i 0) (while (< is_edge_i (count is_edge_neighbors)) (do (when (= (nth is_edge_neighbors is_edge_i) is_edge_v) (throw (ex-info "return" {:v true}))) (set! is_edge_i (+ is_edge_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn path_exists [path_exists_g path_exists_path]
  (binding [path_exists_i nil] (try (do (when (= (count path_exists_path) 0) (throw (ex-info "return" {:v false}))) (set! path_exists_i 0) (while (< (+ path_exists_i 1) (count path_exists_path)) (do (when (not (is_edge path_exists_g (nth path_exists_path path_exists_i) (nth path_exists_path (+ path_exists_i 1)))) (throw (ex-info "return" {:v false}))) (set! path_exists_i (+ path_exists_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_path [print_path_g print_path_s print_path_t]
  (binding [print_path_res nil] (do (set! print_path_res (bidirectional_search print_path_g print_path_s print_path_t)) (if (and (:ok print_path_res) (path_exists print_path_g (:path print_path_res))) (println (str (str (str (str (str "Path from " (str print_path_s)) " to ") (str print_path_t)) ": ") (str (:path print_path_res)))) (println (str (str (str (str "Path from " (str print_path_s)) " to ") (str print_path_t)) ": None"))))))

(defn main []
  (binding [main_disconnected nil main_graph nil] (do (set! main_graph {0 [1 2] 1 [0 3 4] 10 [6 11] 11 [7 8 9 10] 2 [0 5 6] 3 [1 7] 4 [1 8] 5 [2 9] 6 [2 10] 7 [3 11] 8 [4 11] 9 [5 11]}) (print_path main_graph 0 11) (print_path main_graph 5 5) (set! main_disconnected {0 [1 2] 1 [0] 2 [0] 3 [4] 4 [3]}) (print_path main_disconnected 0 3))))

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
