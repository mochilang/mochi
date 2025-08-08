(ns main (:refer-clojure :exclude [abs sqrtApprox heuristic pos_equal contains_pos open_index_of_pos remove_node_at append_pos_list reverse_pos_list concat_pos_lists get_successors find_lowest_f astar combine_paths bidirectional_astar path_to_string]))

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

(declare abs sqrtApprox heuristic pos_equal contains_pos open_index_of_pos remove_node_at append_pos_list reverse_pos_list concat_pos_lists get_successors find_lowest_f astar combine_paths bidirectional_astar path_to_string)

(def ^:dynamic append_pos_list_i nil)

(def ^:dynamic append_pos_list_res nil)

(def ^:dynamic astar_closed nil)

(def ^:dynamic astar_current nil)

(def ^:dynamic astar_f nil)

(def ^:dynamic astar_h nil)

(def ^:dynamic astar_h0 nil)

(def ^:dynamic astar_i nil)

(def ^:dynamic astar_idx nil)

(def ^:dynamic astar_idx_open nil)

(def ^:dynamic astar_new_path nil)

(def ^:dynamic astar_open nil)

(def ^:dynamic astar_pos nil)

(def ^:dynamic astar_succ nil)

(def ^:dynamic astar_tentative_g nil)

(def ^:dynamic bidirectional_astar_closed_b nil)

(def ^:dynamic bidirectional_astar_closed_f nil)

(def ^:dynamic bidirectional_astar_current_b nil)

(def ^:dynamic bidirectional_astar_current_f nil)

(def ^:dynamic bidirectional_astar_f nil)

(def ^:dynamic bidirectional_astar_h nil)

(def ^:dynamic bidirectional_astar_hb nil)

(def ^:dynamic bidirectional_astar_hf nil)

(def ^:dynamic bidirectional_astar_i nil)

(def ^:dynamic bidirectional_astar_idx_b nil)

(def ^:dynamic bidirectional_astar_idx_f nil)

(def ^:dynamic bidirectional_astar_idx_open nil)

(def ^:dynamic bidirectional_astar_new_path nil)

(def ^:dynamic bidirectional_astar_open_b nil)

(def ^:dynamic bidirectional_astar_open_f nil)

(def ^:dynamic bidirectional_astar_pos nil)

(def ^:dynamic bidirectional_astar_succ_b nil)

(def ^:dynamic bidirectional_astar_succ_f nil)

(def ^:dynamic bidirectional_astar_tentative_g nil)

(def ^:dynamic combine_paths_bwd_copy nil)

(def ^:dynamic combine_paths_i nil)

(def ^:dynamic concat_pos_lists_i nil)

(def ^:dynamic concat_pos_lists_j nil)

(def ^:dynamic concat_pos_lists_res nil)

(def ^:dynamic contains_pos_i nil)

(def ^:dynamic find_lowest_f_best nil)

(def ^:dynamic find_lowest_f_i nil)

(def ^:dynamic get_successors_i nil)

(def ^:dynamic get_successors_nx nil)

(def ^:dynamic get_successors_ny nil)

(def ^:dynamic get_successors_res nil)

(def ^:dynamic heuristic_dx nil)

(def ^:dynamic heuristic_dxf nil)

(def ^:dynamic heuristic_dy nil)

(def ^:dynamic heuristic_dyf nil)

(def ^:dynamic open_index_of_pos_i nil)

(def ^:dynamic path_to_string_i nil)

(def ^:dynamic path_to_string_s nil)

(def ^:dynamic remove_node_at_i nil)

(def ^:dynamic remove_node_at_res nil)

(def ^:dynamic reverse_pos_list_i nil)

(def ^:dynamic reverse_pos_list_res nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic main_HEURISTIC 0)

(def ^:dynamic main_grid [[0 0 0 0 0 0 0] [0 1 0 0 0 0 0] [0 0 0 0 0 0 0] [0 0 1 0 0 0 0] [1 0 1 0 0 0 0] [0 0 0 0 0 0 0] [0 0 0 0 1 0 0]])

(def ^:dynamic main_delta [[(- 1) 0] [0 (- 1)] [1 0] [0 1]])

(defn abs [abs_x]
  (try (if (< abs_x 0) (- abs_x) abs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 10) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn heuristic [heuristic_a heuristic_b]
  (binding [heuristic_dx nil heuristic_dxf nil heuristic_dy nil heuristic_dyf nil] (try (do (set! heuristic_dy (- (:y heuristic_a) (:y heuristic_b))) (set! heuristic_dx (- (:x heuristic_a) (:x heuristic_b))) (when (= main_HEURISTIC 1) (throw (ex-info "return" {:v (double (+ (abs heuristic_dy) (abs heuristic_dx)))}))) (set! heuristic_dyf (double heuristic_dy)) (set! heuristic_dxf (double heuristic_dx)) (throw (ex-info "return" {:v (sqrtApprox (+ (* heuristic_dyf heuristic_dyf) (* heuristic_dxf heuristic_dxf)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pos_equal [pos_equal_a pos_equal_b]
  (try (throw (ex-info "return" {:v (and (= (:y pos_equal_a) (:y pos_equal_b)) (= (:x pos_equal_a) (:x pos_equal_b)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn contains_pos [contains_pos_lst contains_pos_p]
  (binding [contains_pos_i nil] (try (do (set! contains_pos_i 0) (while (< contains_pos_i (count contains_pos_lst)) (do (when (pos_equal (nth contains_pos_lst contains_pos_i) contains_pos_p) (throw (ex-info "return" {:v true}))) (set! contains_pos_i (+ contains_pos_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn open_index_of_pos [open_index_of_pos_open open_index_of_pos_p]
  (binding [open_index_of_pos_i nil] (try (do (set! open_index_of_pos_i 0) (while (< open_index_of_pos_i (count open_index_of_pos_open)) (do (when (pos_equal (:pos (nth open_index_of_pos_open open_index_of_pos_i)) open_index_of_pos_p) (throw (ex-info "return" {:v open_index_of_pos_i}))) (set! open_index_of_pos_i (+ open_index_of_pos_i 1)))) (throw (ex-info "return" {:v (- 0 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_node_at [remove_node_at_nodes remove_node_at_idx]
  (binding [remove_node_at_i nil remove_node_at_res nil] (try (do (set! remove_node_at_res []) (set! remove_node_at_i 0) (while (< remove_node_at_i (count remove_node_at_nodes)) (do (when (not= remove_node_at_i remove_node_at_idx) (set! remove_node_at_res (conj remove_node_at_res (nth remove_node_at_nodes remove_node_at_i)))) (set! remove_node_at_i (+ remove_node_at_i 1)))) (throw (ex-info "return" {:v remove_node_at_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn append_pos_list [append_pos_list_path append_pos_list_p]
  (binding [append_pos_list_i nil append_pos_list_res nil] (try (do (set! append_pos_list_res []) (set! append_pos_list_i 0) (while (< append_pos_list_i (count append_pos_list_path)) (do (set! append_pos_list_res (conj append_pos_list_res (nth append_pos_list_path append_pos_list_i))) (set! append_pos_list_i (+ append_pos_list_i 1)))) (set! append_pos_list_res (conj append_pos_list_res append_pos_list_p)) (throw (ex-info "return" {:v append_pos_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_pos_list [reverse_pos_list_lst]
  (binding [reverse_pos_list_i nil reverse_pos_list_res nil] (try (do (set! reverse_pos_list_res []) (set! reverse_pos_list_i (- (count reverse_pos_list_lst) 1)) (while (>= reverse_pos_list_i 0) (do (set! reverse_pos_list_res (conj reverse_pos_list_res (nth reverse_pos_list_lst reverse_pos_list_i))) (set! reverse_pos_list_i (- reverse_pos_list_i 1)))) (throw (ex-info "return" {:v reverse_pos_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn concat_pos_lists [concat_pos_lists_a concat_pos_lists_b]
  (binding [concat_pos_lists_i nil concat_pos_lists_j nil concat_pos_lists_res nil] (try (do (set! concat_pos_lists_res []) (set! concat_pos_lists_i 0) (while (< concat_pos_lists_i (count concat_pos_lists_a)) (do (set! concat_pos_lists_res (conj concat_pos_lists_res (nth concat_pos_lists_a concat_pos_lists_i))) (set! concat_pos_lists_i (+ concat_pos_lists_i 1)))) (set! concat_pos_lists_j 0) (while (< concat_pos_lists_j (count concat_pos_lists_b)) (do (set! concat_pos_lists_res (conj concat_pos_lists_res (nth concat_pos_lists_b concat_pos_lists_j))) (set! concat_pos_lists_j (+ concat_pos_lists_j 1)))) (throw (ex-info "return" {:v concat_pos_lists_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_successors [get_successors_p]
  (binding [get_successors_i nil get_successors_nx nil get_successors_ny nil get_successors_res nil] (try (do (set! get_successors_res []) (set! get_successors_i 0) (while (< get_successors_i (count main_delta)) (do (set! get_successors_nx (+ (:x get_successors_p) (nth (nth main_delta get_successors_i) 1))) (set! get_successors_ny (+ (:y get_successors_p) (nth (nth main_delta get_successors_i) 0))) (when (and (and (and (>= get_successors_nx 0) (>= get_successors_ny 0)) (< get_successors_nx (count (nth main_grid 0)))) (< get_successors_ny (count main_grid))) (when (= (nth (nth main_grid get_successors_ny) get_successors_nx) 0) (set! get_successors_res (conj get_successors_res {:x get_successors_nx :y get_successors_ny})))) (set! get_successors_i (+ get_successors_i 1)))) (throw (ex-info "return" {:v get_successors_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_lowest_f [find_lowest_f_open]
  (binding [find_lowest_f_best nil find_lowest_f_i nil] (try (do (set! find_lowest_f_best 0) (set! find_lowest_f_i 1) (while (< find_lowest_f_i (count find_lowest_f_open)) (do (when (< (:f_cost (nth find_lowest_f_open find_lowest_f_i)) (:f_cost (nth find_lowest_f_open find_lowest_f_best))) (set! find_lowest_f_best find_lowest_f_i)) (set! find_lowest_f_i (+ find_lowest_f_i 1)))) (throw (ex-info "return" {:v find_lowest_f_best}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn astar [astar_start astar_goal]
  (binding [astar_closed nil astar_current nil astar_f nil astar_h nil astar_h0 nil astar_i nil astar_idx nil astar_idx_open nil astar_new_path nil astar_open nil astar_pos nil astar_succ nil astar_tentative_g nil] (try (do (set! astar_h0 (heuristic astar_start astar_goal)) (set! astar_open [{:f_cost astar_h0 :g_cost 0 :h_cost astar_h0 :path [astar_start] :pos astar_start}]) (set! astar_closed []) (while (> (count astar_open) 0) (do (set! astar_idx (find_lowest_f astar_open)) (set! astar_current (nth astar_open astar_idx)) (set! astar_open (remove_node_at astar_open astar_idx)) (when (pos_equal (:pos astar_current) astar_goal) (throw (ex-info "return" {:v (:path astar_current)}))) (set! astar_closed (conj astar_closed (:pos astar_current))) (set! astar_succ (get_successors (:pos astar_current))) (set! astar_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< astar_i (count astar_succ))) (do (set! astar_pos (nth astar_succ astar_i)) (cond (contains_pos astar_closed astar_pos) (do (set! astar_i (+ astar_i 1)) (recur true)) :else (do (set! astar_tentative_g (+ (:g_cost astar_current) 1)) (set! astar_idx_open (open_index_of_pos astar_open astar_pos)) (when (or (= astar_idx_open (- 0 1)) (< astar_tentative_g (:g_cost (nth astar_open astar_idx_open)))) (do (set! astar_new_path (append_pos_list (:path astar_current) astar_pos)) (set! astar_h (heuristic astar_pos astar_goal)) (set! astar_f (+ (double astar_tentative_g) astar_h)) (when (not= astar_idx_open (- 0 1)) (set! astar_open (remove_node_at astar_open astar_idx_open))) (set! astar_open (conj astar_open {:f_cost astar_f :g_cost astar_tentative_g :h_cost astar_h :path astar_new_path :pos astar_pos})))) (set! astar_i (+ astar_i 1)) (recur while_flag_1)))))))) (throw (ex-info "return" {:v [astar_start]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn combine_paths [combine_paths_fwd combine_paths_bwd]
  (binding [combine_paths_bwd_copy nil combine_paths_i nil] (try (do (set! combine_paths_bwd_copy []) (set! combine_paths_i 0) (while (< combine_paths_i (- (count (:path combine_paths_bwd)) 1)) (do (set! combine_paths_bwd_copy (conj combine_paths_bwd_copy (get (:path combine_paths_bwd) combine_paths_i))) (set! combine_paths_i (+ combine_paths_i 1)))) (set! combine_paths_bwd_copy (reverse_pos_list combine_paths_bwd_copy)) (throw (ex-info "return" {:v (concat_pos_lists (:path combine_paths_fwd) combine_paths_bwd_copy)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bidirectional_astar [bidirectional_astar_start bidirectional_astar_goal]
  (binding [bidirectional_astar_closed_b nil bidirectional_astar_closed_f nil bidirectional_astar_current_b nil bidirectional_astar_current_f nil bidirectional_astar_f nil bidirectional_astar_h nil bidirectional_astar_hb nil bidirectional_astar_hf nil bidirectional_astar_i nil bidirectional_astar_idx_b nil bidirectional_astar_idx_f nil bidirectional_astar_idx_open nil bidirectional_astar_new_path nil bidirectional_astar_open_b nil bidirectional_astar_open_f nil bidirectional_astar_pos nil bidirectional_astar_succ_b nil bidirectional_astar_succ_f nil bidirectional_astar_tentative_g nil] (try (do (set! bidirectional_astar_hf (heuristic bidirectional_astar_start bidirectional_astar_goal)) (set! bidirectional_astar_hb (heuristic bidirectional_astar_goal bidirectional_astar_start)) (set! bidirectional_astar_open_f [{:f_cost bidirectional_astar_hf :g_cost 0 :h_cost bidirectional_astar_hf :path [bidirectional_astar_start] :pos bidirectional_astar_start}]) (set! bidirectional_astar_open_b [{:f_cost bidirectional_astar_hb :g_cost 0 :h_cost bidirectional_astar_hb :path [bidirectional_astar_goal] :pos bidirectional_astar_goal}]) (set! bidirectional_astar_closed_f []) (set! bidirectional_astar_closed_b []) (while (and (> (count bidirectional_astar_open_f) 0) (> (count bidirectional_astar_open_b) 0)) (do (set! bidirectional_astar_idx_f (find_lowest_f bidirectional_astar_open_f)) (set! bidirectional_astar_current_f (nth bidirectional_astar_open_f bidirectional_astar_idx_f)) (set! bidirectional_astar_open_f (remove_node_at bidirectional_astar_open_f bidirectional_astar_idx_f)) (set! bidirectional_astar_idx_b (find_lowest_f bidirectional_astar_open_b)) (set! bidirectional_astar_current_b (nth bidirectional_astar_open_b bidirectional_astar_idx_b)) (set! bidirectional_astar_open_b (remove_node_at bidirectional_astar_open_b bidirectional_astar_idx_b)) (when (pos_equal (:pos bidirectional_astar_current_f) (:pos bidirectional_astar_current_b)) (throw (ex-info "return" {:v (combine_paths bidirectional_astar_current_f bidirectional_astar_current_b)}))) (set! bidirectional_astar_closed_f (conj bidirectional_astar_closed_f (:pos bidirectional_astar_current_f))) (set! bidirectional_astar_closed_b (conj bidirectional_astar_closed_b (:pos bidirectional_astar_current_b))) (set! bidirectional_astar_succ_f (get_successors (:pos bidirectional_astar_current_f))) (set! bidirectional_astar_i 0) (loop [while_flag_2 true] (when (and while_flag_2 (< bidirectional_astar_i (count bidirectional_astar_succ_f))) (do (set! bidirectional_astar_pos (nth bidirectional_astar_succ_f bidirectional_astar_i)) (cond (contains_pos bidirectional_astar_closed_f bidirectional_astar_pos) (do (set! bidirectional_astar_i (+ bidirectional_astar_i 1)) (recur true)) :else (do (set! bidirectional_astar_tentative_g (+ (:g_cost bidirectional_astar_current_f) 1)) (set! bidirectional_astar_h (heuristic bidirectional_astar_pos (:pos bidirectional_astar_current_b))) (set! bidirectional_astar_f (+ (double bidirectional_astar_tentative_g) bidirectional_astar_h)) (set! bidirectional_astar_idx_open (open_index_of_pos bidirectional_astar_open_f bidirectional_astar_pos)) (when (or (= bidirectional_astar_idx_open (- 0 1)) (< bidirectional_astar_tentative_g (:g_cost (nth bidirectional_astar_open_f bidirectional_astar_idx_open)))) (do (set! bidirectional_astar_new_path (append_pos_list (:path bidirectional_astar_current_f) bidirectional_astar_pos)) (when (not= bidirectional_astar_idx_open (- 0 1)) (set! bidirectional_astar_open_f (remove_node_at bidirectional_astar_open_f bidirectional_astar_idx_open))) (set! bidirectional_astar_open_f (conj bidirectional_astar_open_f {:f_cost bidirectional_astar_f :g_cost bidirectional_astar_tentative_g :h_cost bidirectional_astar_h :path bidirectional_astar_new_path :pos bidirectional_astar_pos})))) (set! bidirectional_astar_i (+ bidirectional_astar_i 1)) (recur while_flag_2)))))) (set! bidirectional_astar_succ_b (get_successors (:pos bidirectional_astar_current_b))) (set! bidirectional_astar_i 0) (loop [while_flag_3 true] (when (and while_flag_3 (< bidirectional_astar_i (count bidirectional_astar_succ_b))) (do (set! bidirectional_astar_pos (nth bidirectional_astar_succ_b bidirectional_astar_i)) (cond (contains_pos bidirectional_astar_closed_b bidirectional_astar_pos) (do (set! bidirectional_astar_i (+ bidirectional_astar_i 1)) (recur true)) :else (do (set! bidirectional_astar_tentative_g (+ (:g_cost bidirectional_astar_current_b) 1)) (set! bidirectional_astar_h (heuristic bidirectional_astar_pos (:pos bidirectional_astar_current_f))) (set! bidirectional_astar_f (+ (double bidirectional_astar_tentative_g) bidirectional_astar_h)) (set! bidirectional_astar_idx_open (open_index_of_pos bidirectional_astar_open_b bidirectional_astar_pos)) (when (or (= bidirectional_astar_idx_open (- 0 1)) (< bidirectional_astar_tentative_g (:g_cost (nth bidirectional_astar_open_b bidirectional_astar_idx_open)))) (do (set! bidirectional_astar_new_path (append_pos_list (:path bidirectional_astar_current_b) bidirectional_astar_pos)) (when (not= bidirectional_astar_idx_open (- 0 1)) (set! bidirectional_astar_open_b (remove_node_at bidirectional_astar_open_b bidirectional_astar_idx_open))) (set! bidirectional_astar_open_b (conj bidirectional_astar_open_b {:f_cost bidirectional_astar_f :g_cost bidirectional_astar_tentative_g :h_cost bidirectional_astar_h :path bidirectional_astar_new_path :pos bidirectional_astar_pos})))) (set! bidirectional_astar_i (+ bidirectional_astar_i 1)) (recur while_flag_3)))))))) (throw (ex-info "return" {:v [bidirectional_astar_start]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn path_to_string [path_to_string_path]
  (binding [path_to_string_i nil path_to_string_s nil] (try (do (when (= (count path_to_string_path) 0) (throw (ex-info "return" {:v "[]"}))) (set! path_to_string_s (str (str (str (str "[(" (str (:y (nth path_to_string_path 0)))) ", ") (str (:x (nth path_to_string_path 0)))) ")")) (set! path_to_string_i 1) (while (< path_to_string_i (count path_to_string_path)) (do (set! path_to_string_s (str (str (str (str (str path_to_string_s ", (") (str (:y (nth path_to_string_path path_to_string_i)))) ", ") (str (:x (nth path_to_string_path path_to_string_i)))) ")")) (set! path_to_string_i (+ path_to_string_i 1)))) (set! path_to_string_s (str path_to_string_s "]")) (throw (ex-info "return" {:v path_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_start {:x 0 :y 0})

(def ^:dynamic main_goal {:x (- (count (nth main_grid 0)) 1) :y (- (count main_grid) 1)})

(def ^:dynamic main_path1 (astar main_start main_goal))

(def ^:dynamic main_path2 (bidirectional_astar main_start main_goal))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (path_to_string main_path1))
      (println (path_to_string main_path2))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
