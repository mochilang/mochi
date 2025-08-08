(ns main (:refer-clojure :exclude [list_contains_int edge_exists first_key rand_range dg_make_graph dg_add_pair dg_remove_pair dg_all_nodes dg_dfs_util dg_dfs dg_bfs dg_in_degree dg_out_degree dg_topo_util dg_topological_sort dg_cycle_util dg_cycle_nodes dg_has_cycle_util dg_has_cycle dg_fill_graph_randomly dg_dfs_time dg_bfs_time g_make_graph g_add_pair g_remove_pair g_all_nodes g_dfs_util g_dfs g_bfs g_degree g_cycle_util g_cycle_nodes g_has_cycle_util g_has_cycle g_fill_graph_randomly g_dfs_time g_bfs_time main]))

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

(declare list_contains_int edge_exists first_key rand_range dg_make_graph dg_add_pair dg_remove_pair dg_all_nodes dg_dfs_util dg_dfs dg_bfs dg_in_degree dg_out_degree dg_topo_util dg_topological_sort dg_cycle_util dg_cycle_nodes dg_has_cycle_util dg_has_cycle dg_fill_graph_randomly dg_dfs_time dg_bfs_time g_make_graph g_add_pair g_remove_pair g_all_nodes g_dfs_util g_dfs g_bfs g_degree g_cycle_util g_cycle_nodes g_has_cycle_util g_has_cycle g_fill_graph_randomly g_dfs_time g_bfs_time main)

(def ^:dynamic count_v nil)

(def ^:dynamic dg_add_pair_edges nil)

(def ^:dynamic dg_add_pair_g nil)

(def ^:dynamic dg_add_pair_m nil)

(def ^:dynamic dg_add_pair_m0 nil)

(def ^:dynamic dg_add_pair_m1 nil)

(def ^:dynamic dg_all_nodes_res nil)

(def ^:dynamic dg_bfs_edges nil)

(def ^:dynamic dg_bfs_i nil)

(def ^:dynamic dg_bfs_neigh nil)

(def ^:dynamic dg_bfs_node nil)

(def ^:dynamic dg_bfs_order nil)

(def ^:dynamic dg_bfs_queue nil)

(def ^:dynamic dg_bfs_start nil)

(def ^:dynamic dg_bfs_time_begin nil)

(def ^:dynamic dg_bfs_time_end nil)

(def ^:dynamic dg_bfs_visited nil)

(def ^:dynamic dg_cycle_nodes_rec nil)

(def ^:dynamic dg_cycle_nodes_res nil)

(def ^:dynamic dg_cycle_nodes_visited nil)

(def ^:dynamic dg_cycle_util_edges nil)

(def ^:dynamic dg_cycle_util_i nil)

(def ^:dynamic dg_cycle_util_neigh nil)

(def ^:dynamic dg_cycle_util_rec nil)

(def ^:dynamic dg_cycle_util_res nil)

(def ^:dynamic dg_cycle_util_visited nil)

(def ^:dynamic dg_dfs_order nil)

(def ^:dynamic dg_dfs_start nil)

(def ^:dynamic dg_dfs_time_begin nil)

(def ^:dynamic dg_dfs_time_end nil)

(def ^:dynamic dg_dfs_util_edges nil)

(def ^:dynamic dg_dfs_util_i nil)

(def ^:dynamic dg_dfs_util_neigh nil)

(def ^:dynamic dg_dfs_util_order nil)

(def ^:dynamic dg_dfs_util_visited nil)

(def ^:dynamic dg_dfs_visited nil)

(def ^:dynamic dg_fill_graph_randomly_edge_count nil)

(def ^:dynamic dg_fill_graph_randomly_i nil)

(def ^:dynamic dg_fill_graph_randomly_j nil)

(def ^:dynamic dg_fill_graph_randomly_n nil)

(def ^:dynamic dg_has_cycle_rec nil)

(def ^:dynamic dg_has_cycle_util_edges nil)

(def ^:dynamic dg_has_cycle_util_i nil)

(def ^:dynamic dg_has_cycle_util_neigh nil)

(def ^:dynamic dg_has_cycle_util_rec nil)

(def ^:dynamic dg_has_cycle_util_visited nil)

(def ^:dynamic dg_has_cycle_visited nil)

(def ^:dynamic dg_in_degree_edges nil)

(def ^:dynamic dg_in_degree_i nil)

(def ^:dynamic dg_remove_pair_edges nil)

(def ^:dynamic dg_remove_pair_g nil)

(def ^:dynamic dg_remove_pair_i nil)

(def ^:dynamic dg_remove_pair_m nil)

(def ^:dynamic dg_remove_pair_new_edges nil)

(def ^:dynamic dg_topo_util_edges nil)

(def ^:dynamic dg_topo_util_i nil)

(def ^:dynamic dg_topo_util_neigh nil)

(def ^:dynamic dg_topo_util_stack nil)

(def ^:dynamic dg_topo_util_visited nil)

(def ^:dynamic dg_topological_sort_i nil)

(def ^:dynamic dg_topological_sort_res nil)

(def ^:dynamic dg_topological_sort_stack nil)

(def ^:dynamic dg_topological_sort_visited nil)

(def ^:dynamic edge_exists_i nil)

(def ^:dynamic g_add_pair_edges nil)

(def ^:dynamic g_add_pair_edges2 nil)

(def ^:dynamic g_add_pair_g nil)

(def ^:dynamic g_add_pair_m nil)

(def ^:dynamic g_add_pair_m0 nil)

(def ^:dynamic g_add_pair_m2 nil)

(def ^:dynamic g_add_pair_m3 nil)

(def ^:dynamic g_all_nodes_res nil)

(def ^:dynamic g_bfs_edges nil)

(def ^:dynamic g_bfs_i nil)

(def ^:dynamic g_bfs_neigh nil)

(def ^:dynamic g_bfs_node nil)

(def ^:dynamic g_bfs_order nil)

(def ^:dynamic g_bfs_queue nil)

(def ^:dynamic g_bfs_start nil)

(def ^:dynamic g_bfs_time_begin nil)

(def ^:dynamic g_bfs_time_end nil)

(def ^:dynamic g_bfs_visited nil)

(def ^:dynamic g_cycle_nodes_res nil)

(def ^:dynamic g_cycle_nodes_visited nil)

(def ^:dynamic g_cycle_util_edges nil)

(def ^:dynamic g_cycle_util_i nil)

(def ^:dynamic g_cycle_util_neigh nil)

(def ^:dynamic g_cycle_util_res nil)

(def ^:dynamic g_cycle_util_visited nil)

(def ^:dynamic g_dfs_order nil)

(def ^:dynamic g_dfs_start nil)

(def ^:dynamic g_dfs_time_begin nil)

(def ^:dynamic g_dfs_time_end nil)

(def ^:dynamic g_dfs_util_edges nil)

(def ^:dynamic g_dfs_util_i nil)

(def ^:dynamic g_dfs_util_neigh nil)

(def ^:dynamic g_dfs_util_order nil)

(def ^:dynamic g_dfs_util_visited nil)

(def ^:dynamic g_dfs_visited nil)

(def ^:dynamic g_fill_graph_randomly_edge_count nil)

(def ^:dynamic g_fill_graph_randomly_i nil)

(def ^:dynamic g_fill_graph_randomly_j nil)

(def ^:dynamic g_fill_graph_randomly_n nil)

(def ^:dynamic g_has_cycle_util_edges nil)

(def ^:dynamic g_has_cycle_util_i nil)

(def ^:dynamic g_has_cycle_util_neigh nil)

(def ^:dynamic g_has_cycle_util_visited nil)

(def ^:dynamic g_has_cycle_visited nil)

(def ^:dynamic g_remove_pair_edges nil)

(def ^:dynamic g_remove_pair_edges2 nil)

(def ^:dynamic g_remove_pair_g nil)

(def ^:dynamic g_remove_pair_i nil)

(def ^:dynamic g_remove_pair_j nil)

(def ^:dynamic g_remove_pair_m nil)

(def ^:dynamic g_remove_pair_m2 nil)

(def ^:dynamic g_remove_pair_new_edges nil)

(def ^:dynamic g_remove_pair_new_edges2 nil)

(def ^:dynamic list_contains_int_i nil)

(def ^:dynamic main_dg nil)

(def ^:dynamic main_ug nil)

(defn list_contains_int [list_contains_int_xs list_contains_int_x]
  (binding [list_contains_int_i nil] (try (do (set! list_contains_int_i 0) (while (< list_contains_int_i (count list_contains_int_xs)) (do (when (= (nth list_contains_int_xs list_contains_int_i) list_contains_int_x) (throw (ex-info "return" {:v true}))) (set! list_contains_int_i (+ list_contains_int_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn edge_exists [edge_exists_edges edge_exists_w edge_exists_v]
  (binding [edge_exists_i nil] (try (do (set! edge_exists_i 0) (while (< edge_exists_i (count edge_exists_edges)) (do (when (and (= (nth (nth edge_exists_edges edge_exists_i) 0) edge_exists_w) (= (nth (nth edge_exists_edges edge_exists_i) 1) edge_exists_v)) (throw (ex-info "return" {:v true}))) (set! edge_exists_i (+ edge_exists_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn first_key [first_key_m]
  (try (do (doseq [k first_key_m] (throw (ex-info "return" {:v k}))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rand_range [rand_range_low rand_range_high]
  (try (throw (ex-info "return" {:v (+ (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) (- rand_range_high rand_range_low)) rand_range_low)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dg_make_graph []
  (try (throw (ex-info "return" {:v {:graph {}}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dg_add_pair [dg_add_pair_g_p dg_add_pair_u dg_add_pair_v dg_add_pair_w]
  (binding [dg_add_pair_edges nil dg_add_pair_g nil dg_add_pair_m nil dg_add_pair_m0 nil dg_add_pair_m1 nil] (do (set! dg_add_pair_g dg_add_pair_g_p) (if (in dg_add_pair_u (:graph dg_add_pair_g)) (do (set! dg_add_pair_edges (get (:graph dg_add_pair_g) dg_add_pair_u)) (when (not (edge_exists dg_add_pair_edges dg_add_pair_w dg_add_pair_v)) (do (set! dg_add_pair_edges (conj dg_add_pair_edges [dg_add_pair_w dg_add_pair_v])) (set! dg_add_pair_m (:graph dg_add_pair_g)) (set! dg_add_pair_m (assoc dg_add_pair_m dg_add_pair_u dg_add_pair_edges)) (set! dg_add_pair_g (assoc dg_add_pair_g :graph dg_add_pair_m))))) (do (set! dg_add_pair_m0 (:graph dg_add_pair_g)) (set! dg_add_pair_m0 (assoc dg_add_pair_m0 dg_add_pair_u [[dg_add_pair_w dg_add_pair_v]])) (set! dg_add_pair_g (assoc dg_add_pair_g :graph dg_add_pair_m0)))) (when (not (in dg_add_pair_v (:graph dg_add_pair_g))) (do (set! dg_add_pair_m1 (:graph dg_add_pair_g)) (set! dg_add_pair_m1 (assoc dg_add_pair_m1 dg_add_pair_v [])) (set! dg_add_pair_g (assoc dg_add_pair_g :graph dg_add_pair_m1)))))))

(defn dg_remove_pair [dg_remove_pair_g_p dg_remove_pair_u dg_remove_pair_v]
  (binding [dg_remove_pair_edges nil dg_remove_pair_g nil dg_remove_pair_i nil dg_remove_pair_m nil dg_remove_pair_new_edges nil] (do (set! dg_remove_pair_g dg_remove_pair_g_p) (when (in dg_remove_pair_u (:graph dg_remove_pair_g)) (do (set! dg_remove_pair_edges (get (:graph dg_remove_pair_g) dg_remove_pair_u)) (set! dg_remove_pair_new_edges []) (set! dg_remove_pair_i 0) (while (< dg_remove_pair_i (count dg_remove_pair_edges)) (do (when (not= (nth (nth dg_remove_pair_edges dg_remove_pair_i) 1) dg_remove_pair_v) (set! dg_remove_pair_new_edges (conj dg_remove_pair_new_edges (nth dg_remove_pair_edges dg_remove_pair_i)))) (set! dg_remove_pair_i (+ dg_remove_pair_i 1)))) (set! dg_remove_pair_m (:graph dg_remove_pair_g)) (set! dg_remove_pair_m (assoc dg_remove_pair_m dg_remove_pair_u dg_remove_pair_new_edges)) (set! dg_remove_pair_g (assoc dg_remove_pair_g :graph dg_remove_pair_m)))))))

(defn dg_all_nodes [dg_all_nodes_g]
  (binding [dg_all_nodes_res nil] (try (do (set! dg_all_nodes_res []) (doseq [k (keys (:graph dg_all_nodes_g))] (set! dg_all_nodes_res (conj dg_all_nodes_res k))) (throw (ex-info "return" {:v dg_all_nodes_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dg_dfs_util [dg_dfs_util_g dg_dfs_util_node dg_dfs_util_visited_p dg_dfs_util_order_p dg_dfs_util_d]
  (binding [dg_dfs_util_edges nil dg_dfs_util_i nil dg_dfs_util_neigh nil dg_dfs_util_order nil dg_dfs_util_visited nil] (try (do (set! dg_dfs_util_visited dg_dfs_util_visited_p) (set! dg_dfs_util_order dg_dfs_util_order_p) (set! dg_dfs_util_visited (assoc dg_dfs_util_visited dg_dfs_util_node true)) (set! dg_dfs_util_order (conj dg_dfs_util_order dg_dfs_util_node)) (when (and (not= dg_dfs_util_d (- 1)) (= dg_dfs_util_node dg_dfs_util_d)) (throw (ex-info "return" {:v dg_dfs_util_order}))) (set! dg_dfs_util_edges (get (:graph dg_dfs_util_g) dg_dfs_util_node)) (set! dg_dfs_util_i 0) (while (< dg_dfs_util_i (count dg_dfs_util_edges)) (do (set! dg_dfs_util_neigh (nth (nth dg_dfs_util_edges dg_dfs_util_i) 1)) (when (not (in dg_dfs_util_neigh dg_dfs_util_visited)) (do (set! dg_dfs_util_order (dg_dfs_util dg_dfs_util_g dg_dfs_util_neigh dg_dfs_util_visited dg_dfs_util_order dg_dfs_util_d)) (when (and (not= dg_dfs_util_d (- 1)) (= (nth dg_dfs_util_order (- (count dg_dfs_util_order) 1)) dg_dfs_util_d)) (throw (ex-info "return" {:v dg_dfs_util_order}))))) (set! dg_dfs_util_i (+ dg_dfs_util_i 1)))) (throw (ex-info "return" {:v dg_dfs_util_order}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dg_dfs [dg_dfs_g dg_dfs_s dg_dfs_d]
  (binding [dg_dfs_order nil dg_dfs_start nil dg_dfs_visited nil] (try (do (when (= dg_dfs_s dg_dfs_d) (throw (ex-info "return" {:v []}))) (set! dg_dfs_start (if (= dg_dfs_s (- 2)) (first_key (:graph dg_dfs_g)) dg_dfs_s)) (set! dg_dfs_visited {}) (set! dg_dfs_order []) (set! dg_dfs_order (dg_dfs_util dg_dfs_g dg_dfs_start dg_dfs_visited dg_dfs_order dg_dfs_d)) (throw (ex-info "return" {:v dg_dfs_order}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dg_bfs [dg_bfs_g dg_bfs_s]
  (binding [dg_bfs_edges nil dg_bfs_i nil dg_bfs_neigh nil dg_bfs_node nil dg_bfs_order nil dg_bfs_queue nil dg_bfs_start nil dg_bfs_visited nil] (try (do (set! dg_bfs_queue []) (set! dg_bfs_visited {}) (set! dg_bfs_order []) (set! dg_bfs_start (if (= dg_bfs_s (- 2)) (first_key (:graph dg_bfs_g)) dg_bfs_s)) (set! dg_bfs_queue (conj dg_bfs_queue dg_bfs_start)) (set! dg_bfs_visited (assoc dg_bfs_visited dg_bfs_start true)) (while (> (count dg_bfs_queue) 0) (do (set! dg_bfs_node (nth dg_bfs_queue 0)) (set! dg_bfs_queue (subvec dg_bfs_queue 1 (count dg_bfs_queue))) (set! dg_bfs_order (conj dg_bfs_order dg_bfs_node)) (set! dg_bfs_edges (get (:graph dg_bfs_g) dg_bfs_node)) (set! dg_bfs_i 0) (while (< dg_bfs_i (count dg_bfs_edges)) (do (set! dg_bfs_neigh (nth (nth dg_bfs_edges dg_bfs_i) 1)) (when (not (in dg_bfs_neigh dg_bfs_visited)) (do (set! dg_bfs_queue (conj dg_bfs_queue dg_bfs_neigh)) (set! dg_bfs_visited (assoc dg_bfs_visited dg_bfs_neigh true)))) (set! dg_bfs_i (+ dg_bfs_i 1)))))) (throw (ex-info "return" {:v dg_bfs_order}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dg_in_degree [dg_in_degree_g dg_in_degree_u]
  (binding [count_v nil dg_in_degree_edges nil dg_in_degree_i nil] (try (do (set! count_v 0) (doseq [k (keys (:graph dg_in_degree_g))] (do (set! dg_in_degree_edges (get (:graph dg_in_degree_g) k)) (set! dg_in_degree_i 0) (while (< dg_in_degree_i (count dg_in_degree_edges)) (do (when (= (nth (nth dg_in_degree_edges dg_in_degree_i) 1) dg_in_degree_u) (set! count_v (+ count_v 1))) (set! dg_in_degree_i (+ dg_in_degree_i 1)))))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dg_out_degree [dg_out_degree_g dg_out_degree_u]
  (try (if (in dg_out_degree_u (:graph dg_out_degree_g)) (count (get (:graph dg_out_degree_g) dg_out_degree_u)) 0) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dg_topo_util [dg_topo_util_g dg_topo_util_node dg_topo_util_visited_p dg_topo_util_stack_p]
  (binding [dg_topo_util_edges nil dg_topo_util_i nil dg_topo_util_neigh nil dg_topo_util_stack nil dg_topo_util_visited nil] (try (do (set! dg_topo_util_visited dg_topo_util_visited_p) (set! dg_topo_util_stack dg_topo_util_stack_p) (set! dg_topo_util_visited (assoc dg_topo_util_visited dg_topo_util_node true)) (set! dg_topo_util_edges (get (:graph dg_topo_util_g) dg_topo_util_node)) (set! dg_topo_util_i 0) (while (< dg_topo_util_i (count dg_topo_util_edges)) (do (set! dg_topo_util_neigh (nth (nth dg_topo_util_edges dg_topo_util_i) 1)) (when (not (in dg_topo_util_neigh dg_topo_util_visited)) (set! dg_topo_util_stack (dg_topo_util dg_topo_util_g dg_topo_util_neigh dg_topo_util_visited dg_topo_util_stack))) (set! dg_topo_util_i (+ dg_topo_util_i 1)))) (set! dg_topo_util_stack (conj dg_topo_util_stack dg_topo_util_node)) (throw (ex-info "return" {:v dg_topo_util_stack}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dg_topological_sort [dg_topological_sort_g]
  (binding [dg_topological_sort_i nil dg_topological_sort_res nil dg_topological_sort_stack nil dg_topological_sort_visited nil] (try (do (set! dg_topological_sort_visited {}) (set! dg_topological_sort_stack []) (doseq [k (keys (:graph dg_topological_sort_g))] (when (not (in k dg_topological_sort_visited)) (set! dg_topological_sort_stack (dg_topo_util dg_topological_sort_g k dg_topological_sort_visited dg_topological_sort_stack)))) (set! dg_topological_sort_res []) (set! dg_topological_sort_i (- (count dg_topological_sort_stack) 1)) (while (>= dg_topological_sort_i 0) (do (set! dg_topological_sort_res (conj dg_topological_sort_res (nth dg_topological_sort_stack dg_topological_sort_i))) (set! dg_topological_sort_i (- dg_topological_sort_i 1)))) (throw (ex-info "return" {:v dg_topological_sort_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dg_cycle_util [dg_cycle_util_g dg_cycle_util_node dg_cycle_util_visited_p dg_cycle_util_rec_p dg_cycle_util_res_p]
  (binding [dg_cycle_util_edges nil dg_cycle_util_i nil dg_cycle_util_neigh nil dg_cycle_util_rec nil dg_cycle_util_res nil dg_cycle_util_visited nil] (try (do (set! dg_cycle_util_visited dg_cycle_util_visited_p) (set! dg_cycle_util_rec dg_cycle_util_rec_p) (set! dg_cycle_util_res dg_cycle_util_res_p) (set! dg_cycle_util_visited (assoc dg_cycle_util_visited dg_cycle_util_node true)) (set! dg_cycle_util_rec (assoc dg_cycle_util_rec dg_cycle_util_node true)) (set! dg_cycle_util_edges (get (:graph dg_cycle_util_g) dg_cycle_util_node)) (set! dg_cycle_util_i 0) (while (< dg_cycle_util_i (count dg_cycle_util_edges)) (do (set! dg_cycle_util_neigh (nth (nth dg_cycle_util_edges dg_cycle_util_i) 1)) (if (not (in dg_cycle_util_neigh dg_cycle_util_visited)) (set! dg_cycle_util_res (dg_cycle_util dg_cycle_util_g dg_cycle_util_neigh dg_cycle_util_visited dg_cycle_util_rec dg_cycle_util_res)) (when (nth dg_cycle_util_rec dg_cycle_util_neigh) (do (when (not (list_contains_int dg_cycle_util_res dg_cycle_util_neigh)) (set! dg_cycle_util_res (conj dg_cycle_util_res dg_cycle_util_neigh))) (when (not (list_contains_int dg_cycle_util_res dg_cycle_util_node)) (set! dg_cycle_util_res (conj dg_cycle_util_res dg_cycle_util_node)))))) (set! dg_cycle_util_i (+ dg_cycle_util_i 1)))) (set! dg_cycle_util_rec (assoc dg_cycle_util_rec dg_cycle_util_node false)) (throw (ex-info "return" {:v dg_cycle_util_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dg_cycle_nodes [dg_cycle_nodes_g]
  (binding [dg_cycle_nodes_rec nil dg_cycle_nodes_res nil dg_cycle_nodes_visited nil] (try (do (set! dg_cycle_nodes_visited {}) (set! dg_cycle_nodes_rec {}) (set! dg_cycle_nodes_res []) (doseq [k (keys (:graph dg_cycle_nodes_g))] (when (not (in k dg_cycle_nodes_visited)) (set! dg_cycle_nodes_res (dg_cycle_util dg_cycle_nodes_g k dg_cycle_nodes_visited dg_cycle_nodes_rec dg_cycle_nodes_res)))) (throw (ex-info "return" {:v dg_cycle_nodes_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dg_has_cycle_util [dg_has_cycle_util_g dg_has_cycle_util_node dg_has_cycle_util_visited_p dg_has_cycle_util_rec_p]
  (binding [dg_has_cycle_util_edges nil dg_has_cycle_util_i nil dg_has_cycle_util_neigh nil dg_has_cycle_util_rec nil dg_has_cycle_util_visited nil] (try (do (set! dg_has_cycle_util_visited dg_has_cycle_util_visited_p) (set! dg_has_cycle_util_rec dg_has_cycle_util_rec_p) (set! dg_has_cycle_util_visited (assoc dg_has_cycle_util_visited dg_has_cycle_util_node true)) (set! dg_has_cycle_util_rec (assoc dg_has_cycle_util_rec dg_has_cycle_util_node true)) (set! dg_has_cycle_util_edges (get (:graph dg_has_cycle_util_g) dg_has_cycle_util_node)) (set! dg_has_cycle_util_i 0) (while (< dg_has_cycle_util_i (count dg_has_cycle_util_edges)) (do (set! dg_has_cycle_util_neigh (nth (nth dg_has_cycle_util_edges dg_has_cycle_util_i) 1)) (if (not (in dg_has_cycle_util_neigh dg_has_cycle_util_visited)) (when (dg_has_cycle_util dg_has_cycle_util_g dg_has_cycle_util_neigh dg_has_cycle_util_visited dg_has_cycle_util_rec) (throw (ex-info "return" {:v true}))) (when (nth dg_has_cycle_util_rec dg_has_cycle_util_neigh) (throw (ex-info "return" {:v true})))) (set! dg_has_cycle_util_i (+ dg_has_cycle_util_i 1)))) (set! dg_has_cycle_util_rec (assoc dg_has_cycle_util_rec dg_has_cycle_util_node false)) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dg_has_cycle [dg_has_cycle_g]
  (binding [dg_has_cycle_rec nil dg_has_cycle_visited nil] (try (do (set! dg_has_cycle_visited {}) (set! dg_has_cycle_rec {}) (doseq [k (keys (:graph dg_has_cycle_g))] (when (not (in k dg_has_cycle_visited)) (when (dg_has_cycle_util dg_has_cycle_g k dg_has_cycle_visited dg_has_cycle_rec) (throw (ex-info "return" {:v true}))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dg_fill_graph_randomly [dg_fill_graph_randomly_g dg_fill_graph_randomly_c]
  (binding [count_v nil dg_fill_graph_randomly_edge_count nil dg_fill_graph_randomly_i nil dg_fill_graph_randomly_j nil dg_fill_graph_randomly_n nil] (do (set! count_v dg_fill_graph_randomly_c) (when (= count_v (- 1)) (set! count_v (rand_range 10 10010))) (set! dg_fill_graph_randomly_i 0) (while (< dg_fill_graph_randomly_i count_v) (do (set! dg_fill_graph_randomly_edge_count (rand_range 1 103)) (set! dg_fill_graph_randomly_j 0) (while (< dg_fill_graph_randomly_j dg_fill_graph_randomly_edge_count) (do (set! dg_fill_graph_randomly_n (rand_range 0 count_v)) (when (not= dg_fill_graph_randomly_n dg_fill_graph_randomly_i) (dg_add_pair dg_fill_graph_randomly_g dg_fill_graph_randomly_i dg_fill_graph_randomly_n 1)) (set! dg_fill_graph_randomly_j (+ dg_fill_graph_randomly_j 1)))) (set! dg_fill_graph_randomly_i (+ dg_fill_graph_randomly_i 1)))))))

(defn dg_dfs_time [dg_dfs_time_g dg_dfs_time_s dg_dfs_time_e]
  (binding [dg_dfs_time_begin nil dg_dfs_time_end nil] (try (do (set! dg_dfs_time_begin (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647)))) (dg_dfs dg_dfs_time_g dg_dfs_time_s dg_dfs_time_e) (set! dg_dfs_time_end (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647)))) (throw (ex-info "return" {:v (- dg_dfs_time_end dg_dfs_time_begin)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dg_bfs_time [dg_bfs_time_g dg_bfs_time_s]
  (binding [dg_bfs_time_begin nil dg_bfs_time_end nil] (try (do (set! dg_bfs_time_begin (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647)))) (dg_bfs dg_bfs_time_g dg_bfs_time_s) (set! dg_bfs_time_end (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647)))) (throw (ex-info "return" {:v (- dg_bfs_time_end dg_bfs_time_begin)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn g_make_graph []
  (try (throw (ex-info "return" {:v {:graph {}}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn g_add_pair [g_add_pair_g_p g_add_pair_u g_add_pair_v g_add_pair_w]
  (binding [g_add_pair_edges nil g_add_pair_edges2 nil g_add_pair_g nil g_add_pair_m nil g_add_pair_m0 nil g_add_pair_m2 nil g_add_pair_m3 nil] (do (set! g_add_pair_g g_add_pair_g_p) (if (in g_add_pair_u (:graph g_add_pair_g)) (do (set! g_add_pair_edges (get (:graph g_add_pair_g) g_add_pair_u)) (when (not (edge_exists g_add_pair_edges g_add_pair_w g_add_pair_v)) (do (set! g_add_pair_edges (conj g_add_pair_edges [g_add_pair_w g_add_pair_v])) (set! g_add_pair_m (:graph g_add_pair_g)) (set! g_add_pair_m (assoc g_add_pair_m g_add_pair_u g_add_pair_edges)) (set! g_add_pair_g (assoc g_add_pair_g :graph g_add_pair_m))))) (do (set! g_add_pair_m0 (:graph g_add_pair_g)) (set! g_add_pair_m0 (assoc g_add_pair_m0 g_add_pair_u [[g_add_pair_w g_add_pair_v]])) (set! g_add_pair_g (assoc g_add_pair_g :graph g_add_pair_m0)))) (if (in g_add_pair_v (:graph g_add_pair_g)) (do (set! g_add_pair_edges2 (get (:graph g_add_pair_g) g_add_pair_v)) (when (not (edge_exists g_add_pair_edges2 g_add_pair_w g_add_pair_u)) (do (set! g_add_pair_edges2 (conj g_add_pair_edges2 [g_add_pair_w g_add_pair_u])) (set! g_add_pair_m2 (:graph g_add_pair_g)) (set! g_add_pair_m2 (assoc g_add_pair_m2 g_add_pair_v g_add_pair_edges2)) (set! g_add_pair_g (assoc g_add_pair_g :graph g_add_pair_m2))))) (do (set! g_add_pair_m3 (:graph g_add_pair_g)) (set! g_add_pair_m3 (assoc g_add_pair_m3 g_add_pair_v [[g_add_pair_w g_add_pair_u]])) (set! g_add_pair_g (assoc g_add_pair_g :graph g_add_pair_m3)))))))

(defn g_remove_pair [g_remove_pair_g_p g_remove_pair_u g_remove_pair_v]
  (binding [g_remove_pair_edges nil g_remove_pair_edges2 nil g_remove_pair_g nil g_remove_pair_i nil g_remove_pair_j nil g_remove_pair_m nil g_remove_pair_m2 nil g_remove_pair_new_edges nil g_remove_pair_new_edges2 nil] (do (set! g_remove_pair_g g_remove_pair_g_p) (when (in g_remove_pair_u (:graph g_remove_pair_g)) (do (set! g_remove_pair_edges (get (:graph g_remove_pair_g) g_remove_pair_u)) (set! g_remove_pair_new_edges []) (set! g_remove_pair_i 0) (while (< g_remove_pair_i (count g_remove_pair_edges)) (do (when (not= (nth (nth g_remove_pair_edges g_remove_pair_i) 1) g_remove_pair_v) (set! g_remove_pair_new_edges (conj g_remove_pair_new_edges (nth g_remove_pair_edges g_remove_pair_i)))) (set! g_remove_pair_i (+ g_remove_pair_i 1)))) (set! g_remove_pair_m (:graph g_remove_pair_g)) (set! g_remove_pair_m (assoc g_remove_pair_m g_remove_pair_u g_remove_pair_new_edges)) (set! g_remove_pair_g (assoc g_remove_pair_g :graph g_remove_pair_m)))) (when (in g_remove_pair_v (:graph g_remove_pair_g)) (do (set! g_remove_pair_edges2 (get (:graph g_remove_pair_g) g_remove_pair_v)) (set! g_remove_pair_new_edges2 []) (set! g_remove_pair_j 0) (while (< g_remove_pair_j (count g_remove_pair_edges2)) (do (when (not= (nth (nth g_remove_pair_edges2 g_remove_pair_j) 1) g_remove_pair_u) (set! g_remove_pair_new_edges2 (conj g_remove_pair_new_edges2 (nth g_remove_pair_edges2 g_remove_pair_j)))) (set! g_remove_pair_j (+ g_remove_pair_j 1)))) (set! g_remove_pair_m2 (:graph g_remove_pair_g)) (set! g_remove_pair_m2 (assoc g_remove_pair_m2 g_remove_pair_v g_remove_pair_new_edges2)) (set! g_remove_pair_g (assoc g_remove_pair_g :graph g_remove_pair_m2)))))))

(defn g_all_nodes [g_all_nodes_g]
  (binding [g_all_nodes_res nil] (try (do (set! g_all_nodes_res []) (doseq [k (keys (:graph g_all_nodes_g))] (set! g_all_nodes_res (conj g_all_nodes_res k))) (throw (ex-info "return" {:v g_all_nodes_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn g_dfs_util [g_dfs_util_g g_dfs_util_node g_dfs_util_visited_p g_dfs_util_order_p g_dfs_util_d]
  (binding [g_dfs_util_edges nil g_dfs_util_i nil g_dfs_util_neigh nil g_dfs_util_order nil g_dfs_util_visited nil] (try (do (set! g_dfs_util_visited g_dfs_util_visited_p) (set! g_dfs_util_order g_dfs_util_order_p) (set! g_dfs_util_visited (assoc g_dfs_util_visited g_dfs_util_node true)) (set! g_dfs_util_order (conj g_dfs_util_order g_dfs_util_node)) (when (and (not= g_dfs_util_d (- 1)) (= g_dfs_util_node g_dfs_util_d)) (throw (ex-info "return" {:v g_dfs_util_order}))) (set! g_dfs_util_edges (get (:graph g_dfs_util_g) g_dfs_util_node)) (set! g_dfs_util_i 0) (while (< g_dfs_util_i (count g_dfs_util_edges)) (do (set! g_dfs_util_neigh (nth (nth g_dfs_util_edges g_dfs_util_i) 1)) (when (not (in g_dfs_util_neigh g_dfs_util_visited)) (do (set! g_dfs_util_order (g_dfs_util g_dfs_util_g g_dfs_util_neigh g_dfs_util_visited g_dfs_util_order g_dfs_util_d)) (when (and (not= g_dfs_util_d (- 1)) (= (nth g_dfs_util_order (- (count g_dfs_util_order) 1)) g_dfs_util_d)) (throw (ex-info "return" {:v g_dfs_util_order}))))) (set! g_dfs_util_i (+ g_dfs_util_i 1)))) (throw (ex-info "return" {:v g_dfs_util_order}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn g_dfs [g_dfs_g g_dfs_s g_dfs_d]
  (binding [g_dfs_order nil g_dfs_start nil g_dfs_visited nil] (try (do (when (= g_dfs_s g_dfs_d) (throw (ex-info "return" {:v []}))) (set! g_dfs_start (if (= g_dfs_s (- 2)) (first_key (:graph g_dfs_g)) g_dfs_s)) (set! g_dfs_visited {}) (set! g_dfs_order []) (set! g_dfs_order (g_dfs_util g_dfs_g g_dfs_start g_dfs_visited g_dfs_order g_dfs_d)) (throw (ex-info "return" {:v g_dfs_order}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn g_bfs [g_bfs_g g_bfs_s]
  (binding [g_bfs_edges nil g_bfs_i nil g_bfs_neigh nil g_bfs_node nil g_bfs_order nil g_bfs_queue nil g_bfs_start nil g_bfs_visited nil] (try (do (set! g_bfs_queue []) (set! g_bfs_visited {}) (set! g_bfs_order []) (set! g_bfs_start (if (= g_bfs_s (- 2)) (first_key (:graph g_bfs_g)) g_bfs_s)) (set! g_bfs_queue (conj g_bfs_queue g_bfs_start)) (set! g_bfs_visited (assoc g_bfs_visited g_bfs_start true)) (while (> (count g_bfs_queue) 0) (do (set! g_bfs_node (nth g_bfs_queue 0)) (set! g_bfs_queue (subvec g_bfs_queue 1 (count g_bfs_queue))) (set! g_bfs_order (conj g_bfs_order g_bfs_node)) (set! g_bfs_edges (get (:graph g_bfs_g) g_bfs_node)) (set! g_bfs_i 0) (while (< g_bfs_i (count g_bfs_edges)) (do (set! g_bfs_neigh (nth (nth g_bfs_edges g_bfs_i) 1)) (when (not (in g_bfs_neigh g_bfs_visited)) (do (set! g_bfs_queue (conj g_bfs_queue g_bfs_neigh)) (set! g_bfs_visited (assoc g_bfs_visited g_bfs_neigh true)))) (set! g_bfs_i (+ g_bfs_i 1)))))) (throw (ex-info "return" {:v g_bfs_order}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn g_degree [g_degree_g g_degree_u]
  (try (if (in g_degree_u (:graph g_degree_g)) (count (get (:graph g_degree_g) g_degree_u)) 0) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn g_cycle_util [g_cycle_util_g g_cycle_util_node g_cycle_util_visited_p g_cycle_util_parent g_cycle_util_res_p]
  (binding [g_cycle_util_edges nil g_cycle_util_i nil g_cycle_util_neigh nil g_cycle_util_res nil g_cycle_util_visited nil] (try (do (set! g_cycle_util_visited g_cycle_util_visited_p) (set! g_cycle_util_res g_cycle_util_res_p) (set! g_cycle_util_visited (assoc g_cycle_util_visited g_cycle_util_node true)) (set! g_cycle_util_edges (get (:graph g_cycle_util_g) g_cycle_util_node)) (set! g_cycle_util_i 0) (while (< g_cycle_util_i (count g_cycle_util_edges)) (do (set! g_cycle_util_neigh (nth (nth g_cycle_util_edges g_cycle_util_i) 1)) (if (not (in g_cycle_util_neigh g_cycle_util_visited)) (set! g_cycle_util_res (g_cycle_util g_cycle_util_g g_cycle_util_neigh g_cycle_util_visited g_cycle_util_node g_cycle_util_res)) (when (not= g_cycle_util_neigh g_cycle_util_parent) (do (when (not (list_contains_int g_cycle_util_res g_cycle_util_neigh)) (set! g_cycle_util_res (conj g_cycle_util_res g_cycle_util_neigh))) (when (not (list_contains_int g_cycle_util_res g_cycle_util_node)) (set! g_cycle_util_res (conj g_cycle_util_res g_cycle_util_node)))))) (set! g_cycle_util_i (+ g_cycle_util_i 1)))) (throw (ex-info "return" {:v g_cycle_util_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn g_cycle_nodes [g_cycle_nodes_g]
  (binding [g_cycle_nodes_res nil g_cycle_nodes_visited nil] (try (do (set! g_cycle_nodes_visited {}) (set! g_cycle_nodes_res []) (doseq [k (keys (:graph g_cycle_nodes_g))] (when (not (in k g_cycle_nodes_visited)) (set! g_cycle_nodes_res (g_cycle_util g_cycle_nodes_g k g_cycle_nodes_visited (- 1) g_cycle_nodes_res)))) (throw (ex-info "return" {:v g_cycle_nodes_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn g_has_cycle_util [g_has_cycle_util_g g_has_cycle_util_node g_has_cycle_util_visited_p g_has_cycle_util_parent]
  (binding [g_has_cycle_util_edges nil g_has_cycle_util_i nil g_has_cycle_util_neigh nil g_has_cycle_util_visited nil] (try (do (set! g_has_cycle_util_visited g_has_cycle_util_visited_p) (set! g_has_cycle_util_visited (assoc g_has_cycle_util_visited g_has_cycle_util_node true)) (set! g_has_cycle_util_edges (get (:graph g_has_cycle_util_g) g_has_cycle_util_node)) (set! g_has_cycle_util_i 0) (while (< g_has_cycle_util_i (count g_has_cycle_util_edges)) (do (set! g_has_cycle_util_neigh (nth (nth g_has_cycle_util_edges g_has_cycle_util_i) 1)) (if (not (in g_has_cycle_util_neigh g_has_cycle_util_visited)) (when (g_has_cycle_util g_has_cycle_util_g g_has_cycle_util_neigh g_has_cycle_util_visited g_has_cycle_util_node) (throw (ex-info "return" {:v true}))) (when (not= g_has_cycle_util_neigh g_has_cycle_util_parent) (throw (ex-info "return" {:v true})))) (set! g_has_cycle_util_i (+ g_has_cycle_util_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn g_has_cycle [g_has_cycle_g]
  (binding [g_has_cycle_visited nil] (try (do (set! g_has_cycle_visited {}) (doseq [k (keys (:graph g_has_cycle_g))] (when (not (in k g_has_cycle_visited)) (when (g_has_cycle_util g_has_cycle_g k g_has_cycle_visited (- 1)) (throw (ex-info "return" {:v true}))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn g_fill_graph_randomly [g_fill_graph_randomly_g g_fill_graph_randomly_c]
  (binding [count_v nil g_fill_graph_randomly_edge_count nil g_fill_graph_randomly_i nil g_fill_graph_randomly_j nil g_fill_graph_randomly_n nil] (do (set! count_v g_fill_graph_randomly_c) (when (= count_v (- 1)) (set! count_v (rand_range 10 10010))) (set! g_fill_graph_randomly_i 0) (while (< g_fill_graph_randomly_i count_v) (do (set! g_fill_graph_randomly_edge_count (rand_range 1 103)) (set! g_fill_graph_randomly_j 0) (while (< g_fill_graph_randomly_j g_fill_graph_randomly_edge_count) (do (set! g_fill_graph_randomly_n (rand_range 0 count_v)) (when (not= g_fill_graph_randomly_n g_fill_graph_randomly_i) (g_add_pair g_fill_graph_randomly_g g_fill_graph_randomly_i g_fill_graph_randomly_n 1)) (set! g_fill_graph_randomly_j (+ g_fill_graph_randomly_j 1)))) (set! g_fill_graph_randomly_i (+ g_fill_graph_randomly_i 1)))))))

(defn g_dfs_time [g_dfs_time_g g_dfs_time_s g_dfs_time_e]
  (binding [g_dfs_time_begin nil g_dfs_time_end nil] (try (do (set! g_dfs_time_begin (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647)))) (g_dfs g_dfs_time_g g_dfs_time_s g_dfs_time_e) (set! g_dfs_time_end (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647)))) (throw (ex-info "return" {:v (- g_dfs_time_end g_dfs_time_begin)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn g_bfs_time [g_bfs_time_g g_bfs_time_s]
  (binding [g_bfs_time_begin nil g_bfs_time_end nil] (try (do (set! g_bfs_time_begin (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647)))) (g_bfs g_bfs_time_g g_bfs_time_s) (set! g_bfs_time_end (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647)))) (throw (ex-info "return" {:v (- g_bfs_time_end g_bfs_time_begin)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_dg nil main_ug nil] (do (set! main_dg (dg_make_graph)) (dg_add_pair main_dg 0 1 5) (dg_add_pair main_dg 0 2 3) (dg_add_pair main_dg 1 3 2) (dg_add_pair main_dg 2 3 4) (println (str (dg_dfs main_dg (- 2) (- 1)))) (println (str (dg_bfs main_dg (- 2)))) (println (str (dg_in_degree main_dg 3))) (println (str (dg_out_degree main_dg 0))) (println (str (dg_topological_sort main_dg))) (println (str (dg_has_cycle main_dg))) (set! main_ug (g_make_graph)) (g_add_pair main_ug 0 1 1) (g_add_pair main_ug 1 2 1) (g_add_pair main_ug 2 0 1) (println (str (g_dfs main_ug (- 2) (- 1)))) (println (str (g_bfs main_ug (- 2)))) (println (str (g_degree main_ug 1))) (println (str (g_has_cycle main_ug))))))

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
