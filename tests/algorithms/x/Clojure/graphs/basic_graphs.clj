(ns main (:refer-clojure :exclude [dfs bfs sort_ints dijkstra topo floyd prim sort_edges find_parent union_parent kruskal find_isolated_nodes]))

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

(declare dfs bfs sort_ints dijkstra topo floyd prim sort_edges find_parent union_parent kruskal find_isolated_nodes)

(def ^:dynamic bfs_q nil)

(def ^:dynamic bfs_u nil)

(def ^:dynamic bfs_visited nil)

(def ^:dynamic count_v nil)

(def ^:dynamic dfs_found nil)

(def ^:dynamic dfs_stack nil)

(def ^:dynamic dfs_u nil)

(def ^:dynamic dfs_visited nil)

(def ^:dynamic dijkstra_alt nil)

(def ^:dynamic dijkstra_cur nil)

(def ^:dynamic dijkstra_d nil)

(def ^:dynamic dijkstra_dist nil)

(def ^:dynamic dijkstra_i nil)

(def ^:dynamic dijkstra_idx nil)

(def ^:dynamic dijkstra_k nil)

(def ^:dynamic dijkstra_keys nil)

(def ^:dynamic dijkstra_known nil)

(def ^:dynamic dijkstra_mini nil)

(def ^:dynamic dijkstra_ordered nil)

(def ^:dynamic dijkstra_path nil)

(def ^:dynamic dijkstra_u nil)

(def ^:dynamic dijkstra_v nil)

(def ^:dynamic dijkstra_w nil)

(def ^:dynamic find_isolated_nodes_isolated nil)

(def ^:dynamic find_parent_r nil)

(def ^:dynamic floyd_dist nil)

(def ^:dynamic floyd_i nil)

(def ^:dynamic floyd_ii nil)

(def ^:dynamic floyd_j nil)

(def ^:dynamic floyd_jj nil)

(def ^:dynamic floyd_k nil)

(def ^:dynamic floyd_n nil)

(def ^:dynamic floyd_row nil)

(def ^:dynamic kruskal_e nil)

(def ^:dynamic kruskal_es nil)

(def ^:dynamic kruskal_i nil)

(def ^:dynamic kruskal_idx nil)

(def ^:dynamic kruskal_parent nil)

(def ^:dynamic kruskal_ru nil)

(def ^:dynamic kruskal_rv nil)

(def ^:dynamic kruskal_total nil)

(def ^:dynamic kruskal_u nil)

(def ^:dynamic kruskal_v nil)

(def ^:dynamic kruskal_w nil)

(def ^:dynamic prim_cur nil)

(def ^:dynamic prim_d nil)

(def ^:dynamic prim_dist nil)

(def ^:dynamic prim_i nil)

(def ^:dynamic prim_k nil)

(def ^:dynamic prim_keys nil)

(def ^:dynamic prim_known nil)

(def ^:dynamic prim_mini nil)

(def ^:dynamic prim_total nil)

(def ^:dynamic prim_u nil)

(def ^:dynamic prim_v nil)

(def ^:dynamic prim_w nil)

(def ^:dynamic sort_edges_es nil)

(def ^:dynamic sort_edges_i nil)

(def ^:dynamic sort_edges_j nil)

(def ^:dynamic sort_edges_tmp nil)

(def ^:dynamic sort_ints_arr nil)

(def ^:dynamic sort_ints_i nil)

(def ^:dynamic sort_ints_j nil)

(def ^:dynamic sort_ints_tmp nil)

(def ^:dynamic topo_i nil)

(def ^:dynamic topo_ind nil)

(def ^:dynamic topo_j nil)

(def ^:dynamic topo_node nil)

(def ^:dynamic topo_q nil)

(def ^:dynamic topo_v nil)

(def ^:dynamic union_parent_parent nil)

(defn dfs [dfs_g dfs_s]
  (binding [dfs_found nil dfs_stack nil dfs_u nil dfs_visited nil] (do (set! dfs_visited {}) (set! dfs_stack []) (set! dfs_visited (assoc dfs_visited dfs_s true)) (set! dfs_stack (conj dfs_stack dfs_s)) (println dfs_s) (while (> (count dfs_stack) 0) (do (set! dfs_u (nth dfs_stack (- (count dfs_stack) 1))) (set! dfs_found false) (loop [v_seq (nth dfs_g dfs_u)] (when (seq v_seq) (let [v (first v_seq)] (cond (not (in v dfs_visited)) (do (set! dfs_visited (assoc dfs_visited v true)) (set! dfs_stack (conj dfs_stack v)) (println v) (set! dfs_found true) (recur nil)) :else (recur (rest v_seq)))))) (when (not dfs_found) (set! dfs_stack (subvec dfs_stack 0 (min (- (count dfs_stack) 1) (count dfs_stack))))))))))

(defn bfs [bfs_g bfs_s]
  (binding [bfs_q nil bfs_u nil bfs_visited nil] (do (set! bfs_visited {}) (set! bfs_q []) (set! bfs_visited (assoc bfs_visited bfs_s true)) (set! bfs_q (conj bfs_q bfs_s)) (println bfs_s) (while (> (count bfs_q) 0) (do (set! bfs_u (nth bfs_q 0)) (set! bfs_q (subvec bfs_q 1 (min (count bfs_q) (count bfs_q)))) (doseq [v (nth bfs_g bfs_u)] (when (not (in v bfs_visited)) (do (set! bfs_visited (assoc bfs_visited v true)) (set! bfs_q (conj bfs_q v)) (println v)))))))))

(defn sort_ints [sort_ints_a]
  (binding [sort_ints_arr nil sort_ints_i nil sort_ints_j nil sort_ints_tmp nil] (try (do (set! sort_ints_arr sort_ints_a) (set! sort_ints_i 0) (while (< sort_ints_i (count sort_ints_arr)) (do (set! sort_ints_j 0) (while (< sort_ints_j (- (- (count sort_ints_arr) sort_ints_i) 1)) (do (when (> (nth sort_ints_arr sort_ints_j) (nth sort_ints_arr (+ sort_ints_j 1))) (do (set! sort_ints_tmp (nth sort_ints_arr sort_ints_j)) (set! sort_ints_arr (assoc sort_ints_arr sort_ints_j (nth sort_ints_arr (+ sort_ints_j 1)))) (set! sort_ints_arr (assoc sort_ints_arr (+ sort_ints_j 1) sort_ints_tmp)))) (set! sort_ints_j (+ sort_ints_j 1)))) (set! sort_ints_i (+ sort_ints_i 1)))) (throw (ex-info "return" {:v sort_ints_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dijkstra [dijkstra_g dijkstra_s]
  (binding [dijkstra_alt nil dijkstra_cur nil dijkstra_d nil dijkstra_dist nil dijkstra_i nil dijkstra_idx nil dijkstra_k nil dijkstra_keys nil dijkstra_known nil dijkstra_mini nil dijkstra_ordered nil dijkstra_path nil dijkstra_u nil dijkstra_v nil dijkstra_w nil] (do (set! dijkstra_dist {}) (set! dijkstra_dist (assoc dijkstra_dist dijkstra_s 0)) (set! dijkstra_path {}) (set! dijkstra_path (assoc dijkstra_path dijkstra_s 0)) (set! dijkstra_known []) (set! dijkstra_keys [dijkstra_s]) (while (< (count dijkstra_known) (count dijkstra_keys)) (do (set! dijkstra_mini 100000) (set! dijkstra_u (- 1)) (set! dijkstra_i 0) (while (< dijkstra_i (count dijkstra_keys)) (do (set! dijkstra_k (nth dijkstra_keys dijkstra_i)) (set! dijkstra_d (nth dijkstra_dist dijkstra_k)) (when (and (not (in dijkstra_k dijkstra_known)) (< dijkstra_d dijkstra_mini)) (do (set! dijkstra_mini dijkstra_d) (set! dijkstra_u dijkstra_k))) (set! dijkstra_i (+ dijkstra_i 1)))) (set! dijkstra_known (conj dijkstra_known dijkstra_u)) (doseq [e (nth dijkstra_g dijkstra_u)] (do (set! dijkstra_v (nth e 0)) (set! dijkstra_w (nth e 1)) (when (not (in dijkstra_v dijkstra_keys)) (set! dijkstra_keys (conj dijkstra_keys dijkstra_v))) (set! dijkstra_alt (+ (nth dijkstra_dist dijkstra_u) dijkstra_w)) (set! dijkstra_cur (if (in dijkstra_v dijkstra_dist) (nth dijkstra_dist dijkstra_v) 100000)) (when (and (not (in dijkstra_v dijkstra_known)) (< dijkstra_alt dijkstra_cur)) (do (set! dijkstra_dist (assoc dijkstra_dist dijkstra_v dijkstra_alt)) (set! dijkstra_path (assoc dijkstra_path dijkstra_v dijkstra_u)))))))) (set! dijkstra_ordered (sort_ints dijkstra_keys)) (set! dijkstra_idx 0) (while (< dijkstra_idx (count dijkstra_ordered)) (do (set! dijkstra_k (nth dijkstra_ordered dijkstra_idx)) (when (not= dijkstra_k dijkstra_s) (println (nth dijkstra_dist dijkstra_k))) (set! dijkstra_idx (+ dijkstra_idx 1)))))))

(defn topo [topo_g topo_n]
  (binding [topo_i nil topo_ind nil topo_j nil topo_node nil topo_q nil topo_v nil] (do (set! topo_ind []) (set! topo_i 0) (while (<= topo_i topo_n) (do (set! topo_ind (conj topo_ind 0)) (set! topo_i (+ topo_i 1)))) (set! topo_node 1) (while (<= topo_node topo_n) (do (doseq [v (nth topo_g topo_node)] (set! topo_ind (assoc topo_ind v (+ (nth topo_ind v) 1)))) (set! topo_node (+ topo_node 1)))) (set! topo_q []) (set! topo_j 1) (while (<= topo_j topo_n) (do (when (= (nth topo_ind topo_j) 0) (set! topo_q (conj topo_q topo_j))) (set! topo_j (+ topo_j 1)))) (while (> (count topo_q) 0) (do (set! topo_v (nth topo_q 0)) (set! topo_q (subvec topo_q 1 (min (count topo_q) (count topo_q)))) (println topo_v) (doseq [w (nth topo_g topo_v)] (do (set! topo_ind (assoc topo_ind w (- (nth topo_ind w) 1))) (when (= (nth topo_ind w) 0) (set! topo_q (conj topo_q w))))))))))

(defn floyd [floyd_a]
  (binding [floyd_dist nil floyd_i nil floyd_ii nil floyd_j nil floyd_jj nil floyd_k nil floyd_n nil floyd_row nil] (do (set! floyd_n (count floyd_a)) (set! floyd_dist []) (set! floyd_i 0) (while (< floyd_i floyd_n) (do (set! floyd_row []) (set! floyd_j 0) (while (< floyd_j floyd_n) (do (set! floyd_row (conj floyd_row (nth (nth floyd_a floyd_i) floyd_j))) (set! floyd_j (+ floyd_j 1)))) (set! floyd_dist (conj floyd_dist floyd_row)) (set! floyd_i (+ floyd_i 1)))) (set! floyd_k 0) (while (< floyd_k floyd_n) (do (set! floyd_ii 0) (while (< floyd_ii floyd_n) (do (set! floyd_jj 0) (while (< floyd_jj floyd_n) (do (when (> (nth (nth floyd_dist floyd_ii) floyd_jj) (+ (nth (nth floyd_dist floyd_ii) floyd_k) (nth (nth floyd_dist floyd_k) floyd_jj))) (set! floyd_dist (assoc-in floyd_dist [floyd_ii floyd_jj] (+ (nth (nth floyd_dist floyd_ii) floyd_k) (nth (nth floyd_dist floyd_k) floyd_jj))))) (set! floyd_jj (+ floyd_jj 1)))) (set! floyd_ii (+ floyd_ii 1)))) (set! floyd_k (+ floyd_k 1)))) (println floyd_dist))))

(defn prim [prim_g prim_s prim_n]
  (binding [prim_cur nil prim_d nil prim_dist nil prim_i nil prim_k nil prim_keys nil prim_known nil prim_mini nil prim_total nil prim_u nil prim_v nil prim_w nil] (try (do (set! prim_dist {}) (set! prim_dist (assoc prim_dist prim_s 0)) (set! prim_known []) (set! prim_keys [prim_s]) (set! prim_total 0) (while (< (count prim_known) prim_n) (do (set! prim_mini 100000) (set! prim_u (- 1)) (set! prim_i 0) (while (< prim_i (count prim_keys)) (do (set! prim_k (nth prim_keys prim_i)) (set! prim_d (nth prim_dist prim_k)) (when (and (not (in prim_k prim_known)) (< prim_d prim_mini)) (do (set! prim_mini prim_d) (set! prim_u prim_k))) (set! prim_i (+ prim_i 1)))) (set! prim_known (conj prim_known prim_u)) (set! prim_total (+ prim_total prim_mini)) (doseq [e (nth prim_g prim_u)] (do (set! prim_v (nth e 0)) (set! prim_w (nth e 1)) (when (not (in prim_v prim_keys)) (set! prim_keys (conj prim_keys prim_v))) (set! prim_cur (if (in prim_v prim_dist) (nth prim_dist prim_v) 100000)) (when (and (not (in prim_v prim_known)) (< prim_w prim_cur)) (set! prim_dist (assoc prim_dist prim_v prim_w))))))) (throw (ex-info "return" {:v prim_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_edges [sort_edges_edges]
  (binding [sort_edges_es nil sort_edges_i nil sort_edges_j nil sort_edges_tmp nil] (try (do (set! sort_edges_es sort_edges_edges) (set! sort_edges_i 0) (while (< sort_edges_i (count sort_edges_es)) (do (set! sort_edges_j 0) (while (< sort_edges_j (- (- (count sort_edges_es) sort_edges_i) 1)) (do (when (> (nth (nth sort_edges_es sort_edges_j) 2) (nth (nth sort_edges_es (+ sort_edges_j 1)) 2)) (do (set! sort_edges_tmp (nth sort_edges_es sort_edges_j)) (set! sort_edges_es (assoc sort_edges_es sort_edges_j (nth sort_edges_es (+ sort_edges_j 1)))) (set! sort_edges_es (assoc sort_edges_es (+ sort_edges_j 1) sort_edges_tmp)))) (set! sort_edges_j (+ sort_edges_j 1)))) (set! sort_edges_i (+ sort_edges_i 1)))) (throw (ex-info "return" {:v sort_edges_es}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_parent [find_parent_parent find_parent_x]
  (binding [find_parent_r nil] (try (do (set! find_parent_r find_parent_x) (while (not= (nth find_parent_parent find_parent_r) find_parent_r) (set! find_parent_r (nth find_parent_parent find_parent_r))) (throw (ex-info "return" {:v find_parent_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn union_parent [union_parent_parent_p union_parent_a union_parent_b]
  (binding [union_parent_parent nil] (do (set! union_parent_parent union_parent_parent_p) (set! union_parent_parent (assoc union_parent_parent union_parent_a union_parent_b)))))

(defn kruskal [kruskal_edges kruskal_n]
  (binding [count_v nil kruskal_e nil kruskal_es nil kruskal_i nil kruskal_idx nil kruskal_parent nil kruskal_ru nil kruskal_rv nil kruskal_total nil kruskal_u nil kruskal_v nil kruskal_w nil] (try (do (set! kruskal_es (sort_edges kruskal_edges)) (set! kruskal_parent []) (set! kruskal_i 0) (while (<= kruskal_i kruskal_n) (do (set! kruskal_parent (conj kruskal_parent kruskal_i)) (set! kruskal_i (+ kruskal_i 1)))) (set! kruskal_total 0) (set! count_v 0) (set! kruskal_idx 0) (while (and (< count_v (- kruskal_n 1)) (< kruskal_idx (count kruskal_es))) (do (set! kruskal_e (nth kruskal_es kruskal_idx)) (set! kruskal_idx (+ kruskal_idx 1)) (set! kruskal_u (nth kruskal_e 0)) (set! kruskal_v (nth kruskal_e 1)) (set! kruskal_w (nth kruskal_e 2)) (set! kruskal_ru (find_parent kruskal_parent kruskal_u)) (set! kruskal_rv (find_parent kruskal_parent kruskal_v)) (when (not= kruskal_ru kruskal_rv) (do (union_parent kruskal_parent kruskal_ru kruskal_rv) (set! kruskal_total (+ kruskal_total kruskal_w)) (set! count_v (+ count_v 1)))))) (throw (ex-info "return" {:v kruskal_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_isolated_nodes [find_isolated_nodes_g find_isolated_nodes_nodes]
  (binding [find_isolated_nodes_isolated nil] (try (do (set! find_isolated_nodes_isolated []) (doseq [node find_isolated_nodes_nodes] (when (= (count (nth find_isolated_nodes_g node)) 0) (set! find_isolated_nodes_isolated (conj find_isolated_nodes_isolated node)))) (throw (ex-info "return" {:v find_isolated_nodes_isolated}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_g_dfs {1 [2 3] 2 [4 5] 3 [] 4 [] 5 []})

(def ^:dynamic main_g_bfs {1 [2 3] 2 [4 5] 3 [6 7] 4 [] 5 [8] 6 [] 7 [] 8 []})

(def ^:dynamic main_g_weighted {1 [[2 7] [3 9] [6 14]] 2 [[1 7] [3 10] [4 15]] 3 [[1 9] [2 10] [4 11] [6 2]] 4 [[2 15] [3 11] [5 6]] 5 [[4 6] [6 9]] 6 [[1 14] [3 2] [5 9]]})

(def ^:dynamic main_g_topo {1 [2 3] 2 [4] 3 [4] 4 []})

(def ^:dynamic main_matrix [[0 5 9 100000] [100000 0 2 8] [100000 100000 0 7] [4 100000 100000 0]])

(def ^:dynamic main_g_prim {1 [[2 1] [3 3]] 2 [[1 1] [3 1] [4 6]] 3 [[1 3] [2 1] [4 2]] 4 [[2 6] [3 2]]})

(def ^:dynamic main_edges_kruskal [[1 2 1] [2 3 2] [1 3 2] [3 4 1]])

(def ^:dynamic main_g_iso {1 [2 3] 2 [1 3] 3 [1 2] 4 []})

(def ^:dynamic main_iso (find_isolated_nodes main_g_iso [1 2 3 4]))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (dfs main_g_dfs 1)
      (bfs main_g_bfs 1)
      (dijkstra main_g_weighted 1)
      (topo main_g_topo 4)
      (floyd main_matrix)
      (println (prim main_g_prim 1 4))
      (println (kruskal main_edges_kruskal 4))
      (println main_iso)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
