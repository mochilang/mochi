(ns main (:refer-clojure :exclude [make_matrix dfs check_circuit_or_path check_euler]))

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

(declare make_matrix dfs check_circuit_or_path check_euler)

(def ^:dynamic check_circuit_or_path_i nil)

(def ^:dynamic check_circuit_or_path_odd_degree_nodes nil)

(def ^:dynamic check_circuit_or_path_odd_node nil)

(def ^:dynamic check_euler_path nil)

(def ^:dynamic check_euler_res nil)

(def ^:dynamic check_euler_start_node nil)

(def ^:dynamic check_euler_visited_edge nil)

(def ^:dynamic dfs_i nil)

(def ^:dynamic dfs_neighbors nil)

(def ^:dynamic dfs_path nil)

(def ^:dynamic dfs_v nil)

(def ^:dynamic dfs_visited_edge nil)

(def ^:dynamic make_matrix_i nil)

(def ^:dynamic make_matrix_j nil)

(def ^:dynamic make_matrix_matrix nil)

(def ^:dynamic make_matrix_row nil)

(defn make_matrix [make_matrix_n]
  (binding [make_matrix_i nil make_matrix_j nil make_matrix_matrix nil make_matrix_row nil] (try (do (set! make_matrix_matrix []) (set! make_matrix_i 0) (while (<= make_matrix_i make_matrix_n) (do (set! make_matrix_row []) (set! make_matrix_j 0) (while (<= make_matrix_j make_matrix_n) (do (set! make_matrix_row (conj make_matrix_row false)) (set! make_matrix_j (+ make_matrix_j 1)))) (set! make_matrix_matrix (conj make_matrix_matrix make_matrix_row)) (set! make_matrix_i (+ make_matrix_i 1)))) (throw (ex-info "return" {:v make_matrix_matrix}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dfs [dfs_u dfs_graph dfs_visited_edge_p dfs_path_p]
  (binding [dfs_i nil dfs_neighbors nil dfs_path nil dfs_v nil dfs_visited_edge nil] (try (do (set! dfs_visited_edge dfs_visited_edge_p) (set! dfs_path dfs_path_p) (set! dfs_path (conj dfs_path dfs_u)) (when (in dfs_u dfs_graph) (do (set! dfs_neighbors (get dfs_graph dfs_u)) (set! dfs_i 0) (while (< dfs_i (count dfs_neighbors)) (do (set! dfs_v (nth dfs_neighbors dfs_i)) (when (= (nth (nth dfs_visited_edge dfs_u) dfs_v) false) (do (set! dfs_visited_edge (assoc-in dfs_visited_edge [dfs_u dfs_v] true)) (set! dfs_visited_edge (assoc-in dfs_visited_edge [dfs_v dfs_u] true)) (set! dfs_path (dfs dfs_v dfs_graph dfs_visited_edge dfs_path)))) (set! dfs_i (+ dfs_i 1)))))) (throw (ex-info "return" {:v dfs_path}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn check_circuit_or_path [check_circuit_or_path_graph check_circuit_or_path_max_node]
  (binding [check_circuit_or_path_i nil check_circuit_or_path_odd_degree_nodes nil check_circuit_or_path_odd_node nil] (try (do (set! check_circuit_or_path_odd_degree_nodes 0) (set! check_circuit_or_path_odd_node (- 1)) (set! check_circuit_or_path_i 0) (while (< check_circuit_or_path_i check_circuit_or_path_max_node) (do (when (in check_circuit_or_path_i check_circuit_or_path_graph) (when (= (mod (count (get check_circuit_or_path_graph check_circuit_or_path_i)) 2) 1) (do (set! check_circuit_or_path_odd_degree_nodes (+ check_circuit_or_path_odd_degree_nodes 1)) (set! check_circuit_or_path_odd_node check_circuit_or_path_i)))) (set! check_circuit_or_path_i (+ check_circuit_or_path_i 1)))) (when (= check_circuit_or_path_odd_degree_nodes 0) (throw (ex-info "return" {:v {:odd_node check_circuit_or_path_odd_node :status 1}}))) (if (= check_circuit_or_path_odd_degree_nodes 2) {:odd_node check_circuit_or_path_odd_node :status 2} {:odd_node check_circuit_or_path_odd_node :status 3})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn check_euler [check_euler_graph check_euler_max_node]
  (binding [check_euler_path nil check_euler_res nil check_euler_start_node nil check_euler_visited_edge nil] (try (do (set! check_euler_visited_edge (make_matrix check_euler_max_node)) (set! check_euler_res (check_circuit_or_path check_euler_graph check_euler_max_node)) (when (= (:status check_euler_res) 3) (do (println "graph is not Eulerian") (println "no path") (throw (ex-info "return" {:v nil})))) (set! check_euler_start_node 1) (when (= (:status check_euler_res) 2) (do (set! check_euler_start_node (:odd_node check_euler_res)) (println "graph has a Euler path"))) (when (= (:status check_euler_res) 1) (println "graph has a Euler cycle")) (set! check_euler_path (dfs check_euler_start_node check_euler_graph check_euler_visited_edge [])) (println (str check_euler_path))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_g1 {1 [2 3 4] 2 [1 3] 3 [1 2] 4 [1 5] 5 [4]})

(def ^:dynamic main_g2 {1 [2 3 4 5] 2 [1 3] 3 [1 2] 4 [1 5] 5 [1 4]})

(def ^:dynamic main_g3 {1 [2 3 4] 2 [1 3 4] 3 [1 2] 4 [1 2 5] 5 [4]})

(def ^:dynamic main_g4 {1 [2 3] 2 [1 3] 3 [1 2]})

(def ^:dynamic main_g5 {1 [] 2 []})

(def ^:dynamic main_max_node 10)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (check_euler main_g1 main_max_node)
      (check_euler main_g2 main_max_node)
      (check_euler main_g3 main_max_node)
      (check_euler main_g4 main_max_node)
      (check_euler main_g5 main_max_node)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
