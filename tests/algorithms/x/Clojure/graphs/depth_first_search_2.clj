(ns main (:refer-clojure :exclude [add_edge list_to_string list_to_arrow print_graph dfs_recursive dfs]))

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

(declare add_edge list_to_string list_to_arrow print_graph dfs_recursive dfs)

(def ^:dynamic add_edge_g nil)

(def ^:dynamic add_edge_lst nil)

(def ^:dynamic add_edge_v nil)

(def ^:dynamic dfs_i nil)

(def ^:dynamic dfs_n nil)

(def ^:dynamic dfs_order nil)

(def ^:dynamic dfs_recursive_i nil)

(def ^:dynamic dfs_recursive_nb nil)

(def ^:dynamic dfs_recursive_neighbors nil)

(def ^:dynamic dfs_recursive_order nil)

(def ^:dynamic dfs_recursive_visited nil)

(def ^:dynamic dfs_visited nil)

(def ^:dynamic list_to_arrow_i nil)

(def ^:dynamic list_to_arrow_res nil)

(def ^:dynamic list_to_string_i nil)

(def ^:dynamic list_to_string_res nil)

(def ^:dynamic main_g nil)

(def ^:dynamic print_graph_edges nil)

(def ^:dynamic print_graph_i nil)

(def ^:dynamic print_graph_line nil)

(defn add_edge [add_edge_g_p add_edge_from_vertex add_edge_to_vertex]
  (binding [add_edge_g nil add_edge_lst nil add_edge_v nil] (try (do (set! add_edge_g add_edge_g_p) (set! add_edge_v (:vertex add_edge_g)) (if (in add_edge_from_vertex add_edge_v) (do (set! add_edge_lst (nth add_edge_v add_edge_from_vertex)) (set! add_edge_lst (conj add_edge_lst add_edge_to_vertex)) (set! add_edge_v (assoc add_edge_v add_edge_from_vertex add_edge_lst))) (set! add_edge_v (assoc add_edge_v add_edge_from_vertex [add_edge_to_vertex]))) (set! add_edge_g (assoc add_edge_g :vertex add_edge_v)) (when (> (+ add_edge_from_vertex 1) (:size add_edge_g)) (set! add_edge_g (assoc add_edge_g :size (+ add_edge_from_vertex 1)))) (when (> (+ add_edge_to_vertex 1) (:size add_edge_g)) (set! add_edge_g (assoc add_edge_g :size (+ add_edge_to_vertex 1)))) (throw (ex-info "return" {:v add_edge_g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_to_string [list_to_string_lst]
  (binding [list_to_string_i nil list_to_string_res nil] (try (do (set! list_to_string_res "") (set! list_to_string_i 0) (while (< list_to_string_i (count list_to_string_lst)) (do (set! list_to_string_res (str list_to_string_res (str (nth list_to_string_lst list_to_string_i)))) (when (< list_to_string_i (- (count list_to_string_lst) 1)) (set! list_to_string_res (str list_to_string_res " "))) (set! list_to_string_i (+ list_to_string_i 1)))) (throw (ex-info "return" {:v list_to_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_to_arrow [list_to_arrow_lst]
  (binding [list_to_arrow_i nil list_to_arrow_res nil] (try (do (set! list_to_arrow_res "") (set! list_to_arrow_i 0) (while (< list_to_arrow_i (count list_to_arrow_lst)) (do (set! list_to_arrow_res (str list_to_arrow_res (str (nth list_to_arrow_lst list_to_arrow_i)))) (when (< list_to_arrow_i (- (count list_to_arrow_lst) 1)) (set! list_to_arrow_res (str list_to_arrow_res " -> "))) (set! list_to_arrow_i (+ list_to_arrow_i 1)))) (throw (ex-info "return" {:v list_to_arrow_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_graph [print_graph_g]
  (binding [print_graph_edges nil print_graph_i nil print_graph_line nil] (do (println (str (:vertex print_graph_g))) (set! print_graph_i 0) (while (< print_graph_i (:size print_graph_g)) (do (set! print_graph_edges []) (when (in print_graph_i (:vertex print_graph_g)) (set! print_graph_edges (get (:vertex print_graph_g) print_graph_i))) (set! print_graph_line (str (str (str print_graph_i) "  ->  ") (list_to_arrow print_graph_edges))) (println print_graph_line) (set! print_graph_i (+ print_graph_i 1)))))))

(defn dfs_recursive [dfs_recursive_g dfs_recursive_start_vertex dfs_recursive_visited_p dfs_recursive_order_p]
  (binding [dfs_recursive_i nil dfs_recursive_nb nil dfs_recursive_neighbors nil dfs_recursive_order nil dfs_recursive_visited nil] (try (do (set! dfs_recursive_visited dfs_recursive_visited_p) (set! dfs_recursive_order dfs_recursive_order_p) (set! dfs_recursive_visited (assoc dfs_recursive_visited dfs_recursive_start_vertex true)) (set! dfs_recursive_order (conj dfs_recursive_order dfs_recursive_start_vertex)) (when (in dfs_recursive_start_vertex (:vertex dfs_recursive_g)) (do (set! dfs_recursive_neighbors (get (:vertex dfs_recursive_g) dfs_recursive_start_vertex)) (set! dfs_recursive_i 0) (while (< dfs_recursive_i (count dfs_recursive_neighbors)) (do (set! dfs_recursive_nb (nth dfs_recursive_neighbors dfs_recursive_i)) (when (not (nth dfs_recursive_visited dfs_recursive_nb)) (set! dfs_recursive_order (dfs_recursive dfs_recursive_g dfs_recursive_nb dfs_recursive_visited dfs_recursive_order))) (set! dfs_recursive_i (+ dfs_recursive_i 1)))))) (throw (ex-info "return" {:v dfs_recursive_order}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dfs [dfs_g]
  (binding [dfs_i nil dfs_n nil dfs_order nil dfs_visited nil] (try (do (set! dfs_n (:size dfs_g)) (set! dfs_visited []) (set! dfs_i 0) (while (< dfs_i dfs_n) (do (set! dfs_visited (conj dfs_visited false)) (set! dfs_i (+ dfs_i 1)))) (set! dfs_order []) (set! dfs_i 0) (while (< dfs_i dfs_n) (do (when (not (nth dfs_visited dfs_i)) (set! dfs_order (dfs_recursive dfs_g dfs_i dfs_visited dfs_order))) (set! dfs_i (+ dfs_i 1)))) (throw (ex-info "return" {:v dfs_order}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_g {:size 0 :vertex {}})

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def main_g (add_edge main_g 0 1))
      (def main_g (add_edge main_g 0 2))
      (def main_g (add_edge main_g 1 2))
      (def main_g (add_edge main_g 2 0))
      (def main_g (add_edge main_g 2 3))
      (def main_g (add_edge main_g 3 3))
      (print_graph main_g)
      (println "DFS:")
      (println (list_to_string (dfs main_g)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
