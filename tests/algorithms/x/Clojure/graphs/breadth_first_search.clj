(ns main (:refer-clojure :exclude [add_edge print_graph bfs]))

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

(declare add_edge print_graph bfs)

(def ^:dynamic add_edge_graph nil)

(def ^:dynamic bfs_head nil)

(def ^:dynamic bfs_i nil)

(def ^:dynamic bfs_neighbor nil)

(def ^:dynamic bfs_neighbors nil)

(def ^:dynamic bfs_order nil)

(def ^:dynamic bfs_queue nil)

(def ^:dynamic bfs_vertex nil)

(def ^:dynamic bfs_visited nil)

(def ^:dynamic print_graph_adj nil)

(def ^:dynamic print_graph_i nil)

(def ^:dynamic print_graph_line nil)

(defn add_edge [add_edge_graph_p add_edge_from add_edge_to]
  (binding [add_edge_graph nil] (do (set! add_edge_graph add_edge_graph_p) (if (in add_edge_from add_edge_graph) (set! add_edge_graph (assoc add_edge_graph add_edge_from (conj (nth add_edge_graph add_edge_from) add_edge_to))) (set! add_edge_graph (assoc add_edge_graph add_edge_from [add_edge_to]))))))

(defn print_graph [print_graph_graph]
  (binding [print_graph_adj nil print_graph_i nil print_graph_line nil] (doseq [v (keys print_graph_graph)] (do (set! print_graph_adj (nth print_graph_graph v)) (set! print_graph_line (str (str v) "  :  ")) (set! print_graph_i 0) (while (< print_graph_i (count print_graph_adj)) (do (set! print_graph_line (str print_graph_line (str (nth print_graph_adj print_graph_i)))) (when (< print_graph_i (- (count print_graph_adj) 1)) (set! print_graph_line (str print_graph_line " -> "))) (set! print_graph_i (+ print_graph_i 1)))) (println print_graph_line)))))

(defn bfs [bfs_graph bfs_start]
  (binding [bfs_head nil bfs_i nil bfs_neighbor nil bfs_neighbors nil bfs_order nil bfs_queue nil bfs_vertex nil bfs_visited nil] (try (do (set! bfs_visited {}) (set! bfs_queue []) (set! bfs_order []) (set! bfs_queue (conj bfs_queue bfs_start)) (set! bfs_visited (assoc bfs_visited bfs_start true)) (set! bfs_head 0) (while (< bfs_head (count bfs_queue)) (do (set! bfs_vertex (nth bfs_queue bfs_head)) (set! bfs_head (+ bfs_head 1)) (set! bfs_order (conj bfs_order bfs_vertex)) (set! bfs_neighbors (nth bfs_graph bfs_vertex)) (set! bfs_i 0) (while (< bfs_i (count bfs_neighbors)) (do (set! bfs_neighbor (nth bfs_neighbors bfs_i)) (when (not (in bfs_neighbor bfs_visited)) (do (set! bfs_visited (assoc bfs_visited bfs_neighbor true)) (set! bfs_queue (conj bfs_queue bfs_neighbor)))) (set! bfs_i (+ bfs_i 1)))))) (throw (ex-info "return" {:v bfs_order}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_g {})

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (add_edge main_g 0 1)
      (add_edge main_g 0 2)
      (add_edge main_g 1 2)
      (add_edge main_g 2 0)
      (add_edge main_g 2 3)
      (add_edge main_g 3 3)
      (print_graph main_g)
      (println (bfs main_g 2))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
