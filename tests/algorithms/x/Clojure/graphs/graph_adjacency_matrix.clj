(ns main (:refer-clojure :exclude [make_graph contains_vertex add_vertex remove_key decrement_indices remove_vertex add_edge remove_edge contains_edge clear_graph]))

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

(declare make_graph contains_vertex add_vertex remove_key decrement_indices remove_vertex add_edge remove_edge contains_edge clear_graph)

(def ^:dynamic add_edge_g nil)

(def ^:dynamic add_edge_i nil)

(def ^:dynamic add_edge_j nil)

(def ^:dynamic add_edge_matrix nil)

(def ^:dynamic add_vertex_g nil)

(def ^:dynamic add_vertex_i nil)

(def ^:dynamic add_vertex_idx_map nil)

(def ^:dynamic add_vertex_j nil)

(def ^:dynamic add_vertex_matrix nil)

(def ^:dynamic add_vertex_row nil)

(def ^:dynamic clear_graph_g nil)

(def ^:dynamic contains_edge_i nil)

(def ^:dynamic contains_edge_j nil)

(def ^:dynamic contains_edge_matrix nil)

(def ^:dynamic decrement_indices_idx nil)

(def ^:dynamic decrement_indices_out nil)

(def ^:dynamic make_graph_e nil)

(def ^:dynamic make_graph_g nil)

(def ^:dynamic make_graph_i nil)

(def ^:dynamic make_graph_j nil)

(def ^:dynamic remove_edge_g nil)

(def ^:dynamic remove_edge_i nil)

(def ^:dynamic remove_edge_j nil)

(def ^:dynamic remove_edge_matrix nil)

(def ^:dynamic remove_key_out nil)

(def ^:dynamic remove_vertex_g nil)

(def ^:dynamic remove_vertex_i nil)

(def ^:dynamic remove_vertex_idx nil)

(def ^:dynamic remove_vertex_j nil)

(def ^:dynamic remove_vertex_m nil)

(def ^:dynamic remove_vertex_new_matrix nil)

(def ^:dynamic remove_vertex_new_row nil)

(def ^:dynamic remove_vertex_row nil)

(defn make_graph [make_graph_vertices make_graph_edges make_graph_directed]
  (binding [make_graph_e nil make_graph_g nil make_graph_i nil make_graph_j nil] (try (do (set! make_graph_g {:adj_matrix [] :directed make_graph_directed :vertex_to_index {}}) (set! make_graph_i 0) (while (< make_graph_i (count make_graph_vertices)) (do (set! make_graph_g (add_vertex make_graph_g (nth make_graph_vertices make_graph_i))) (set! make_graph_i (+ make_graph_i 1)))) (set! make_graph_j 0) (while (< make_graph_j (count make_graph_edges)) (do (set! make_graph_e (nth make_graph_edges make_graph_j)) (set! make_graph_g (add_edge make_graph_g (nth make_graph_e 0) (nth make_graph_e 1))) (set! make_graph_j (+ make_graph_j 1)))) (throw (ex-info "return" {:v make_graph_g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_vertex [contains_vertex_g contains_vertex_v]
  (try (throw (ex-info "return" {:v (in contains_vertex_v (:vertex_to_index contains_vertex_g))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn add_vertex [add_vertex_g_p add_vertex_v]
  (binding [add_vertex_g nil add_vertex_i nil add_vertex_idx_map nil add_vertex_j nil add_vertex_matrix nil add_vertex_row nil] (do (set! add_vertex_g add_vertex_g_p) (when (contains_vertex add_vertex_g add_vertex_v) (throw (Exception. "vertex already exists"))) (set! add_vertex_matrix (:adj_matrix add_vertex_g)) (set! add_vertex_i 0) (while (< add_vertex_i (count add_vertex_matrix)) (do (set! add_vertex_matrix (assoc add_vertex_matrix add_vertex_i (conj (get add_vertex_matrix add_vertex_i) 0))) (set! add_vertex_i (+ add_vertex_i 1)))) (set! add_vertex_row []) (set! add_vertex_j 0) (while (< add_vertex_j (+ (count add_vertex_matrix) 1)) (do (set! add_vertex_row (conj add_vertex_row 0)) (set! add_vertex_j (+ add_vertex_j 1)))) (set! add_vertex_matrix (conj add_vertex_matrix add_vertex_row)) (set! add_vertex_g (assoc add_vertex_g :adj_matrix add_vertex_matrix)) (set! add_vertex_idx_map (:vertex_to_index add_vertex_g)) (set! add_vertex_idx_map (assoc add_vertex_idx_map add_vertex_v (- (count add_vertex_matrix) 1))) (set! add_vertex_g (assoc add_vertex_g :vertex_to_index add_vertex_idx_map)) add_vertex_g)))

(defn remove_key [remove_key_m remove_key_k]
  (binding [remove_key_out nil] (try (do (set! remove_key_out {}) (doseq [key (keys remove_key_m)] (when (not= key remove_key_k) (set! remove_key_out (assoc remove_key_out key (get remove_key_m key))))) (throw (ex-info "return" {:v remove_key_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrement_indices [decrement_indices_m decrement_indices_start]
  (binding [decrement_indices_idx nil decrement_indices_out nil] (try (do (set! decrement_indices_out {}) (doseq [key (keys decrement_indices_m)] (do (set! decrement_indices_idx (get decrement_indices_m key)) (if (> decrement_indices_idx decrement_indices_start) (set! decrement_indices_out (assoc decrement_indices_out key (- decrement_indices_idx 1))) (set! decrement_indices_out (assoc decrement_indices_out key decrement_indices_idx))))) (throw (ex-info "return" {:v decrement_indices_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_vertex [remove_vertex_g_p remove_vertex_v]
  (binding [remove_vertex_g nil remove_vertex_i nil remove_vertex_idx nil remove_vertex_j nil remove_vertex_m nil remove_vertex_new_matrix nil remove_vertex_new_row nil remove_vertex_row nil] (do (set! remove_vertex_g remove_vertex_g_p) (when (not (contains_vertex remove_vertex_g remove_vertex_v)) (throw (Exception. "vertex does not exist"))) (set! remove_vertex_idx (get (:vertex_to_index remove_vertex_g) remove_vertex_v)) (set! remove_vertex_new_matrix []) (set! remove_vertex_i 0) (while (< remove_vertex_i (count (:adj_matrix remove_vertex_g))) (do (when (not= remove_vertex_i remove_vertex_idx) (do (set! remove_vertex_row (get (:adj_matrix remove_vertex_g) remove_vertex_i)) (set! remove_vertex_new_row []) (set! remove_vertex_j 0) (while (< remove_vertex_j (count remove_vertex_row)) (do (when (not= remove_vertex_j remove_vertex_idx) (set! remove_vertex_new_row (conj remove_vertex_new_row (nth remove_vertex_row remove_vertex_j)))) (set! remove_vertex_j (+ remove_vertex_j 1)))) (set! remove_vertex_new_matrix (conj remove_vertex_new_matrix remove_vertex_new_row)))) (set! remove_vertex_i (+ remove_vertex_i 1)))) (set! remove_vertex_g (assoc remove_vertex_g :adj_matrix remove_vertex_new_matrix)) (set! remove_vertex_m (remove_key (:vertex_to_index remove_vertex_g) remove_vertex_v)) (set! remove_vertex_g (assoc remove_vertex_g :vertex_to_index (decrement_indices remove_vertex_m remove_vertex_idx))) remove_vertex_g)))

(defn add_edge [add_edge_g_p add_edge_u add_edge_v]
  (binding [add_edge_g nil add_edge_i nil add_edge_j nil add_edge_matrix nil] (do (set! add_edge_g add_edge_g_p) (when (not (and (contains_vertex add_edge_g add_edge_u) (contains_vertex add_edge_g add_edge_v))) (throw (Exception. "missing vertex"))) (set! add_edge_i (get (:vertex_to_index add_edge_g) add_edge_u)) (set! add_edge_j (get (:vertex_to_index add_edge_g) add_edge_v)) (set! add_edge_matrix (:adj_matrix add_edge_g)) (set! add_edge_matrix (assoc-in add_edge_matrix [add_edge_i add_edge_j] 1)) (when (not (:directed add_edge_g)) (set! add_edge_matrix (assoc-in add_edge_matrix [add_edge_j add_edge_i] 1))) (set! add_edge_g (assoc add_edge_g :adj_matrix add_edge_matrix)) add_edge_g)))

(defn remove_edge [remove_edge_g_p remove_edge_u remove_edge_v]
  (binding [remove_edge_g nil remove_edge_i nil remove_edge_j nil remove_edge_matrix nil] (do (set! remove_edge_g remove_edge_g_p) (when (not (and (contains_vertex remove_edge_g remove_edge_u) (contains_vertex remove_edge_g remove_edge_v))) (throw (Exception. "missing vertex"))) (set! remove_edge_i (get (:vertex_to_index remove_edge_g) remove_edge_u)) (set! remove_edge_j (get (:vertex_to_index remove_edge_g) remove_edge_v)) (set! remove_edge_matrix (:adj_matrix remove_edge_g)) (set! remove_edge_matrix (assoc-in remove_edge_matrix [remove_edge_i remove_edge_j] 0)) (when (not (:directed remove_edge_g)) (set! remove_edge_matrix (assoc-in remove_edge_matrix [remove_edge_j remove_edge_i] 0))) (set! remove_edge_g (assoc remove_edge_g :adj_matrix remove_edge_matrix)) remove_edge_g)))

(defn contains_edge [contains_edge_g contains_edge_u contains_edge_v]
  (binding [contains_edge_i nil contains_edge_j nil contains_edge_matrix nil] (try (do (when (not (and (contains_vertex contains_edge_g contains_edge_u) (contains_vertex contains_edge_g contains_edge_v))) (throw (Exception. "missing vertex"))) (set! contains_edge_i (get (:vertex_to_index contains_edge_g) contains_edge_u)) (set! contains_edge_j (get (:vertex_to_index contains_edge_g) contains_edge_v)) (set! contains_edge_matrix (:adj_matrix contains_edge_g)) (throw (ex-info "return" {:v (= (nth (get contains_edge_matrix contains_edge_i) contains_edge_j) 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn clear_graph [clear_graph_g_p]
  (binding [clear_graph_g nil] (do (set! clear_graph_g clear_graph_g_p) (set! clear_graph_g (assoc clear_graph_g :vertex_to_index {})) (set! clear_graph_g (assoc clear_graph_g :adj_matrix [])) clear_graph_g)))

(def ^:dynamic main_g (make_graph [1 2 3] [[1 2] [2 3]] false))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (:adj_matrix main_g)))
      (println (str (contains_edge main_g 1 2)))
      (println (str (contains_edge main_g 2 1)))
      (alter-var-root (var main_g) (constantly (remove_edge main_g 1 2)))
      (println (str (contains_edge main_g 1 2)))
      (alter-var-root (var main_g) (constantly (remove_vertex main_g 2)))
      (println (str (:adj_matrix main_g)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
