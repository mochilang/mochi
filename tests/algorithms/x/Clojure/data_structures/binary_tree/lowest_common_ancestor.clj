(ns main (:refer-clojure :exclude [pow2 create_sparse lowest_common_ancestor breadth_first_search main]))

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

(declare pow2 create_sparse lowest_common_ancestor breadth_first_search main)

(def ^:dynamic breadth_first_search_adj nil)

(def ^:dynamic breadth_first_search_head nil)

(def ^:dynamic breadth_first_search_j nil)

(def ^:dynamic breadth_first_search_level nil)

(def ^:dynamic breadth_first_search_parent nil)

(def ^:dynamic breadth_first_search_q nil)

(def ^:dynamic breadth_first_search_u nil)

(def ^:dynamic breadth_first_search_v nil)

(def ^:dynamic create_sparse_i nil)

(def ^:dynamic create_sparse_j nil)

(def ^:dynamic create_sparse_parent nil)

(def ^:dynamic lowest_common_ancestor_i nil)

(def ^:dynamic lowest_common_ancestor_pu nil)

(def ^:dynamic lowest_common_ancestor_pv nil)

(def ^:dynamic lowest_common_ancestor_temp nil)

(def ^:dynamic lowest_common_ancestor_u nil)

(def ^:dynamic lowest_common_ancestor_v nil)

(def ^:dynamic main_graph nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_j nil)

(def ^:dynamic main_level nil)

(def ^:dynamic main_max_node nil)

(def ^:dynamic main_parent nil)

(def ^:dynamic main_row nil)

(def ^:dynamic pow2_i nil)

(def ^:dynamic pow2_res nil)

(defn pow2 [pow2_exp]
  (binding [pow2_i nil pow2_res nil] (try (do (set! pow2_res 1) (set! pow2_i 0) (while (< pow2_i pow2_exp) (do (set! pow2_res (* pow2_res 2)) (set! pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn create_sparse [create_sparse_max_node create_sparse_parent_p]
  (binding [create_sparse_i nil create_sparse_j nil create_sparse_parent nil] (try (do (set! create_sparse_parent create_sparse_parent_p) (set! create_sparse_j 1) (while (< (pow2 create_sparse_j) create_sparse_max_node) (do (set! create_sparse_i 1) (while (<= create_sparse_i create_sparse_max_node) (do (set! create_sparse_parent (assoc-in create_sparse_parent [create_sparse_j create_sparse_i] (nth (nth create_sparse_parent (- create_sparse_j 1)) (nth (nth create_sparse_parent (- create_sparse_j 1)) create_sparse_i)))) (set! create_sparse_i (+ create_sparse_i 1)))) (set! create_sparse_j (+ create_sparse_j 1)))) (throw (ex-info "return" {:v create_sparse_parent}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lowest_common_ancestor [lowest_common_ancestor_u_p lowest_common_ancestor_v_p lowest_common_ancestor_level lowest_common_ancestor_parent]
  (binding [lowest_common_ancestor_i nil lowest_common_ancestor_pu nil lowest_common_ancestor_pv nil lowest_common_ancestor_temp nil lowest_common_ancestor_u nil lowest_common_ancestor_v nil] (try (do (set! lowest_common_ancestor_u lowest_common_ancestor_u_p) (set! lowest_common_ancestor_v lowest_common_ancestor_v_p) (when (< (nth lowest_common_ancestor_level lowest_common_ancestor_u) (nth lowest_common_ancestor_level lowest_common_ancestor_v)) (do (set! lowest_common_ancestor_temp lowest_common_ancestor_u) (set! lowest_common_ancestor_u lowest_common_ancestor_v) (set! lowest_common_ancestor_v lowest_common_ancestor_temp))) (set! lowest_common_ancestor_i 18) (while (>= lowest_common_ancestor_i 0) (do (when (>= (- (nth lowest_common_ancestor_level lowest_common_ancestor_u) (pow2 lowest_common_ancestor_i)) (nth lowest_common_ancestor_level lowest_common_ancestor_v)) (set! lowest_common_ancestor_u (nth (nth lowest_common_ancestor_parent lowest_common_ancestor_i) lowest_common_ancestor_u))) (set! lowest_common_ancestor_i (- lowest_common_ancestor_i 1)))) (when (= lowest_common_ancestor_u lowest_common_ancestor_v) (throw (ex-info "return" {:v lowest_common_ancestor_u}))) (set! lowest_common_ancestor_i 18) (while (>= lowest_common_ancestor_i 0) (do (set! lowest_common_ancestor_pu (nth (nth lowest_common_ancestor_parent lowest_common_ancestor_i) lowest_common_ancestor_u)) (set! lowest_common_ancestor_pv (nth (nth lowest_common_ancestor_parent lowest_common_ancestor_i) lowest_common_ancestor_v)) (when (and (not= lowest_common_ancestor_pu 0) (not= lowest_common_ancestor_pu lowest_common_ancestor_pv)) (do (set! lowest_common_ancestor_u lowest_common_ancestor_pu) (set! lowest_common_ancestor_v lowest_common_ancestor_pv))) (set! lowest_common_ancestor_i (- lowest_common_ancestor_i 1)))) (throw (ex-info "return" {:v (nth (nth lowest_common_ancestor_parent 0) lowest_common_ancestor_u)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn breadth_first_search [breadth_first_search_level_p breadth_first_search_parent_p breadth_first_search_max_node breadth_first_search_graph breadth_first_search_root]
  (binding [breadth_first_search_adj nil breadth_first_search_head nil breadth_first_search_j nil breadth_first_search_level nil breadth_first_search_parent nil breadth_first_search_q nil breadth_first_search_u nil breadth_first_search_v nil] (do (set! breadth_first_search_level breadth_first_search_level_p) (set! breadth_first_search_parent breadth_first_search_parent_p) (set! breadth_first_search_level (assoc breadth_first_search_level breadth_first_search_root 0)) (set! breadth_first_search_q []) (set! breadth_first_search_q (conj breadth_first_search_q breadth_first_search_root)) (set! breadth_first_search_head 0) (while (< breadth_first_search_head (count breadth_first_search_q)) (do (set! breadth_first_search_u (nth breadth_first_search_q breadth_first_search_head)) (set! breadth_first_search_head (+ breadth_first_search_head 1)) (set! breadth_first_search_adj (nth breadth_first_search_graph breadth_first_search_u)) (set! breadth_first_search_j 0) (while (< breadth_first_search_j (count breadth_first_search_adj)) (do (set! breadth_first_search_v (nth breadth_first_search_adj breadth_first_search_j)) (when (= (nth breadth_first_search_level breadth_first_search_v) (- 0 1)) (do (set! breadth_first_search_level (assoc breadth_first_search_level breadth_first_search_v (+ (nth breadth_first_search_level breadth_first_search_u) 1))) (set! breadth_first_search_parent (assoc-in breadth_first_search_parent [0 breadth_first_search_v] breadth_first_search_u)) (set! breadth_first_search_q (conj breadth_first_search_q breadth_first_search_v)))) (set! breadth_first_search_j (+ breadth_first_search_j 1)))))))))

(defn main []
  (binding [main_graph nil main_i nil main_j nil main_level nil main_max_node nil main_parent nil main_row nil] (do (set! main_max_node 13) (set! main_parent []) (set! main_i 0) (while (< main_i 20) (do (set! main_row []) (set! main_j 0) (while (< main_j (+ main_max_node 10)) (do (set! main_row (conj main_row 0)) (set! main_j (+ main_j 1)))) (set! main_parent (conj main_parent main_row)) (set! main_i (+ main_i 1)))) (set! main_level []) (set! main_i 0) (while (< main_i (+ main_max_node 10)) (do (set! main_level (conj main_level (- 0 1))) (set! main_i (+ main_i 1)))) (set! main_graph {}) (set! main_graph (assoc main_graph 1 [2 3 4])) (set! main_graph (assoc main_graph 2 [5])) (set! main_graph (assoc main_graph 3 [6 7])) (set! main_graph (assoc main_graph 4 [8])) (set! main_graph (assoc main_graph 5 [9 10])) (set! main_graph (assoc main_graph 6 [11])) (set! main_graph (assoc main_graph 7 [])) (set! main_graph (assoc main_graph 8 [12 13])) (set! main_graph (assoc main_graph 9 [])) (set! main_graph (assoc main_graph 10 [])) (set! main_graph (assoc main_graph 11 [])) (set! main_graph (assoc main_graph 12 [])) (set! main_graph (assoc main_graph 13 [])) (breadth_first_search main_level main_parent main_max_node main_graph 1) (set! main_parent (create_sparse main_max_node main_parent)) (println (str "LCA of node 1 and 3 is: " (str (lowest_common_ancestor 1 3 main_level main_parent)))) (println (str "LCA of node 5 and 6 is: " (str (lowest_common_ancestor 5 6 main_level main_parent)))) (println (str "LCA of node 7 and 11 is: " (str (lowest_common_ancestor 7 11 main_level main_parent)))) (println (str "LCA of node 6 and 7 is: " (str (lowest_common_ancestor 6 7 main_level main_parent)))) (println (str "LCA of node 4 and 12 is: " (str (lowest_common_ancestor 4 12 main_level main_parent)))) (println (str "LCA of node 8 and 8 is: " (str (lowest_common_ancestor 8 8 main_level main_parent)))))))

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
