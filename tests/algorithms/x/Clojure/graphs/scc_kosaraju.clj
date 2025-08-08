(ns main (:refer-clojure :exclude [dfs dfs2 kosaraju main]))

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

(declare dfs dfs2 kosaraju main)

(def ^:dynamic dfs2_component nil)

(def ^:dynamic dfs2_visit nil)

(def ^:dynamic dfs_stack nil)

(def ^:dynamic dfs_visit nil)

(def ^:dynamic kosaraju_component nil)

(def ^:dynamic kosaraju_i nil)

(def ^:dynamic kosaraju_idx nil)

(def ^:dynamic kosaraju_n nil)

(def ^:dynamic kosaraju_node nil)

(def ^:dynamic kosaraju_reversed_graph nil)

(def ^:dynamic kosaraju_scc nil)

(def ^:dynamic kosaraju_stack nil)

(def ^:dynamic kosaraju_visit nil)

(def ^:dynamic main_comps nil)

(def ^:dynamic main_graph nil)

(def ^:dynamic main_i nil)

(defn dfs [dfs_u dfs_graph dfs_visit_p dfs_stack_p]
  (binding [dfs_stack nil dfs_visit nil] (try (do (set! dfs_visit dfs_visit_p) (set! dfs_stack dfs_stack_p) (when (nth dfs_visit dfs_u) (throw (ex-info "return" {:v dfs_stack}))) (set! dfs_visit (assoc dfs_visit dfs_u true)) (doseq [v (nth dfs_graph dfs_u)] (set! dfs_stack (dfs v dfs_graph dfs_visit dfs_stack))) (set! dfs_stack (conj dfs_stack dfs_u)) (throw (ex-info "return" {:v dfs_stack}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dfs2 [dfs2_u dfs2_reversed_graph dfs2_visit_p dfs2_component_p]
  (binding [dfs2_component nil dfs2_visit nil] (try (do (set! dfs2_visit dfs2_visit_p) (set! dfs2_component dfs2_component_p) (when (nth dfs2_visit dfs2_u) (throw (ex-info "return" {:v dfs2_component}))) (set! dfs2_visit (assoc dfs2_visit dfs2_u true)) (set! dfs2_component (conj dfs2_component dfs2_u)) (doseq [v (nth dfs2_reversed_graph dfs2_u)] (set! dfs2_component (dfs2 v dfs2_reversed_graph dfs2_visit dfs2_component))) (throw (ex-info "return" {:v dfs2_component}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn kosaraju [kosaraju_graph]
  (binding [kosaraju_component nil kosaraju_i nil kosaraju_idx nil kosaraju_n nil kosaraju_node nil kosaraju_reversed_graph nil kosaraju_scc nil kosaraju_stack nil kosaraju_visit nil] (try (do (set! kosaraju_n (count kosaraju_graph)) (set! kosaraju_reversed_graph []) (set! kosaraju_i 0) (while (< kosaraju_i kosaraju_n) (do (set! kosaraju_reversed_graph (conj kosaraju_reversed_graph [])) (set! kosaraju_i (+ kosaraju_i 1)))) (set! kosaraju_i 0) (while (< kosaraju_i kosaraju_n) (do (doseq [v (nth kosaraju_graph kosaraju_i)] (set! kosaraju_reversed_graph (assoc kosaraju_reversed_graph v (conj (nth kosaraju_reversed_graph v) kosaraju_i)))) (set! kosaraju_i (+ kosaraju_i 1)))) (set! kosaraju_visit []) (set! kosaraju_i 0) (while (< kosaraju_i kosaraju_n) (do (set! kosaraju_visit (conj kosaraju_visit false)) (set! kosaraju_i (+ kosaraju_i 1)))) (set! kosaraju_stack []) (set! kosaraju_i 0) (while (< kosaraju_i kosaraju_n) (do (when (= (nth kosaraju_visit kosaraju_i) false) (set! kosaraju_stack (dfs kosaraju_i kosaraju_graph kosaraju_visit kosaraju_stack))) (set! kosaraju_i (+ kosaraju_i 1)))) (set! kosaraju_i 0) (while (< kosaraju_i kosaraju_n) (do (set! kosaraju_visit (assoc kosaraju_visit kosaraju_i false)) (set! kosaraju_i (+ kosaraju_i 1)))) (set! kosaraju_scc []) (set! kosaraju_idx (- (count kosaraju_stack) 1)) (while (>= kosaraju_idx 0) (do (set! kosaraju_node (nth kosaraju_stack kosaraju_idx)) (when (= (nth kosaraju_visit kosaraju_node) false) (do (set! kosaraju_component []) (set! kosaraju_component (dfs2 kosaraju_node kosaraju_reversed_graph kosaraju_visit kosaraju_component)) (set! kosaraju_scc (conj kosaraju_scc kosaraju_component)))) (set! kosaraju_idx (- kosaraju_idx 1)))) (throw (ex-info "return" {:v kosaraju_scc}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_comps nil main_graph nil main_i nil] (do (set! main_graph [[1] [2] [0 3] [4] []]) (set! main_comps (kosaraju main_graph)) (set! main_i 0) (while (< main_i (count main_comps)) (do (println (nth main_comps main_i)) (set! main_i (+ main_i 1)))))))

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
