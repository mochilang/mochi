(ns main (:refer-clojure :exclude [dfs compute_bridges get_demo_graph]))

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

(declare dfs compute_bridges get_demo_graph)

(def ^:dynamic compute_bridges_bridges nil)

(def ^:dynamic compute_bridges_i nil)

(def ^:dynamic compute_bridges_id nil)

(def ^:dynamic compute_bridges_ids nil)

(def ^:dynamic compute_bridges_low nil)

(def ^:dynamic compute_bridges_n nil)

(def ^:dynamic compute_bridges_result nil)

(def ^:dynamic compute_bridges_visited nil)

(def ^:dynamic dfs_current_id nil)

(def ^:dynamic dfs_edge nil)

(def ^:dynamic dfs_ids nil)

(def ^:dynamic dfs_low nil)

(def ^:dynamic dfs_res_bridges nil)

(def ^:dynamic dfs_result nil)

(def ^:dynamic dfs_visited nil)

(defn dfs [dfs_graph dfs_at dfs_parent dfs_visited_p dfs_ids_p dfs_low_p dfs_id dfs_bridges]
  (binding [dfs_current_id nil dfs_edge nil dfs_ids nil dfs_low nil dfs_res_bridges nil dfs_result nil dfs_visited nil] (try (do (set! dfs_visited dfs_visited_p) (set! dfs_ids dfs_ids_p) (set! dfs_low dfs_low_p) (set! dfs_visited (assoc dfs_visited dfs_at true)) (set! dfs_ids (assoc dfs_ids dfs_at dfs_id)) (set! dfs_low (assoc dfs_low dfs_at dfs_id)) (set! dfs_current_id (+ dfs_id 1)) (set! dfs_res_bridges dfs_bridges) (loop [to_seq (get dfs_graph dfs_at)] (when (seq to_seq) (let [to (first to_seq)] (cond (= to dfs_parent) (recur (rest to_seq)) :else (do (if (not (nth dfs_visited to)) (do (set! dfs_result (dfs dfs_graph to dfs_at dfs_visited dfs_ids dfs_low dfs_current_id dfs_res_bridges)) (set! dfs_current_id (:id dfs_result)) (set! dfs_res_bridges (:bridges dfs_result)) (when (> (nth dfs_low dfs_at) (nth dfs_low to)) (set! dfs_low (assoc dfs_low dfs_at (nth dfs_low to)))) (when (< (nth dfs_ids dfs_at) (nth dfs_low to)) (do (set! dfs_edge (if (< dfs_at to) [dfs_at to] [to dfs_at])) (set! dfs_res_bridges (conj dfs_res_bridges dfs_edge))))) (when (> (nth dfs_low dfs_at) (nth dfs_ids to)) (set! dfs_low (assoc dfs_low dfs_at (nth dfs_ids to))))) (recur (rest to_seq))))))) (throw (ex-info "return" {:v {:bridges dfs_res_bridges :id dfs_current_id}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn compute_bridges [compute_bridges_graph]
  (binding [compute_bridges_bridges nil compute_bridges_i nil compute_bridges_id nil compute_bridges_ids nil compute_bridges_low nil compute_bridges_n nil compute_bridges_result nil compute_bridges_visited nil] (try (do (set! compute_bridges_n (count compute_bridges_graph)) (set! compute_bridges_visited []) (set! compute_bridges_ids []) (set! compute_bridges_low []) (set! compute_bridges_i 0) (while (< compute_bridges_i compute_bridges_n) (do (set! compute_bridges_visited (conj compute_bridges_visited false)) (set! compute_bridges_ids (conj compute_bridges_ids 0)) (set! compute_bridges_low (conj compute_bridges_low 0)) (set! compute_bridges_i (+ compute_bridges_i 1)))) (set! compute_bridges_bridges []) (set! compute_bridges_id 0) (set! compute_bridges_i 0) (while (< compute_bridges_i compute_bridges_n) (do (when (not (nth compute_bridges_visited compute_bridges_i)) (do (set! compute_bridges_result (dfs compute_bridges_graph compute_bridges_i (- 1) compute_bridges_visited compute_bridges_ids compute_bridges_low compute_bridges_id compute_bridges_bridges)) (set! compute_bridges_id (:id compute_bridges_result)) (set! compute_bridges_bridges (:bridges compute_bridges_result)))) (set! compute_bridges_i (+ compute_bridges_i 1)))) (throw (ex-info "return" {:v compute_bridges_bridges}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_demo_graph [get_demo_graph_index]
  (try (do (when (= get_demo_graph_index 0) (throw (ex-info "return" {:v {0 [1 2] 1 [0 2] 2 [0 1 3 5] 3 [2 4] 4 [3] 5 [2 6 8] 6 [5 7] 7 [6 8] 8 [5 7]}}))) (when (= get_demo_graph_index 1) (throw (ex-info "return" {:v {0 [6] 1 [9] 2 [4 5] 3 [4] 4 [2 3] 5 [2] 6 [0 7] 7 [6] 8 [] 9 [1]}}))) (if (= get_demo_graph_index 2) {0 [4] 1 [6] 2 [] 3 [5 6 7] 4 [0 6] 5 [3 8 9] 6 [1 3 4 7] 7 [3 6 8 9] 8 [5 7] 9 [5 7]} {0 [1 3] 1 [0 2 4] 2 [1 3 4] 3 [0 2 4] 4 [1 2 3]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (compute_bridges (get_demo_graph 0)))
      (println (compute_bridges (get_demo_graph 1)))
      (println (compute_bridges (get_demo_graph 2)))
      (println (compute_bridges (get_demo_graph 3)))
      (println (compute_bridges {}))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
