(ns main (:refer-clojure :exclude [is_bipartite_bfs]))

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

(declare is_bipartite_bfs)

(def ^:dynamic is_bipartite_bfs_curr nil)

(def ^:dynamic is_bipartite_bfs_queue nil)

(def ^:dynamic is_bipartite_bfs_visited nil)

(defn is_bipartite_bfs [is_bipartite_bfs_graph]
  (binding [is_bipartite_bfs_curr nil is_bipartite_bfs_queue nil is_bipartite_bfs_visited nil] (try (do (set! is_bipartite_bfs_visited {}) (doseq [node (keys is_bipartite_bfs_graph)] (when (not (in node is_bipartite_bfs_visited)) (do (set! is_bipartite_bfs_queue []) (set! is_bipartite_bfs_queue (conj is_bipartite_bfs_queue node)) (set! is_bipartite_bfs_visited (assoc is_bipartite_bfs_visited node 0)) (while (> (count is_bipartite_bfs_queue) 0) (do (set! is_bipartite_bfs_curr (nth is_bipartite_bfs_queue 0)) (set! is_bipartite_bfs_queue (subvec is_bipartite_bfs_queue 1 (count is_bipartite_bfs_queue))) (doseq [neighbor (get is_bipartite_bfs_graph is_bipartite_bfs_curr)] (if (not (in neighbor is_bipartite_bfs_visited)) (do (set! is_bipartite_bfs_visited (assoc is_bipartite_bfs_visited neighbor (- 1 (nth is_bipartite_bfs_visited is_bipartite_bfs_curr)))) (set! is_bipartite_bfs_queue (conj is_bipartite_bfs_queue neighbor))) (when (= (nth is_bipartite_bfs_visited neighbor) (nth is_bipartite_bfs_visited is_bipartite_bfs_curr)) (throw (ex-info "return" {:v false})))))))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_graph {0 [1 3] 1 [0 2] 2 [1 3] 3 [0 2]})

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_bipartite_bfs main_graph)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
