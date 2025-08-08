(ns main (:refer-clojure :exclude [dfs connected_components]))

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

(declare dfs connected_components)

(def ^:dynamic connected_components_component nil)

(def ^:dynamic connected_components_components_list nil)

(def ^:dynamic connected_components_graph_size nil)

(def ^:dynamic connected_components_visited nil)

(def ^:dynamic dfs_connected_verts nil)

(def ^:dynamic dfs_visited nil)

(def ^:dynamic main_test_graph_1 {0 [1 2] 1 [0 3] 2 [0] 3 [1] 4 [5 6] 5 [4 6] 6 [4 5]})

(def ^:dynamic main_test_graph_2 {0 [1 2 3] 1 [0 3] 2 [0] 3 [0 1] 4 [] 5 []})

(defn dfs [dfs_graph dfs_vert dfs_visited_p]
  (binding [dfs_connected_verts nil dfs_visited nil] (try (do (set! dfs_visited dfs_visited_p) (set! dfs_visited (assoc dfs_visited dfs_vert true)) (set! dfs_connected_verts []) (doseq [neighbour (nth dfs_graph dfs_vert)] (when (not (nth dfs_visited neighbour)) (set! dfs_connected_verts (concat dfs_connected_verts (dfs dfs_graph neighbour dfs_visited))))) (throw (ex-info "return" {:v (concat [dfs_vert] dfs_connected_verts)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn connected_components [connected_components_graph]
  (binding [connected_components_component nil connected_components_components_list nil connected_components_graph_size nil connected_components_visited nil] (try (do (set! connected_components_graph_size (count connected_components_graph)) (set! connected_components_visited []) (dotimes [_ connected_components_graph_size] (set! connected_components_visited (conj connected_components_visited false))) (set! connected_components_components_list []) (dotimes [i connected_components_graph_size] (when (not (nth connected_components_visited i)) (do (set! connected_components_component (dfs connected_components_graph i connected_components_visited)) (set! connected_components_components_list (conj connected_components_components_list connected_components_component))))) (throw (ex-info "return" {:v connected_components_components_list}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (connected_components main_test_graph_1)))
      (println (str (connected_components main_test_graph_2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
