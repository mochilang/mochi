(ns main (:refer-clojure :exclude [minimum_distance dijkstra print_solution]))

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

(declare minimum_distance dijkstra print_solution)

(def ^:dynamic count_v nil)

(def ^:dynamic dijkstra_distances nil)

(def ^:dynamic dijkstra_i nil)

(def ^:dynamic dijkstra_u nil)

(def ^:dynamic dijkstra_v nil)

(def ^:dynamic dijkstra_vertices nil)

(def ^:dynamic dijkstra_visited nil)

(def ^:dynamic minimum_distance_min_index nil)

(def ^:dynamic minimum_distance_minimum nil)

(def ^:dynamic minimum_distance_vertex nil)

(def ^:dynamic print_solution_v nil)

(defn minimum_distance [minimum_distance_distances minimum_distance_visited]
  (binding [minimum_distance_min_index nil minimum_distance_minimum nil minimum_distance_vertex nil] (try (do (set! minimum_distance_minimum 10000000) (set! minimum_distance_min_index 0) (set! minimum_distance_vertex 0) (while (< minimum_distance_vertex (count minimum_distance_distances)) (do (when (and (< (nth minimum_distance_distances minimum_distance_vertex) minimum_distance_minimum) (= (nth minimum_distance_visited minimum_distance_vertex) false)) (do (set! minimum_distance_minimum (nth minimum_distance_distances minimum_distance_vertex)) (set! minimum_distance_min_index minimum_distance_vertex))) (set! minimum_distance_vertex (+ minimum_distance_vertex 1)))) (throw (ex-info "return" {:v minimum_distance_min_index}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dijkstra [dijkstra_graph dijkstra_source]
  (binding [count_v nil dijkstra_distances nil dijkstra_i nil dijkstra_u nil dijkstra_v nil dijkstra_vertices nil dijkstra_visited nil] (try (do (set! dijkstra_vertices (count dijkstra_graph)) (set! dijkstra_distances []) (set! dijkstra_i 0) (while (< dijkstra_i dijkstra_vertices) (do (set! dijkstra_distances (conj dijkstra_distances 10000000)) (set! dijkstra_i (+ dijkstra_i 1)))) (set! dijkstra_distances (assoc dijkstra_distances dijkstra_source 0)) (set! dijkstra_visited []) (set! dijkstra_i 0) (while (< dijkstra_i dijkstra_vertices) (do (set! dijkstra_visited (conj dijkstra_visited false)) (set! dijkstra_i (+ dijkstra_i 1)))) (set! count_v 0) (while (< count_v dijkstra_vertices) (do (set! dijkstra_u (minimum_distance dijkstra_distances dijkstra_visited)) (set! dijkstra_visited (assoc dijkstra_visited dijkstra_u true)) (set! dijkstra_v 0) (while (< dijkstra_v dijkstra_vertices) (do (when (and (and (> (nth (nth dijkstra_graph dijkstra_u) dijkstra_v) 0) (= (nth dijkstra_visited dijkstra_v) false)) (> (nth dijkstra_distances dijkstra_v) (+ (nth dijkstra_distances dijkstra_u) (nth (nth dijkstra_graph dijkstra_u) dijkstra_v)))) (set! dijkstra_distances (assoc dijkstra_distances dijkstra_v (+ (nth dijkstra_distances dijkstra_u) (nth (nth dijkstra_graph dijkstra_u) dijkstra_v))))) (set! dijkstra_v (+ dijkstra_v 1)))) (set! count_v (+ count_v 1)))) (throw (ex-info "return" {:v dijkstra_distances}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_solution [print_solution_distances]
  (binding [print_solution_v nil] (do (println "Vertex \t Distance from Source") (set! print_solution_v 0) (while (< print_solution_v (count print_solution_distances)) (do (println (str (str (str print_solution_v) "\t\t") (str (nth print_solution_distances print_solution_v)))) (set! print_solution_v (+ print_solution_v 1)))))))

(def ^:dynamic main_graph [[0 4 0 0 0 0 0 8 0] [4 0 8 0 0 0 0 11 0] [0 8 0 7 0 4 0 0 2] [0 0 7 0 9 14 0 0 0] [0 0 0 9 0 10 0 0 0] [0 0 4 14 10 0 2 0 0] [0 0 0 0 0 2 0 1 6] [8 11 0 0 0 0 1 0 7] [0 0 2 0 0 0 6 7 0]])

(def ^:dynamic main_distances (dijkstra main_graph 0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_solution main_distances)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
