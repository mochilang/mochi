(ns main (:refer-clojure :exclude [contains contains_key bfs_shortest_path bfs_shortest_path_distance]))

(require 'clojure.set)

(defrecord DemoGraph [A B C D E F G])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def ^:dynamic bfs_shortest_path_distance_adj nil)

(def ^:dynamic bfs_shortest_path_distance_dist nil)

(def ^:dynamic bfs_shortest_path_distance_i nil)

(def ^:dynamic bfs_shortest_path_distance_node nil)

(def ^:dynamic bfs_shortest_path_distance_queue nil)

(def ^:dynamic bfs_shortest_path_distance_visited nil)

(def ^:dynamic bfs_shortest_path_explored nil)

(def ^:dynamic bfs_shortest_path_i nil)

(def ^:dynamic bfs_shortest_path_neighbour nil)

(def ^:dynamic bfs_shortest_path_neighbours nil)

(def ^:dynamic bfs_shortest_path_new_path nil)

(def ^:dynamic bfs_shortest_path_node nil)

(def ^:dynamic bfs_shortest_path_path nil)

(def ^:dynamic bfs_shortest_path_queue nil)

(def ^:dynamic contains_i nil)

(def ^:dynamic next_v nil)

(declare contains contains_key bfs_shortest_path bfs_shortest_path_distance)

(defn contains [contains_xs contains_x]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_xs)) (do (when (= (nth contains_xs contains_i) contains_x) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_key [contains_key_m contains_key_key]
  (try (do (doseq [k contains_key_m] (when (= k contains_key_key) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bfs_shortest_path [bfs_shortest_path_graph bfs_shortest_path_start bfs_shortest_path_goal]
  (binding [bfs_shortest_path_explored nil bfs_shortest_path_i nil bfs_shortest_path_neighbour nil bfs_shortest_path_neighbours nil bfs_shortest_path_new_path nil bfs_shortest_path_node nil bfs_shortest_path_path nil bfs_shortest_path_queue nil] (try (do (set! bfs_shortest_path_explored []) (set! bfs_shortest_path_queue [[bfs_shortest_path_start]]) (when (= bfs_shortest_path_start bfs_shortest_path_goal) (throw (ex-info "return" {:v [bfs_shortest_path_start]}))) (while (> (count bfs_shortest_path_queue) 0) (do (set! bfs_shortest_path_path (nth bfs_shortest_path_queue 0)) (set! bfs_shortest_path_queue (subvec bfs_shortest_path_queue 1 (min (count bfs_shortest_path_queue) (count bfs_shortest_path_queue)))) (set! bfs_shortest_path_node (nth bfs_shortest_path_path (- (count bfs_shortest_path_path) 1))) (when (not (contains bfs_shortest_path_explored bfs_shortest_path_node)) (do (set! bfs_shortest_path_neighbours (nth bfs_shortest_path_graph bfs_shortest_path_node)) (set! bfs_shortest_path_i 0) (while (< bfs_shortest_path_i (count bfs_shortest_path_neighbours)) (do (set! bfs_shortest_path_neighbour (nth bfs_shortest_path_neighbours bfs_shortest_path_i)) (set! bfs_shortest_path_new_path bfs_shortest_path_path) (set! bfs_shortest_path_new_path (conj bfs_shortest_path_new_path bfs_shortest_path_neighbour)) (set! bfs_shortest_path_queue (conj bfs_shortest_path_queue bfs_shortest_path_new_path)) (when (= bfs_shortest_path_neighbour bfs_shortest_path_goal) (throw (ex-info "return" {:v bfs_shortest_path_new_path}))) (set! bfs_shortest_path_i (+ bfs_shortest_path_i 1)))) (set! bfs_shortest_path_explored (conj bfs_shortest_path_explored bfs_shortest_path_node)))))) (throw (ex-info "return" {:v []}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bfs_shortest_path_distance [bfs_shortest_path_distance_graph bfs_shortest_path_distance_start bfs_shortest_path_distance_target]
  (binding [bfs_shortest_path_distance_adj nil bfs_shortest_path_distance_dist nil bfs_shortest_path_distance_i nil bfs_shortest_path_distance_node nil bfs_shortest_path_distance_queue nil bfs_shortest_path_distance_visited nil next_v nil] (try (do (when (or (= (contains_key bfs_shortest_path_distance_graph bfs_shortest_path_distance_start) false) (= (contains_key bfs_shortest_path_distance_graph bfs_shortest_path_distance_target) false)) (throw (ex-info "return" {:v (- 1)}))) (when (= bfs_shortest_path_distance_start bfs_shortest_path_distance_target) (throw (ex-info "return" {:v 0}))) (set! bfs_shortest_path_distance_queue [bfs_shortest_path_distance_start]) (set! bfs_shortest_path_distance_visited [bfs_shortest_path_distance_start]) (set! bfs_shortest_path_distance_dist {}) (set! bfs_shortest_path_distance_dist (assoc bfs_shortest_path_distance_dist bfs_shortest_path_distance_start 0)) (set! bfs_shortest_path_distance_dist (assoc bfs_shortest_path_distance_dist bfs_shortest_path_distance_target (- 1))) (while (> (count bfs_shortest_path_distance_queue) 0) (do (set! bfs_shortest_path_distance_node (nth bfs_shortest_path_distance_queue 0)) (set! bfs_shortest_path_distance_queue (subvec bfs_shortest_path_distance_queue 1 (min (count bfs_shortest_path_distance_queue) (count bfs_shortest_path_distance_queue)))) (when (= bfs_shortest_path_distance_node bfs_shortest_path_distance_target) (when (or (= (get bfs_shortest_path_distance_dist bfs_shortest_path_distance_target) (- 1)) (< (get bfs_shortest_path_distance_dist bfs_shortest_path_distance_node) (get bfs_shortest_path_distance_dist bfs_shortest_path_distance_target))) (set! bfs_shortest_path_distance_dist (assoc bfs_shortest_path_distance_dist bfs_shortest_path_distance_target (get bfs_shortest_path_distance_dist bfs_shortest_path_distance_node))))) (set! bfs_shortest_path_distance_adj (get bfs_shortest_path_distance_graph bfs_shortest_path_distance_node)) (set! bfs_shortest_path_distance_i 0) (while (< bfs_shortest_path_distance_i (count bfs_shortest_path_distance_adj)) (do (set! next_v (nth bfs_shortest_path_distance_adj bfs_shortest_path_distance_i)) (when (not (contains bfs_shortest_path_distance_visited next_v)) (do (set! bfs_shortest_path_distance_visited (conj bfs_shortest_path_distance_visited next_v)) (set! bfs_shortest_path_distance_queue (conj bfs_shortest_path_distance_queue next_v)) (set! bfs_shortest_path_distance_dist (assoc bfs_shortest_path_distance_dist next_v (+ (get bfs_shortest_path_distance_dist bfs_shortest_path_distance_node) 1))))) (set! bfs_shortest_path_distance_i (+ bfs_shortest_path_distance_i 1)))))) (throw (ex-info "return" {:v (get bfs_shortest_path_distance_dist bfs_shortest_path_distance_target)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_demo_graph {"A" ["B" "C" "E"] "B" ["A" "D" "E"] "C" ["A" "F" "G"] "D" ["B"] "E" ["A" "B" "D"] "F" ["C"] "G" ["C"]})

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
