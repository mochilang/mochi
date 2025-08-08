(ns main (:refer-clojure :exclude [topological_sort main]))

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

(declare topological_sort main)

(def ^:dynamic main_cyclic nil)

(def ^:dynamic main_graph nil)

(def ^:dynamic topological_sort_head nil)

(def ^:dynamic topological_sort_i nil)

(def ^:dynamic topological_sort_indegree nil)

(def ^:dynamic topological_sort_j nil)

(def ^:dynamic topological_sort_k nil)

(def ^:dynamic topological_sort_nb nil)

(def ^:dynamic topological_sort_neighbors nil)

(def ^:dynamic topological_sort_order nil)

(def ^:dynamic topological_sort_processed nil)

(def ^:dynamic topological_sort_queue nil)

(def ^:dynamic topological_sort_v nil)

(defn topological_sort [topological_sort_graph]
  (binding [topological_sort_head nil topological_sort_i nil topological_sort_indegree nil topological_sort_j nil topological_sort_k nil topological_sort_nb nil topological_sort_neighbors nil topological_sort_order nil topological_sort_processed nil topological_sort_queue nil topological_sort_v nil] (try (do (set! topological_sort_indegree []) (set! topological_sort_i 0) (while (< topological_sort_i (count topological_sort_graph)) (do (set! topological_sort_indegree (conj topological_sort_indegree 0)) (set! topological_sort_i (+ topological_sort_i 1)))) (doseq [edges (vals topological_sort_graph)] (do (set! topological_sort_j 0) (while (< topological_sort_j (count edges)) (do (set! topological_sort_v (nth edges topological_sort_j)) (set! topological_sort_indegree (assoc topological_sort_indegree topological_sort_v (+ (nth topological_sort_indegree topological_sort_v) 1))) (set! topological_sort_j (+ topological_sort_j 1)))))) (set! topological_sort_queue []) (set! topological_sort_i 0) (while (< topological_sort_i (count topological_sort_indegree)) (do (when (= (nth topological_sort_indegree topological_sort_i) 0) (set! topological_sort_queue (conj topological_sort_queue topological_sort_i))) (set! topological_sort_i (+ topological_sort_i 1)))) (set! topological_sort_order []) (set! topological_sort_head 0) (set! topological_sort_processed 0) (while (< topological_sort_head (count topological_sort_queue)) (do (set! topological_sort_v (nth topological_sort_queue topological_sort_head)) (set! topological_sort_head (+ topological_sort_head 1)) (set! topological_sort_processed (+ topological_sort_processed 1)) (set! topological_sort_order (conj topological_sort_order topological_sort_v)) (set! topological_sort_neighbors (get topological_sort_graph topological_sort_v)) (set! topological_sort_k 0) (while (< topological_sort_k (count topological_sort_neighbors)) (do (set! topological_sort_nb (nth topological_sort_neighbors topological_sort_k)) (set! topological_sort_indegree (assoc topological_sort_indegree topological_sort_nb (- (nth topological_sort_indegree topological_sort_nb) 1))) (when (= (nth topological_sort_indegree topological_sort_nb) 0) (set! topological_sort_queue (conj topological_sort_queue topological_sort_nb))) (set! topological_sort_k (+ topological_sort_k 1)))))) (if (not= topological_sort_processed (count topological_sort_graph)) nil topological_sort_order)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_cyclic nil main_graph nil] (do (set! main_graph {0 [1 2] 1 [3] 2 [3] 3 [4 5] 4 [] 5 []}) (println (topological_sort main_graph)) (set! main_cyclic {0 [1] 1 [2] 2 [0]}) (println (topological_sort main_cyclic)))))

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
