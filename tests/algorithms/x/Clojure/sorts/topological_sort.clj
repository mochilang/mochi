(ns main (:refer-clojure :exclude [topological_sort main]))

(require 'clojure.set)

(defrecord Edges [a b c d e])

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def ^:dynamic main_result nil)

(def ^:dynamic topological_sort_i nil)

(def ^:dynamic topological_sort_j nil)

(def ^:dynamic topological_sort_neighbor nil)

(def ^:dynamic topological_sort_neighbors nil)

(def ^:dynamic topological_sort_sort nil)

(def ^:dynamic topological_sort_v nil)

(def ^:dynamic topological_sort_visited nil)

(declare topological_sort main)

(def ^:dynamic main_edges {"a" ["c" "b"] "b" ["d" "e"] "c" [] "d" [] "e" []})

(def ^:dynamic main_vertices ["a" "b" "c" "d" "e"])

(defn topological_sort [topological_sort_start topological_sort_visited_p topological_sort_sort_p]
  (binding [topological_sort_i nil topological_sort_j nil topological_sort_neighbor nil topological_sort_neighbors nil topological_sort_sort nil topological_sort_v nil topological_sort_visited nil] (try (do (set! topological_sort_visited topological_sort_visited_p) (set! topological_sort_sort topological_sort_sort_p) (set! topological_sort_visited (assoc topological_sort_visited topological_sort_start true)) (set! topological_sort_neighbors (get main_edges topological_sort_start)) (set! topological_sort_i 0) (while (< topological_sort_i (count topological_sort_neighbors)) (do (set! topological_sort_neighbor (nth topological_sort_neighbors topological_sort_i)) (when (not (in topological_sort_neighbor topological_sort_visited)) (set! topological_sort_sort (topological_sort topological_sort_neighbor topological_sort_visited topological_sort_sort))) (set! topological_sort_i (+ topological_sort_i 1)))) (set! topological_sort_sort (conj topological_sort_sort topological_sort_start)) (when (not= (count topological_sort_visited) (count main_vertices)) (do (set! topological_sort_j 0) (while (< topological_sort_j (count main_vertices)) (do (set! topological_sort_v (nth main_vertices topological_sort_j)) (when (not (in topological_sort_v topological_sort_visited)) (set! topological_sort_sort (topological_sort topological_sort_v topological_sort_visited topological_sort_sort))) (set! topological_sort_j (+ topological_sort_j 1)))))) (throw (ex-info "return" {:v topological_sort_sort}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_result nil] (do (set! main_result (topological_sort "a" {} [])) (println (str main_result)))))

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
