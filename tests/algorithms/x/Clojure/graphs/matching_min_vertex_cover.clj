(ns main (:refer-clojure :exclude [contains get_edges matching_min_vertex_cover]))

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

(declare contains get_edges matching_min_vertex_cover)

(def ^:dynamic get_edges_edges nil)

(def ^:dynamic get_edges_n nil)

(def ^:dynamic matching_min_vertex_cover_a nil)

(def ^:dynamic matching_min_vertex_cover_b nil)

(def ^:dynamic matching_min_vertex_cover_chosen nil)

(def ^:dynamic matching_min_vertex_cover_e nil)

(def ^:dynamic matching_min_vertex_cover_edges nil)

(def ^:dynamic matching_min_vertex_cover_filtered nil)

(def ^:dynamic matching_min_vertex_cover_idx nil)

(def ^:dynamic matching_min_vertex_cover_u nil)

(def ^:dynamic matching_min_vertex_cover_v nil)

(defn contains [contains_xs contains_v]
  (try (do (doseq [x contains_xs] (when (= x contains_v) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_edges [get_edges_graph]
  (binding [get_edges_edges nil get_edges_n nil] (try (do (set! get_edges_n (count get_edges_graph)) (set! get_edges_edges []) (dotimes [i get_edges_n] (doseq [j (get get_edges_graph i)] (set! get_edges_edges (conj get_edges_edges [i j])))) (throw (ex-info "return" {:v get_edges_edges}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matching_min_vertex_cover [matching_min_vertex_cover_graph]
  (binding [matching_min_vertex_cover_a nil matching_min_vertex_cover_b nil matching_min_vertex_cover_chosen nil matching_min_vertex_cover_e nil matching_min_vertex_cover_edges nil matching_min_vertex_cover_filtered nil matching_min_vertex_cover_idx nil matching_min_vertex_cover_u nil matching_min_vertex_cover_v nil] (try (do (set! matching_min_vertex_cover_chosen []) (set! matching_min_vertex_cover_edges (get_edges matching_min_vertex_cover_graph)) (while (> (count matching_min_vertex_cover_edges) 0) (do (set! matching_min_vertex_cover_idx (- (count matching_min_vertex_cover_edges) 1)) (set! matching_min_vertex_cover_e (nth matching_min_vertex_cover_edges matching_min_vertex_cover_idx)) (set! matching_min_vertex_cover_edges (subvec matching_min_vertex_cover_edges 0 (min matching_min_vertex_cover_idx (count matching_min_vertex_cover_edges)))) (set! matching_min_vertex_cover_u (nth matching_min_vertex_cover_e 0)) (set! matching_min_vertex_cover_v (nth matching_min_vertex_cover_e 1)) (when (not (contains matching_min_vertex_cover_chosen matching_min_vertex_cover_u)) (set! matching_min_vertex_cover_chosen (conj matching_min_vertex_cover_chosen matching_min_vertex_cover_u))) (when (not (contains matching_min_vertex_cover_chosen matching_min_vertex_cover_v)) (set! matching_min_vertex_cover_chosen (conj matching_min_vertex_cover_chosen matching_min_vertex_cover_v))) (set! matching_min_vertex_cover_filtered []) (doseq [edge matching_min_vertex_cover_edges] (do (set! matching_min_vertex_cover_a (nth edge 0)) (set! matching_min_vertex_cover_b (nth edge 1)) (when (and (and (and (not= matching_min_vertex_cover_a matching_min_vertex_cover_u) (not= matching_min_vertex_cover_b matching_min_vertex_cover_u)) (not= matching_min_vertex_cover_a matching_min_vertex_cover_v)) (not= matching_min_vertex_cover_b matching_min_vertex_cover_v)) (set! matching_min_vertex_cover_filtered (conj matching_min_vertex_cover_filtered edge))))) (set! matching_min_vertex_cover_edges matching_min_vertex_cover_filtered))) (throw (ex-info "return" {:v matching_min_vertex_cover_chosen}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_graph {0 [1 3] 1 [0 3] 2 [0 3 4] 3 [0 1 2] 4 [2 3]})

(def ^:dynamic main_cover (matching_min_vertex_cover main_graph))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_cover))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
