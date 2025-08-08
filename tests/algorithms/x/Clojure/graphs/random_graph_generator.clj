(ns main (:refer-clojure :exclude [rand random complete_graph random_graph main]))

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

(declare rand random complete_graph random_graph main)

(def ^:dynamic complete_graph_graph nil)

(def ^:dynamic complete_graph_i nil)

(def ^:dynamic complete_graph_j nil)

(def ^:dynamic complete_graph_neighbors nil)

(def ^:dynamic main_g1 nil)

(def ^:dynamic main_g2 nil)

(def ^:dynamic random_graph_graph nil)

(def ^:dynamic random_graph_i nil)

(def ^:dynamic random_graph_j nil)

(def ^:dynamic main_seed 1)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn random []
  (try (throw (ex-info "return" {:v (/ (* 1.0 (rand)) 2147483648.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn complete_graph [complete_graph_vertices_number]
  (binding [complete_graph_graph nil complete_graph_i nil complete_graph_j nil complete_graph_neighbors nil] (try (do (set! complete_graph_graph {}) (set! complete_graph_i 0) (while (< complete_graph_i complete_graph_vertices_number) (do (set! complete_graph_neighbors []) (set! complete_graph_j 0) (while (< complete_graph_j complete_graph_vertices_number) (do (when (not= complete_graph_j complete_graph_i) (set! complete_graph_neighbors (conj complete_graph_neighbors complete_graph_j))) (set! complete_graph_j (+ complete_graph_j 1)))) (set! complete_graph_graph (assoc complete_graph_graph complete_graph_i complete_graph_neighbors)) (set! complete_graph_i (+ complete_graph_i 1)))) (throw (ex-info "return" {:v complete_graph_graph}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn random_graph [random_graph_vertices_number random_graph_probability random_graph_directed]
  (binding [random_graph_graph nil random_graph_i nil random_graph_j nil] (try (do (set! random_graph_graph {}) (set! random_graph_i 0) (while (< random_graph_i random_graph_vertices_number) (do (set! random_graph_graph (assoc random_graph_graph random_graph_i [])) (set! random_graph_i (+ random_graph_i 1)))) (when (>= random_graph_probability 1.0) (throw (ex-info "return" {:v (complete_graph random_graph_vertices_number)}))) (when (<= random_graph_probability 0.0) (throw (ex-info "return" {:v random_graph_graph}))) (set! random_graph_i 0) (while (< random_graph_i random_graph_vertices_number) (do (set! random_graph_j (+ random_graph_i 1)) (while (< random_graph_j random_graph_vertices_number) (do (when (< (random) random_graph_probability) (do (set! random_graph_graph (assoc random_graph_graph random_graph_i (conj (get random_graph_graph random_graph_i) random_graph_j))) (when (not random_graph_directed) (set! random_graph_graph (assoc random_graph_graph random_graph_j (conj (get random_graph_graph random_graph_j) random_graph_i)))))) (set! random_graph_j (+ random_graph_j 1)))) (set! random_graph_i (+ random_graph_i 1)))) (throw (ex-info "return" {:v random_graph_graph}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_g1 nil main_g2 nil] (do (alter-var-root (var main_seed) (fn [_] 1)) (set! main_g1 (random_graph 4 0.5 false)) (println main_g1) (alter-var-root (var main_seed) (fn [_] 1)) (set! main_g2 (random_graph 4 0.5 true)) (println main_g2))))

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
