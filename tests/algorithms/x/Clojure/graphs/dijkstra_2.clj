(ns main (:refer-clojure :exclude [print_dist min_dist dijkstra main]))

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

(declare print_dist min_dist dijkstra main)

(def ^:dynamic count_v nil)

(def ^:dynamic dijkstra_alt nil)

(def ^:dynamic dijkstra_i nil)

(def ^:dynamic dijkstra_mdist nil)

(def ^:dynamic dijkstra_u nil)

(def ^:dynamic dijkstra_v nil)

(def ^:dynamic dijkstra_vset nil)

(def ^:dynamic main_dist nil)

(def ^:dynamic main_graph nil)

(def ^:dynamic min_dist_i nil)

(def ^:dynamic min_dist_min_ind nil)

(def ^:dynamic min_dist_min_val nil)

(def ^:dynamic print_dist_i nil)

(def ^:dynamic main_INF 1000000000.0)

(defn print_dist [print_dist_dist]
  (binding [print_dist_i nil] (do (println "Vertex Distance") (set! print_dist_i 0) (while (< print_dist_i (count print_dist_dist)) (do (if (>= (nth print_dist_dist print_dist_i) main_INF) (println print_dist_i "\tINF") (println print_dist_i "\t" (long (nth print_dist_dist print_dist_i)))) (set! print_dist_i (+ print_dist_i 1)))))))

(defn min_dist [min_dist_mdist min_dist_vset]
  (binding [min_dist_i nil min_dist_min_ind nil min_dist_min_val nil] (try (do (set! min_dist_min_val main_INF) (set! min_dist_min_ind (- 1)) (set! min_dist_i 0) (while (< min_dist_i (count min_dist_mdist)) (do (when (and (not (nth min_dist_vset min_dist_i)) (< (nth min_dist_mdist min_dist_i) min_dist_min_val)) (do (set! min_dist_min_val (nth min_dist_mdist min_dist_i)) (set! min_dist_min_ind min_dist_i))) (set! min_dist_i (+ min_dist_i 1)))) (throw (ex-info "return" {:v min_dist_min_ind}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dijkstra [dijkstra_graph dijkstra_src]
  (binding [count_v nil dijkstra_alt nil dijkstra_i nil dijkstra_mdist nil dijkstra_u nil dijkstra_v nil dijkstra_vset nil] (try (do (set! dijkstra_v (count dijkstra_graph)) (set! dijkstra_mdist []) (set! dijkstra_vset []) (set! dijkstra_i 0) (while (< dijkstra_i dijkstra_v) (do (set! dijkstra_mdist (conj dijkstra_mdist main_INF)) (set! dijkstra_vset (conj dijkstra_vset false)) (set! dijkstra_i (+ dijkstra_i 1)))) (set! dijkstra_mdist (assoc dijkstra_mdist dijkstra_src 0.0)) (set! count_v 0) (while (< count_v (- dijkstra_v 1)) (do (set! dijkstra_u (min_dist dijkstra_mdist dijkstra_vset)) (set! dijkstra_vset (assoc dijkstra_vset dijkstra_u true)) (set! dijkstra_i 0) (while (< dijkstra_i dijkstra_v) (do (set! dijkstra_alt (+ (nth dijkstra_mdist dijkstra_u) (nth (nth dijkstra_graph dijkstra_u) dijkstra_i))) (when (and (and (not (nth dijkstra_vset dijkstra_i)) (< (nth (nth dijkstra_graph dijkstra_u) dijkstra_i) main_INF)) (< dijkstra_alt (nth dijkstra_mdist dijkstra_i))) (set! dijkstra_mdist (assoc dijkstra_mdist dijkstra_i dijkstra_alt))) (set! dijkstra_i (+ dijkstra_i 1)))) (set! count_v (+ count_v 1)))) (throw (ex-info "return" {:v dijkstra_mdist}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_dist nil main_graph nil] (do (set! main_graph [[0.0 10.0 main_INF main_INF 5.0] [main_INF 0.0 1.0 main_INF 2.0] [main_INF main_INF 0.0 4.0 main_INF] [main_INF main_INF 6.0 0.0 main_INF] [main_INF 3.0 9.0 2.0 0.0]]) (set! main_dist (dijkstra main_graph 0)) (print_dist main_dist))))

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
