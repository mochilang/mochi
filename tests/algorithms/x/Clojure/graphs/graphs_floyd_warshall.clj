(ns main (:refer-clojure :exclude [floyd_warshall print_dist]))

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

(declare floyd_warshall print_dist)

(def ^:dynamic floyd_warshall_dist nil)

(def ^:dynamic floyd_warshall_i nil)

(def ^:dynamic floyd_warshall_j nil)

(def ^:dynamic floyd_warshall_k nil)

(def ^:dynamic floyd_warshall_row nil)

(def ^:dynamic floyd_warshall_v nil)

(def ^:dynamic print_dist_i nil)

(def ^:dynamic print_dist_j nil)

(def ^:dynamic print_dist_line nil)

(def ^:dynamic main_INF 1000000000.0)

(defn floyd_warshall [floyd_warshall_graph]
  (binding [floyd_warshall_dist nil floyd_warshall_i nil floyd_warshall_j nil floyd_warshall_k nil floyd_warshall_row nil floyd_warshall_v nil] (try (do (set! floyd_warshall_v (count floyd_warshall_graph)) (set! floyd_warshall_dist []) (set! floyd_warshall_i 0) (while (< floyd_warshall_i floyd_warshall_v) (do (set! floyd_warshall_row []) (set! floyd_warshall_j 0) (while (< floyd_warshall_j floyd_warshall_v) (do (set! floyd_warshall_row (conj floyd_warshall_row (nth (nth floyd_warshall_graph floyd_warshall_i) floyd_warshall_j))) (set! floyd_warshall_j (+ floyd_warshall_j 1)))) (set! floyd_warshall_dist (conj floyd_warshall_dist floyd_warshall_row)) (set! floyd_warshall_i (+ floyd_warshall_i 1)))) (set! floyd_warshall_k 0) (while (< floyd_warshall_k floyd_warshall_v) (do (set! floyd_warshall_i 0) (while (< floyd_warshall_i floyd_warshall_v) (do (set! floyd_warshall_j 0) (while (< floyd_warshall_j floyd_warshall_v) (do (when (and (and (< (nth (nth floyd_warshall_dist floyd_warshall_i) floyd_warshall_k) main_INF) (< (nth (nth floyd_warshall_dist floyd_warshall_k) floyd_warshall_j) main_INF)) (< (+ (nth (nth floyd_warshall_dist floyd_warshall_i) floyd_warshall_k) (nth (nth floyd_warshall_dist floyd_warshall_k) floyd_warshall_j)) (nth (nth floyd_warshall_dist floyd_warshall_i) floyd_warshall_j))) (set! floyd_warshall_dist (assoc-in floyd_warshall_dist [floyd_warshall_i floyd_warshall_j] (+ (nth (nth floyd_warshall_dist floyd_warshall_i) floyd_warshall_k) (nth (nth floyd_warshall_dist floyd_warshall_k) floyd_warshall_j))))) (set! floyd_warshall_j (+ floyd_warshall_j 1)))) (set! floyd_warshall_i (+ floyd_warshall_i 1)))) (set! floyd_warshall_k (+ floyd_warshall_k 1)))) (throw (ex-info "return" {:v floyd_warshall_dist}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_dist [print_dist_dist]
  (binding [print_dist_i nil print_dist_j nil print_dist_line nil] (do (println "\nThe shortest path matrix using Floyd Warshall algorithm\n") (set! print_dist_i 0) (while (< print_dist_i (count print_dist_dist)) (do (set! print_dist_j 0) (set! print_dist_line "") (while (< print_dist_j (count (nth print_dist_dist print_dist_i))) (do (if (>= (nth (nth print_dist_dist print_dist_i) print_dist_j) (/ main_INF 2.0)) (set! print_dist_line (str print_dist_line "INF\t")) (set! print_dist_line (str (str print_dist_line (str (Integer/parseInt (nth (nth print_dist_dist print_dist_i) print_dist_j)))) "\t"))) (set! print_dist_j (+ print_dist_j 1)))) (println print_dist_line) (set! print_dist_i (+ print_dist_i 1)))))))

(def ^:dynamic main_graph [[0.0 5.0 main_INF 10.0] [main_INF 0.0 3.0 main_INF] [main_INF main_INF 0.0 1.0] [main_INF main_INF main_INF 0.0]])

(def ^:dynamic main_result (floyd_warshall main_graph))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_result) (constantly (print_dist main_result)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
