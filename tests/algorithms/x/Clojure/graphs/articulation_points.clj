(ns main (:refer-clojure :exclude [dfs_skip articulation_points main]))

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

(declare dfs_skip articulation_points main)

(def ^:dynamic articulation_points_i nil)

(def ^:dynamic articulation_points_n nil)

(def ^:dynamic articulation_points_reach nil)

(def ^:dynamic articulation_points_result nil)

(def ^:dynamic articulation_points_start nil)

(def ^:dynamic articulation_points_v nil)

(def ^:dynamic articulation_points_visited nil)

(def ^:dynamic count_v nil)

(def ^:dynamic dfs_skip_visited nil)

(def ^:dynamic main_graph nil)

(defn dfs_skip [dfs_skip_graph dfs_skip_visited_p dfs_skip_skip dfs_skip_at]
  (binding [count_v nil dfs_skip_visited nil] (try (do (set! dfs_skip_visited dfs_skip_visited_p) (set! dfs_skip_visited (assoc dfs_skip_visited dfs_skip_at true)) (set! count_v 1) (loop [to_seq (nth dfs_skip_graph dfs_skip_at)] (when (seq to_seq) (let [to (first to_seq)] (cond (= to dfs_skip_skip) (recur (rest to_seq)) :else (do (when (= (nth dfs_skip_visited to) false) (set! count_v (+ count_v (dfs_skip dfs_skip_graph dfs_skip_visited dfs_skip_skip to)))) (recur (rest to_seq))))))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn articulation_points [articulation_points_graph]
  (binding [articulation_points_i nil articulation_points_n nil articulation_points_reach nil articulation_points_result nil articulation_points_start nil articulation_points_v nil articulation_points_visited nil] (try (do (set! articulation_points_n (count articulation_points_graph)) (set! articulation_points_result []) (set! articulation_points_v 0) (while (< articulation_points_v articulation_points_n) (do (set! articulation_points_visited []) (set! articulation_points_i 0) (while (< articulation_points_i articulation_points_n) (do (set! articulation_points_visited (conj articulation_points_visited false)) (set! articulation_points_i (+ articulation_points_i 1)))) (set! articulation_points_start 0) (while (and (= articulation_points_start articulation_points_v) (< articulation_points_start articulation_points_n)) (set! articulation_points_start (+ articulation_points_start 1))) (set! articulation_points_reach (dfs_skip articulation_points_graph articulation_points_visited articulation_points_v articulation_points_start)) (when (< articulation_points_reach (- articulation_points_n 1)) (do (set! articulation_points_result (conj articulation_points_result articulation_points_v)) (println articulation_points_v))) (set! articulation_points_v (+ articulation_points_v 1)))) (throw (ex-info "return" {:v articulation_points_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_graph nil] (do (set! main_graph [[1 2] [0 2] [0 1 3 5] [2 4] [3] [2 6 8] [5 7] [6 8] [5 7]]) (articulation_points main_graph))))

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
