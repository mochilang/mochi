(ns main (:refer-clojure :exclude [longest_distance]))

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

(declare longest_distance)

(def ^:dynamic longest_distance_head nil)

(def ^:dynamic longest_distance_i nil)

(def ^:dynamic longest_distance_indegree nil)

(def ^:dynamic longest_distance_j nil)

(def ^:dynamic longest_distance_k nil)

(def ^:dynamic longest_distance_long_dist nil)

(def ^:dynamic longest_distance_m nil)

(def ^:dynamic longest_distance_max_len nil)

(def ^:dynamic longest_distance_n nil)

(def ^:dynamic longest_distance_new_dist nil)

(def ^:dynamic longest_distance_queue nil)

(def ^:dynamic longest_distance_u nil)

(def ^:dynamic longest_distance_vertex nil)

(defn longest_distance [longest_distance_graph]
  (binding [longest_distance_head nil longest_distance_i nil longest_distance_indegree nil longest_distance_j nil longest_distance_k nil longest_distance_long_dist nil longest_distance_m nil longest_distance_max_len nil longest_distance_n nil longest_distance_new_dist nil longest_distance_queue nil longest_distance_u nil longest_distance_vertex nil] (try (do (set! longest_distance_n (count longest_distance_graph)) (set! longest_distance_indegree []) (set! longest_distance_i 0) (while (< longest_distance_i longest_distance_n) (do (set! longest_distance_indegree (conj longest_distance_indegree 0)) (set! longest_distance_i (+ longest_distance_i 1)))) (set! longest_distance_long_dist []) (set! longest_distance_j 0) (while (< longest_distance_j longest_distance_n) (do (set! longest_distance_long_dist (conj longest_distance_long_dist 1)) (set! longest_distance_j (+ longest_distance_j 1)))) (set! longest_distance_u 0) (while (< longest_distance_u longest_distance_n) (do (doseq [v (nth longest_distance_graph longest_distance_u)] (set! longest_distance_indegree (assoc longest_distance_indegree v (+ (nth longest_distance_indegree v) 1)))) (set! longest_distance_u (+ longest_distance_u 1)))) (set! longest_distance_queue []) (set! longest_distance_head 0) (set! longest_distance_k 0) (while (< longest_distance_k longest_distance_n) (do (when (= (nth longest_distance_indegree longest_distance_k) 0) (set! longest_distance_queue (conj longest_distance_queue longest_distance_k))) (set! longest_distance_k (+ longest_distance_k 1)))) (while (< longest_distance_head (count longest_distance_queue)) (do (set! longest_distance_vertex (nth longest_distance_queue longest_distance_head)) (set! longest_distance_head (+ longest_distance_head 1)) (doseq [x (nth longest_distance_graph longest_distance_vertex)] (do (set! longest_distance_indegree (assoc longest_distance_indegree x (- (nth longest_distance_indegree x) 1))) (set! longest_distance_new_dist (+ (nth longest_distance_long_dist longest_distance_vertex) 1)) (when (> longest_distance_new_dist (nth longest_distance_long_dist x)) (set! longest_distance_long_dist (assoc longest_distance_long_dist x longest_distance_new_dist))) (when (= (nth longest_distance_indegree x) 0) (set! longest_distance_queue (conj longest_distance_queue x))))))) (set! longest_distance_max_len (nth longest_distance_long_dist 0)) (set! longest_distance_m 1) (while (< longest_distance_m longest_distance_n) (do (when (> (nth longest_distance_long_dist longest_distance_m) longest_distance_max_len) (set! longest_distance_max_len (nth longest_distance_long_dist longest_distance_m))) (set! longest_distance_m (+ longest_distance_m 1)))) (throw (ex-info "return" {:v longest_distance_max_len}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_graph [[2 3 4] [2 7] [5] [5 7] [7] [6] [7] []])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (longest_distance main_graph))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
