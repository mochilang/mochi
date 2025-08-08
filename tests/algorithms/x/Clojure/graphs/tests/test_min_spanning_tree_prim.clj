(ns main (:refer-clojure :exclude [prims_algorithm test_prim_successful_result]))

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

(declare prims_algorithm test_prim_successful_result)

(def ^:dynamic count_v nil)

(def ^:dynamic prims_algorithm_best_cost nil)

(def ^:dynamic prims_algorithm_best_u nil)

(def ^:dynamic prims_algorithm_best_v nil)

(def ^:dynamic prims_algorithm_mst nil)

(def ^:dynamic prims_algorithm_total nil)

(def ^:dynamic prims_algorithm_u nil)

(def ^:dynamic prims_algorithm_visited nil)

(def ^:dynamic test_prim_successful_result_adjacency nil)

(def ^:dynamic test_prim_successful_result_edges nil)

(def ^:dynamic test_prim_successful_result_expected nil)

(def ^:dynamic test_prim_successful_result_key nil)

(def ^:dynamic test_prim_successful_result_key1 nil)

(def ^:dynamic test_prim_successful_result_key2 nil)

(def ^:dynamic test_prim_successful_result_result nil)

(def ^:dynamic test_prim_successful_result_seen nil)

(def ^:dynamic test_prim_successful_result_u nil)

(def ^:dynamic test_prim_successful_result_v nil)

(def ^:dynamic test_prim_successful_result_w nil)

(defn prims_algorithm [prims_algorithm_adjacency]
  (binding [count_v nil prims_algorithm_best_cost nil prims_algorithm_best_u nil prims_algorithm_best_v nil prims_algorithm_mst nil prims_algorithm_total nil prims_algorithm_u nil prims_algorithm_visited nil] (try (do (set! prims_algorithm_visited {}) (set! prims_algorithm_visited (assoc prims_algorithm_visited 0 true)) (set! prims_algorithm_mst []) (set! count_v 1) (set! prims_algorithm_total 0) (doseq [k (keys prims_algorithm_adjacency)] (set! prims_algorithm_total (+ prims_algorithm_total 1))) (while (< count_v prims_algorithm_total) (do (set! prims_algorithm_best_u 0) (set! prims_algorithm_best_v 0) (set! prims_algorithm_best_cost 2147483647) (doseq [u_str (keys prims_algorithm_adjacency)] (do (set! prims_algorithm_u (Integer/parseInt u_str)) (when (get prims_algorithm_visited prims_algorithm_u) (doseq [n (get prims_algorithm_adjacency prims_algorithm_u)] (when (and (not (get prims_algorithm_visited (:node n))) (< (:cost n) prims_algorithm_best_cost)) (do (set! prims_algorithm_best_cost (:cost n)) (set! prims_algorithm_best_u prims_algorithm_u) (set! prims_algorithm_best_v (:node n)))))))) (set! prims_algorithm_visited (assoc prims_algorithm_visited prims_algorithm_best_v true)) (set! prims_algorithm_mst (conj prims_algorithm_mst {:u prims_algorithm_best_u :v prims_algorithm_best_v})) (set! count_v (+ count_v 1)))) (throw (ex-info "return" {:v prims_algorithm_mst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_prim_successful_result []
  (binding [test_prim_successful_result_adjacency nil test_prim_successful_result_edges nil test_prim_successful_result_expected nil test_prim_successful_result_key nil test_prim_successful_result_key1 nil test_prim_successful_result_key2 nil test_prim_successful_result_result nil test_prim_successful_result_seen nil test_prim_successful_result_u nil test_prim_successful_result_v nil test_prim_successful_result_w nil] (try (do (set! test_prim_successful_result_edges [[0 1 4] [0 7 8] [1 2 8] [7 8 7] [7 6 1] [2 8 2] [8 6 6] [2 3 7] [2 5 4] [6 5 2] [3 5 14] [3 4 9] [5 4 10] [1 7 11]]) (set! test_prim_successful_result_adjacency {}) (doseq [e test_prim_successful_result_edges] (do (set! test_prim_successful_result_u (nth e 0)) (set! test_prim_successful_result_v (nth e 1)) (set! test_prim_successful_result_w (nth e 2)) (when (not (in test_prim_successful_result_u test_prim_successful_result_adjacency)) (set! test_prim_successful_result_adjacency (assoc test_prim_successful_result_adjacency test_prim_successful_result_u []))) (when (not (in test_prim_successful_result_v test_prim_successful_result_adjacency)) (set! test_prim_successful_result_adjacency (assoc test_prim_successful_result_adjacency test_prim_successful_result_v []))) (set! test_prim_successful_result_adjacency (assoc test_prim_successful_result_adjacency test_prim_successful_result_u (conj (get test_prim_successful_result_adjacency test_prim_successful_result_u) {:cost test_prim_successful_result_w :node test_prim_successful_result_v}))) (set! test_prim_successful_result_adjacency (assoc test_prim_successful_result_adjacency test_prim_successful_result_v (conj (get test_prim_successful_result_adjacency test_prim_successful_result_v) {:cost test_prim_successful_result_w :node test_prim_successful_result_u}))))) (set! test_prim_successful_result_result (prims_algorithm test_prim_successful_result_adjacency)) (set! test_prim_successful_result_seen {}) (doseq [e test_prim_successful_result_result] (do (set! test_prim_successful_result_key1 (str (str (str (:u e)) ",") (str (:v e)))) (set! test_prim_successful_result_key2 (str (str (str (:v e)) ",") (str (:u e)))) (set! test_prim_successful_result_seen (assoc test_prim_successful_result_seen test_prim_successful_result_key1 true)) (set! test_prim_successful_result_seen (assoc test_prim_successful_result_seen test_prim_successful_result_key2 true)))) (set! test_prim_successful_result_expected [[7 6 1] [2 8 2] [6 5 2] [0 1 4] [2 5 4] [2 3 7] [0 7 8] [3 4 9]]) (doseq [ans test_prim_successful_result_expected] (do (set! test_prim_successful_result_key (str (str (str (nth ans 0)) ",") (str (nth ans 1)))) (when (not (get test_prim_successful_result_seen test_prim_successful_result_key)) (throw (ex-info "return" {:v false}))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (test_prim_successful_result))
      (println true)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
