(ns main (:refer-clojure :exclude [count_nodes count_coins iabs dfs distribute_coins main]))

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

(declare count_nodes count_coins iabs dfs distribute_coins main)

(def ^:dynamic count_coins_node nil)

(def ^:dynamic count_nodes_node nil)

(def ^:dynamic dfs_abs_left nil)

(def ^:dynamic dfs_abs_right nil)

(def ^:dynamic dfs_left_excess nil)

(def ^:dynamic dfs_node nil)

(def ^:dynamic dfs_right_excess nil)

(def ^:dynamic dfs_total_moves nil)

(def ^:dynamic distribute_coins_total_moves nil)

(def ^:dynamic main_example1 nil)

(def ^:dynamic main_example2 nil)

(def ^:dynamic main_example3 nil)

(defn count_nodes [count_nodes_nodes count_nodes_idx]
  (binding [count_nodes_node nil] (try (do (when (= count_nodes_idx 0) (throw (ex-info "return" {:v 0}))) (set! count_nodes_node (nth count_nodes_nodes count_nodes_idx)) (throw (ex-info "return" {:v (+ (+ (count_nodes count_nodes_nodes (:left count_nodes_node)) (count_nodes count_nodes_nodes (:right count_nodes_node))) 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_coins [count_coins_nodes count_coins_idx]
  (binding [count_coins_node nil] (try (do (when (= count_coins_idx 0) (throw (ex-info "return" {:v 0}))) (set! count_coins_node (nth count_coins_nodes count_coins_idx)) (throw (ex-info "return" {:v (+ (+ (count_coins count_coins_nodes (:left count_coins_node)) (count_coins count_coins_nodes (:right count_coins_node))) (:data count_coins_node))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_total_moves 0)

(defn iabs [iabs_x]
  (try (if (< iabs_x 0) (- iabs_x) iabs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dfs [dfs_nodes dfs_idx]
  (binding [dfs_abs_left nil dfs_abs_right nil dfs_left_excess nil dfs_node nil dfs_right_excess nil dfs_total_moves nil] (try (do (when (= dfs_idx 0) (throw (ex-info "return" {:v 0}))) (set! dfs_node (nth dfs_nodes dfs_idx)) (set! dfs_left_excess (dfs dfs_nodes (:left dfs_node))) (set! dfs_right_excess (dfs dfs_nodes (:right dfs_node))) (set! dfs_abs_left (iabs dfs_left_excess)) (set! dfs_abs_right (iabs dfs_right_excess)) (set! dfs_total_moves (+ (+ main_total_moves dfs_abs_left) dfs_abs_right)) (throw (ex-info "return" {:v (- (+ (+ (:data dfs_node) dfs_left_excess) dfs_right_excess) 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn distribute_coins [distribute_coins_nodes distribute_coins_root]
  (binding [distribute_coins_total_moves nil] (try (do (when (= distribute_coins_root 0) (throw (ex-info "return" {:v 0}))) (when (not= (count_nodes distribute_coins_nodes distribute_coins_root) (count_coins distribute_coins_nodes distribute_coins_root)) (throw (Exception. "The nodes number should be same as the number of coins"))) (set! distribute_coins_total_moves 0) (dfs distribute_coins_nodes distribute_coins_root) (throw (ex-info "return" {:v distribute_coins_total_moves}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_example1 nil main_example2 nil main_example3 nil] (do (set! main_example1 [{:data 0 :left 0 :right 0} {:data 3 :left 2 :right 3} {:data 0 :left 0 :right 0} {:data 0 :left 0 :right 0}]) (set! main_example2 [{:data 0 :left 0 :right 0} {:data 0 :left 2 :right 3} {:data 3 :left 0 :right 0} {:data 0 :left 0 :right 0}]) (set! main_example3 [{:data 0 :left 0 :right 0} {:data 0 :left 2 :right 3} {:data 0 :left 0 :right 0} {:data 3 :left 0 :right 0}]) (println (distribute_coins main_example1 1)) (println (distribute_coins main_example2 1)) (println (distribute_coins main_example3 1)) (println (distribute_coins [{:data 0 :left 0 :right 0}] 0)))))

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
