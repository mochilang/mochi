(ns main (:refer-clojure :exclude [tree_sum is_sum_node build_a_tree build_a_sum_tree]))

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

(declare tree_sum is_sum_node build_a_tree build_a_sum_tree)

(def ^:dynamic is_sum_node_left_ok nil)

(def ^:dynamic is_sum_node_left_sum nil)

(def ^:dynamic is_sum_node_node nil)

(def ^:dynamic is_sum_node_right_ok nil)

(def ^:dynamic is_sum_node_right_sum nil)

(def ^:dynamic tree_sum_node nil)

(defn tree_sum [tree_sum_nodes tree_sum_idx]
  (binding [tree_sum_node nil] (try (do (when (= tree_sum_idx (- 1)) (throw (ex-info "return" {:v 0}))) (set! tree_sum_node (nth tree_sum_nodes tree_sum_idx)) (throw (ex-info "return" {:v (+ (+ (:value tree_sum_node) (tree_sum tree_sum_nodes (:left tree_sum_node))) (tree_sum tree_sum_nodes (:right tree_sum_node)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_sum_node [is_sum_node_nodes is_sum_node_idx]
  (binding [is_sum_node_left_ok nil is_sum_node_left_sum nil is_sum_node_node nil is_sum_node_right_ok nil is_sum_node_right_sum nil] (try (do (set! is_sum_node_node (nth is_sum_node_nodes is_sum_node_idx)) (when (and (= (:left is_sum_node_node) (- 1)) (= (:right is_sum_node_node) (- 1))) (throw (ex-info "return" {:v true}))) (set! is_sum_node_left_sum (tree_sum is_sum_node_nodes (:left is_sum_node_node))) (set! is_sum_node_right_sum (tree_sum is_sum_node_nodes (:right is_sum_node_node))) (when (not= (:value is_sum_node_node) (+ is_sum_node_left_sum is_sum_node_right_sum)) (throw (ex-info "return" {:v false}))) (set! is_sum_node_left_ok true) (when (not= (:left is_sum_node_node) (- 1)) (set! is_sum_node_left_ok (is_sum_node is_sum_node_nodes (:left is_sum_node_node)))) (set! is_sum_node_right_ok true) (when (not= (:right is_sum_node_node) (- 1)) (set! is_sum_node_right_ok (is_sum_node is_sum_node_nodes (:right is_sum_node_node)))) (throw (ex-info "return" {:v (and is_sum_node_left_ok is_sum_node_right_ok)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn build_a_tree []
  (try (throw (ex-info "return" {:v [{:value 11 :left 1 :right 2} {:value 2 :left 3 :right 4} {:value 29 :left 5 :right 6} {:value 1 :left (- 1) :right (- 1)} {:value 7 :left (- 1) :right (- 1)} {:value 15 :left (- 1) :right (- 1)} {:value 40 :left 7 :right (- 1)} {:value 35 :left (- 1) :right (- 1)}]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn build_a_sum_tree []
  (try (throw (ex-info "return" {:v [{:value 26 :left 1 :right 2} {:value 10 :left 3 :right 4} {:value 3 :left (- 1) :right 5} {:value 4 :left (- 1) :right (- 1)} {:value 6 :left (- 1) :right (- 1)} {:value 3 :left (- 1) :right (- 1)}]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
