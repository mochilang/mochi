(ns main (:refer-clojure :exclude [node_sum]))

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

(declare node_sum)

(def ^:dynamic node_sum_node nil)

(defn node_sum [node_sum_tree node_sum_index]
  (binding [node_sum_node nil] (try (do (when (= node_sum_index (- 1)) (throw (ex-info "return" {:v 0}))) (set! node_sum_node (nth node_sum_tree node_sum_index)) (throw (ex-info "return" {:v (+ (+ (:value node_sum_node) (node_sum node_sum_tree (:left node_sum_node))) (node_sum node_sum_tree (:right node_sum_node)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_example [{:value 10 :left 1 :right 2} {:value 5 :left 3 :right (- 1)} {:value (- 3) :left 4 :right 5} {:value 12 :left (- 1) :right (- 1)} {:value 8 :left (- 1) :right (- 1)} {:value 0 :left (- 1) :right (- 1)}])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (node_sum main_example 0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
