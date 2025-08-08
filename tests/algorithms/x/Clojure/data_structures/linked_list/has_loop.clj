(ns main (:refer-clojure :exclude [has_loop make_nodes main]))

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

(declare has_loop make_nodes main)

(def ^:dynamic has_loop_fast nil)

(def ^:dynamic has_loop_fast_node1 nil)

(def ^:dynamic has_loop_fast_node2 nil)

(def ^:dynamic has_loop_slow nil)

(def ^:dynamic has_loop_slow_node nil)

(def ^:dynamic main_list1 nil)

(def ^:dynamic main_list2 nil)

(def ^:dynamic main_list3 nil)

(def ^:dynamic make_nodes_i nil)

(def ^:dynamic make_nodes_next_idx nil)

(def ^:dynamic make_nodes_nodes nil)

(defn has_loop [has_loop_nodes has_loop_head]
  (binding [has_loop_fast nil has_loop_fast_node1 nil has_loop_fast_node2 nil has_loop_slow nil has_loop_slow_node nil] (try (do (set! has_loop_slow has_loop_head) (set! has_loop_fast has_loop_head) (while (not= has_loop_fast (- 0 1)) (do (set! has_loop_fast_node1 (nth has_loop_nodes has_loop_fast)) (when (= (:next has_loop_fast_node1) (- 0 1)) (throw (ex-info "return" {:v false}))) (set! has_loop_fast_node2 (nth has_loop_nodes (:next has_loop_fast_node1))) (when (= (:next has_loop_fast_node2) (- 0 1)) (throw (ex-info "return" {:v false}))) (set! has_loop_slow_node (nth has_loop_nodes has_loop_slow)) (set! has_loop_slow (:next has_loop_slow_node)) (set! has_loop_fast (:next has_loop_fast_node2)) (when (= has_loop_slow has_loop_fast) (throw (ex-info "return" {:v true}))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_nodes [make_nodes_values]
  (binding [make_nodes_i nil make_nodes_next_idx nil make_nodes_nodes nil] (try (do (set! make_nodes_nodes []) (set! make_nodes_i 0) (while (< make_nodes_i (count make_nodes_values)) (do (set! make_nodes_next_idx (if (= make_nodes_i (- (count make_nodes_values) 1)) (- 0 1) (+ make_nodes_i 1))) (set! make_nodes_nodes (conj make_nodes_nodes {:data (nth make_nodes_values make_nodes_i) :next make_nodes_next_idx})) (set! make_nodes_i (+ make_nodes_i 1)))) (throw (ex-info "return" {:v make_nodes_nodes}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_list1 nil main_list2 nil main_list3 nil] (do (set! main_list1 (make_nodes [1 2 3 4])) (println (str (has_loop main_list1 0))) (set! main_list1 (assoc-in main_list1 [3 :next] 1)) (println (str (has_loop main_list1 0))) (set! main_list2 (make_nodes [5 6 5 6])) (println (str (has_loop main_list2 0))) (set! main_list3 (make_nodes [1])) (println (str (has_loop main_list3 0))))))

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
