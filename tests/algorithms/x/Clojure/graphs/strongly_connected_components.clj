(ns main (:refer-clojure :exclude [topology_sort find_component strongly_connected_components main]))

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

(declare topology_sort find_component strongly_connected_components main)

(def ^:dynamic find_component_comp nil)

(def ^:dynamic find_component_visited nil)

(def ^:dynamic main_test_graph_1 nil)

(def ^:dynamic main_test_graph_2 nil)

(def ^:dynamic strongly_connected_components_comp nil)

(def ^:dynamic strongly_connected_components_components nil)

(def ^:dynamic strongly_connected_components_i nil)

(def ^:dynamic strongly_connected_components_n nil)

(def ^:dynamic strongly_connected_components_order nil)

(def ^:dynamic strongly_connected_components_reversed nil)

(def ^:dynamic strongly_connected_components_v nil)

(def ^:dynamic strongly_connected_components_visited nil)

(def ^:dynamic topology_sort_order nil)

(def ^:dynamic topology_sort_visited nil)

(defn topology_sort [topology_sort_graph topology_sort_vert topology_sort_visited_p]
  (binding [topology_sort_order nil topology_sort_visited nil] (try (do (set! topology_sort_visited topology_sort_visited_p) (set! topology_sort_visited (assoc topology_sort_visited topology_sort_vert true)) (set! topology_sort_order []) (doseq [neighbour (nth topology_sort_graph topology_sort_vert)] (when (not (nth topology_sort_visited neighbour)) (set! topology_sort_order (concat topology_sort_order (topology_sort topology_sort_graph neighbour topology_sort_visited))))) (set! topology_sort_order (conj topology_sort_order topology_sort_vert)) (throw (ex-info "return" {:v topology_sort_order}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_component [find_component_graph find_component_vert find_component_visited_p]
  (binding [find_component_comp nil find_component_visited nil] (try (do (set! find_component_visited find_component_visited_p) (set! find_component_visited (assoc find_component_visited find_component_vert true)) (set! find_component_comp [find_component_vert]) (doseq [neighbour (nth find_component_graph find_component_vert)] (when (not (nth find_component_visited neighbour)) (set! find_component_comp (concat find_component_comp (find_component find_component_graph neighbour find_component_visited))))) (throw (ex-info "return" {:v find_component_comp}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn strongly_connected_components [strongly_connected_components_graph]
  (binding [strongly_connected_components_comp nil strongly_connected_components_components nil strongly_connected_components_i nil strongly_connected_components_n nil strongly_connected_components_order nil strongly_connected_components_reversed nil strongly_connected_components_v nil strongly_connected_components_visited nil] (try (do (set! strongly_connected_components_n (count strongly_connected_components_graph)) (set! strongly_connected_components_visited []) (dotimes [_ strongly_connected_components_n] (set! strongly_connected_components_visited (conj strongly_connected_components_visited false))) (set! strongly_connected_components_reversed []) (dotimes [_ strongly_connected_components_n] (set! strongly_connected_components_reversed (conj strongly_connected_components_reversed []))) (dotimes [i strongly_connected_components_n] (doseq [neighbour (nth strongly_connected_components_graph i)] (set! strongly_connected_components_reversed (assoc strongly_connected_components_reversed neighbour (conj (nth strongly_connected_components_reversed neighbour) i))))) (set! strongly_connected_components_order []) (dotimes [i strongly_connected_components_n] (when (not (nth strongly_connected_components_visited i)) (set! strongly_connected_components_order (concat strongly_connected_components_order (topology_sort strongly_connected_components_graph i strongly_connected_components_visited))))) (set! strongly_connected_components_visited []) (dotimes [_ strongly_connected_components_n] (set! strongly_connected_components_visited (conj strongly_connected_components_visited false))) (set! strongly_connected_components_components []) (set! strongly_connected_components_i 0) (while (< strongly_connected_components_i strongly_connected_components_n) (do (set! strongly_connected_components_v (nth strongly_connected_components_order (- (- strongly_connected_components_n strongly_connected_components_i) 1))) (when (not (nth strongly_connected_components_visited strongly_connected_components_v)) (do (set! strongly_connected_components_comp (find_component strongly_connected_components_reversed strongly_connected_components_v strongly_connected_components_visited)) (set! strongly_connected_components_components (conj strongly_connected_components_components strongly_connected_components_comp)))) (set! strongly_connected_components_i (+ strongly_connected_components_i 1)))) (throw (ex-info "return" {:v strongly_connected_components_components}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_test_graph_1 nil main_test_graph_2 nil] (do (set! main_test_graph_1 [[2 3] [0] [1] [4] []]) (set! main_test_graph_2 [[1 2 3] [2] [0] [4] [5] [3]]) (println (str (strongly_connected_components main_test_graph_1))) (println (str (strongly_connected_components main_test_graph_2))))))

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
