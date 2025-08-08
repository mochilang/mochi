(ns main (:refer-clojure :exclude [square_distance search nearest_neighbour_search]))

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

(declare square_distance search nearest_neighbour_search)

(def ^:dynamic main_q nil)

(def ^:dynamic nearest_neighbour_search_initial nil)

(def ^:dynamic search_axis nil)

(def ^:dynamic search_current_dist nil)

(def ^:dynamic search_current_point nil)

(def ^:dynamic search_diff nil)

(def ^:dynamic search_further nil)

(def ^:dynamic search_k nil)

(def ^:dynamic search_nearer nil)

(def ^:dynamic search_node nil)

(def ^:dynamic search_result nil)

(def ^:dynamic square_distance_diff nil)

(def ^:dynamic square_distance_i nil)

(def ^:dynamic square_distance_total nil)

(defn square_distance [square_distance_a square_distance_b]
  (binding [square_distance_diff nil square_distance_i nil square_distance_total nil] (try (do (set! square_distance_i 0) (set! square_distance_total 0.0) (while (< square_distance_i (count square_distance_a)) (do (set! square_distance_diff (- (nth square_distance_a square_distance_i) (nth square_distance_b square_distance_i))) (set! square_distance_total (+ square_distance_total (* square_distance_diff square_distance_diff))) (set! square_distance_i (+ square_distance_i 1)))) (throw (ex-info "return" {:v square_distance_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn search [search_nodes search_index search_query_point search_depth search_best]
  (binding [search_axis nil search_current_dist nil search_current_point nil search_diff nil search_further nil search_k nil search_nearer nil search_node nil search_result nil] (try (do (when (= search_index (- 1)) (throw (ex-info "return" {:v search_best}))) (set! search_result search_best) (set! search_result (assoc search_result :nodes_visited (+ (:nodes_visited search_result) 1))) (set! search_node (nth search_nodes search_index)) (set! search_current_point (:point search_node)) (set! search_current_dist (square_distance search_query_point search_current_point)) (when (or (= (count (:point search_result)) 0) (< search_current_dist (:distance search_result))) (do (set! search_result (assoc search_result :point search_current_point)) (set! search_result (assoc search_result :distance search_current_dist)))) (set! search_k (count search_query_point)) (set! search_axis (mod search_depth search_k)) (set! search_nearer (:left search_node)) (set! search_further (:right search_node)) (when (> (nth search_query_point search_axis) (nth search_current_point search_axis)) (do (set! search_nearer (:right search_node)) (set! search_further (:left search_node)))) (set! search_result (search search_nodes search_nearer search_query_point (+ search_depth 1) search_result)) (set! search_diff (- (nth search_query_point search_axis) (nth search_current_point search_axis))) (when (< (* search_diff search_diff) (:distance search_result)) (set! search_result (search search_nodes search_further search_query_point (+ search_depth 1) search_result))) (throw (ex-info "return" {:v search_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn nearest_neighbour_search [nearest_neighbour_search_nodes nearest_neighbour_search_root nearest_neighbour_search_query_point]
  (binding [nearest_neighbour_search_initial nil] (try (do (set! nearest_neighbour_search_initial {:distance 1000000000000000000000000000000.0 :nodes_visited 0 :point []}) (throw (ex-info "return" {:v (search nearest_neighbour_search_nodes nearest_neighbour_search_root nearest_neighbour_search_query_point 0 nearest_neighbour_search_initial)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_nodes [{:left 1 :point [9.0 1.0] :right 4} {:left 2 :point [2.0 7.0] :right 3} {:left (- 1) :point [3.0 6.0] :right (- 1)} {:left (- 1) :point [6.0 12.0] :right (- 1)} {:left 5 :point [17.0 15.0] :right 6} {:left (- 1) :point [13.0 15.0] :right (- 1)} {:left (- 1) :point [10.0 19.0] :right (- 1)}])

(def ^:dynamic main_queries [[9.0 2.0] [12.0 15.0] [1.0 3.0]])

(def ^:dynamic main_q 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_q (count main_queries)) (do (def ^:dynamic main_res (nearest_neighbour_search main_nodes 0 (nth main_queries main_q))) (println (str (str (str (str (str (str (:point main_res)) " ") (str (:distance main_res))) " ") (str (:nodes_visited main_res))) "\n")) (def main_q (+ main_q 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
