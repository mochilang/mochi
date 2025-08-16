(ns main (:refer-clojure :exclude [push_relabel_max_flow]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare push_relabel_max_flow)

(def ^:dynamic push_relabel_max_flow_bandwidth nil)

(def ^:dynamic push_relabel_max_flow_c nil)

(def ^:dynamic push_relabel_max_flow_capacity nil)

(def ^:dynamic push_relabel_max_flow_delta nil)

(def ^:dynamic push_relabel_max_flow_excesses nil)

(def ^:dynamic push_relabel_max_flow_flow nil)

(def ^:dynamic push_relabel_max_flow_g nil)

(def ^:dynamic push_relabel_max_flow_heights nil)

(def ^:dynamic push_relabel_max_flow_i nil)

(def ^:dynamic push_relabel_max_flow_idx nil)

(def ^:dynamic push_relabel_max_flow_j nil)

(def ^:dynamic push_relabel_max_flow_last_row nil)

(def ^:dynamic push_relabel_max_flow_max_input_flow nil)

(def ^:dynamic push_relabel_max_flow_min_height nil)

(def ^:dynamic push_relabel_max_flow_n nil)

(def ^:dynamic push_relabel_max_flow_nb nil)

(def ^:dynamic push_relabel_max_flow_new_graph nil)

(def ^:dynamic push_relabel_max_flow_preflow nil)

(def ^:dynamic push_relabel_max_flow_prev_height nil)

(def ^:dynamic push_relabel_max_flow_r nil)

(def ^:dynamic push_relabel_max_flow_row nil)

(def ^:dynamic push_relabel_max_flow_row2 nil)

(def ^:dynamic push_relabel_max_flow_sink_index nil)

(def ^:dynamic push_relabel_max_flow_size nil)

(def ^:dynamic push_relabel_max_flow_source_index nil)

(def ^:dynamic push_relabel_max_flow_v nil)

(def ^:dynamic push_relabel_max_flow_vertex nil)

(def ^:dynamic push_relabel_max_flow_vertices_list nil)

(def ^:dynamic push_relabel_max_flow_zero_row nil)

(defn push_relabel_max_flow [push_relabel_max_flow_graph push_relabel_max_flow_sources push_relabel_max_flow_sinks]
  (binding [push_relabel_max_flow_bandwidth nil push_relabel_max_flow_c nil push_relabel_max_flow_capacity nil push_relabel_max_flow_delta nil push_relabel_max_flow_excesses nil push_relabel_max_flow_flow nil push_relabel_max_flow_g nil push_relabel_max_flow_heights nil push_relabel_max_flow_i nil push_relabel_max_flow_idx nil push_relabel_max_flow_j nil push_relabel_max_flow_last_row nil push_relabel_max_flow_max_input_flow nil push_relabel_max_flow_min_height nil push_relabel_max_flow_n nil push_relabel_max_flow_nb nil push_relabel_max_flow_new_graph nil push_relabel_max_flow_preflow nil push_relabel_max_flow_prev_height nil push_relabel_max_flow_r nil push_relabel_max_flow_row nil push_relabel_max_flow_row2 nil push_relabel_max_flow_sink_index nil push_relabel_max_flow_size nil push_relabel_max_flow_source_index nil push_relabel_max_flow_v nil push_relabel_max_flow_vertex nil push_relabel_max_flow_vertices_list nil push_relabel_max_flow_zero_row nil] (try (do (when (or (= (count push_relabel_max_flow_sources) 0) (= (count push_relabel_max_flow_sinks) 0)) (throw (ex-info "return" {:v 0}))) (set! push_relabel_max_flow_g push_relabel_max_flow_graph) (set! push_relabel_max_flow_source_index (nth push_relabel_max_flow_sources 0)) (set! push_relabel_max_flow_sink_index (nth push_relabel_max_flow_sinks 0)) (when (or (> (count push_relabel_max_flow_sources) 1) (> (count push_relabel_max_flow_sinks) 1)) (do (set! push_relabel_max_flow_max_input_flow 0) (set! push_relabel_max_flow_i 0) (while (< push_relabel_max_flow_i (count push_relabel_max_flow_sources)) (do (set! push_relabel_max_flow_j 0) (while (< push_relabel_max_flow_j (count (nth push_relabel_max_flow_g (nth push_relabel_max_flow_sources push_relabel_max_flow_i)))) (do (set! push_relabel_max_flow_max_input_flow (+ push_relabel_max_flow_max_input_flow (nth (nth push_relabel_max_flow_g (nth push_relabel_max_flow_sources push_relabel_max_flow_i)) push_relabel_max_flow_j))) (set! push_relabel_max_flow_j (+ push_relabel_max_flow_j 1)))) (set! push_relabel_max_flow_i (+ push_relabel_max_flow_i 1)))) (set! push_relabel_max_flow_size (+ (count push_relabel_max_flow_g) 1)) (set! push_relabel_max_flow_new_graph []) (set! push_relabel_max_flow_zero_row []) (set! push_relabel_max_flow_j 0) (while (< push_relabel_max_flow_j push_relabel_max_flow_size) (do (set! push_relabel_max_flow_zero_row (conj push_relabel_max_flow_zero_row 0)) (set! push_relabel_max_flow_j (+ push_relabel_max_flow_j 1)))) (set! push_relabel_max_flow_new_graph (conj push_relabel_max_flow_new_graph push_relabel_max_flow_zero_row)) (set! push_relabel_max_flow_r 0) (while (< push_relabel_max_flow_r (count push_relabel_max_flow_g)) (do (set! push_relabel_max_flow_row [0]) (set! push_relabel_max_flow_c 0) (while (< push_relabel_max_flow_c (count (nth push_relabel_max_flow_g push_relabel_max_flow_r))) (do (set! push_relabel_max_flow_row (conj push_relabel_max_flow_row (nth (nth push_relabel_max_flow_g push_relabel_max_flow_r) push_relabel_max_flow_c))) (set! push_relabel_max_flow_c (+ push_relabel_max_flow_c 1)))) (set! push_relabel_max_flow_new_graph (conj push_relabel_max_flow_new_graph push_relabel_max_flow_row)) (set! push_relabel_max_flow_r (+ push_relabel_max_flow_r 1)))) (set! push_relabel_max_flow_g push_relabel_max_flow_new_graph) (set! push_relabel_max_flow_i 0) (while (< push_relabel_max_flow_i (count push_relabel_max_flow_sources)) (do (set! push_relabel_max_flow_g (assoc-in push_relabel_max_flow_g [0 (+ (nth push_relabel_max_flow_sources push_relabel_max_flow_i) 1)] push_relabel_max_flow_max_input_flow)) (set! push_relabel_max_flow_i (+ push_relabel_max_flow_i 1)))) (set! push_relabel_max_flow_source_index 0) (set! push_relabel_max_flow_size (+ (count push_relabel_max_flow_g) 1)) (set! push_relabel_max_flow_new_graph []) (set! push_relabel_max_flow_r 0) (while (< push_relabel_max_flow_r (count push_relabel_max_flow_g)) (do (set! push_relabel_max_flow_row2 (nth push_relabel_max_flow_g push_relabel_max_flow_r)) (set! push_relabel_max_flow_row2 (conj push_relabel_max_flow_row2 0)) (set! push_relabel_max_flow_new_graph (conj push_relabel_max_flow_new_graph push_relabel_max_flow_row2)) (set! push_relabel_max_flow_r (+ push_relabel_max_flow_r 1)))) (set! push_relabel_max_flow_last_row []) (set! push_relabel_max_flow_j 0) (while (< push_relabel_max_flow_j push_relabel_max_flow_size) (do (set! push_relabel_max_flow_last_row (conj push_relabel_max_flow_last_row 0)) (set! push_relabel_max_flow_j (+ push_relabel_max_flow_j 1)))) (set! push_relabel_max_flow_new_graph (conj push_relabel_max_flow_new_graph push_relabel_max_flow_last_row)) (set! push_relabel_max_flow_g push_relabel_max_flow_new_graph) (set! push_relabel_max_flow_i 0) (while (< push_relabel_max_flow_i (count push_relabel_max_flow_sinks)) (do (set! push_relabel_max_flow_g (assoc-in push_relabel_max_flow_g [(+ (nth push_relabel_max_flow_sinks push_relabel_max_flow_i) 1) (- push_relabel_max_flow_size 1)] push_relabel_max_flow_max_input_flow)) (set! push_relabel_max_flow_i (+ push_relabel_max_flow_i 1)))) (set! push_relabel_max_flow_sink_index (- push_relabel_max_flow_size 1)))) (set! push_relabel_max_flow_n (count push_relabel_max_flow_g)) (set! push_relabel_max_flow_preflow []) (set! push_relabel_max_flow_i 0) (while (< push_relabel_max_flow_i push_relabel_max_flow_n) (do (set! push_relabel_max_flow_row []) (set! push_relabel_max_flow_j 0) (while (< push_relabel_max_flow_j push_relabel_max_flow_n) (do (set! push_relabel_max_flow_row (conj push_relabel_max_flow_row 0)) (set! push_relabel_max_flow_j (+ push_relabel_max_flow_j 1)))) (set! push_relabel_max_flow_preflow (conj push_relabel_max_flow_preflow push_relabel_max_flow_row)) (set! push_relabel_max_flow_i (+ push_relabel_max_flow_i 1)))) (set! push_relabel_max_flow_heights []) (set! push_relabel_max_flow_i 0) (while (< push_relabel_max_flow_i push_relabel_max_flow_n) (do (set! push_relabel_max_flow_heights (conj push_relabel_max_flow_heights 0)) (set! push_relabel_max_flow_i (+ push_relabel_max_flow_i 1)))) (set! push_relabel_max_flow_excesses []) (set! push_relabel_max_flow_i 0) (while (< push_relabel_max_flow_i push_relabel_max_flow_n) (do (set! push_relabel_max_flow_excesses (conj push_relabel_max_flow_excesses 0)) (set! push_relabel_max_flow_i (+ push_relabel_max_flow_i 1)))) (set! push_relabel_max_flow_heights (assoc push_relabel_max_flow_heights push_relabel_max_flow_source_index push_relabel_max_flow_n)) (set! push_relabel_max_flow_i 0) (while (< push_relabel_max_flow_i push_relabel_max_flow_n) (do (set! push_relabel_max_flow_bandwidth (nth (nth push_relabel_max_flow_g push_relabel_max_flow_source_index) push_relabel_max_flow_i)) (set! push_relabel_max_flow_preflow (assoc-in push_relabel_max_flow_preflow [push_relabel_max_flow_source_index push_relabel_max_flow_i] (+ (nth (nth push_relabel_max_flow_preflow push_relabel_max_flow_source_index) push_relabel_max_flow_i) push_relabel_max_flow_bandwidth))) (set! push_relabel_max_flow_preflow (assoc-in push_relabel_max_flow_preflow [push_relabel_max_flow_i push_relabel_max_flow_source_index] (- (nth (nth push_relabel_max_flow_preflow push_relabel_max_flow_i) push_relabel_max_flow_source_index) push_relabel_max_flow_bandwidth))) (set! push_relabel_max_flow_excesses (assoc push_relabel_max_flow_excesses push_relabel_max_flow_i (+ (nth push_relabel_max_flow_excesses push_relabel_max_flow_i) push_relabel_max_flow_bandwidth))) (set! push_relabel_max_flow_i (+ push_relabel_max_flow_i 1)))) (set! push_relabel_max_flow_vertices_list []) (set! push_relabel_max_flow_i 0) (while (< push_relabel_max_flow_i push_relabel_max_flow_n) (do (when (and (not= push_relabel_max_flow_i push_relabel_max_flow_source_index) (not= push_relabel_max_flow_i push_relabel_max_flow_sink_index)) (set! push_relabel_max_flow_vertices_list (conj push_relabel_max_flow_vertices_list push_relabel_max_flow_i))) (set! push_relabel_max_flow_i (+ push_relabel_max_flow_i 1)))) (set! push_relabel_max_flow_idx 0) (loop [while_flag_1 true] (when (and while_flag_1 (< push_relabel_max_flow_idx (count push_relabel_max_flow_vertices_list))) (do (set! push_relabel_max_flow_v (nth push_relabel_max_flow_vertices_list push_relabel_max_flow_idx)) (set! push_relabel_max_flow_prev_height (nth push_relabel_max_flow_heights push_relabel_max_flow_v)) (loop [while_flag_2 true] (when (and while_flag_2 (> (nth push_relabel_max_flow_excesses push_relabel_max_flow_v) 0)) (do (set! push_relabel_max_flow_nb 0) (while (< push_relabel_max_flow_nb push_relabel_max_flow_n) (do (when (and (> (- (nth (nth push_relabel_max_flow_g push_relabel_max_flow_v) push_relabel_max_flow_nb) (nth (nth push_relabel_max_flow_preflow push_relabel_max_flow_v) push_relabel_max_flow_nb)) 0) (> (nth push_relabel_max_flow_heights push_relabel_max_flow_v) (nth push_relabel_max_flow_heights push_relabel_max_flow_nb))) (do (set! push_relabel_max_flow_delta (nth push_relabel_max_flow_excesses push_relabel_max_flow_v)) (set! push_relabel_max_flow_capacity (- (nth (nth push_relabel_max_flow_g push_relabel_max_flow_v) push_relabel_max_flow_nb) (nth (nth push_relabel_max_flow_preflow push_relabel_max_flow_v) push_relabel_max_flow_nb))) (when (> push_relabel_max_flow_delta push_relabel_max_flow_capacity) (set! push_relabel_max_flow_delta push_relabel_max_flow_capacity)) (set! push_relabel_max_flow_preflow (assoc-in push_relabel_max_flow_preflow [push_relabel_max_flow_v push_relabel_max_flow_nb] (+ (nth (nth push_relabel_max_flow_preflow push_relabel_max_flow_v) push_relabel_max_flow_nb) push_relabel_max_flow_delta))) (set! push_relabel_max_flow_preflow (assoc-in push_relabel_max_flow_preflow [push_relabel_max_flow_nb push_relabel_max_flow_v] (- (nth (nth push_relabel_max_flow_preflow push_relabel_max_flow_nb) push_relabel_max_flow_v) push_relabel_max_flow_delta))) (set! push_relabel_max_flow_excesses (assoc push_relabel_max_flow_excesses push_relabel_max_flow_v (- (nth push_relabel_max_flow_excesses push_relabel_max_flow_v) push_relabel_max_flow_delta))) (set! push_relabel_max_flow_excesses (assoc push_relabel_max_flow_excesses push_relabel_max_flow_nb (+ (nth push_relabel_max_flow_excesses push_relabel_max_flow_nb) push_relabel_max_flow_delta))))) (set! push_relabel_max_flow_nb (+ push_relabel_max_flow_nb 1)))) (set! push_relabel_max_flow_min_height (- 1)) (set! push_relabel_max_flow_nb 0) (while (< push_relabel_max_flow_nb push_relabel_max_flow_n) (do (when (> (- (nth (nth push_relabel_max_flow_g push_relabel_max_flow_v) push_relabel_max_flow_nb) (nth (nth push_relabel_max_flow_preflow push_relabel_max_flow_v) push_relabel_max_flow_nb)) 0) (when (or (= push_relabel_max_flow_min_height (- 1)) (< (nth push_relabel_max_flow_heights push_relabel_max_flow_nb) push_relabel_max_flow_min_height)) (set! push_relabel_max_flow_min_height (nth push_relabel_max_flow_heights push_relabel_max_flow_nb)))) (set! push_relabel_max_flow_nb (+ push_relabel_max_flow_nb 1)))) (if (not= push_relabel_max_flow_min_height (- 1)) (do (set! push_relabel_max_flow_heights (assoc push_relabel_max_flow_heights push_relabel_max_flow_v (+ push_relabel_max_flow_min_height 1))) (recur while_flag_2)) (recur false))))) (if (> (nth push_relabel_max_flow_heights push_relabel_max_flow_v) push_relabel_max_flow_prev_height) (do (set! push_relabel_max_flow_vertex (nth push_relabel_max_flow_vertices_list push_relabel_max_flow_idx)) (set! push_relabel_max_flow_j push_relabel_max_flow_idx) (while (> push_relabel_max_flow_j 0) (do (set! push_relabel_max_flow_vertices_list (assoc push_relabel_max_flow_vertices_list push_relabel_max_flow_j (nth push_relabel_max_flow_vertices_list (- push_relabel_max_flow_j 1)))) (set! push_relabel_max_flow_j (- push_relabel_max_flow_j 1)))) (set! push_relabel_max_flow_vertices_list (assoc push_relabel_max_flow_vertices_list 0 push_relabel_max_flow_vertex)) (set! push_relabel_max_flow_idx 0)) (set! push_relabel_max_flow_idx (+ push_relabel_max_flow_idx 1))) (cond :else (recur while_flag_1))))) (set! push_relabel_max_flow_flow 0) (set! push_relabel_max_flow_i 0) (while (< push_relabel_max_flow_i push_relabel_max_flow_n) (do (set! push_relabel_max_flow_flow (+ push_relabel_max_flow_flow (nth (nth push_relabel_max_flow_preflow push_relabel_max_flow_source_index) push_relabel_max_flow_i))) (set! push_relabel_max_flow_i (+ push_relabel_max_flow_i 1)))) (when (< push_relabel_max_flow_flow 0) (set! push_relabel_max_flow_flow (- push_relabel_max_flow_flow))) (throw (ex-info "return" {:v push_relabel_max_flow_flow}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_graph nil)

(def ^:dynamic main_sources nil)

(def ^:dynamic main_sinks nil)

(def ^:dynamic main_result nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_graph) (constantly [[0 7 0 0] [0 0 6 0] [0 0 0 8] [9 0 0 0]]))
      (alter-var-root (var main_sources) (constantly [0]))
      (alter-var-root (var main_sinks) (constantly [3]))
      (alter-var-root (var main_result) (constantly (push_relabel_max_flow main_graph main_sources main_sinks)))
      (println (str "maximum flow is " (str main_result)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
