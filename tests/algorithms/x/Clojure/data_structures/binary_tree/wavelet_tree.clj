(ns main (:refer-clojure :exclude [make_list min_list max_list build_tree rank_till_index rank quantile range_counting]))

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

(declare make_list min_list max_list build_tree rank_till_index rank quantile range_counting)

(def ^:dynamic build_tree_i nil)

(def ^:dynamic build_tree_left_arr nil)

(def ^:dynamic build_tree_ml nil)

(def ^:dynamic build_tree_n nil)

(def ^:dynamic build_tree_num nil)

(def ^:dynamic build_tree_pivot nil)

(def ^:dynamic build_tree_right_arr nil)

(def ^:dynamic make_list_i nil)

(def ^:dynamic make_list_lst nil)

(def ^:dynamic max_list_i nil)

(def ^:dynamic max_list_m nil)

(def ^:dynamic min_list_i nil)

(def ^:dynamic min_list_m nil)

(def ^:dynamic quantile_left_start nil)

(def ^:dynamic quantile_node nil)

(def ^:dynamic quantile_num_left nil)

(def ^:dynamic range_counting_left nil)

(def ^:dynamic range_counting_node nil)

(def ^:dynamic range_counting_right nil)

(def ^:dynamic rank_rank_before_start nil)

(def ^:dynamic rank_rank_till_end nil)

(def ^:dynamic rank_till_index_node nil)

(def ^:dynamic rank_till_index_pivot nil)

(def ^:dynamic main_nodes [])

(defn make_list [make_list_length make_list_value]
  (binding [make_list_i nil make_list_lst nil] (try (do (set! make_list_lst []) (set! make_list_i 0) (while (< make_list_i make_list_length) (do (set! make_list_lst (conj make_list_lst make_list_value)) (set! make_list_i (+ make_list_i 1)))) (throw (ex-info "return" {:v make_list_lst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn min_list [min_list_arr]
  (binding [min_list_i nil min_list_m nil] (try (do (set! min_list_m (nth min_list_arr 0)) (set! min_list_i 1) (while (< min_list_i (count min_list_arr)) (do (when (< (nth min_list_arr min_list_i) min_list_m) (set! min_list_m (nth min_list_arr min_list_i))) (set! min_list_i (+ min_list_i 1)))) (throw (ex-info "return" {:v min_list_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_list [max_list_arr]
  (binding [max_list_i nil max_list_m nil] (try (do (set! max_list_m (nth max_list_arr 0)) (set! max_list_i 1) (while (< max_list_i (count max_list_arr)) (do (when (> (nth max_list_arr max_list_i) max_list_m) (set! max_list_m (nth max_list_arr max_list_i))) (set! max_list_i (+ max_list_i 1)))) (throw (ex-info "return" {:v max_list_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn build_tree [build_tree_arr]
  (binding [build_tree_i nil build_tree_left_arr nil build_tree_ml nil build_tree_n nil build_tree_num nil build_tree_pivot nil build_tree_right_arr nil] (try (do (set! build_tree_n {:minn (min_list build_tree_arr) :maxx (max_list build_tree_arr) :map_left (make_list (count build_tree_arr) 0) :left (- 1) :right (- 1)}) (when (= (:minn build_tree_n) (:maxx build_tree_n)) (do (alter-var-root (var main_nodes) (fn [_] (conj main_nodes build_tree_n))) (throw (ex-info "return" {:v (- (count main_nodes) 1)})))) (set! build_tree_pivot (quot (+ (:minn build_tree_n) (:maxx build_tree_n)) 2)) (set! build_tree_left_arr []) (set! build_tree_right_arr []) (set! build_tree_i 0) (while (< build_tree_i (count build_tree_arr)) (do (set! build_tree_num (nth build_tree_arr build_tree_i)) (if (<= build_tree_num build_tree_pivot) (set! build_tree_left_arr (conj build_tree_left_arr build_tree_num)) (set! build_tree_right_arr (conj build_tree_right_arr build_tree_num))) (set! build_tree_ml (:map_left build_tree_n)) (set! build_tree_ml (assoc build_tree_ml build_tree_i (count build_tree_left_arr))) (set! build_tree_n (assoc build_tree_n :map_left build_tree_ml)) (set! build_tree_i (+ build_tree_i 1)))) (when (> (count build_tree_left_arr) 0) (set! build_tree_n (assoc build_tree_n :left (build_tree build_tree_left_arr)))) (when (> (count build_tree_right_arr) 0) (set! build_tree_n (assoc build_tree_n :right (build_tree build_tree_right_arr)))) (alter-var-root (var main_nodes) (fn [_] (conj main_nodes build_tree_n))) (throw (ex-info "return" {:v (- (count main_nodes) 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rank_till_index [rank_till_index_node_idx rank_till_index_num rank_till_index_index]
  (binding [rank_till_index_node nil rank_till_index_pivot nil] (try (do (when (or (< rank_till_index_index 0) (< rank_till_index_node_idx 0)) (throw (ex-info "return" {:v 0}))) (set! rank_till_index_node (nth main_nodes rank_till_index_node_idx)) (when (= (:minn rank_till_index_node) (:maxx rank_till_index_node)) (if (= (:minn rank_till_index_node) rank_till_index_num) (throw (ex-info "return" {:v (+ rank_till_index_index 1)})) (throw (ex-info "return" {:v 0})))) (set! rank_till_index_pivot (quot (+ (:minn rank_till_index_node) (:maxx rank_till_index_node)) 2)) (if (<= rank_till_index_num rank_till_index_pivot) (throw (ex-info "return" {:v (rank_till_index (:left rank_till_index_node) rank_till_index_num (- (get (:map_left rank_till_index_node) rank_till_index_index) 1))})) (throw (ex-info "return" {:v (rank_till_index (:right rank_till_index_node) rank_till_index_num (- rank_till_index_index (get (:map_left rank_till_index_node) rank_till_index_index)))})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rank [rank_node_idx rank_num rank_start rank_end]
  (binding [rank_rank_before_start nil rank_rank_till_end nil] (try (do (when (> rank_start rank_end) (throw (ex-info "return" {:v 0}))) (set! rank_rank_till_end (rank_till_index rank_node_idx rank_num rank_end)) (set! rank_rank_before_start (rank_till_index rank_node_idx rank_num (- rank_start 1))) (throw (ex-info "return" {:v (- rank_rank_till_end rank_rank_before_start)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn quantile [quantile_node_idx quantile_index quantile_start quantile_end]
  (binding [quantile_left_start nil quantile_node nil quantile_num_left nil] (try (do (when (or (or (> quantile_index (- quantile_end quantile_start)) (> quantile_start quantile_end)) (< quantile_node_idx 0)) (throw (ex-info "return" {:v (- 1)}))) (set! quantile_node (nth main_nodes quantile_node_idx)) (when (= (:minn quantile_node) (:maxx quantile_node)) (throw (ex-info "return" {:v (:minn quantile_node)}))) (set! quantile_left_start (if (= quantile_start 0) 0 (get (:map_left quantile_node) (- quantile_start 1)))) (set! quantile_num_left (- (get (:map_left quantile_node) quantile_end) quantile_left_start)) (if (> quantile_num_left quantile_index) (throw (ex-info "return" {:v (quantile (:left quantile_node) quantile_index quantile_left_start (- (get (:map_left quantile_node) quantile_end) 1))})) (throw (ex-info "return" {:v (quantile (:right quantile_node) (- quantile_index quantile_num_left) (- quantile_start quantile_left_start) (- quantile_end (get (:map_left quantile_node) quantile_end)))})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn range_counting [range_counting_node_idx range_counting_start range_counting_end range_counting_start_num range_counting_end_num]
  (binding [range_counting_left nil range_counting_node nil range_counting_right nil] (try (do (when (or (or (> range_counting_start range_counting_end) (< range_counting_node_idx 0)) (> range_counting_start_num range_counting_end_num)) (throw (ex-info "return" {:v 0}))) (set! range_counting_node (nth main_nodes range_counting_node_idx)) (when (or (> (:minn range_counting_node) range_counting_end_num) (< (:maxx range_counting_node) range_counting_start_num)) (throw (ex-info "return" {:v 0}))) (when (and (<= range_counting_start_num (:minn range_counting_node)) (<= (:maxx range_counting_node) range_counting_end_num)) (throw (ex-info "return" {:v (+ (- range_counting_end range_counting_start) 1)}))) (set! range_counting_left (range_counting (:left range_counting_node) (if (= range_counting_start 0) 0 (get (:map_left range_counting_node) (- range_counting_start 1))) (- (get (:map_left range_counting_node) range_counting_end) 1) range_counting_start_num range_counting_end_num)) (set! range_counting_right (range_counting (:right range_counting_node) (- range_counting_start (if (= range_counting_start 0) 0 (get (:map_left range_counting_node) (- range_counting_start 1)))) (- range_counting_end (get (:map_left range_counting_node) range_counting_end)) range_counting_start_num range_counting_end_num)) (throw (ex-info "return" {:v (+ range_counting_left range_counting_right)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_test_array [2 1 4 5 6 0 8 9 1 2 0 6 4 2 0 6 5 3 2 7])

(def ^:dynamic main_root (build_tree main_test_array))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "rank_till_index 6 at 6 -> " (str (rank_till_index main_root 6 6))))
      (println (str "rank 6 in [3,13] -> " (str (rank main_root 6 3 13))))
      (println (str "quantile index 2 in [2,5] -> " (str (quantile main_root 2 2 5))))
      (println (str "range_counting [3,7] in [1,10] -> " (str (range_counting main_root 1 10 3 7))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
