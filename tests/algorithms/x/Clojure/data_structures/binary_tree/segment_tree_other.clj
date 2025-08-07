(ns main (:refer-clojure :exclude [combine build_tree new_segment_tree update query_range traverse node_to_string print_traverse]))

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

(declare combine build_tree new_segment_tree update query_range traverse node_to_string print_traverse)

(def ^:dynamic build_tree_left_node nil)

(def ^:dynamic build_tree_left_res nil)

(def ^:dynamic build_tree_mid nil)

(def ^:dynamic build_tree_new_nodes nil)

(def ^:dynamic build_tree_node nil)

(def ^:dynamic build_tree_parent nil)

(def ^:dynamic build_tree_right_node nil)

(def ^:dynamic build_tree_right_res nil)

(def ^:dynamic build_tree_val nil)

(def ^:dynamic main_tree nil)

(def ^:dynamic print_traverse_i nil)

(def ^:dynamic print_traverse_nodes nil)

(def ^:dynamic query_range_idx nil)

(def ^:dynamic query_range_result nil)

(def ^:dynamic traverse_res nil)

(def ^:dynamic update_idx nil)

(def ^:dynamic update_new_arr nil)

(defn combine [combine_a combine_b combine_op]
  (try (do (when (= combine_op 0) (throw (ex-info "return" {:v (+ combine_a combine_b)}))) (when (= combine_op 1) (do (when (> combine_a combine_b) (throw (ex-info "return" {:v combine_a}))) (throw (ex-info "return" {:v combine_b})))) (if (< combine_a combine_b) combine_a combine_b)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn build_tree [build_tree_nodes build_tree_arr build_tree_start build_tree_end build_tree_op]
  (binding [build_tree_left_node nil build_tree_left_res nil build_tree_mid nil build_tree_new_nodes nil build_tree_node nil build_tree_parent nil build_tree_right_node nil build_tree_right_res nil build_tree_val nil] (try (do (when (= build_tree_start build_tree_end) (do (set! build_tree_node {:start build_tree_start :end build_tree_end :val (nth build_tree_arr build_tree_start) :mid build_tree_start :left (- 1) :right (- 1)}) (set! build_tree_new_nodes (conj build_tree_nodes build_tree_node)) (throw (ex-info "return" {:v {:nodes build_tree_new_nodes :idx (- (count build_tree_new_nodes) 1)}})))) (set! build_tree_mid (quot (+ build_tree_start build_tree_end) 2)) (set! build_tree_left_res (build_tree build_tree_nodes build_tree_arr build_tree_start build_tree_mid build_tree_op)) (set! build_tree_right_res (build_tree (:nodes build_tree_left_res) build_tree_arr (+ build_tree_mid 1) build_tree_end build_tree_op)) (set! build_tree_left_node (get (:nodes build_tree_right_res) (:idx build_tree_left_res))) (set! build_tree_right_node (get (:nodes build_tree_right_res) (:idx build_tree_right_res))) (set! build_tree_val (combine (:val build_tree_left_node) (:val build_tree_right_node) build_tree_op)) (set! build_tree_parent {:start build_tree_start :end build_tree_end :val build_tree_val :mid build_tree_mid :left (:idx build_tree_left_res) :right (:idx build_tree_right_res)}) (set! build_tree_new_nodes (conj (:nodes build_tree_right_res) build_tree_parent)) (throw (ex-info "return" {:v {:nodes build_tree_new_nodes :idx (- (count build_tree_new_nodes) 1)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn new_segment_tree [new_segment_tree_collection new_segment_tree_op]
  (try (throw (ex-info "return" {:v {:arr new_segment_tree_collection :op new_segment_tree_op}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn update [update_tree update_i update_val]
  (binding [update_idx nil update_new_arr nil] (try (do (set! update_new_arr []) (set! update_idx 0) (while (< update_idx (count (:arr update_tree))) (do (if (= update_idx update_i) (set! update_new_arr (conj update_new_arr update_val)) (set! update_new_arr (conj update_new_arr (get (:arr update_tree) update_idx)))) (set! update_idx (+ update_idx 1)))) (throw (ex-info "return" {:v {:arr update_new_arr :op (:op update_tree)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn query_range [query_range_tree query_range_i query_range_j]
  (binding [query_range_idx nil query_range_result nil] (try (do (set! query_range_result (get (:arr query_range_tree) query_range_i)) (set! query_range_idx (+ query_range_i 1)) (while (<= query_range_idx query_range_j) (do (set! query_range_result (combine query_range_result (get (:arr query_range_tree) query_range_idx) (:op query_range_tree))) (set! query_range_idx (+ query_range_idx 1)))) (throw (ex-info "return" {:v query_range_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn traverse [traverse_tree]
  (binding [traverse_res nil] (try (do (when (= (count (:arr traverse_tree)) 0) (throw (ex-info "return" {:v []}))) (set! traverse_res (build_tree [] (:arr traverse_tree) 0 (- (count (:arr traverse_tree)) 1) (:op traverse_tree))) (throw (ex-info "return" {:v (:nodes traverse_res)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn node_to_string [node_to_string_node]
  (try (throw (ex-info "return" {:v (str (str (str (str (str (str "SegmentTreeNode(start=" (str (:start node_to_string_node))) ", end=") (str (:end node_to_string_node))) ", val=") (str (:val node_to_string_node))) ")")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn print_traverse [print_traverse_tree]
  (binding [print_traverse_i nil print_traverse_nodes nil] (do (set! print_traverse_nodes (traverse print_traverse_tree)) (set! print_traverse_i 0) (while (< print_traverse_i (count print_traverse_nodes)) (do (println (node_to_string (nth print_traverse_nodes print_traverse_i))) (set! print_traverse_i (+ print_traverse_i 1)))) (println ""))))

(def ^:dynamic main_arr [2 1 5 3 4])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (doseq [op [0 1 2]] (do (println "**************************************************") (def ^:dynamic main_tree (new_segment_tree main_arr op)) (print_traverse main_tree) (def main_tree (update main_tree 1 5)) (print_traverse main_tree) (println (query_range main_tree 3 4)) (println (query_range main_tree 2 2)) (println (query_range main_tree 1 3)) (println "")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
