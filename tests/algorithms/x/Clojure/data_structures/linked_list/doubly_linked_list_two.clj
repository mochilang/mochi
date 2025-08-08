(ns main (:refer-clojure :exclude [empty_list get_head_data get_tail_data insert_before_node insert_after_node set_head set_tail insert insert_at_position get_node remove_node_pointers delete_value contains is_empty to_string print_list main]))

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

(declare empty_list get_head_data get_tail_data insert_before_node insert_after_node set_head set_tail insert insert_at_position get_node remove_node_pointers delete_value contains is_empty to_string print_list main)

(def ^:dynamic delete_value_idx nil)

(def ^:dynamic delete_value_ll nil)

(def ^:dynamic delete_value_node nil)

(def ^:dynamic first_v nil)

(def ^:dynamic get_head_data_node nil)

(def ^:dynamic get_node_current nil)

(def ^:dynamic get_node_node nil)

(def ^:dynamic get_tail_data_node nil)

(def ^:dynamic insert_after_node_ll nil)

(def ^:dynamic insert_after_node_new_node nil)

(def ^:dynamic insert_after_node_next_node nil)

(def ^:dynamic insert_after_node_node nil)

(def ^:dynamic insert_after_node_nodes nil)

(def ^:dynamic insert_after_node_nxt nil)

(def ^:dynamic insert_at_position_current nil)

(def ^:dynamic insert_at_position_current_pos nil)

(def ^:dynamic insert_at_position_ll nil)

(def ^:dynamic insert_at_position_new_idx nil)

(def ^:dynamic insert_at_position_node nil)

(def ^:dynamic insert_at_position_nodes nil)

(def ^:dynamic insert_before_node_ll nil)

(def ^:dynamic insert_before_node_new_node nil)

(def ^:dynamic insert_before_node_node nil)

(def ^:dynamic insert_before_node_nodes nil)

(def ^:dynamic insert_before_node_p nil)

(def ^:dynamic insert_before_node_prev_node nil)

(def ^:dynamic insert_idx nil)

(def ^:dynamic insert_ll nil)

(def ^:dynamic insert_nodes nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_idx_head nil)

(def ^:dynamic main_idx_tail nil)

(def ^:dynamic main_ll nil)

(def ^:dynamic main_ll2 nil)

(def ^:dynamic main_nodes nil)

(def ^:dynamic print_list_current nil)

(def ^:dynamic print_list_node nil)

(def ^:dynamic remove_node_pointers_ll nil)

(def ^:dynamic remove_node_pointers_node nil)

(def ^:dynamic remove_node_pointers_nodes nil)

(def ^:dynamic remove_node_pointers_nxt nil)

(def ^:dynamic remove_node_pointers_nxt_node nil)

(def ^:dynamic remove_node_pointers_p nil)

(def ^:dynamic remove_node_pointers_prev_node nil)

(def ^:dynamic set_head_ll nil)

(def ^:dynamic set_tail_ll nil)

(def ^:dynamic to_string_current nil)

(def ^:dynamic to_string_node nil)

(def ^:dynamic to_string_res nil)

(def ^:dynamic to_string_val nil)

(defn empty_list []
  (try (throw (ex-info "return" {:v {:head_idx (- 1) :nodes [] :tail_idx (- 1)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_head_data [get_head_data_ll]
  (binding [get_head_data_node nil] (try (do (when (= (:head_idx get_head_data_ll) (- 1)) (throw (ex-info "return" {:v (- 1)}))) (set! get_head_data_node (get (:nodes get_head_data_ll) (:head_idx get_head_data_ll))) (throw (ex-info "return" {:v (:data get_head_data_node)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_tail_data [get_tail_data_ll]
  (binding [get_tail_data_node nil] (try (do (when (= (:tail_idx get_tail_data_ll) (- 1)) (throw (ex-info "return" {:v (- 1)}))) (set! get_tail_data_node (get (:nodes get_tail_data_ll) (:tail_idx get_tail_data_ll))) (throw (ex-info "return" {:v (:data get_tail_data_node)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert_before_node [insert_before_node_ll_p insert_before_node_idx insert_before_node_new_idx]
  (binding [insert_before_node_ll nil insert_before_node_new_node nil insert_before_node_node nil insert_before_node_nodes nil insert_before_node_p nil insert_before_node_prev_node nil] (do (set! insert_before_node_ll insert_before_node_ll_p) (set! insert_before_node_nodes (:nodes insert_before_node_ll)) (set! insert_before_node_new_node (nth insert_before_node_nodes insert_before_node_new_idx)) (set! insert_before_node_new_node (assoc insert_before_node_new_node :next_index insert_before_node_idx)) (set! insert_before_node_node (nth insert_before_node_nodes insert_before_node_idx)) (set! insert_before_node_p (:prev_index insert_before_node_node)) (set! insert_before_node_new_node (assoc insert_before_node_new_node :prev_index insert_before_node_p)) (set! insert_before_node_nodes (assoc insert_before_node_nodes insert_before_node_new_idx insert_before_node_new_node)) (if (= insert_before_node_p (- 1)) (set! insert_before_node_ll (assoc insert_before_node_ll :head_idx insert_before_node_new_idx)) (do (set! insert_before_node_prev_node (nth insert_before_node_nodes insert_before_node_p)) (set! insert_before_node_prev_node (assoc insert_before_node_prev_node :next_index insert_before_node_new_idx)) (set! insert_before_node_nodes (assoc insert_before_node_nodes insert_before_node_p insert_before_node_prev_node)))) (set! insert_before_node_node (assoc insert_before_node_node :prev_index insert_before_node_new_idx)) (set! insert_before_node_nodes (assoc insert_before_node_nodes insert_before_node_idx insert_before_node_node)) (set! insert_before_node_ll (assoc insert_before_node_ll :nodes insert_before_node_nodes)))))

(defn insert_after_node [insert_after_node_ll_p insert_after_node_idx insert_after_node_new_idx]
  (binding [insert_after_node_ll nil insert_after_node_new_node nil insert_after_node_next_node nil insert_after_node_node nil insert_after_node_nodes nil insert_after_node_nxt nil] (do (set! insert_after_node_ll insert_after_node_ll_p) (set! insert_after_node_nodes (:nodes insert_after_node_ll)) (set! insert_after_node_new_node (nth insert_after_node_nodes insert_after_node_new_idx)) (set! insert_after_node_new_node (assoc insert_after_node_new_node :prev_index insert_after_node_idx)) (set! insert_after_node_node (nth insert_after_node_nodes insert_after_node_idx)) (set! insert_after_node_nxt (:next_index insert_after_node_node)) (set! insert_after_node_new_node (assoc insert_after_node_new_node :next_index insert_after_node_nxt)) (set! insert_after_node_nodes (assoc insert_after_node_nodes insert_after_node_new_idx insert_after_node_new_node)) (if (= insert_after_node_nxt (- 1)) (set! insert_after_node_ll (assoc insert_after_node_ll :tail_idx insert_after_node_new_idx)) (do (set! insert_after_node_next_node (nth insert_after_node_nodes insert_after_node_nxt)) (set! insert_after_node_next_node (assoc insert_after_node_next_node :prev_index insert_after_node_new_idx)) (set! insert_after_node_nodes (assoc insert_after_node_nodes insert_after_node_nxt insert_after_node_next_node)))) (set! insert_after_node_node (assoc insert_after_node_node :next_index insert_after_node_new_idx)) (set! insert_after_node_nodes (assoc insert_after_node_nodes insert_after_node_idx insert_after_node_node)) (set! insert_after_node_ll (assoc insert_after_node_ll :nodes insert_after_node_nodes)))))

(defn set_head [set_head_ll_p set_head_idx]
  (binding [set_head_ll nil] (do (set! set_head_ll set_head_ll_p) (if (= (:head_idx set_head_ll) (- 1)) (do (set! set_head_ll (assoc set_head_ll :head_idx set_head_idx)) (set! set_head_ll (assoc set_head_ll :tail_idx set_head_idx))) (insert_before_node set_head_ll (:head_idx set_head_ll) set_head_idx)))))

(defn set_tail [set_tail_ll_p set_tail_idx]
  (binding [set_tail_ll nil] (do (set! set_tail_ll set_tail_ll_p) (if (= (:tail_idx set_tail_ll) (- 1)) (do (set! set_tail_ll (assoc set_tail_ll :head_idx set_tail_idx)) (set! set_tail_ll (assoc set_tail_ll :tail_idx set_tail_idx))) (insert_after_node set_tail_ll (:tail_idx set_tail_ll) set_tail_idx)))))

(defn insert [insert_ll_p insert_value]
  (binding [insert_idx nil insert_ll nil insert_nodes nil] (do (set! insert_ll insert_ll_p) (set! insert_nodes (:nodes insert_ll)) (set! insert_nodes (conj insert_nodes {:data insert_value :next_index (- 1) :prev_index (- 1)})) (set! insert_idx (- (count insert_nodes) 1)) (set! insert_ll (assoc insert_ll :nodes insert_nodes)) (if (= (:head_idx insert_ll) (- 1)) (do (set! insert_ll (assoc insert_ll :head_idx insert_idx)) (set! insert_ll (assoc insert_ll :tail_idx insert_idx))) (insert_after_node insert_ll (:tail_idx insert_ll) insert_idx)))))

(defn insert_at_position [insert_at_position_ll_p insert_at_position_position insert_at_position_value]
  (binding [insert_at_position_current nil insert_at_position_current_pos nil insert_at_position_ll nil insert_at_position_new_idx nil insert_at_position_node nil insert_at_position_nodes nil] (try (do (set! insert_at_position_ll insert_at_position_ll_p) (set! insert_at_position_current (:head_idx insert_at_position_ll)) (set! insert_at_position_current_pos 1) (while (not= insert_at_position_current (- 1)) (do (when (= insert_at_position_current_pos insert_at_position_position) (do (set! insert_at_position_nodes (:nodes insert_at_position_ll)) (set! insert_at_position_nodes (conj insert_at_position_nodes {:data insert_at_position_value :next_index (- 1) :prev_index (- 1)})) (set! insert_at_position_new_idx (- (count insert_at_position_nodes) 1)) (set! insert_at_position_ll (assoc insert_at_position_ll :nodes insert_at_position_nodes)) (insert_before_node insert_at_position_ll insert_at_position_current insert_at_position_new_idx) (throw (ex-info "return" {:v nil})))) (set! insert_at_position_node (get (:nodes insert_at_position_ll) insert_at_position_current)) (set! insert_at_position_current (:next_index insert_at_position_node)) (set! insert_at_position_current_pos (+ insert_at_position_current_pos 1)))) (insert insert_at_position_ll insert_at_position_value)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_node [get_node_ll get_node_item]
  (binding [get_node_current nil get_node_node nil] (try (do (set! get_node_current (:head_idx get_node_ll)) (while (not= get_node_current (- 1)) (do (set! get_node_node (get (:nodes get_node_ll) get_node_current)) (when (= (:data get_node_node) get_node_item) (throw (ex-info "return" {:v get_node_current}))) (set! get_node_current (:next_index get_node_node)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_node_pointers [remove_node_pointers_ll_p remove_node_pointers_idx]
  (binding [remove_node_pointers_ll nil remove_node_pointers_node nil remove_node_pointers_nodes nil remove_node_pointers_nxt nil remove_node_pointers_nxt_node nil remove_node_pointers_p nil remove_node_pointers_prev_node nil] (do (set! remove_node_pointers_ll remove_node_pointers_ll_p) (set! remove_node_pointers_nodes (:nodes remove_node_pointers_ll)) (set! remove_node_pointers_node (nth remove_node_pointers_nodes remove_node_pointers_idx)) (set! remove_node_pointers_nxt (:next_index remove_node_pointers_node)) (set! remove_node_pointers_p (:prev_index remove_node_pointers_node)) (when (not= remove_node_pointers_nxt (- 1)) (do (set! remove_node_pointers_nxt_node (nth remove_node_pointers_nodes remove_node_pointers_nxt)) (set! remove_node_pointers_nxt_node (assoc remove_node_pointers_nxt_node :prev_index remove_node_pointers_p)) (set! remove_node_pointers_nodes (assoc remove_node_pointers_nodes remove_node_pointers_nxt remove_node_pointers_nxt_node)))) (when (not= remove_node_pointers_p (- 1)) (do (set! remove_node_pointers_prev_node (nth remove_node_pointers_nodes remove_node_pointers_p)) (set! remove_node_pointers_prev_node (assoc remove_node_pointers_prev_node :next_index remove_node_pointers_nxt)) (set! remove_node_pointers_nodes (assoc remove_node_pointers_nodes remove_node_pointers_p remove_node_pointers_prev_node)))) (set! remove_node_pointers_node (assoc remove_node_pointers_node :next_index (- 1))) (set! remove_node_pointers_node (assoc remove_node_pointers_node :prev_index (- 1))) (set! remove_node_pointers_nodes (assoc remove_node_pointers_nodes remove_node_pointers_idx remove_node_pointers_node)) (set! remove_node_pointers_ll (assoc remove_node_pointers_ll :nodes remove_node_pointers_nodes)))))

(defn delete_value [delete_value_ll_p delete_value_value]
  (binding [delete_value_idx nil delete_value_ll nil delete_value_node nil] (try (do (set! delete_value_ll delete_value_ll_p) (set! delete_value_idx (get_node delete_value_ll delete_value_value)) (when (= delete_value_idx (- 1)) (throw (ex-info "return" {:v nil}))) (when (= delete_value_idx (:head_idx delete_value_ll)) (do (set! delete_value_node (get (:nodes delete_value_ll) delete_value_idx)) (set! delete_value_ll (assoc delete_value_ll :head_idx (:next_index delete_value_node))))) (when (= delete_value_idx (:tail_idx delete_value_ll)) (do (set! delete_value_node (get (:nodes delete_value_ll) delete_value_idx)) (set! delete_value_ll (assoc delete_value_ll :tail_idx (:prev_index delete_value_node))))) (remove_node_pointers delete_value_ll delete_value_idx)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains [contains_ll contains_value]
  (try (throw (ex-info "return" {:v (not= (get_node contains_ll contains_value) (- 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_empty [is_empty_ll]
  (try (throw (ex-info "return" {:v (= (:head_idx is_empty_ll) (- 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_string [to_string_ll]
  (binding [first_v nil to_string_current nil to_string_node nil to_string_res nil to_string_val nil] (try (do (set! to_string_res "") (set! first_v true) (set! to_string_current (:head_idx to_string_ll)) (while (not= to_string_current (- 1)) (do (set! to_string_node (get (:nodes to_string_ll) to_string_current)) (set! to_string_val (str (:data to_string_node))) (if first_v (do (set! to_string_res to_string_val) (set! first_v false)) (set! to_string_res (str (str to_string_res " ") to_string_val))) (set! to_string_current (:next_index to_string_node)))) (throw (ex-info "return" {:v to_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_list [print_list_ll]
  (binding [print_list_current nil print_list_node nil] (do (set! print_list_current (:head_idx print_list_ll)) (while (not= print_list_current (- 1)) (do (set! print_list_node (get (:nodes print_list_ll) print_list_current)) (println (str (:data print_list_node))) (set! print_list_current (:next_index print_list_node)))))))

(defn main []
  (binding [main_i nil main_idx_head nil main_idx_tail nil main_ll nil main_ll2 nil main_nodes nil] (do (set! main_ll (empty_list)) (println (str (get_head_data main_ll))) (println (str (get_tail_data main_ll))) (println (str (is_empty main_ll))) (insert main_ll 10) (println (str (get_head_data main_ll))) (println (str (get_tail_data main_ll))) (insert_at_position main_ll 3 20) (println (str (get_head_data main_ll))) (println (str (get_tail_data main_ll))) (set! main_nodes (:nodes main_ll)) (set! main_nodes (conj main_nodes {:data 1000 :next_index (- 1) :prev_index (- 1)})) (set! main_idx_head (- (count main_nodes) 1)) (set! main_ll (assoc main_ll :nodes main_nodes)) (set_head main_ll main_idx_head) (set! main_nodes (:nodes main_ll)) (set! main_nodes (conj main_nodes {:data 2000 :next_index (- 1) :prev_index (- 1)})) (set! main_idx_tail (- (count main_nodes) 1)) (set! main_ll (assoc main_ll :nodes main_nodes)) (set_tail main_ll main_idx_tail) (print_list main_ll) (println (str (is_empty main_ll))) (print_list main_ll) (println (str (contains main_ll 10))) (delete_value main_ll 10) (println (str (contains main_ll 10))) (delete_value main_ll 2000) (println (str (get_tail_data main_ll))) (delete_value main_ll 1000) (println (str (get_tail_data main_ll))) (println (str (get_head_data main_ll))) (print_list main_ll) (delete_value main_ll 20) (print_list main_ll) (set! main_i 1) (while (< main_i 10) (do (insert main_ll main_i) (set! main_i (+ main_i 1)))) (print_list main_ll) (set! main_ll2 (empty_list)) (insert_at_position main_ll2 1 10) (println (to_string main_ll2)) (insert_at_position main_ll2 2 20) (println (to_string main_ll2)) (insert_at_position main_ll2 1 30) (println (to_string main_ll2)) (insert_at_position main_ll2 3 40) (println (to_string main_ll2)) (insert_at_position main_ll2 5 50) (println (to_string main_ll2)))))

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
