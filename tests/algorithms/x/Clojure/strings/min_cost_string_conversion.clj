(ns main (:refer-clojure :exclude [string_to_chars join_chars insert_at remove_at make_matrix_int make_matrix_string compute_transform_tables assemble_transformation main]))

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

(declare string_to_chars join_chars insert_at remove_at make_matrix_int make_matrix_string compute_transform_tables assemble_transformation main)

(def ^:dynamic assemble_transformation_kind nil)

(def ^:dynamic assemble_transformation_op nil)

(def ^:dynamic assemble_transformation_seq nil)

(def ^:dynamic compute_transform_tables_costs nil)

(def ^:dynamic compute_transform_tables_dest_seq nil)

(def ^:dynamic compute_transform_tables_i nil)

(def ^:dynamic compute_transform_tables_j nil)

(def ^:dynamic compute_transform_tables_m nil)

(def ^:dynamic compute_transform_tables_n nil)

(def ^:dynamic compute_transform_tables_ops nil)

(def ^:dynamic compute_transform_tables_source_seq nil)

(def ^:dynamic insert_at_i nil)

(def ^:dynamic insert_at_res nil)

(def ^:dynamic join_chars_i nil)

(def ^:dynamic join_chars_res nil)

(def ^:dynamic main_copy_cost nil)

(def ^:dynamic main_cost nil)

(def ^:dynamic main_delete_cost nil)

(def ^:dynamic main_dst nil)

(def ^:dynamic main_idx nil)

(def ^:dynamic main_insert_cost nil)

(def ^:dynamic main_k nil)

(def ^:dynamic main_kind nil)

(def ^:dynamic main_m nil)

(def ^:dynamic main_n nil)

(def ^:dynamic main_op nil)

(def ^:dynamic main_operations nil)

(def ^:dynamic main_replace_cost nil)

(def ^:dynamic main_sequence nil)

(def ^:dynamic main_src nil)

(def ^:dynamic main_string_list nil)

(def ^:dynamic main_tables nil)

(def ^:dynamic make_matrix_int_matrix nil)

(def ^:dynamic make_matrix_int_row nil)

(def ^:dynamic make_matrix_string_matrix nil)

(def ^:dynamic make_matrix_string_row nil)

(def ^:dynamic remove_at_i nil)

(def ^:dynamic remove_at_res nil)

(def ^:dynamic string_to_chars_chars nil)

(def ^:dynamic string_to_chars_i nil)

(defn string_to_chars [string_to_chars_s]
  (binding [string_to_chars_chars nil string_to_chars_i nil] (try (do (set! string_to_chars_chars []) (set! string_to_chars_i 0) (while (< string_to_chars_i (count string_to_chars_s)) (do (set! string_to_chars_chars (conj string_to_chars_chars (subs string_to_chars_s string_to_chars_i (min (+ string_to_chars_i 1) (count string_to_chars_s))))) (set! string_to_chars_i (+ string_to_chars_i 1)))) (throw (ex-info "return" {:v string_to_chars_chars}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn join_chars [join_chars_chars]
  (binding [join_chars_i nil join_chars_res nil] (try (do (set! join_chars_res "") (set! join_chars_i 0) (while (< join_chars_i (count join_chars_chars)) (do (set! join_chars_res (str join_chars_res (nth join_chars_chars join_chars_i))) (set! join_chars_i (+ join_chars_i 1)))) (throw (ex-info "return" {:v join_chars_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert_at [insert_at_chars insert_at_index insert_at_ch]
  (binding [insert_at_i nil insert_at_res nil] (try (do (set! insert_at_res []) (set! insert_at_i 0) (while (< insert_at_i insert_at_index) (do (set! insert_at_res (conj insert_at_res (nth insert_at_chars insert_at_i))) (set! insert_at_i (+ insert_at_i 1)))) (set! insert_at_res (conj insert_at_res insert_at_ch)) (while (< insert_at_i (count insert_at_chars)) (do (set! insert_at_res (conj insert_at_res (nth insert_at_chars insert_at_i))) (set! insert_at_i (+ insert_at_i 1)))) (throw (ex-info "return" {:v insert_at_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_at [remove_at_chars remove_at_index]
  (binding [remove_at_i nil remove_at_res nil] (try (do (set! remove_at_res []) (set! remove_at_i 0) (while (< remove_at_i (count remove_at_chars)) (do (when (not= remove_at_i remove_at_index) (set! remove_at_res (conj remove_at_res (nth remove_at_chars remove_at_i)))) (set! remove_at_i (+ remove_at_i 1)))) (throw (ex-info "return" {:v remove_at_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_matrix_int [make_matrix_int_rows make_matrix_int_cols make_matrix_int_init]
  (binding [make_matrix_int_matrix nil make_matrix_int_row nil] (try (do (set! make_matrix_int_matrix []) (dotimes [_ make_matrix_int_rows] (do (set! make_matrix_int_row []) (dotimes [_2 make_matrix_int_cols] (set! make_matrix_int_row (conj make_matrix_int_row make_matrix_int_init))) (set! make_matrix_int_matrix (conj make_matrix_int_matrix make_matrix_int_row)))) (throw (ex-info "return" {:v make_matrix_int_matrix}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_matrix_string [make_matrix_string_rows make_matrix_string_cols make_matrix_string_init]
  (binding [make_matrix_string_matrix nil make_matrix_string_row nil] (try (do (set! make_matrix_string_matrix []) (dotimes [_ make_matrix_string_rows] (do (set! make_matrix_string_row []) (dotimes [_2 make_matrix_string_cols] (set! make_matrix_string_row (conj make_matrix_string_row make_matrix_string_init))) (set! make_matrix_string_matrix (conj make_matrix_string_matrix make_matrix_string_row)))) (throw (ex-info "return" {:v make_matrix_string_matrix}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn compute_transform_tables [compute_transform_tables_source_string compute_transform_tables_destination_string compute_transform_tables_copy_cost compute_transform_tables_replace_cost compute_transform_tables_delete_cost compute_transform_tables_insert_cost]
  (binding [compute_transform_tables_costs nil compute_transform_tables_dest_seq nil compute_transform_tables_i nil compute_transform_tables_j nil compute_transform_tables_m nil compute_transform_tables_n nil compute_transform_tables_ops nil compute_transform_tables_source_seq nil] (try (do (set! compute_transform_tables_source_seq (string_to_chars compute_transform_tables_source_string)) (set! compute_transform_tables_dest_seq (string_to_chars compute_transform_tables_destination_string)) (set! compute_transform_tables_m (count compute_transform_tables_source_seq)) (set! compute_transform_tables_n (count compute_transform_tables_dest_seq)) (set! compute_transform_tables_costs (make_matrix_int (+ compute_transform_tables_m 1) (+ compute_transform_tables_n 1) 0)) (set! compute_transform_tables_ops (make_matrix_string (+ compute_transform_tables_m 1) (+ compute_transform_tables_n 1) "0")) (set! compute_transform_tables_i 1) (while (<= compute_transform_tables_i compute_transform_tables_m) (do (set! compute_transform_tables_costs (assoc-in compute_transform_tables_costs [compute_transform_tables_i 0] (* compute_transform_tables_i compute_transform_tables_delete_cost))) (set! compute_transform_tables_ops (assoc-in compute_transform_tables_ops [compute_transform_tables_i 0] (str "D" (nth compute_transform_tables_source_seq (- compute_transform_tables_i 1))))) (set! compute_transform_tables_i (+ compute_transform_tables_i 1)))) (set! compute_transform_tables_j 1) (while (<= compute_transform_tables_j compute_transform_tables_n) (do (set! compute_transform_tables_costs (assoc-in compute_transform_tables_costs [0 compute_transform_tables_j] (* compute_transform_tables_j compute_transform_tables_insert_cost))) (set! compute_transform_tables_ops (assoc-in compute_transform_tables_ops [0 compute_transform_tables_j] (str "I" (nth compute_transform_tables_dest_seq (- compute_transform_tables_j 1))))) (set! compute_transform_tables_j (+ compute_transform_tables_j 1)))) (set! compute_transform_tables_i 1) (while (<= compute_transform_tables_i compute_transform_tables_m) (do (set! compute_transform_tables_j 1) (while (<= compute_transform_tables_j compute_transform_tables_n) (do (if (= (nth compute_transform_tables_source_seq (- compute_transform_tables_i 1)) (nth compute_transform_tables_dest_seq (- compute_transform_tables_j 1))) (do (set! compute_transform_tables_costs (assoc-in compute_transform_tables_costs [compute_transform_tables_i compute_transform_tables_j] (+ (nth (nth compute_transform_tables_costs (- compute_transform_tables_i 1)) (- compute_transform_tables_j 1)) compute_transform_tables_copy_cost))) (set! compute_transform_tables_ops (assoc-in compute_transform_tables_ops [compute_transform_tables_i compute_transform_tables_j] (str "C" (nth compute_transform_tables_source_seq (- compute_transform_tables_i 1)))))) (do (set! compute_transform_tables_costs (assoc-in compute_transform_tables_costs [compute_transform_tables_i compute_transform_tables_j] (+ (nth (nth compute_transform_tables_costs (- compute_transform_tables_i 1)) (- compute_transform_tables_j 1)) compute_transform_tables_replace_cost))) (set! compute_transform_tables_ops (assoc-in compute_transform_tables_ops [compute_transform_tables_i compute_transform_tables_j] (str (str "R" (nth compute_transform_tables_source_seq (- compute_transform_tables_i 1))) (nth compute_transform_tables_dest_seq (- compute_transform_tables_j 1))))))) (when (< (+ (nth (nth compute_transform_tables_costs (- compute_transform_tables_i 1)) compute_transform_tables_j) compute_transform_tables_delete_cost) (nth (nth compute_transform_tables_costs compute_transform_tables_i) compute_transform_tables_j)) (do (set! compute_transform_tables_costs (assoc-in compute_transform_tables_costs [compute_transform_tables_i compute_transform_tables_j] (+ (nth (nth compute_transform_tables_costs (- compute_transform_tables_i 1)) compute_transform_tables_j) compute_transform_tables_delete_cost))) (set! compute_transform_tables_ops (assoc-in compute_transform_tables_ops [compute_transform_tables_i compute_transform_tables_j] (str "D" (nth compute_transform_tables_source_seq (- compute_transform_tables_i 1))))))) (when (< (+ (nth (nth compute_transform_tables_costs compute_transform_tables_i) (- compute_transform_tables_j 1)) compute_transform_tables_insert_cost) (nth (nth compute_transform_tables_costs compute_transform_tables_i) compute_transform_tables_j)) (do (set! compute_transform_tables_costs (assoc-in compute_transform_tables_costs [compute_transform_tables_i compute_transform_tables_j] (+ (nth (nth compute_transform_tables_costs compute_transform_tables_i) (- compute_transform_tables_j 1)) compute_transform_tables_insert_cost))) (set! compute_transform_tables_ops (assoc-in compute_transform_tables_ops [compute_transform_tables_i compute_transform_tables_j] (str "I" (nth compute_transform_tables_dest_seq (- compute_transform_tables_j 1))))))) (set! compute_transform_tables_j (+ compute_transform_tables_j 1)))) (set! compute_transform_tables_i (+ compute_transform_tables_i 1)))) (throw (ex-info "return" {:v {:costs compute_transform_tables_costs :ops compute_transform_tables_ops}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn assemble_transformation [assemble_transformation_ops assemble_transformation_i assemble_transformation_j]
  (binding [assemble_transformation_kind nil assemble_transformation_op nil assemble_transformation_seq nil] (try (do (when (and (= assemble_transformation_i 0) (= assemble_transformation_j 0)) (throw (ex-info "return" {:v []}))) (set! assemble_transformation_op (nth (nth assemble_transformation_ops assemble_transformation_i) assemble_transformation_j)) (set! assemble_transformation_kind (subs assemble_transformation_op 0 (min 1 (count assemble_transformation_op)))) (if (or (= assemble_transformation_kind "C") (= assemble_transformation_kind "R")) (do (set! assemble_transformation_seq (assemble_transformation assemble_transformation_ops (- assemble_transformation_i 1) (- assemble_transformation_j 1))) (set! assemble_transformation_seq (conj assemble_transformation_seq assemble_transformation_op)) (throw (ex-info "return" {:v assemble_transformation_seq}))) (if (= assemble_transformation_kind "D") (do (set! assemble_transformation_seq (assemble_transformation assemble_transformation_ops (- assemble_transformation_i 1) assemble_transformation_j)) (set! assemble_transformation_seq (conj assemble_transformation_seq assemble_transformation_op)) (throw (ex-info "return" {:v assemble_transformation_seq}))) (do (set! assemble_transformation_seq (assemble_transformation assemble_transformation_ops assemble_transformation_i (- assemble_transformation_j 1))) (set! assemble_transformation_seq (conj assemble_transformation_seq assemble_transformation_op)) (throw (ex-info "return" {:v assemble_transformation_seq})))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_copy_cost nil main_cost nil main_delete_cost nil main_dst nil main_idx nil main_insert_cost nil main_k nil main_kind nil main_m nil main_n nil main_op nil main_operations nil main_replace_cost nil main_sequence nil main_src nil main_string_list nil main_tables nil] (do (set! main_copy_cost (- 1)) (set! main_replace_cost 1) (set! main_delete_cost 2) (set! main_insert_cost 2) (set! main_src "Python") (set! main_dst "Algorithms") (set! main_tables (compute_transform_tables main_src main_dst main_copy_cost main_replace_cost main_delete_cost main_insert_cost)) (set! main_operations (:ops main_tables)) (set! main_m (count main_operations)) (set! main_n (count (get main_operations 0))) (set! main_sequence (assemble_transformation main_operations (- main_m 1) (- main_n 1))) (set! main_string_list (string_to_chars main_src)) (set! main_idx 0) (set! main_cost 0) (set! main_k 0) (while (< main_k (count main_sequence)) (do (println (join_chars main_string_list)) (set! main_op (nth main_sequence main_k)) (set! main_kind (subs main_op 0 (min 1 (count main_op)))) (if (= main_kind "C") (set! main_cost (+ main_cost main_copy_cost)) (if (= main_kind "R") (do (set! main_string_list (assoc main_string_list main_idx (subs main_op 2 (min 3 (count main_op))))) (set! main_cost (+ main_cost main_replace_cost))) (if (= main_kind "D") (do (set! main_string_list (remove_at main_string_list main_idx)) (set! main_cost (+ main_cost main_delete_cost))) (do (set! main_string_list (insert_at main_string_list main_idx (subs main_op 1 (min 2 (count main_op))))) (set! main_cost (+ main_cost main_insert_cost)))))) (set! main_idx (+ main_idx 1)) (set! main_k (+ main_k 1)))) (println (join_chars main_string_list)) (println (str "Cost: " (str main_cost))))))

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
