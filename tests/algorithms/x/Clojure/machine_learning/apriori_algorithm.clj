(ns main (:refer-clojure :exclude [load_data contains_string is_subset lists_equal contains_list count_list slice_list combinations_lists prune sort_strings itemset_to_string apriori]))

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

(declare load_data contains_string is_subset lists_equal contains_list count_list slice_list combinations_lists prune sort_strings itemset_to_string apriori)

(def ^:dynamic apriori_candidate nil)

(def ^:dynamic apriori_combos nil)

(def ^:dynamic apriori_counts nil)

(def ^:dynamic apriori_frequent nil)

(def ^:dynamic apriori_idx nil)

(def ^:dynamic apriori_itemset nil)

(def ^:dynamic apriori_j nil)

(def ^:dynamic apriori_k nil)

(def ^:dynamic apriori_length nil)

(def ^:dynamic apriori_m nil)

(def ^:dynamic apriori_new_itemset nil)

(def ^:dynamic apriori_sorted_item nil)

(def ^:dynamic apriori_t nil)

(def ^:dynamic combinations_lists_head nil)

(def ^:dynamic combinations_lists_i nil)

(def ^:dynamic combinations_lists_new_combo nil)

(def ^:dynamic combinations_lists_result nil)

(def ^:dynamic combinations_lists_tail nil)

(def ^:dynamic combinations_lists_tail_combos nil)

(def ^:dynamic count_list_c nil)

(def ^:dynamic itemset_to_string_i nil)

(def ^:dynamic itemset_to_string_s nil)

(def ^:dynamic lists_equal_i nil)

(def ^:dynamic prune_is_subsequence nil)

(def ^:dynamic prune_pruned nil)

(def ^:dynamic slice_list_i nil)

(def ^:dynamic slice_list_res nil)

(def ^:dynamic sort_strings_i nil)

(def ^:dynamic sort_strings_j nil)

(def ^:dynamic sort_strings_res nil)

(def ^:dynamic sort_strings_tmp nil)

(defn load_data []
  (try (throw (ex-info "return" {:v [["milk"] ["milk" "butter"] ["milk" "bread"] ["milk" "bread" "chips"]]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn contains_string [contains_string_xs contains_string_s]
  (try (do (doseq [v contains_string_xs] (when (= v contains_string_s) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_subset [is_subset_candidate is_subset_transaction]
  (try (do (doseq [it is_subset_candidate] (when (not (contains_string is_subset_transaction it)) (throw (ex-info "return" {:v false})))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn lists_equal [lists_equal_a lists_equal_b]
  (binding [lists_equal_i nil] (try (do (when (not= (count lists_equal_a) (count lists_equal_b)) (throw (ex-info "return" {:v false}))) (set! lists_equal_i 0) (while (< lists_equal_i (count lists_equal_a)) (do (when (not= (nth lists_equal_a lists_equal_i) (nth lists_equal_b lists_equal_i)) (throw (ex-info "return" {:v false}))) (set! lists_equal_i (+ lists_equal_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_list [contains_list_itemset contains_list_item]
  (try (do (doseq [l contains_list_itemset] (when (lists_equal l contains_list_item) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn count_list [count_list_itemset count_list_item]
  (binding [count_list_c nil] (try (do (set! count_list_c 0) (doseq [l count_list_itemset] (when (lists_equal l count_list_item) (set! count_list_c (+ count_list_c 1)))) (throw (ex-info "return" {:v count_list_c}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn slice_list [slice_list_xs slice_list_start]
  (binding [slice_list_i nil slice_list_res nil] (try (do (set! slice_list_res []) (set! slice_list_i slice_list_start) (while (< slice_list_i (count slice_list_xs)) (do (set! slice_list_res (conj slice_list_res (nth slice_list_xs slice_list_i))) (set! slice_list_i (+ slice_list_i 1)))) (throw (ex-info "return" {:v slice_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn combinations_lists [combinations_lists_xs combinations_lists_k]
  (binding [combinations_lists_head nil combinations_lists_i nil combinations_lists_new_combo nil combinations_lists_result nil combinations_lists_tail nil combinations_lists_tail_combos nil] (try (do (set! combinations_lists_result []) (when (= combinations_lists_k 0) (do (set! combinations_lists_result (conj combinations_lists_result [])) (throw (ex-info "return" {:v combinations_lists_result})))) (set! combinations_lists_i 0) (while (< combinations_lists_i (count combinations_lists_xs)) (do (set! combinations_lists_head (nth combinations_lists_xs combinations_lists_i)) (set! combinations_lists_tail (slice_list combinations_lists_xs (+ combinations_lists_i 1))) (set! combinations_lists_tail_combos (combinations_lists combinations_lists_tail (- combinations_lists_k 1))) (doseq [combo combinations_lists_tail_combos] (do (set! combinations_lists_new_combo []) (set! combinations_lists_new_combo (conj combinations_lists_new_combo combinations_lists_head)) (doseq [c combo] (set! combinations_lists_new_combo (conj combinations_lists_new_combo c))) (set! combinations_lists_result (conj combinations_lists_result combinations_lists_new_combo)))) (set! combinations_lists_i (+ combinations_lists_i 1)))) (throw (ex-info "return" {:v combinations_lists_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn prune [prune_itemset prune_candidates prune_length]
  (binding [prune_is_subsequence nil prune_pruned nil] (try (do (set! prune_pruned []) (doseq [candidate prune_candidates] (do (set! prune_is_subsequence true) (loop [item_seq candidate] (when (seq item_seq) (let [item (first item_seq)] (cond (or (not (contains_list prune_itemset item)) (< (count_list prune_itemset item) (- prune_length 1))) (do (set! prune_is_subsequence false) (recur nil)) :else (recur (rest item_seq)))))) (when prune_is_subsequence (set! prune_pruned (conj prune_pruned candidate))))) (throw (ex-info "return" {:v prune_pruned}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_strings [sort_strings_xs]
  (binding [sort_strings_i nil sort_strings_j nil sort_strings_res nil sort_strings_tmp nil] (try (do (set! sort_strings_res []) (doseq [s sort_strings_xs] (set! sort_strings_res (conj sort_strings_res s))) (set! sort_strings_i 0) (while (< sort_strings_i (count sort_strings_res)) (do (set! sort_strings_j (+ sort_strings_i 1)) (while (< sort_strings_j (count sort_strings_res)) (do (when (< (nth sort_strings_res sort_strings_j) (nth sort_strings_res sort_strings_i)) (do (set! sort_strings_tmp (nth sort_strings_res sort_strings_i)) (set! sort_strings_res (assoc sort_strings_res sort_strings_i (nth sort_strings_res sort_strings_j))) (set! sort_strings_res (assoc sort_strings_res sort_strings_j sort_strings_tmp)))) (set! sort_strings_j (+ sort_strings_j 1)))) (set! sort_strings_i (+ sort_strings_i 1)))) (throw (ex-info "return" {:v sort_strings_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn itemset_to_string [itemset_to_string_xs]
  (binding [itemset_to_string_i nil itemset_to_string_s nil] (try (do (set! itemset_to_string_s "[") (set! itemset_to_string_i 0) (while (< itemset_to_string_i (count itemset_to_string_xs)) (do (when (> itemset_to_string_i 0) (set! itemset_to_string_s (str itemset_to_string_s ", "))) (set! itemset_to_string_s (str (str (str itemset_to_string_s "'") (nth itemset_to_string_xs itemset_to_string_i)) "'")) (set! itemset_to_string_i (+ itemset_to_string_i 1)))) (set! itemset_to_string_s (str itemset_to_string_s "]")) (throw (ex-info "return" {:v itemset_to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn apriori [apriori_data apriori_min_support]
  (binding [apriori_candidate nil apriori_combos nil apriori_counts nil apriori_frequent nil apriori_idx nil apriori_itemset nil apriori_j nil apriori_k nil apriori_length nil apriori_m nil apriori_new_itemset nil apriori_sorted_item nil apriori_t nil] (try (do (set! apriori_itemset []) (doseq [transaction apriori_data] (do (set! apriori_t []) (doseq [v transaction] (set! apriori_t (conj apriori_t v))) (set! apriori_itemset (conj apriori_itemset apriori_t)))) (set! apriori_frequent []) (set! apriori_length 1) (while (> (count apriori_itemset) 0) (do (set! apriori_counts []) (set! apriori_idx 0) (while (< apriori_idx (count apriori_itemset)) (do (set! apriori_counts (conj apriori_counts 0)) (set! apriori_idx (+ apriori_idx 1)))) (doseq [transaction apriori_data] (do (set! apriori_j 0) (while (< apriori_j (count apriori_itemset)) (do (set! apriori_candidate (nth apriori_itemset apriori_j)) (when (is_subset apriori_candidate transaction) (set! apriori_counts (assoc apriori_counts apriori_j (+ (nth apriori_counts apriori_j) 1)))) (set! apriori_j (+ apriori_j 1)))))) (set! apriori_new_itemset []) (set! apriori_k 0) (while (< apriori_k (count apriori_itemset)) (do (when (>= (nth apriori_counts apriori_k) apriori_min_support) (set! apriori_new_itemset (conj apriori_new_itemset (nth apriori_itemset apriori_k)))) (set! apriori_k (+ apriori_k 1)))) (set! apriori_itemset apriori_new_itemset) (set! apriori_m 0) (while (< apriori_m (count apriori_itemset)) (do (set! apriori_sorted_item (sort_strings (nth apriori_itemset apriori_m))) (set! apriori_frequent (conj apriori_frequent {:items apriori_sorted_item :support (nth apriori_counts apriori_m)})) (set! apriori_m (+ apriori_m 1)))) (set! apriori_length (+ apriori_length 1)) (set! apriori_combos (combinations_lists apriori_itemset apriori_length)) (set! apriori_itemset (prune apriori_itemset apriori_combos apriori_length)))) (throw (ex-info "return" {:v apriori_frequent}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_frequent_itemsets (apriori (load_data) 2))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (doseq [fi main_frequent_itemsets] (println (str (str (itemset_to_string (:items fi)) ": ") (str (:support fi)))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
