(ns main (:refer-clojure :exclude [sort_list make_sorted_linked_list len_sll str_sll merge_lists main]))

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

(declare sort_list make_sorted_linked_list len_sll str_sll merge_lists main)

(def ^:dynamic main_merged nil)

(def ^:dynamic main_sll_one nil)

(def ^:dynamic main_sll_two nil)

(def ^:dynamic main_test_data_even nil)

(def ^:dynamic main_test_data_odd nil)

(def ^:dynamic merge_lists_combined nil)

(def ^:dynamic merge_lists_i nil)

(def ^:dynamic sort_list_arr nil)

(def ^:dynamic sort_list_i nil)

(def ^:dynamic sort_list_j nil)

(def ^:dynamic sort_list_k nil)

(def ^:dynamic sort_list_tmp nil)

(def ^:dynamic str_sll_i nil)

(def ^:dynamic str_sll_res nil)

(defn sort_list [sort_list_nums]
  (binding [sort_list_arr nil sort_list_i nil sort_list_j nil sort_list_k nil sort_list_tmp nil] (try (do (set! sort_list_arr []) (set! sort_list_i 0) (while (< sort_list_i (count sort_list_nums)) (do (set! sort_list_arr (conj sort_list_arr (nth sort_list_nums sort_list_i))) (set! sort_list_i (+ sort_list_i 1)))) (set! sort_list_j 0) (while (< sort_list_j (count sort_list_arr)) (do (set! sort_list_k (+ sort_list_j 1)) (while (< sort_list_k (count sort_list_arr)) (do (when (< (nth sort_list_arr sort_list_k) (nth sort_list_arr sort_list_j)) (do (set! sort_list_tmp (nth sort_list_arr sort_list_j)) (set! sort_list_arr (assoc sort_list_arr sort_list_j (nth sort_list_arr sort_list_k))) (set! sort_list_arr (assoc sort_list_arr sort_list_k sort_list_tmp)))) (set! sort_list_k (+ sort_list_k 1)))) (set! sort_list_j (+ sort_list_j 1)))) (throw (ex-info "return" {:v sort_list_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn make_sorted_linked_list [make_sorted_linked_list_ints]
  (try (throw (ex-info "return" {:v {:values (sort_list make_sorted_linked_list_ints)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn len_sll [len_sll_sll]
  (try (throw (ex-info "return" {:v (count (:values len_sll_sll))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn str_sll [str_sll_sll]
  (binding [str_sll_i nil str_sll_res nil] (try (do (set! str_sll_res "") (set! str_sll_i 0) (while (< str_sll_i (count (:values str_sll_sll))) (do (set! str_sll_res (str str_sll_res (str (get (:values str_sll_sll) str_sll_i)))) (when (< (+ str_sll_i 1) (count (:values str_sll_sll))) (set! str_sll_res (str str_sll_res " -> "))) (set! str_sll_i (+ str_sll_i 1)))) (throw (ex-info "return" {:v str_sll_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn merge_lists [merge_lists_a merge_lists_b]
  (binding [merge_lists_combined nil merge_lists_i nil] (try (do (set! merge_lists_combined []) (set! merge_lists_i 0) (while (< merge_lists_i (count (:values merge_lists_a))) (do (set! merge_lists_combined (conj merge_lists_combined (get (:values merge_lists_a) merge_lists_i))) (set! merge_lists_i (+ merge_lists_i 1)))) (set! merge_lists_i 0) (while (< merge_lists_i (count (:values merge_lists_b))) (do (set! merge_lists_combined (conj merge_lists_combined (get (:values merge_lists_b) merge_lists_i))) (set! merge_lists_i (+ merge_lists_i 1)))) (throw (ex-info "return" {:v (make_sorted_linked_list merge_lists_combined)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_merged nil main_sll_one nil main_sll_two nil main_test_data_even nil main_test_data_odd nil] (do (set! main_test_data_odd [3 9 (- 11) 0 7 5 1 (- 1)]) (set! main_test_data_even [4 6 2 0 8 10 3 (- 2)]) (set! main_sll_one (make_sorted_linked_list main_test_data_odd)) (set! main_sll_two (make_sorted_linked_list main_test_data_even)) (set! main_merged (merge_lists main_sll_one main_sll_two)) (println (str (len_sll main_merged))) (println (str_sll main_merged)))))

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
