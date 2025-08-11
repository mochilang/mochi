(ns main (:refer-clojure :exclude [bisect_left reverse_list patience_sort]))

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

(declare bisect_left reverse_list patience_sort)

(def ^:dynamic bisect_left_high nil)

(def ^:dynamic bisect_left_low nil)

(def ^:dynamic bisect_left_mid nil)

(def ^:dynamic bisect_left_stack nil)

(def ^:dynamic bisect_left_top nil)

(def ^:dynamic bisect_left_top_idx nil)

(def ^:dynamic count_v nil)

(def ^:dynamic patience_sort_collection nil)

(def ^:dynamic patience_sort_element nil)

(def ^:dynamic patience_sort_i nil)

(def ^:dynamic patience_sort_idx nil)

(def ^:dynamic patience_sort_indices nil)

(def ^:dynamic patience_sort_j nil)

(def ^:dynamic patience_sort_min_stack nil)

(def ^:dynamic patience_sort_min_val nil)

(def ^:dynamic patience_sort_new_stack nil)

(def ^:dynamic patience_sort_result nil)

(def ^:dynamic patience_sort_stack nil)

(def ^:dynamic patience_sort_stacks nil)

(def ^:dynamic patience_sort_total nil)

(def ^:dynamic patience_sort_val nil)

(def ^:dynamic reverse_list_i nil)

(def ^:dynamic reverse_list_res nil)

(defn bisect_left [bisect_left_stacks bisect_left_value]
  (binding [bisect_left_high nil bisect_left_low nil bisect_left_mid nil bisect_left_stack nil bisect_left_top nil bisect_left_top_idx nil] (try (do (set! bisect_left_low 0) (set! bisect_left_high (count bisect_left_stacks)) (while (< bisect_left_low bisect_left_high) (do (set! bisect_left_mid (/ (+ bisect_left_low bisect_left_high) 2)) (set! bisect_left_stack (nth bisect_left_stacks bisect_left_mid)) (set! bisect_left_top_idx (- (count bisect_left_stack) 1)) (set! bisect_left_top (nth bisect_left_stack bisect_left_top_idx)) (if (< bisect_left_top bisect_left_value) (set! bisect_left_low (+ bisect_left_mid 1)) (set! bisect_left_high bisect_left_mid)))) (throw (ex-info "return" {:v bisect_left_low}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_list [reverse_list_src]
  (binding [reverse_list_i nil reverse_list_res nil] (try (do (set! reverse_list_res []) (set! reverse_list_i (- (count reverse_list_src) 1)) (while (>= reverse_list_i 0) (do (set! reverse_list_res (conj reverse_list_res (nth reverse_list_src reverse_list_i))) (set! reverse_list_i (- reverse_list_i 1)))) (throw (ex-info "return" {:v reverse_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn patience_sort [patience_sort_collection_p]
  (binding [count_v nil patience_sort_collection nil patience_sort_element nil patience_sort_i nil patience_sort_idx nil patience_sort_indices nil patience_sort_j nil patience_sort_min_stack nil patience_sort_min_val nil patience_sort_new_stack nil patience_sort_result nil patience_sort_stack nil patience_sort_stacks nil patience_sort_total nil patience_sort_val nil] (try (do (set! patience_sort_collection patience_sort_collection_p) (set! patience_sort_stacks []) (set! patience_sort_i 0) (while (< patience_sort_i (count patience_sort_collection)) (do (set! patience_sort_element (nth patience_sort_collection patience_sort_i)) (set! patience_sort_idx (bisect_left patience_sort_stacks patience_sort_element)) (if (not= patience_sort_idx (count patience_sort_stacks)) (do (set! patience_sort_stack (nth patience_sort_stacks patience_sort_idx)) (set! patience_sort_stacks (assoc patience_sort_stacks patience_sort_idx (conj patience_sort_stack patience_sort_element)))) (do (set! patience_sort_new_stack [patience_sort_element]) (set! patience_sort_stacks (conj patience_sort_stacks patience_sort_new_stack)))) (set! patience_sort_i (+ patience_sort_i 1)))) (set! patience_sort_i 0) (while (< patience_sort_i (count patience_sort_stacks)) (do (set! patience_sort_stacks (assoc patience_sort_stacks patience_sort_i (reverse_list (nth patience_sort_stacks patience_sort_i)))) (set! patience_sort_i (+ patience_sort_i 1)))) (set! patience_sort_indices []) (set! patience_sort_i 0) (while (< patience_sort_i (count patience_sort_stacks)) (do (set! patience_sort_indices (conj patience_sort_indices 0)) (set! patience_sort_i (+ patience_sort_i 1)))) (set! patience_sort_total 0) (set! patience_sort_i 0) (while (< patience_sort_i (count patience_sort_stacks)) (do (set! patience_sort_total (+ patience_sort_total (count (nth patience_sort_stacks patience_sort_i)))) (set! patience_sort_i (+ patience_sort_i 1)))) (set! patience_sort_result []) (set! count_v 0) (while (< count_v patience_sort_total) (do (set! patience_sort_min_val 0) (set! patience_sort_min_stack (- 1)) (set! patience_sort_j 0) (while (< patience_sort_j (count patience_sort_stacks)) (do (set! patience_sort_idx (nth patience_sort_indices patience_sort_j)) (when (< patience_sort_idx (count (nth patience_sort_stacks patience_sort_j))) (do (set! patience_sort_val (nth (nth patience_sort_stacks patience_sort_j) patience_sort_idx)) (if (< patience_sort_min_stack 0) (do (set! patience_sort_min_val patience_sort_val) (set! patience_sort_min_stack patience_sort_j)) (when (< patience_sort_val patience_sort_min_val) (do (set! patience_sort_min_val patience_sort_val) (set! patience_sort_min_stack patience_sort_j)))))) (set! patience_sort_j (+ patience_sort_j 1)))) (set! patience_sort_result (conj patience_sort_result patience_sort_min_val)) (set! patience_sort_indices (assoc patience_sort_indices patience_sort_min_stack (+ (nth patience_sort_indices patience_sort_min_stack) 1))) (set! count_v (+ count_v 1)))) (set! patience_sort_i 0) (while (< patience_sort_i (count patience_sort_result)) (do (set! patience_sort_collection (assoc patience_sort_collection patience_sort_i (nth patience_sort_result patience_sort_i))) (set! patience_sort_i (+ patience_sort_i 1)))) (throw (ex-info "return" {:v patience_sort_collection}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (patience_sort [1 9 5 21 17 6])))
      (println (str (patience_sort [])))
      (println (str (patience_sort [(- 3) (- 17) (- 48)])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
