(ns main (:refer-clojure :exclude [sort_list largest_divisible_subset main]))

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

(declare sort_list largest_divisible_subset main)

(def ^:dynamic largest_divisible_subset_ans nil)

(def ^:dynamic largest_divisible_subset_i nil)

(def ^:dynamic largest_divisible_subset_j nil)

(def ^:dynamic largest_divisible_subset_last_index nil)

(def ^:dynamic largest_divisible_subset_memo nil)

(def ^:dynamic largest_divisible_subset_n nil)

(def ^:dynamic largest_divisible_subset_nums nil)

(def ^:dynamic largest_divisible_subset_prev nil)

(def ^:dynamic largest_divisible_subset_result nil)

(def ^:dynamic main_items nil)

(def ^:dynamic main_subset nil)

(def ^:dynamic sort_list_arr nil)

(def ^:dynamic sort_list_i nil)

(def ^:dynamic sort_list_j nil)

(def ^:dynamic sort_list_key nil)

(defn sort_list [sort_list_nums]
  (binding [sort_list_arr nil sort_list_i nil sort_list_j nil sort_list_key nil] (try (do (set! sort_list_arr sort_list_nums) (set! sort_list_i 1) (while (< sort_list_i (count sort_list_arr)) (do (set! sort_list_key (nth sort_list_arr sort_list_i)) (set! sort_list_j (- sort_list_i 1)) (while (and (>= sort_list_j 0) (> (nth sort_list_arr sort_list_j) sort_list_key)) (do (set! sort_list_arr (assoc sort_list_arr (+ sort_list_j 1) (nth sort_list_arr sort_list_j))) (set! sort_list_j (- sort_list_j 1)))) (set! sort_list_arr (assoc sort_list_arr (+ sort_list_j 1) sort_list_key)) (set! sort_list_i (+ sort_list_i 1)))) (throw (ex-info "return" {:v sort_list_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn largest_divisible_subset [largest_divisible_subset_items]
  (binding [largest_divisible_subset_ans nil largest_divisible_subset_i nil largest_divisible_subset_j nil largest_divisible_subset_last_index nil largest_divisible_subset_memo nil largest_divisible_subset_n nil largest_divisible_subset_nums nil largest_divisible_subset_prev nil largest_divisible_subset_result nil] (try (do (when (= (count largest_divisible_subset_items) 0) (throw (ex-info "return" {:v []}))) (set! largest_divisible_subset_nums (sort_list largest_divisible_subset_items)) (set! largest_divisible_subset_n (count largest_divisible_subset_nums)) (set! largest_divisible_subset_memo []) (set! largest_divisible_subset_prev []) (set! largest_divisible_subset_i 0) (while (< largest_divisible_subset_i largest_divisible_subset_n) (do (set! largest_divisible_subset_memo (conj largest_divisible_subset_memo 1)) (set! largest_divisible_subset_prev (conj largest_divisible_subset_prev largest_divisible_subset_i)) (set! largest_divisible_subset_i (+ largest_divisible_subset_i 1)))) (set! largest_divisible_subset_i 0) (while (< largest_divisible_subset_i largest_divisible_subset_n) (do (set! largest_divisible_subset_j 0) (while (< largest_divisible_subset_j largest_divisible_subset_i) (do (when (and (or (= (nth largest_divisible_subset_nums largest_divisible_subset_j) 0) (= (mod (nth largest_divisible_subset_nums largest_divisible_subset_i) (nth largest_divisible_subset_nums largest_divisible_subset_j)) 0)) (> (+ (nth largest_divisible_subset_memo largest_divisible_subset_j) 1) (nth largest_divisible_subset_memo largest_divisible_subset_i))) (do (set! largest_divisible_subset_memo (assoc largest_divisible_subset_memo largest_divisible_subset_i (+ (nth largest_divisible_subset_memo largest_divisible_subset_j) 1))) (set! largest_divisible_subset_prev (assoc largest_divisible_subset_prev largest_divisible_subset_i largest_divisible_subset_j)))) (set! largest_divisible_subset_j (+ largest_divisible_subset_j 1)))) (set! largest_divisible_subset_i (+ largest_divisible_subset_i 1)))) (set! largest_divisible_subset_ans (- 0 1)) (set! largest_divisible_subset_last_index (- 0 1)) (set! largest_divisible_subset_i 0) (while (< largest_divisible_subset_i largest_divisible_subset_n) (do (when (> (nth largest_divisible_subset_memo largest_divisible_subset_i) largest_divisible_subset_ans) (do (set! largest_divisible_subset_ans (nth largest_divisible_subset_memo largest_divisible_subset_i)) (set! largest_divisible_subset_last_index largest_divisible_subset_i))) (set! largest_divisible_subset_i (+ largest_divisible_subset_i 1)))) (when (= largest_divisible_subset_last_index (- 0 1)) (throw (ex-info "return" {:v []}))) (set! largest_divisible_subset_result [(nth largest_divisible_subset_nums largest_divisible_subset_last_index)]) (while (not= (nth largest_divisible_subset_prev largest_divisible_subset_last_index) largest_divisible_subset_last_index) (do (set! largest_divisible_subset_last_index (nth largest_divisible_subset_prev largest_divisible_subset_last_index)) (set! largest_divisible_subset_result (conj largest_divisible_subset_result (nth largest_divisible_subset_nums largest_divisible_subset_last_index))))) (throw (ex-info "return" {:v largest_divisible_subset_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_items nil main_subset nil] (do (set! main_items [1 16 7 8 4]) (set! main_subset (largest_divisible_subset main_items)) (println (str (str (str (str "The longest divisible subset of " (str main_items)) " is ") (str main_subset)) ".")))))

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
