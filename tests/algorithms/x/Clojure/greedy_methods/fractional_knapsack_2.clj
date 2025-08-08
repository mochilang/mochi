(ns main (:refer-clojure :exclude [sort_by_ratio fractional_knapsack]))

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

(declare sort_by_ratio fractional_knapsack)

(def ^:dynamic fractional_knapsack_capacity nil)

(def ^:dynamic fractional_knapsack_fractions nil)

(def ^:dynamic fractional_knapsack_i nil)

(def ^:dynamic fractional_knapsack_idx nil)

(def ^:dynamic fractional_knapsack_index nil)

(def ^:dynamic fractional_knapsack_item nil)

(def ^:dynamic fractional_knapsack_max_value nil)

(def ^:dynamic fractional_knapsack_n nil)

(def ^:dynamic fractional_knapsack_ratio nil)

(def ^:dynamic sort_by_ratio_i nil)

(def ^:dynamic sort_by_ratio_index nil)

(def ^:dynamic sort_by_ratio_j nil)

(def ^:dynamic sort_by_ratio_key nil)

(def ^:dynamic sort_by_ratio_key_ratio nil)

(defn sort_by_ratio [sort_by_ratio_index_p sort_by_ratio_ratio]
  (binding [sort_by_ratio_i nil sort_by_ratio_index nil sort_by_ratio_j nil sort_by_ratio_key nil sort_by_ratio_key_ratio nil] (try (do (set! sort_by_ratio_index sort_by_ratio_index_p) (set! sort_by_ratio_i 1) (while (< sort_by_ratio_i (count sort_by_ratio_index)) (do (set! sort_by_ratio_key (nth sort_by_ratio_index sort_by_ratio_i)) (set! sort_by_ratio_key_ratio (nth sort_by_ratio_ratio sort_by_ratio_key)) (set! sort_by_ratio_j (- sort_by_ratio_i 1)) (while (and (>= sort_by_ratio_j 0) (< (nth sort_by_ratio_ratio (nth sort_by_ratio_index sort_by_ratio_j)) sort_by_ratio_key_ratio)) (do (set! sort_by_ratio_index (assoc sort_by_ratio_index (+ sort_by_ratio_j 1) (nth sort_by_ratio_index sort_by_ratio_j))) (set! sort_by_ratio_j (- sort_by_ratio_j 1)))) (set! sort_by_ratio_index (assoc sort_by_ratio_index (+ sort_by_ratio_j 1) sort_by_ratio_key)) (set! sort_by_ratio_i (+ sort_by_ratio_i 1)))) (throw (ex-info "return" {:v sort_by_ratio_index}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fractional_knapsack [fractional_knapsack_value fractional_knapsack_weight fractional_knapsack_capacity_p]
  (binding [fractional_knapsack_capacity nil fractional_knapsack_fractions nil fractional_knapsack_i nil fractional_knapsack_idx nil fractional_knapsack_index nil fractional_knapsack_item nil fractional_knapsack_max_value nil fractional_knapsack_n nil fractional_knapsack_ratio nil] (try (do (set! fractional_knapsack_capacity fractional_knapsack_capacity_p) (set! fractional_knapsack_n (count fractional_knapsack_value)) (set! fractional_knapsack_index []) (set! fractional_knapsack_i 0) (while (< fractional_knapsack_i fractional_knapsack_n) (do (set! fractional_knapsack_index (conj fractional_knapsack_index fractional_knapsack_i)) (set! fractional_knapsack_i (+ fractional_knapsack_i 1)))) (set! fractional_knapsack_ratio []) (set! fractional_knapsack_i 0) (while (< fractional_knapsack_i fractional_knapsack_n) (do (set! fractional_knapsack_ratio (conj fractional_knapsack_ratio (quot (nth fractional_knapsack_value fractional_knapsack_i) (nth fractional_knapsack_weight fractional_knapsack_i)))) (set! fractional_knapsack_i (+ fractional_knapsack_i 1)))) (set! fractional_knapsack_index (sort_by_ratio fractional_knapsack_index fractional_knapsack_ratio)) (set! fractional_knapsack_fractions []) (set! fractional_knapsack_i 0) (while (< fractional_knapsack_i fractional_knapsack_n) (do (set! fractional_knapsack_fractions (conj fractional_knapsack_fractions 0.0)) (set! fractional_knapsack_i (+ fractional_knapsack_i 1)))) (set! fractional_knapsack_max_value 0.0) (set! fractional_knapsack_idx 0) (loop [while_flag_1 true] (when (and while_flag_1 (< fractional_knapsack_idx (count fractional_knapsack_index))) (do (set! fractional_knapsack_item (nth fractional_knapsack_index fractional_knapsack_idx)) (if (<= (nth fractional_knapsack_weight fractional_knapsack_item) fractional_knapsack_capacity) (do (set! fractional_knapsack_fractions (assoc fractional_knapsack_fractions fractional_knapsack_item 1.0)) (set! fractional_knapsack_max_value (+ fractional_knapsack_max_value (nth fractional_knapsack_value fractional_knapsack_item))) (set! fractional_knapsack_capacity (- fractional_knapsack_capacity (nth fractional_knapsack_weight fractional_knapsack_item)))) (do (set! fractional_knapsack_fractions (assoc fractional_knapsack_fractions fractional_knapsack_item (quot fractional_knapsack_capacity (nth fractional_knapsack_weight fractional_knapsack_item)))) (set! fractional_knapsack_max_value (+ fractional_knapsack_max_value (quot (* (nth fractional_knapsack_value fractional_knapsack_item) fractional_knapsack_capacity) (nth fractional_knapsack_weight fractional_knapsack_item)))) (recur false))) (set! fractional_knapsack_idx (+ fractional_knapsack_idx 1)) (cond :else (recur while_flag_1))))) (throw (ex-info "return" {:v {:fractions fractional_knapsack_fractions :max_value fractional_knapsack_max_value}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_v [1.0 3.0 5.0 7.0 9.0])

(def ^:dynamic main_w [0.9 0.7 0.5 0.3 0.1])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (fractional_knapsack main_v main_w 5.0))
      (println (fractional_knapsack [1.0 3.0 5.0 7.0] [0.9 0.7 0.5 0.3] 30.0))
      (println (fractional_knapsack [] [] 30.0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
