(ns main (:refer-clojure :exclude [sort_by_ratio_desc sum_first frac_knapsack]))

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

(declare sort_by_ratio_desc sum_first frac_knapsack)

(def ^:dynamic frac_knapsack_acc nil)

(def ^:dynamic frac_knapsack_i nil)

(def ^:dynamic frac_knapsack_items nil)

(def ^:dynamic frac_knapsack_itm nil)

(def ^:dynamic frac_knapsack_k nil)

(def ^:dynamic frac_knapsack_total nil)

(def ^:dynamic frac_knapsack_values nil)

(def ^:dynamic frac_knapsack_weights nil)

(def ^:dynamic sort_by_ratio_desc_arr nil)

(def ^:dynamic sort_by_ratio_desc_current nil)

(def ^:dynamic sort_by_ratio_desc_i nil)

(def ^:dynamic sort_by_ratio_desc_j nil)

(def ^:dynamic sort_by_ratio_desc_key nil)

(def ^:dynamic sum_first_i nil)

(def ^:dynamic sum_first_s nil)

(defn sort_by_ratio_desc [sort_by_ratio_desc_arr_p]
  (binding [sort_by_ratio_desc_arr nil sort_by_ratio_desc_current nil sort_by_ratio_desc_i nil sort_by_ratio_desc_j nil sort_by_ratio_desc_key nil] (try (do (set! sort_by_ratio_desc_arr sort_by_ratio_desc_arr_p) (set! sort_by_ratio_desc_i 1) (while (< sort_by_ratio_desc_i (count sort_by_ratio_desc_arr)) (do (set! sort_by_ratio_desc_key (nth sort_by_ratio_desc_arr sort_by_ratio_desc_i)) (set! sort_by_ratio_desc_j (- sort_by_ratio_desc_i 1)) (loop [while_flag_1 true] (when (and while_flag_1 (>= sort_by_ratio_desc_j 0)) (do (set! sort_by_ratio_desc_current (nth sort_by_ratio_desc_arr sort_by_ratio_desc_j)) (if (< (quot (:value sort_by_ratio_desc_current) (:weight sort_by_ratio_desc_current)) (quot (:value sort_by_ratio_desc_key) (:weight sort_by_ratio_desc_key))) (do (set! sort_by_ratio_desc_arr (assoc sort_by_ratio_desc_arr (+ sort_by_ratio_desc_j 1) sort_by_ratio_desc_current)) (set! sort_by_ratio_desc_j (- sort_by_ratio_desc_j 1)) (recur while_flag_1)) (recur false))))) (set! sort_by_ratio_desc_arr (assoc sort_by_ratio_desc_arr (+ sort_by_ratio_desc_j 1) sort_by_ratio_desc_key)) (set! sort_by_ratio_desc_i (+ sort_by_ratio_desc_i 1)))) (throw (ex-info "return" {:v sort_by_ratio_desc_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sum_first [sum_first_arr sum_first_k]
  (binding [sum_first_i nil sum_first_s nil] (try (do (set! sum_first_s 0.0) (set! sum_first_i 0) (while (and (< sum_first_i sum_first_k) (< sum_first_i (count sum_first_arr))) (do (set! sum_first_s (+ sum_first_s (nth sum_first_arr sum_first_i))) (set! sum_first_i (+ sum_first_i 1)))) (throw (ex-info "return" {:v sum_first_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn frac_knapsack [frac_knapsack_vl frac_knapsack_wt frac_knapsack_w frac_knapsack_n]
  (binding [frac_knapsack_acc nil frac_knapsack_i nil frac_knapsack_items nil frac_knapsack_itm nil frac_knapsack_k nil frac_knapsack_total nil frac_knapsack_values nil frac_knapsack_weights nil] (try (do (set! frac_knapsack_items []) (set! frac_knapsack_i 0) (while (and (< frac_knapsack_i (count frac_knapsack_vl)) (< frac_knapsack_i (count frac_knapsack_wt))) (do (set! frac_knapsack_items (conj frac_knapsack_items {:value (nth frac_knapsack_vl frac_knapsack_i) :weight (nth frac_knapsack_wt frac_knapsack_i)})) (set! frac_knapsack_i (+ frac_knapsack_i 1)))) (set! frac_knapsack_items (sort_by_ratio_desc frac_knapsack_items)) (set! frac_knapsack_values []) (set! frac_knapsack_weights []) (set! frac_knapsack_i 0) (while (< frac_knapsack_i (count frac_knapsack_items)) (do (set! frac_knapsack_itm (nth frac_knapsack_items frac_knapsack_i)) (set! frac_knapsack_values (conj frac_knapsack_values (:value frac_knapsack_itm))) (set! frac_knapsack_weights (conj frac_knapsack_weights (:weight frac_knapsack_itm))) (set! frac_knapsack_i (+ frac_knapsack_i 1)))) (set! frac_knapsack_acc []) (set! frac_knapsack_total 0.0) (set! frac_knapsack_i 0) (while (< frac_knapsack_i (count frac_knapsack_weights)) (do (set! frac_knapsack_total (+ frac_knapsack_total (nth frac_knapsack_weights frac_knapsack_i))) (set! frac_knapsack_acc (conj frac_knapsack_acc frac_knapsack_total)) (set! frac_knapsack_i (+ frac_knapsack_i 1)))) (set! frac_knapsack_k 0) (while (and (< frac_knapsack_k (count frac_knapsack_acc)) (>= frac_knapsack_w (nth frac_knapsack_acc frac_knapsack_k))) (set! frac_knapsack_k (+ frac_knapsack_k 1))) (when (= frac_knapsack_k 0) (throw (ex-info "return" {:v 0.0}))) (when (>= frac_knapsack_k (count frac_knapsack_values)) (throw (ex-info "return" {:v (sum_first frac_knapsack_values (count frac_knapsack_values))}))) (if (not= frac_knapsack_k frac_knapsack_n) (+ (sum_first frac_knapsack_values frac_knapsack_k) (quot (* (- frac_knapsack_w (nth frac_knapsack_acc (- frac_knapsack_k 1))) (nth frac_knapsack_values frac_knapsack_k)) (nth frac_knapsack_weights frac_knapsack_k))) (sum_first frac_knapsack_values frac_knapsack_k))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_vl [60.0 100.0 120.0])

(def ^:dynamic main_wt [10.0 20.0 30.0])

(def ^:dynamic main_result (frac_knapsack main_vl main_wt 50.0 3))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str main_result))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
