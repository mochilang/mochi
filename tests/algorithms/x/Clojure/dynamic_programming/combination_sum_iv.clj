(ns main (:refer-clojure :exclude [make_list count_recursive combination_sum_iv count_dp combination_sum_iv_dp_array combination_sum_iv_bottom_up]))

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

(declare make_list count_recursive combination_sum_iv count_dp combination_sum_iv_dp_array combination_sum_iv_bottom_up)

(def ^:dynamic combination_sum_iv_bottom_up_dp nil)

(def ^:dynamic combination_sum_iv_bottom_up_i nil)

(def ^:dynamic combination_sum_iv_bottom_up_j nil)

(def ^:dynamic combination_sum_iv_dp_array_dp nil)

(def ^:dynamic count_dp_dp nil)

(def ^:dynamic count_dp_i nil)

(def ^:dynamic count_dp_total nil)

(def ^:dynamic count_recursive_i nil)

(def ^:dynamic count_recursive_total nil)

(def ^:dynamic make_list_arr nil)

(def ^:dynamic make_list_i nil)

(defn make_list [make_list_len make_list_value]
  (binding [make_list_arr nil make_list_i nil] (try (do (set! make_list_arr []) (set! make_list_i 0) (while (< make_list_i make_list_len) (do (set! make_list_arr (conj make_list_arr make_list_value)) (set! make_list_i (+ make_list_i 1)))) (throw (ex-info "return" {:v make_list_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_recursive [count_recursive_array count_recursive_target]
  (binding [count_recursive_i nil count_recursive_total nil] (try (do (when (< count_recursive_target 0) (throw (ex-info "return" {:v 0}))) (when (= count_recursive_target 0) (throw (ex-info "return" {:v 1}))) (set! count_recursive_total 0) (set! count_recursive_i 0) (while (< count_recursive_i (count count_recursive_array)) (do (set! count_recursive_total (+ count_recursive_total (count_recursive count_recursive_array (- count_recursive_target (nth count_recursive_array count_recursive_i))))) (set! count_recursive_i (+ count_recursive_i 1)))) (throw (ex-info "return" {:v count_recursive_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn combination_sum_iv [combination_sum_iv_array combination_sum_iv_target]
  (try (throw (ex-info "return" {:v (count_recursive combination_sum_iv_array combination_sum_iv_target)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn count_dp [count_dp_array count_dp_target count_dp_dp_p]
  (binding [count_dp_dp nil count_dp_i nil count_dp_total nil] (try (do (set! count_dp_dp count_dp_dp_p) (when (< count_dp_target 0) (throw (ex-info "return" {:v 0}))) (when (= count_dp_target 0) (throw (ex-info "return" {:v 1}))) (when (> (nth count_dp_dp count_dp_target) (- 0 1)) (throw (ex-info "return" {:v (nth count_dp_dp count_dp_target)}))) (set! count_dp_total 0) (set! count_dp_i 0) (while (< count_dp_i (count count_dp_array)) (do (set! count_dp_total (+ count_dp_total (count_dp count_dp_array (- count_dp_target (nth count_dp_array count_dp_i)) count_dp_dp))) (set! count_dp_i (+ count_dp_i 1)))) (set! count_dp_dp (assoc count_dp_dp count_dp_target count_dp_total)) (throw (ex-info "return" {:v count_dp_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn combination_sum_iv_dp_array [combination_sum_iv_dp_array_array combination_sum_iv_dp_array_target]
  (binding [combination_sum_iv_dp_array_dp nil] (try (do (set! combination_sum_iv_dp_array_dp (make_list (+ combination_sum_iv_dp_array_target 1) (- 1))) (throw (ex-info "return" {:v (count_dp combination_sum_iv_dp_array_array combination_sum_iv_dp_array_target combination_sum_iv_dp_array_dp)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn combination_sum_iv_bottom_up [combination_sum_iv_bottom_up_n combination_sum_iv_bottom_up_array combination_sum_iv_bottom_up_target]
  (binding [combination_sum_iv_bottom_up_dp nil combination_sum_iv_bottom_up_i nil combination_sum_iv_bottom_up_j nil] (try (do (set! combination_sum_iv_bottom_up_dp (make_list (+ combination_sum_iv_bottom_up_target 1) 0)) (set! combination_sum_iv_bottom_up_dp (assoc combination_sum_iv_bottom_up_dp 0 1)) (set! combination_sum_iv_bottom_up_i 1) (while (<= combination_sum_iv_bottom_up_i combination_sum_iv_bottom_up_target) (do (set! combination_sum_iv_bottom_up_j 0) (while (< combination_sum_iv_bottom_up_j combination_sum_iv_bottom_up_n) (do (when (>= (- combination_sum_iv_bottom_up_i (nth combination_sum_iv_bottom_up_array combination_sum_iv_bottom_up_j)) 0) (set! combination_sum_iv_bottom_up_dp (assoc combination_sum_iv_bottom_up_dp combination_sum_iv_bottom_up_i (+ (nth combination_sum_iv_bottom_up_dp combination_sum_iv_bottom_up_i) (nth combination_sum_iv_bottom_up_dp (- combination_sum_iv_bottom_up_i (nth combination_sum_iv_bottom_up_array combination_sum_iv_bottom_up_j))))))) (set! combination_sum_iv_bottom_up_j (+ combination_sum_iv_bottom_up_j 1)))) (set! combination_sum_iv_bottom_up_i (+ combination_sum_iv_bottom_up_i 1)))) (throw (ex-info "return" {:v (nth combination_sum_iv_bottom_up_dp combination_sum_iv_bottom_up_target)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (combination_sum_iv [1 2 5] 5)))
      (println (str (combination_sum_iv_dp_array [1 2 5] 5)))
      (println (str (combination_sum_iv_bottom_up 3 [1 2 5] 5)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
