(ns main (:refer-clojure :exclude [max_sum_sliding_window test_max_sum_sliding_window main]))

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

(declare max_sum_sliding_window test_max_sum_sliding_window main)

(def ^:dynamic main_sample nil)

(def ^:dynamic max_sum_sliding_window_current_sum nil)

(def ^:dynamic max_sum_sliding_window_i nil)

(def ^:dynamic max_sum_sliding_window_idx nil)

(def ^:dynamic max_sum_sliding_window_max_sum nil)

(def ^:dynamic test_max_sum_sliding_window_arr1 nil)

(def ^:dynamic test_max_sum_sliding_window_arr2 nil)

(defn max_sum_sliding_window [max_sum_sliding_window_arr max_sum_sliding_window_k]
  (binding [max_sum_sliding_window_current_sum nil max_sum_sliding_window_i nil max_sum_sliding_window_idx nil max_sum_sliding_window_max_sum nil] (try (do (when (or (< max_sum_sliding_window_k 0) (< (count max_sum_sliding_window_arr) max_sum_sliding_window_k)) (throw (Exception. "Invalid Input"))) (set! max_sum_sliding_window_idx 0) (set! max_sum_sliding_window_current_sum 0) (while (< max_sum_sliding_window_idx max_sum_sliding_window_k) (do (set! max_sum_sliding_window_current_sum (+ max_sum_sliding_window_current_sum (nth max_sum_sliding_window_arr max_sum_sliding_window_idx))) (set! max_sum_sliding_window_idx (+ max_sum_sliding_window_idx 1)))) (set! max_sum_sliding_window_max_sum max_sum_sliding_window_current_sum) (set! max_sum_sliding_window_i 0) (while (< max_sum_sliding_window_i (- (count max_sum_sliding_window_arr) max_sum_sliding_window_k)) (do (set! max_sum_sliding_window_current_sum (+ (- max_sum_sliding_window_current_sum (nth max_sum_sliding_window_arr max_sum_sliding_window_i)) (nth max_sum_sliding_window_arr (+ max_sum_sliding_window_i max_sum_sliding_window_k)))) (when (> max_sum_sliding_window_current_sum max_sum_sliding_window_max_sum) (set! max_sum_sliding_window_max_sum max_sum_sliding_window_current_sum)) (set! max_sum_sliding_window_i (+ max_sum_sliding_window_i 1)))) (throw (ex-info "return" {:v max_sum_sliding_window_max_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_max_sum_sliding_window []
  (binding [test_max_sum_sliding_window_arr1 nil test_max_sum_sliding_window_arr2 nil] (do (set! test_max_sum_sliding_window_arr1 [1 4 2 10 2 3 1 0 20]) (when (not= (max_sum_sliding_window test_max_sum_sliding_window_arr1 4) 24) (throw (Exception. "test1 failed"))) (set! test_max_sum_sliding_window_arr2 [1 4 2 10 2 13 1 0 2]) (when (not= (max_sum_sliding_window test_max_sum_sliding_window_arr2 4) 27) (throw (Exception. "test2 failed"))))))

(defn main []
  (binding [main_sample nil] (do (test_max_sum_sliding_window) (set! main_sample [1 4 2 10 2 3 1 0 20]) (println (str (max_sum_sliding_window main_sample 4))))))

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
