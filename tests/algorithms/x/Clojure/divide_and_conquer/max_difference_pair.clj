(ns main (:refer-clojure :exclude [min_slice max_slice max_diff_range max_difference main]))

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

(declare min_slice max_slice max_diff_range max_difference main)

(def ^:dynamic main_result nil)

(def ^:dynamic max_diff_range_big1 nil)

(def ^:dynamic max_diff_range_big2 nil)

(def ^:dynamic max_diff_range_cross_diff nil)

(def ^:dynamic max_diff_range_left nil)

(def ^:dynamic max_diff_range_left_diff nil)

(def ^:dynamic max_diff_range_max_right nil)

(def ^:dynamic max_diff_range_mid nil)

(def ^:dynamic max_diff_range_min_left nil)

(def ^:dynamic max_diff_range_right nil)

(def ^:dynamic max_diff_range_right_diff nil)

(def ^:dynamic max_diff_range_small1 nil)

(def ^:dynamic max_diff_range_small2 nil)

(def ^:dynamic max_diff_range_v nil)

(def ^:dynamic max_slice_i nil)

(def ^:dynamic max_slice_m nil)

(def ^:dynamic min_slice_i nil)

(def ^:dynamic min_slice_m nil)

(defn min_slice [min_slice_a min_slice_start min_slice_end]
  (binding [min_slice_i nil min_slice_m nil] (try (do (set! min_slice_m (nth min_slice_a min_slice_start)) (set! min_slice_i (+ min_slice_start 1)) (while (< min_slice_i min_slice_end) (do (when (< (nth min_slice_a min_slice_i) min_slice_m) (set! min_slice_m (nth min_slice_a min_slice_i))) (set! min_slice_i (+ min_slice_i 1)))) (throw (ex-info "return" {:v min_slice_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_slice [max_slice_a max_slice_start max_slice_end]
  (binding [max_slice_i nil max_slice_m nil] (try (do (set! max_slice_m (nth max_slice_a max_slice_start)) (set! max_slice_i (+ max_slice_start 1)) (while (< max_slice_i max_slice_end) (do (when (> (nth max_slice_a max_slice_i) max_slice_m) (set! max_slice_m (nth max_slice_a max_slice_i))) (set! max_slice_i (+ max_slice_i 1)))) (throw (ex-info "return" {:v max_slice_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_diff_range [max_diff_range_a max_diff_range_start max_diff_range_end]
  (binding [max_diff_range_big1 nil max_diff_range_big2 nil max_diff_range_cross_diff nil max_diff_range_left nil max_diff_range_left_diff nil max_diff_range_max_right nil max_diff_range_mid nil max_diff_range_min_left nil max_diff_range_right nil max_diff_range_right_diff nil max_diff_range_small1 nil max_diff_range_small2 nil max_diff_range_v nil] (try (do (when (= (- max_diff_range_end max_diff_range_start) 1) (do (set! max_diff_range_v (nth max_diff_range_a max_diff_range_start)) (throw (ex-info "return" {:v [max_diff_range_v max_diff_range_v]})))) (set! max_diff_range_mid (quot (+ max_diff_range_start max_diff_range_end) 2)) (set! max_diff_range_left (max_diff_range max_diff_range_a max_diff_range_start max_diff_range_mid)) (set! max_diff_range_right (max_diff_range max_diff_range_a max_diff_range_mid max_diff_range_end)) (set! max_diff_range_small1 (nth max_diff_range_left 0)) (set! max_diff_range_big1 (nth max_diff_range_left 1)) (set! max_diff_range_small2 (nth max_diff_range_right 0)) (set! max_diff_range_big2 (nth max_diff_range_right 1)) (set! max_diff_range_min_left (min_slice max_diff_range_a max_diff_range_start max_diff_range_mid)) (set! max_diff_range_max_right (max_slice max_diff_range_a max_diff_range_mid max_diff_range_end)) (set! max_diff_range_cross_diff (- max_diff_range_max_right max_diff_range_min_left)) (set! max_diff_range_left_diff (- max_diff_range_big1 max_diff_range_small1)) (set! max_diff_range_right_diff (- max_diff_range_big2 max_diff_range_small2)) (if (and (> max_diff_range_right_diff max_diff_range_cross_diff) (> max_diff_range_right_diff max_diff_range_left_diff)) (throw (ex-info "return" {:v [max_diff_range_small2 max_diff_range_big2]})) (if (> max_diff_range_left_diff max_diff_range_cross_diff) (throw (ex-info "return" {:v [max_diff_range_small1 max_diff_range_big1]})) (throw (ex-info "return" {:v [max_diff_range_min_left max_diff_range_max_right]}))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_difference [max_difference_a]
  (try (throw (ex-info "return" {:v (max_diff_range max_difference_a 0 (count max_difference_a))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_result nil] (do (set! main_result (max_difference [5 11 2 1 7 9 0 7])) (println (str main_result)))))

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
