(ns main (:refer-clojure :exclude [max_cross_sum max_subarray show main]))

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

(declare max_cross_sum max_subarray show main)

(def ^:dynamic main_nums1 nil)

(def ^:dynamic main_nums2 nil)

(def ^:dynamic main_nums3 nil)

(def ^:dynamic main_nums4 nil)

(def ^:dynamic main_nums5 nil)

(def ^:dynamic main_nums6 nil)

(def ^:dynamic main_res1 nil)

(def ^:dynamic main_res2 nil)

(def ^:dynamic main_res3 nil)

(def ^:dynamic main_res4 nil)

(def ^:dynamic main_res5 nil)

(def ^:dynamic main_res6 nil)

(def ^:dynamic max_cross_sum_i nil)

(def ^:dynamic max_cross_sum_left_sum nil)

(def ^:dynamic max_cross_sum_max_left nil)

(def ^:dynamic max_cross_sum_max_right nil)

(def ^:dynamic max_cross_sum_right_sum nil)

(def ^:dynamic max_cross_sum_sum nil)

(def ^:dynamic max_subarray_cross nil)

(def ^:dynamic max_subarray_left nil)

(def ^:dynamic max_subarray_mid nil)

(def ^:dynamic max_subarray_right nil)

(defn max_cross_sum [max_cross_sum_arr max_cross_sum_low max_cross_sum_mid max_cross_sum_high]
  (binding [max_cross_sum_i nil max_cross_sum_left_sum nil max_cross_sum_max_left nil max_cross_sum_max_right nil max_cross_sum_right_sum nil max_cross_sum_sum nil] (try (do (set! max_cross_sum_left_sum (- 1000000000000000000.0)) (set! max_cross_sum_max_left (- 1)) (set! max_cross_sum_sum 0.0) (set! max_cross_sum_i max_cross_sum_mid) (while (>= max_cross_sum_i max_cross_sum_low) (do (set! max_cross_sum_sum (+ max_cross_sum_sum (nth max_cross_sum_arr max_cross_sum_i))) (when (> max_cross_sum_sum max_cross_sum_left_sum) (do (set! max_cross_sum_left_sum max_cross_sum_sum) (set! max_cross_sum_max_left max_cross_sum_i))) (set! max_cross_sum_i (- max_cross_sum_i 1)))) (set! max_cross_sum_right_sum (- 1000000000000000000.0)) (set! max_cross_sum_max_right (- 1)) (set! max_cross_sum_sum 0.0) (set! max_cross_sum_i (+ max_cross_sum_mid 1)) (while (<= max_cross_sum_i max_cross_sum_high) (do (set! max_cross_sum_sum (+ max_cross_sum_sum (nth max_cross_sum_arr max_cross_sum_i))) (when (> max_cross_sum_sum max_cross_sum_right_sum) (do (set! max_cross_sum_right_sum max_cross_sum_sum) (set! max_cross_sum_max_right max_cross_sum_i))) (set! max_cross_sum_i (+ max_cross_sum_i 1)))) (throw (ex-info "return" {:v {:end max_cross_sum_max_right :start max_cross_sum_max_left :sum (+ max_cross_sum_left_sum max_cross_sum_right_sum)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_subarray [max_subarray_arr max_subarray_low max_subarray_high]
  (binding [max_subarray_cross nil max_subarray_left nil max_subarray_mid nil max_subarray_right nil] (try (do (when (= (count max_subarray_arr) 0) (throw (ex-info "return" {:v {:end (- 1) :start (- 1) :sum 0.0}}))) (when (= max_subarray_low max_subarray_high) (throw (ex-info "return" {:v {:end max_subarray_high :start max_subarray_low :sum (nth max_subarray_arr max_subarray_low)}}))) (set! max_subarray_mid (quot (+ max_subarray_low max_subarray_high) 2)) (set! max_subarray_left (max_subarray max_subarray_arr max_subarray_low max_subarray_mid)) (set! max_subarray_right (max_subarray max_subarray_arr (+ max_subarray_mid 1) max_subarray_high)) (set! max_subarray_cross (max_cross_sum max_subarray_arr max_subarray_low max_subarray_mid max_subarray_high)) (when (and (>= (:sum max_subarray_left) (:sum max_subarray_right)) (>= (:sum max_subarray_left) (:sum max_subarray_cross))) (throw (ex-info "return" {:v max_subarray_left}))) (if (and (>= (:sum max_subarray_right) (:sum max_subarray_left)) (>= (:sum max_subarray_right) (:sum max_subarray_cross))) max_subarray_right max_subarray_cross)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn show [show_res]
  (println (str (str (str (str (str (str "[" (str (:start show_res))) ", ") (str (:end show_res))) ", ") (str (:sum show_res))) "]")))

(defn main []
  (binding [main_nums1 nil main_nums2 nil main_nums3 nil main_nums4 nil main_nums5 nil main_nums6 nil main_res1 nil main_res2 nil main_res3 nil main_res4 nil main_res5 nil main_res6 nil] (do (set! main_nums1 [(- 2.0) 1.0 (- 3.0) 4.0 (- 1.0) 2.0 1.0 (- 5.0) 4.0]) (set! main_res1 (max_subarray main_nums1 0 (- (count main_nums1) 1))) (show main_res1) (set! main_nums2 [2.0 8.0 9.0]) (set! main_res2 (max_subarray main_nums2 0 (- (count main_nums2) 1))) (show main_res2) (set! main_nums3 [0.0 0.0]) (set! main_res3 (max_subarray main_nums3 0 (- (count main_nums3) 1))) (show main_res3) (set! main_nums4 [(- 1.0) 0.0 1.0]) (set! main_res4 (max_subarray main_nums4 0 (- (count main_nums4) 1))) (show main_res4) (set! main_nums5 [(- 2.0) (- 3.0) (- 1.0) (- 4.0) (- 6.0)]) (set! main_res5 (max_subarray main_nums5 0 (- (count main_nums5) 1))) (show main_res5) (set! main_nums6 []) (set! main_res6 (max_subarray main_nums6 0 0)) (show main_res6))))

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
