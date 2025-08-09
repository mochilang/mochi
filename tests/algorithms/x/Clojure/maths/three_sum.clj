(ns main (:refer-clojure :exclude [bubble_sort three_sum]))

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

(declare bubble_sort three_sum)

(def ^:dynamic bubble_sort_arr nil)

(def ^:dynamic bubble_sort_i nil)

(def ^:dynamic bubble_sort_j nil)

(def ^:dynamic bubble_sort_n nil)

(def ^:dynamic bubble_sort_temp nil)

(def ^:dynamic three_sum_c nil)

(def ^:dynamic three_sum_high nil)

(def ^:dynamic three_sum_i nil)

(def ^:dynamic three_sum_low nil)

(def ^:dynamic three_sum_n nil)

(def ^:dynamic three_sum_res nil)

(def ^:dynamic three_sum_s nil)

(def ^:dynamic three_sum_sorted nil)

(def ^:dynamic three_sum_triple nil)

(defn bubble_sort [bubble_sort_nums]
  (binding [bubble_sort_arr nil bubble_sort_i nil bubble_sort_j nil bubble_sort_n nil bubble_sort_temp nil] (try (do (set! bubble_sort_arr bubble_sort_nums) (set! bubble_sort_n (count bubble_sort_arr)) (set! bubble_sort_i 0) (while (< bubble_sort_i bubble_sort_n) (do (set! bubble_sort_j 0) (while (< bubble_sort_j (- bubble_sort_n 1)) (do (when (> (nth bubble_sort_arr bubble_sort_j) (nth bubble_sort_arr (+ bubble_sort_j 1))) (do (set! bubble_sort_temp (nth bubble_sort_arr bubble_sort_j)) (set! bubble_sort_arr (assoc bubble_sort_arr bubble_sort_j (nth bubble_sort_arr (+ bubble_sort_j 1)))) (set! bubble_sort_arr (assoc bubble_sort_arr (+ bubble_sort_j 1) bubble_sort_temp)))) (set! bubble_sort_j (+ bubble_sort_j 1)))) (set! bubble_sort_i (+ bubble_sort_i 1)))) (throw (ex-info "return" {:v bubble_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn three_sum [three_sum_nums]
  (binding [three_sum_c nil three_sum_high nil three_sum_i nil three_sum_low nil three_sum_n nil three_sum_res nil three_sum_s nil three_sum_sorted nil three_sum_triple nil] (try (do (set! three_sum_sorted (bubble_sort three_sum_nums)) (set! three_sum_res []) (set! three_sum_n (count three_sum_sorted)) (set! three_sum_i 0) (while (< three_sum_i (- three_sum_n 2)) (do (when (or (= three_sum_i 0) (not= (nth three_sum_sorted three_sum_i) (nth three_sum_sorted (- three_sum_i 1)))) (do (set! three_sum_low (+ three_sum_i 1)) (set! three_sum_high (- three_sum_n 1)) (set! three_sum_c (- 0 (nth three_sum_sorted three_sum_i))) (while (< three_sum_low three_sum_high) (do (set! three_sum_s (+ (nth three_sum_sorted three_sum_low) (nth three_sum_sorted three_sum_high))) (if (= three_sum_s three_sum_c) (do (set! three_sum_triple [(nth three_sum_sorted three_sum_i) (nth three_sum_sorted three_sum_low) (nth three_sum_sorted three_sum_high)]) (set! three_sum_res (conj three_sum_res three_sum_triple)) (while (and (< three_sum_low three_sum_high) (= (nth three_sum_sorted three_sum_low) (nth three_sum_sorted (+ three_sum_low 1)))) (set! three_sum_low (+ three_sum_low 1))) (while (and (< three_sum_low three_sum_high) (= (nth three_sum_sorted three_sum_high) (nth three_sum_sorted (- three_sum_high 1)))) (set! three_sum_high (- three_sum_high 1))) (set! three_sum_low (+ three_sum_low 1)) (set! three_sum_high (- three_sum_high 1))) (if (< three_sum_s three_sum_c) (set! three_sum_low (+ three_sum_low 1)) (set! three_sum_high (- three_sum_high 1)))))))) (set! three_sum_i (+ three_sum_i 1)))) (throw (ex-info "return" {:v three_sum_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (three_sum [(- 1) 0 1 2 (- 1) (- 4)])))
      (println (str (three_sum [1 2 3 4])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
