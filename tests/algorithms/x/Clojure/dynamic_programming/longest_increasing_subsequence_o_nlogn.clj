(ns main (:refer-clojure :exclude [ceil_index longest_increasing_subsequence_length main]))

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

(declare ceil_index longest_increasing_subsequence_length main)

(def ^:dynamic ceil_index_l nil)

(def ^:dynamic ceil_index_middle nil)

(def ^:dynamic ceil_index_r nil)

(def ^:dynamic longest_increasing_subsequence_length_i nil)

(def ^:dynamic longest_increasing_subsequence_length_idx nil)

(def ^:dynamic longest_increasing_subsequence_length_j nil)

(def ^:dynamic longest_increasing_subsequence_length_length nil)

(def ^:dynamic longest_increasing_subsequence_length_tail nil)

(def ^:dynamic main_example1 nil)

(def ^:dynamic main_example2 nil)

(def ^:dynamic main_example3 nil)

(def ^:dynamic main_example4 nil)

(defn ceil_index [ceil_index_v ceil_index_left ceil_index_right ceil_index_key]
  (binding [ceil_index_l nil ceil_index_middle nil ceil_index_r nil] (try (do (set! ceil_index_l ceil_index_left) (set! ceil_index_r ceil_index_right) (while (> (- ceil_index_r ceil_index_l) 1) (do (set! ceil_index_middle (quot (+ ceil_index_l ceil_index_r) 2)) (if (>= (nth ceil_index_v ceil_index_middle) ceil_index_key) (set! ceil_index_r ceil_index_middle) (set! ceil_index_l ceil_index_middle)))) (throw (ex-info "return" {:v ceil_index_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn longest_increasing_subsequence_length [longest_increasing_subsequence_length_v]
  (binding [longest_increasing_subsequence_length_i nil longest_increasing_subsequence_length_idx nil longest_increasing_subsequence_length_j nil longest_increasing_subsequence_length_length nil longest_increasing_subsequence_length_tail nil] (try (do (when (= (count longest_increasing_subsequence_length_v) 0) (throw (ex-info "return" {:v 0}))) (set! longest_increasing_subsequence_length_tail []) (set! longest_increasing_subsequence_length_i 0) (while (< longest_increasing_subsequence_length_i (count longest_increasing_subsequence_length_v)) (do (set! longest_increasing_subsequence_length_tail (conj longest_increasing_subsequence_length_tail 0)) (set! longest_increasing_subsequence_length_i (+ longest_increasing_subsequence_length_i 1)))) (set! longest_increasing_subsequence_length_length 1) (set! longest_increasing_subsequence_length_tail (assoc longest_increasing_subsequence_length_tail 0 (nth longest_increasing_subsequence_length_v 0))) (set! longest_increasing_subsequence_length_j 1) (while (< longest_increasing_subsequence_length_j (count longest_increasing_subsequence_length_v)) (do (if (< (nth longest_increasing_subsequence_length_v longest_increasing_subsequence_length_j) (nth longest_increasing_subsequence_length_tail 0)) (set! longest_increasing_subsequence_length_tail (assoc longest_increasing_subsequence_length_tail 0 (nth longest_increasing_subsequence_length_v longest_increasing_subsequence_length_j))) (if (> (nth longest_increasing_subsequence_length_v longest_increasing_subsequence_length_j) (nth longest_increasing_subsequence_length_tail (- longest_increasing_subsequence_length_length 1))) (do (set! longest_increasing_subsequence_length_tail (assoc longest_increasing_subsequence_length_tail longest_increasing_subsequence_length_length (nth longest_increasing_subsequence_length_v longest_increasing_subsequence_length_j))) (set! longest_increasing_subsequence_length_length (+ longest_increasing_subsequence_length_length 1))) (do (set! longest_increasing_subsequence_length_idx (ceil_index longest_increasing_subsequence_length_tail (- 1) (- longest_increasing_subsequence_length_length 1) (nth longest_increasing_subsequence_length_v longest_increasing_subsequence_length_j))) (set! longest_increasing_subsequence_length_tail (assoc longest_increasing_subsequence_length_tail longest_increasing_subsequence_length_idx (nth longest_increasing_subsequence_length_v longest_increasing_subsequence_length_j)))))) (set! longest_increasing_subsequence_length_j (+ longest_increasing_subsequence_length_j 1)))) (throw (ex-info "return" {:v longest_increasing_subsequence_length_length}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_example1 nil main_example2 nil main_example3 nil main_example4 nil] (do (set! main_example1 [2 5 3 7 11 8 10 13 6]) (set! main_example2 []) (set! main_example3 [0 8 4 12 2 10 6 14 1 9 5 13 3 11 7 15]) (set! main_example4 [5 4 3 2 1]) (println (longest_increasing_subsequence_length main_example1)) (println (longest_increasing_subsequence_length main_example2)) (println (longest_increasing_subsequence_length main_example3)) (println (longest_increasing_subsequence_length main_example4)))))

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
