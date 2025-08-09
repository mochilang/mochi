(ns main (:refer-clojure :exclude [invert_matrix]))

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

(declare invert_matrix)

(def ^:dynamic invert_matrix_aug nil)

(def ^:dynamic invert_matrix_c nil)

(def ^:dynamic invert_matrix_c2 nil)

(def ^:dynamic invert_matrix_c3 nil)

(def ^:dynamic invert_matrix_col nil)

(def ^:dynamic invert_matrix_factor nil)

(def ^:dynamic invert_matrix_i nil)

(def ^:dynamic invert_matrix_inv nil)

(def ^:dynamic invert_matrix_j nil)

(def ^:dynamic invert_matrix_k nil)

(def ^:dynamic invert_matrix_n nil)

(def ^:dynamic invert_matrix_pivot nil)

(def ^:dynamic invert_matrix_pivot_row nil)

(def ^:dynamic invert_matrix_r nil)

(def ^:dynamic invert_matrix_r2 nil)

(def ^:dynamic invert_matrix_r3 nil)

(def ^:dynamic invert_matrix_row nil)

(def ^:dynamic invert_matrix_temp nil)

(defn invert_matrix [invert_matrix_matrix]
  (binding [invert_matrix_aug nil invert_matrix_c nil invert_matrix_c2 nil invert_matrix_c3 nil invert_matrix_col nil invert_matrix_factor nil invert_matrix_i nil invert_matrix_inv nil invert_matrix_j nil invert_matrix_k nil invert_matrix_n nil invert_matrix_pivot nil invert_matrix_pivot_row nil invert_matrix_r nil invert_matrix_r2 nil invert_matrix_r3 nil invert_matrix_row nil invert_matrix_temp nil] (try (do (set! invert_matrix_n (count invert_matrix_matrix)) (set! invert_matrix_aug []) (set! invert_matrix_i 0) (while (< invert_matrix_i invert_matrix_n) (do (set! invert_matrix_row []) (set! invert_matrix_j 0) (while (< invert_matrix_j invert_matrix_n) (do (set! invert_matrix_row (conj invert_matrix_row (nth (nth invert_matrix_matrix invert_matrix_i) invert_matrix_j))) (set! invert_matrix_j (+ invert_matrix_j 1)))) (set! invert_matrix_k 0) (while (< invert_matrix_k invert_matrix_n) (do (if (= invert_matrix_i invert_matrix_k) (set! invert_matrix_row (conj invert_matrix_row 1.0)) (set! invert_matrix_row (conj invert_matrix_row 0.0))) (set! invert_matrix_k (+ invert_matrix_k 1)))) (set! invert_matrix_aug (conj invert_matrix_aug invert_matrix_row)) (set! invert_matrix_i (+ invert_matrix_i 1)))) (set! invert_matrix_col 0) (while (< invert_matrix_col invert_matrix_n) (do (set! invert_matrix_pivot_row invert_matrix_col) (set! invert_matrix_r invert_matrix_col) (loop [while_flag_1 true] (when (and while_flag_1 (< invert_matrix_r invert_matrix_n)) (cond (not= (nth (nth invert_matrix_aug invert_matrix_r) invert_matrix_col) 0.0) (do (set! invert_matrix_pivot_row invert_matrix_r) (recur false)) :else (do (set! invert_matrix_r (+ invert_matrix_r 1)) (recur while_flag_1))))) (when (= (nth (nth invert_matrix_aug invert_matrix_pivot_row) invert_matrix_col) 0.0) (throw (Exception. "Matrix is not invertible"))) (when (not= invert_matrix_pivot_row invert_matrix_col) (do (set! invert_matrix_temp (nth invert_matrix_aug invert_matrix_col)) (set! invert_matrix_aug (assoc invert_matrix_aug invert_matrix_col (nth invert_matrix_aug invert_matrix_pivot_row))) (set! invert_matrix_aug (assoc invert_matrix_aug invert_matrix_pivot_row invert_matrix_temp)))) (set! invert_matrix_pivot (nth (nth invert_matrix_aug invert_matrix_col) invert_matrix_col)) (set! invert_matrix_c 0) (while (< invert_matrix_c (* 2 invert_matrix_n)) (do (set! invert_matrix_aug (assoc-in invert_matrix_aug [invert_matrix_col invert_matrix_c] (quot (nth (nth invert_matrix_aug invert_matrix_col) invert_matrix_c) invert_matrix_pivot))) (set! invert_matrix_c (+ invert_matrix_c 1)))) (set! invert_matrix_r2 0) (while (< invert_matrix_r2 invert_matrix_n) (do (when (not= invert_matrix_r2 invert_matrix_col) (do (set! invert_matrix_factor (nth (nth invert_matrix_aug invert_matrix_r2) invert_matrix_col)) (set! invert_matrix_c2 0) (while (< invert_matrix_c2 (* 2 invert_matrix_n)) (do (set! invert_matrix_aug (assoc-in invert_matrix_aug [invert_matrix_r2 invert_matrix_c2] (- (nth (nth invert_matrix_aug invert_matrix_r2) invert_matrix_c2) (* invert_matrix_factor (nth (nth invert_matrix_aug invert_matrix_col) invert_matrix_c2))))) (set! invert_matrix_c2 (+ invert_matrix_c2 1)))))) (set! invert_matrix_r2 (+ invert_matrix_r2 1)))) (set! invert_matrix_col (+ invert_matrix_col 1)))) (set! invert_matrix_inv []) (set! invert_matrix_r3 0) (while (< invert_matrix_r3 invert_matrix_n) (do (set! invert_matrix_row []) (set! invert_matrix_c3 0) (while (< invert_matrix_c3 invert_matrix_n) (do (set! invert_matrix_row (conj invert_matrix_row (nth (nth invert_matrix_aug invert_matrix_r3) (+ invert_matrix_c3 invert_matrix_n)))) (set! invert_matrix_c3 (+ invert_matrix_c3 1)))) (set! invert_matrix_inv (conj invert_matrix_inv invert_matrix_row)) (set! invert_matrix_r3 (+ invert_matrix_r3 1)))) (throw (ex-info "return" {:v invert_matrix_inv}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_mat [[4.0 7.0] [2.0 6.0]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Original Matrix:")
      (println main_mat)
      (println "Inverted Matrix:")
      (println (invert_matrix main_mat))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
