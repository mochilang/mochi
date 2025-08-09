(ns main (:refer-clojure :exclude [retroactive_resolution gaussian_elimination]))

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

(declare retroactive_resolution gaussian_elimination)

(def ^:dynamic gaussian_elimination_augmented nil)

(def ^:dynamic gaussian_elimination_c nil)

(def ^:dynamic gaussian_elimination_coeffs nil)

(def ^:dynamic gaussian_elimination_col nil)

(def ^:dynamic gaussian_elimination_columns nil)

(def ^:dynamic gaussian_elimination_factor nil)

(def ^:dynamic gaussian_elimination_i nil)

(def ^:dynamic gaussian_elimination_j nil)

(def ^:dynamic gaussian_elimination_k nil)

(def ^:dynamic gaussian_elimination_pivot nil)

(def ^:dynamic gaussian_elimination_r nil)

(def ^:dynamic gaussian_elimination_row nil)

(def ^:dynamic gaussian_elimination_row_idx nil)

(def ^:dynamic gaussian_elimination_rows nil)

(def ^:dynamic gaussian_elimination_vec nil)

(def ^:dynamic gaussian_elimination_x nil)

(def ^:dynamic retroactive_resolution_c nil)

(def ^:dynamic retroactive_resolution_i nil)

(def ^:dynamic retroactive_resolution_inner nil)

(def ^:dynamic retroactive_resolution_r nil)

(def ^:dynamic retroactive_resolution_rows nil)

(def ^:dynamic retroactive_resolution_total nil)

(def ^:dynamic retroactive_resolution_x nil)

(defn retroactive_resolution [retroactive_resolution_coefficients retroactive_resolution_vector]
  (binding [retroactive_resolution_c nil retroactive_resolution_i nil retroactive_resolution_inner nil retroactive_resolution_r nil retroactive_resolution_rows nil retroactive_resolution_total nil retroactive_resolution_x nil] (try (do (set! retroactive_resolution_rows (count retroactive_resolution_coefficients)) (set! retroactive_resolution_x []) (set! retroactive_resolution_i 0) (while (< retroactive_resolution_i retroactive_resolution_rows) (do (set! retroactive_resolution_inner []) (set! retroactive_resolution_inner (conj retroactive_resolution_inner 0.0)) (set! retroactive_resolution_x (conj retroactive_resolution_x retroactive_resolution_inner)) (set! retroactive_resolution_i (+ retroactive_resolution_i 1)))) (set! retroactive_resolution_r (- retroactive_resolution_rows 1)) (while (>= retroactive_resolution_r 0) (do (set! retroactive_resolution_total 0.0) (set! retroactive_resolution_c (+ retroactive_resolution_r 1)) (while (< retroactive_resolution_c retroactive_resolution_rows) (do (set! retroactive_resolution_total (+ retroactive_resolution_total (* (nth (nth retroactive_resolution_coefficients retroactive_resolution_r) retroactive_resolution_c) (nth (nth retroactive_resolution_x retroactive_resolution_c) 0)))) (set! retroactive_resolution_c (+ retroactive_resolution_c 1)))) (set! retroactive_resolution_x (assoc-in retroactive_resolution_x [retroactive_resolution_r 0] (quot (- (nth (nth retroactive_resolution_vector retroactive_resolution_r) 0) retroactive_resolution_total) (nth (nth retroactive_resolution_coefficients retroactive_resolution_r) retroactive_resolution_r)))) (set! retroactive_resolution_r (- retroactive_resolution_r 1)))) (throw (ex-info "return" {:v retroactive_resolution_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gaussian_elimination [gaussian_elimination_coefficients gaussian_elimination_vector]
  (binding [gaussian_elimination_augmented nil gaussian_elimination_c nil gaussian_elimination_coeffs nil gaussian_elimination_col nil gaussian_elimination_columns nil gaussian_elimination_factor nil gaussian_elimination_i nil gaussian_elimination_j nil gaussian_elimination_k nil gaussian_elimination_pivot nil gaussian_elimination_r nil gaussian_elimination_row nil gaussian_elimination_row_idx nil gaussian_elimination_rows nil gaussian_elimination_vec nil gaussian_elimination_x nil] (try (do (set! gaussian_elimination_rows (count gaussian_elimination_coefficients)) (set! gaussian_elimination_columns (count (nth gaussian_elimination_coefficients 0))) (when (not= gaussian_elimination_rows gaussian_elimination_columns) (throw (ex-info "return" {:v []}))) (set! gaussian_elimination_augmented []) (set! gaussian_elimination_i 0) (while (< gaussian_elimination_i gaussian_elimination_rows) (do (set! gaussian_elimination_row []) (set! gaussian_elimination_j 0) (while (< gaussian_elimination_j gaussian_elimination_columns) (do (set! gaussian_elimination_row (conj gaussian_elimination_row (nth (nth gaussian_elimination_coefficients gaussian_elimination_i) gaussian_elimination_j))) (set! gaussian_elimination_j (+ gaussian_elimination_j 1)))) (set! gaussian_elimination_row (conj gaussian_elimination_row (nth (nth gaussian_elimination_vector gaussian_elimination_i) 0))) (set! gaussian_elimination_augmented (conj gaussian_elimination_augmented gaussian_elimination_row)) (set! gaussian_elimination_i (+ gaussian_elimination_i 1)))) (set! gaussian_elimination_row_idx 0) (while (< gaussian_elimination_row_idx (- gaussian_elimination_rows 1)) (do (set! gaussian_elimination_pivot (nth (nth gaussian_elimination_augmented gaussian_elimination_row_idx) gaussian_elimination_row_idx)) (set! gaussian_elimination_col (+ gaussian_elimination_row_idx 1)) (while (< gaussian_elimination_col gaussian_elimination_rows) (do (set! gaussian_elimination_factor (quot (nth (nth gaussian_elimination_augmented gaussian_elimination_col) gaussian_elimination_row_idx) gaussian_elimination_pivot)) (set! gaussian_elimination_k gaussian_elimination_row_idx) (while (< gaussian_elimination_k (+ gaussian_elimination_columns 1)) (do (set! gaussian_elimination_augmented (assoc-in gaussian_elimination_augmented [gaussian_elimination_col gaussian_elimination_k] (- (nth (nth gaussian_elimination_augmented gaussian_elimination_col) gaussian_elimination_k) (* gaussian_elimination_factor (nth (nth gaussian_elimination_augmented gaussian_elimination_row_idx) gaussian_elimination_k))))) (set! gaussian_elimination_k (+ gaussian_elimination_k 1)))) (set! gaussian_elimination_col (+ gaussian_elimination_col 1)))) (set! gaussian_elimination_row_idx (+ gaussian_elimination_row_idx 1)))) (set! gaussian_elimination_coeffs []) (set! gaussian_elimination_vec []) (set! gaussian_elimination_r 0) (while (< gaussian_elimination_r gaussian_elimination_rows) (do (set! gaussian_elimination_row []) (set! gaussian_elimination_c 0) (while (< gaussian_elimination_c gaussian_elimination_columns) (do (set! gaussian_elimination_row (conj gaussian_elimination_row (nth (nth gaussian_elimination_augmented gaussian_elimination_r) gaussian_elimination_c))) (set! gaussian_elimination_c (+ gaussian_elimination_c 1)))) (set! gaussian_elimination_coeffs (conj gaussian_elimination_coeffs gaussian_elimination_row)) (set! gaussian_elimination_vec (conj gaussian_elimination_vec [(nth (nth gaussian_elimination_augmented gaussian_elimination_r) gaussian_elimination_columns)])) (set! gaussian_elimination_r (+ gaussian_elimination_r 1)))) (set! gaussian_elimination_x (retroactive_resolution gaussian_elimination_coeffs gaussian_elimination_vec)) (throw (ex-info "return" {:v gaussian_elimination_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (gaussian_elimination [[1.0 (- 4.0) (- 2.0)] [5.0 2.0 (- 2.0)] [1.0 (- 1.0) 0.0]] [[(- 2.0)] [(- 3.0)] [4.0]]))
      (println (gaussian_elimination [[1.0 2.0] [5.0 2.0]] [[5.0] [5.0]]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
