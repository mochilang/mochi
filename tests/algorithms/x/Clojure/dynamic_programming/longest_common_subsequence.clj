(ns main (:refer-clojure :exclude [zeros_matrix longest_common_subsequence]))

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

(declare zeros_matrix longest_common_subsequence)

(def ^:dynamic longest_common_subsequence_dp nil)

(def ^:dynamic longest_common_subsequence_i nil)

(def ^:dynamic longest_common_subsequence_i2 nil)

(def ^:dynamic longest_common_subsequence_j nil)

(def ^:dynamic longest_common_subsequence_j2 nil)

(def ^:dynamic longest_common_subsequence_m nil)

(def ^:dynamic longest_common_subsequence_n nil)

(def ^:dynamic longest_common_subsequence_seq nil)

(def ^:dynamic zeros_matrix_i nil)

(def ^:dynamic zeros_matrix_j nil)

(def ^:dynamic zeros_matrix_matrix nil)

(def ^:dynamic zeros_matrix_row nil)

(defn zeros_matrix [zeros_matrix_rows zeros_matrix_cols]
  (binding [zeros_matrix_i nil zeros_matrix_j nil zeros_matrix_matrix nil zeros_matrix_row nil] (try (do (set! zeros_matrix_matrix []) (set! zeros_matrix_i 0) (while (<= zeros_matrix_i zeros_matrix_rows) (do (set! zeros_matrix_row []) (set! zeros_matrix_j 0) (while (<= zeros_matrix_j zeros_matrix_cols) (do (set! zeros_matrix_row (conj zeros_matrix_row 0)) (set! zeros_matrix_j (+ zeros_matrix_j 1)))) (set! zeros_matrix_matrix (conj zeros_matrix_matrix zeros_matrix_row)) (set! zeros_matrix_i (+ zeros_matrix_i 1)))) (throw (ex-info "return" {:v zeros_matrix_matrix}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn longest_common_subsequence [longest_common_subsequence_x longest_common_subsequence_y]
  (binding [longest_common_subsequence_dp nil longest_common_subsequence_i nil longest_common_subsequence_i2 nil longest_common_subsequence_j nil longest_common_subsequence_j2 nil longest_common_subsequence_m nil longest_common_subsequence_n nil longest_common_subsequence_seq nil] (try (do (set! longest_common_subsequence_m (count longest_common_subsequence_x)) (set! longest_common_subsequence_n (count longest_common_subsequence_y)) (set! longest_common_subsequence_dp (zeros_matrix longest_common_subsequence_m longest_common_subsequence_n)) (set! longest_common_subsequence_i 1) (while (<= longest_common_subsequence_i longest_common_subsequence_m) (do (set! longest_common_subsequence_j 1) (while (<= longest_common_subsequence_j longest_common_subsequence_n) (do (if (= (nth longest_common_subsequence_x (- longest_common_subsequence_i 1)) (nth longest_common_subsequence_y (- longest_common_subsequence_j 1))) (set! longest_common_subsequence_dp (assoc-in longest_common_subsequence_dp [longest_common_subsequence_i longest_common_subsequence_j] (+ (nth (nth longest_common_subsequence_dp (- longest_common_subsequence_i 1)) (- longest_common_subsequence_j 1)) 1))) (if (> (nth (nth longest_common_subsequence_dp (- longest_common_subsequence_i 1)) longest_common_subsequence_j) (nth (nth longest_common_subsequence_dp longest_common_subsequence_i) (- longest_common_subsequence_j 1))) (set! longest_common_subsequence_dp (assoc-in longest_common_subsequence_dp [longest_common_subsequence_i longest_common_subsequence_j] (nth (nth longest_common_subsequence_dp (- longest_common_subsequence_i 1)) longest_common_subsequence_j))) (set! longest_common_subsequence_dp (assoc-in longest_common_subsequence_dp [longest_common_subsequence_i longest_common_subsequence_j] (nth (nth longest_common_subsequence_dp longest_common_subsequence_i) (- longest_common_subsequence_j 1)))))) (set! longest_common_subsequence_j (+ longest_common_subsequence_j 1)))) (set! longest_common_subsequence_i (+ longest_common_subsequence_i 1)))) (set! longest_common_subsequence_seq "") (set! longest_common_subsequence_i2 longest_common_subsequence_m) (set! longest_common_subsequence_j2 longest_common_subsequence_n) (while (and (> longest_common_subsequence_i2 0) (> longest_common_subsequence_j2 0)) (if (= (nth longest_common_subsequence_x (- longest_common_subsequence_i2 1)) (nth longest_common_subsequence_y (- longest_common_subsequence_j2 1))) (do (set! longest_common_subsequence_seq (str (nth longest_common_subsequence_x (- longest_common_subsequence_i2 1)) longest_common_subsequence_seq)) (set! longest_common_subsequence_i2 (- longest_common_subsequence_i2 1)) (set! longest_common_subsequence_j2 (- longest_common_subsequence_j2 1))) (if (>= (nth (nth longest_common_subsequence_dp (- longest_common_subsequence_i2 1)) longest_common_subsequence_j2) (nth (nth longest_common_subsequence_dp longest_common_subsequence_i2) (- longest_common_subsequence_j2 1))) (set! longest_common_subsequence_i2 (- longest_common_subsequence_i2 1)) (set! longest_common_subsequence_j2 (- longest_common_subsequence_j2 1))))) (throw (ex-info "return" {:v {:length (nth (nth longest_common_subsequence_dp longest_common_subsequence_m) longest_common_subsequence_n) :sequence longest_common_subsequence_seq}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_a "AGGTAB")

(def ^:dynamic main_b "GXTXAYB")

(def ^:dynamic main_res (longest_common_subsequence main_a main_b))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (str (str "len = " (str (:length main_res))) ", sub-sequence = ") (:sequence main_res)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
