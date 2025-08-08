(ns main (:refer-clojure :exclude [matrix_chain_multiply]))

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

(declare matrix_chain_multiply)

(def ^:dynamic matrix_chain_multiply_cost nil)

(def ^:dynamic matrix_chain_multiply_dp nil)

(def ^:dynamic matrix_chain_multiply_i nil)

(def ^:dynamic matrix_chain_multiply_j nil)

(def ^:dynamic matrix_chain_multiply_k nil)

(def ^:dynamic matrix_chain_multiply_n nil)

(def ^:dynamic matrix_chain_multiply_row nil)

(def ^:dynamic main_INF 1000000000)

(defn matrix_chain_multiply [matrix_chain_multiply_arr]
  (binding [matrix_chain_multiply_cost nil matrix_chain_multiply_dp nil matrix_chain_multiply_i nil matrix_chain_multiply_j nil matrix_chain_multiply_k nil matrix_chain_multiply_n nil matrix_chain_multiply_row nil] (try (do (when (< (count matrix_chain_multiply_arr) 2) (throw (ex-info "return" {:v 0}))) (set! matrix_chain_multiply_n (count matrix_chain_multiply_arr)) (set! matrix_chain_multiply_dp []) (set! matrix_chain_multiply_i 0) (while (< matrix_chain_multiply_i matrix_chain_multiply_n) (do (set! matrix_chain_multiply_row []) (set! matrix_chain_multiply_j 0) (while (< matrix_chain_multiply_j matrix_chain_multiply_n) (do (set! matrix_chain_multiply_row (conj matrix_chain_multiply_row main_INF)) (set! matrix_chain_multiply_j (+ matrix_chain_multiply_j 1)))) (set! matrix_chain_multiply_dp (conj matrix_chain_multiply_dp matrix_chain_multiply_row)) (set! matrix_chain_multiply_i (+ matrix_chain_multiply_i 1)))) (set! matrix_chain_multiply_i (- matrix_chain_multiply_n 1)) (while (> matrix_chain_multiply_i 0) (do (set! matrix_chain_multiply_j matrix_chain_multiply_i) (while (< matrix_chain_multiply_j matrix_chain_multiply_n) (do (if (= matrix_chain_multiply_i matrix_chain_multiply_j) (set! matrix_chain_multiply_dp (assoc-in matrix_chain_multiply_dp [matrix_chain_multiply_i matrix_chain_multiply_j] 0)) (do (set! matrix_chain_multiply_k matrix_chain_multiply_i) (while (< matrix_chain_multiply_k matrix_chain_multiply_j) (do (set! matrix_chain_multiply_cost (+ (+ (nth (nth matrix_chain_multiply_dp matrix_chain_multiply_i) matrix_chain_multiply_k) (nth (nth matrix_chain_multiply_dp (+ matrix_chain_multiply_k 1)) matrix_chain_multiply_j)) (* (* (nth matrix_chain_multiply_arr (- matrix_chain_multiply_i 1)) (nth matrix_chain_multiply_arr matrix_chain_multiply_k)) (nth matrix_chain_multiply_arr matrix_chain_multiply_j)))) (when (< matrix_chain_multiply_cost (nth (nth matrix_chain_multiply_dp matrix_chain_multiply_i) matrix_chain_multiply_j)) (set! matrix_chain_multiply_dp (assoc-in matrix_chain_multiply_dp [matrix_chain_multiply_i matrix_chain_multiply_j] matrix_chain_multiply_cost))) (set! matrix_chain_multiply_k (+ matrix_chain_multiply_k 1)))))) (set! matrix_chain_multiply_j (+ matrix_chain_multiply_j 1)))) (set! matrix_chain_multiply_i (- matrix_chain_multiply_i 1)))) (throw (ex-info "return" {:v (nth (nth matrix_chain_multiply_dp 1) (- matrix_chain_multiply_n 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
