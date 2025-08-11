(ns main (:refer-clojure :exclude [make_2d matrix_chain_order optimal_parenthesization main]))

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

(declare make_2d matrix_chain_order optimal_parenthesization main)

(def ^:dynamic main_arr nil)

(def ^:dynamic main_m nil)

(def ^:dynamic main_n nil)

(def ^:dynamic main_res nil)

(def ^:dynamic main_s nil)

(def ^:dynamic main_seq nil)

(def ^:dynamic make_2d_i nil)

(def ^:dynamic make_2d_j nil)

(def ^:dynamic make_2d_res nil)

(def ^:dynamic make_2d_row nil)

(def ^:dynamic matrix_chain_order_a nil)

(def ^:dynamic matrix_chain_order_b nil)

(def ^:dynamic matrix_chain_order_c nil)

(def ^:dynamic matrix_chain_order_chain_length nil)

(def ^:dynamic matrix_chain_order_cost nil)

(def ^:dynamic matrix_chain_order_m nil)

(def ^:dynamic matrix_chain_order_n nil)

(def ^:dynamic matrix_chain_order_s nil)

(def ^:dynamic optimal_parenthesization_left nil)

(def ^:dynamic optimal_parenthesization_right nil)

(defn make_2d [make_2d_n]
  (binding [make_2d_i nil make_2d_j nil make_2d_res nil make_2d_row nil] (try (do (set! make_2d_res []) (set! make_2d_i 0) (while (< make_2d_i make_2d_n) (do (set! make_2d_row []) (set! make_2d_j 0) (while (< make_2d_j make_2d_n) (do (set! make_2d_row (conj make_2d_row 0)) (set! make_2d_j (+ make_2d_j 1)))) (set! make_2d_res (conj make_2d_res make_2d_row)) (set! make_2d_i (+ make_2d_i 1)))) (throw (ex-info "return" {:v make_2d_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn matrix_chain_order [matrix_chain_order_arr]
  (binding [matrix_chain_order_a nil matrix_chain_order_b nil matrix_chain_order_c nil matrix_chain_order_chain_length nil matrix_chain_order_cost nil matrix_chain_order_m nil matrix_chain_order_n nil matrix_chain_order_s nil] (try (do (set! matrix_chain_order_n (count matrix_chain_order_arr)) (set! matrix_chain_order_m (make_2d matrix_chain_order_n)) (set! matrix_chain_order_s (make_2d matrix_chain_order_n)) (set! matrix_chain_order_chain_length 2) (while (< matrix_chain_order_chain_length matrix_chain_order_n) (do (set! matrix_chain_order_a 1) (while (< matrix_chain_order_a (+ (- matrix_chain_order_n matrix_chain_order_chain_length) 1)) (do (set! matrix_chain_order_b (- (+ matrix_chain_order_a matrix_chain_order_chain_length) 1)) (set! matrix_chain_order_m (assoc-in matrix_chain_order_m [matrix_chain_order_a matrix_chain_order_b] 1000000000)) (set! matrix_chain_order_c matrix_chain_order_a) (while (< matrix_chain_order_c matrix_chain_order_b) (do (set! matrix_chain_order_cost (+ (+ (nth (nth matrix_chain_order_m matrix_chain_order_a) matrix_chain_order_c) (nth (nth matrix_chain_order_m (+ matrix_chain_order_c 1)) matrix_chain_order_b)) (* (* (nth matrix_chain_order_arr (- matrix_chain_order_a 1)) (nth matrix_chain_order_arr matrix_chain_order_c)) (nth matrix_chain_order_arr matrix_chain_order_b)))) (when (< matrix_chain_order_cost (nth (nth matrix_chain_order_m matrix_chain_order_a) matrix_chain_order_b)) (do (set! matrix_chain_order_m (assoc-in matrix_chain_order_m [matrix_chain_order_a matrix_chain_order_b] matrix_chain_order_cost)) (set! matrix_chain_order_s (assoc-in matrix_chain_order_s [matrix_chain_order_a matrix_chain_order_b] matrix_chain_order_c)))) (set! matrix_chain_order_c (+ matrix_chain_order_c 1)))) (set! matrix_chain_order_a (+ matrix_chain_order_a 1)))) (set! matrix_chain_order_chain_length (+ matrix_chain_order_chain_length 1)))) (throw (ex-info "return" {:v {:matrix matrix_chain_order_m :solution matrix_chain_order_s}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn optimal_parenthesization [optimal_parenthesization_s optimal_parenthesization_i optimal_parenthesization_j]
  (binding [optimal_parenthesization_left nil optimal_parenthesization_right nil] (try (if (= optimal_parenthesization_i optimal_parenthesization_j) (throw (ex-info "return" {:v (str "A" (str optimal_parenthesization_i))})) (do (set! optimal_parenthesization_left (optimal_parenthesization optimal_parenthesization_s optimal_parenthesization_i (nth (nth optimal_parenthesization_s optimal_parenthesization_i) optimal_parenthesization_j))) (set! optimal_parenthesization_right (optimal_parenthesization optimal_parenthesization_s (+ (nth (nth optimal_parenthesization_s optimal_parenthesization_i) optimal_parenthesization_j) 1) optimal_parenthesization_j)) (throw (ex-info "return" {:v (str (str (str (str "( " optimal_parenthesization_left) " ") optimal_parenthesization_right) " )")})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_arr nil main_m nil main_n nil main_res nil main_s nil main_seq nil] (do (set! main_arr [30 35 15 5 10 20 25]) (set! main_n (count main_arr)) (set! main_res (matrix_chain_order main_arr)) (set! main_m (:matrix main_res)) (set! main_s (:solution main_res)) (println (str "No. of Operation required: " (str (nth (get main_m 1) (- main_n 1))))) (set! main_seq (optimal_parenthesization main_s 1 (- main_n 1))) (println main_seq))))

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
