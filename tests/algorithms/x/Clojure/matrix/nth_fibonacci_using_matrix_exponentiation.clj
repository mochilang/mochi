(ns main (:refer-clojure :exclude [multiply identity nth_fibonacci_matrix nth_fibonacci_bruteforce parse_number main]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare multiply identity nth_fibonacci_matrix nth_fibonacci_bruteforce parse_number main)

(def ^:dynamic identity_i nil)

(def ^:dynamic identity_j nil)

(def ^:dynamic identity_res nil)

(def ^:dynamic identity_row nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_msg nil)

(def ^:dynamic main_n nil)

(def ^:dynamic main_ordinal nil)

(def ^:dynamic main_ordinals nil)

(def ^:dynamic multiply_i nil)

(def ^:dynamic multiply_j nil)

(def ^:dynamic multiply_k nil)

(def ^:dynamic multiply_matrix_c nil)

(def ^:dynamic multiply_n nil)

(def ^:dynamic multiply_row nil)

(def ^:dynamic multiply_val nil)

(def ^:dynamic next_v nil)

(def ^:dynamic nth_fibonacci_bruteforce_fib0 nil)

(def ^:dynamic nth_fibonacci_bruteforce_fib1 nil)

(def ^:dynamic nth_fibonacci_bruteforce_i nil)

(def ^:dynamic nth_fibonacci_matrix_fib_matrix nil)

(def ^:dynamic nth_fibonacci_matrix_m nil)

(def ^:dynamic nth_fibonacci_matrix_res_matrix nil)

(def ^:dynamic parse_number_ch nil)

(def ^:dynamic parse_number_i nil)

(def ^:dynamic parse_number_result nil)

(defn multiply [multiply_matrix_a multiply_matrix_b]
  (binding [multiply_i nil multiply_j nil multiply_k nil multiply_matrix_c nil multiply_n nil multiply_row nil multiply_val nil] (try (do (set! multiply_n (count multiply_matrix_a)) (set! multiply_matrix_c []) (set! multiply_i 0) (while (< multiply_i multiply_n) (do (set! multiply_row []) (set! multiply_j 0) (while (< multiply_j multiply_n) (do (set! multiply_val 0) (set! multiply_k 0) (while (< multiply_k multiply_n) (do (set! multiply_val (+ multiply_val (* (nth (nth multiply_matrix_a multiply_i) multiply_k) (nth (nth multiply_matrix_b multiply_k) multiply_j)))) (set! multiply_k (+ multiply_k 1)))) (set! multiply_row (conj multiply_row multiply_val)) (set! multiply_j (+ multiply_j 1)))) (set! multiply_matrix_c (conj multiply_matrix_c multiply_row)) (set! multiply_i (+ multiply_i 1)))) (throw (ex-info "return" {:v multiply_matrix_c}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn identity [identity_n]
  (binding [identity_i nil identity_j nil identity_res nil identity_row nil] (try (do (set! identity_res []) (set! identity_i 0) (while (< identity_i identity_n) (do (set! identity_row []) (set! identity_j 0) (while (< identity_j identity_n) (do (if (= identity_i identity_j) (set! identity_row (conj identity_row 1)) (set! identity_row (conj identity_row 0))) (set! identity_j (+ identity_j 1)))) (set! identity_res (conj identity_res identity_row)) (set! identity_i (+ identity_i 1)))) (throw (ex-info "return" {:v identity_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn nth_fibonacci_matrix [nth_fibonacci_matrix_n]
  (binding [nth_fibonacci_matrix_fib_matrix nil nth_fibonacci_matrix_m nil nth_fibonacci_matrix_res_matrix nil] (try (do (when (<= nth_fibonacci_matrix_n 1) (throw (ex-info "return" {:v nth_fibonacci_matrix_n}))) (set! nth_fibonacci_matrix_res_matrix (identity 2)) (set! nth_fibonacci_matrix_fib_matrix [[1 1] [1 0]]) (set! nth_fibonacci_matrix_m (- nth_fibonacci_matrix_n 1)) (while (> nth_fibonacci_matrix_m 0) (do (when (= (mod nth_fibonacci_matrix_m 2) 1) (set! nth_fibonacci_matrix_res_matrix (multiply nth_fibonacci_matrix_res_matrix nth_fibonacci_matrix_fib_matrix))) (set! nth_fibonacci_matrix_fib_matrix (multiply nth_fibonacci_matrix_fib_matrix nth_fibonacci_matrix_fib_matrix)) (set! nth_fibonacci_matrix_m (/ nth_fibonacci_matrix_m 2)))) (throw (ex-info "return" {:v (nth (nth nth_fibonacci_matrix_res_matrix 0) 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn nth_fibonacci_bruteforce [nth_fibonacci_bruteforce_n]
  (binding [next_v nil nth_fibonacci_bruteforce_fib0 nil nth_fibonacci_bruteforce_fib1 nil nth_fibonacci_bruteforce_i nil] (try (do (when (<= nth_fibonacci_bruteforce_n 1) (throw (ex-info "return" {:v nth_fibonacci_bruteforce_n}))) (set! nth_fibonacci_bruteforce_fib0 0) (set! nth_fibonacci_bruteforce_fib1 1) (set! nth_fibonacci_bruteforce_i 2) (while (<= nth_fibonacci_bruteforce_i nth_fibonacci_bruteforce_n) (do (set! next_v (+ nth_fibonacci_bruteforce_fib0 nth_fibonacci_bruteforce_fib1)) (set! nth_fibonacci_bruteforce_fib0 nth_fibonacci_bruteforce_fib1) (set! nth_fibonacci_bruteforce_fib1 next_v) (set! nth_fibonacci_bruteforce_i (+ nth_fibonacci_bruteforce_i 1)))) (throw (ex-info "return" {:v nth_fibonacci_bruteforce_fib1}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn parse_number [parse_number_s]
  (binding [parse_number_ch nil parse_number_i nil parse_number_result nil] (try (do (set! parse_number_result 0) (set! parse_number_i 0) (while (< parse_number_i (count parse_number_s)) (do (set! parse_number_ch (subs parse_number_s parse_number_i (min (+ parse_number_i 1) (count parse_number_s)))) (when (and (>= (compare parse_number_ch "0") 0) (<= (compare parse_number_ch "9") 0)) (set! parse_number_result (+ (* parse_number_result 10) (Long/parseLong parse_number_ch)))) (set! parse_number_i (+ parse_number_i 1)))) (throw (ex-info "return" {:v parse_number_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_i nil main_msg nil main_n nil main_ordinal nil main_ordinals nil] (do (set! main_ordinals ["0th" "1st" "2nd" "3rd" "10th" "100th" "1000th"]) (set! main_i 0) (while (< main_i (count main_ordinals)) (do (set! main_ordinal (nth main_ordinals main_i)) (set! main_n (parse_number main_ordinal)) (set! main_msg (str (str (str (str main_ordinal " fibonacci number using matrix exponentiation is ") (str (nth_fibonacci_matrix main_n))) " and using bruteforce is ") (str (nth_fibonacci_bruteforce main_n)))) (println main_msg) (set! main_i (+ main_i 1)))))))

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
