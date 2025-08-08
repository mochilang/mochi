(ns main (:refer-clojure :exclude [is_digit slice_without_last_int slice_without_last_string dijkstras_two_stack_algorithm]))

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

(declare is_digit slice_without_last_int slice_without_last_string dijkstras_two_stack_algorithm)

(def ^:dynamic dijkstras_two_stack_algorithm_ch nil)

(def ^:dynamic dijkstras_two_stack_algorithm_idx nil)

(def ^:dynamic dijkstras_two_stack_algorithm_num1 nil)

(def ^:dynamic dijkstras_two_stack_algorithm_num2 nil)

(def ^:dynamic dijkstras_two_stack_algorithm_operand_stack nil)

(def ^:dynamic dijkstras_two_stack_algorithm_operator_stack nil)

(def ^:dynamic dijkstras_two_stack_algorithm_opr nil)

(def ^:dynamic dijkstras_two_stack_algorithm_total nil)

(def ^:dynamic slice_without_last_int_i nil)

(def ^:dynamic slice_without_last_int_res nil)

(def ^:dynamic slice_without_last_string_i nil)

(def ^:dynamic slice_without_last_string_res nil)

(defn is_digit [is_digit_ch]
  (try (throw (ex-info "return" {:v (or (or (or (or (or (or (or (or (or (= is_digit_ch "0") (= is_digit_ch "1")) (= is_digit_ch "2")) (= is_digit_ch "3")) (= is_digit_ch "4")) (= is_digit_ch "5")) (= is_digit_ch "6")) (= is_digit_ch "7")) (= is_digit_ch "8")) (= is_digit_ch "9"))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn slice_without_last_int [slice_without_last_int_xs]
  (binding [slice_without_last_int_i nil slice_without_last_int_res nil] (try (do (set! slice_without_last_int_res []) (set! slice_without_last_int_i 0) (while (< slice_without_last_int_i (- (count slice_without_last_int_xs) 1)) (do (set! slice_without_last_int_res (conj slice_without_last_int_res (nth slice_without_last_int_xs slice_without_last_int_i))) (set! slice_without_last_int_i (+ slice_without_last_int_i 1)))) (throw (ex-info "return" {:v slice_without_last_int_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn slice_without_last_string [slice_without_last_string_xs]
  (binding [slice_without_last_string_i nil slice_without_last_string_res nil] (try (do (set! slice_without_last_string_res []) (set! slice_without_last_string_i 0) (while (< slice_without_last_string_i (- (count slice_without_last_string_xs) 1)) (do (set! slice_without_last_string_res (conj slice_without_last_string_res (nth slice_without_last_string_xs slice_without_last_string_i))) (set! slice_without_last_string_i (+ slice_without_last_string_i 1)))) (throw (ex-info "return" {:v slice_without_last_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dijkstras_two_stack_algorithm [dijkstras_two_stack_algorithm_equation]
  (binding [dijkstras_two_stack_algorithm_ch nil dijkstras_two_stack_algorithm_idx nil dijkstras_two_stack_algorithm_num1 nil dijkstras_two_stack_algorithm_num2 nil dijkstras_two_stack_algorithm_operand_stack nil dijkstras_two_stack_algorithm_operator_stack nil dijkstras_two_stack_algorithm_opr nil dijkstras_two_stack_algorithm_total nil] (try (do (set! dijkstras_two_stack_algorithm_operand_stack []) (set! dijkstras_two_stack_algorithm_operator_stack []) (set! dijkstras_two_stack_algorithm_idx 0) (while (< dijkstras_two_stack_algorithm_idx (count dijkstras_two_stack_algorithm_equation)) (do (set! dijkstras_two_stack_algorithm_ch (subs dijkstras_two_stack_algorithm_equation dijkstras_two_stack_algorithm_idx (min (+ dijkstras_two_stack_algorithm_idx 1) (count dijkstras_two_stack_algorithm_equation)))) (if (is_digit dijkstras_two_stack_algorithm_ch) (set! dijkstras_two_stack_algorithm_operand_stack (conj dijkstras_two_stack_algorithm_operand_stack (Integer/parseInt dijkstras_two_stack_algorithm_ch))) (if (or (or (or (= dijkstras_two_stack_algorithm_ch "+") (= dijkstras_two_stack_algorithm_ch "-")) (= dijkstras_two_stack_algorithm_ch "*")) (= dijkstras_two_stack_algorithm_ch "/")) (set! dijkstras_two_stack_algorithm_operator_stack (conj dijkstras_two_stack_algorithm_operator_stack dijkstras_two_stack_algorithm_ch)) (when (= dijkstras_two_stack_algorithm_ch ")") (do (set! dijkstras_two_stack_algorithm_opr (nth dijkstras_two_stack_algorithm_operator_stack (- (count dijkstras_two_stack_algorithm_operator_stack) 1))) (set! dijkstras_two_stack_algorithm_operator_stack (slice_without_last_string dijkstras_two_stack_algorithm_operator_stack)) (set! dijkstras_two_stack_algorithm_num1 (nth dijkstras_two_stack_algorithm_operand_stack (- (count dijkstras_two_stack_algorithm_operand_stack) 1))) (set! dijkstras_two_stack_algorithm_operand_stack (slice_without_last_int dijkstras_two_stack_algorithm_operand_stack)) (set! dijkstras_two_stack_algorithm_num2 (nth dijkstras_two_stack_algorithm_operand_stack (- (count dijkstras_two_stack_algorithm_operand_stack) 1))) (set! dijkstras_two_stack_algorithm_operand_stack (slice_without_last_int dijkstras_two_stack_algorithm_operand_stack)) (set! dijkstras_two_stack_algorithm_total (if (= dijkstras_two_stack_algorithm_opr "+") (+ dijkstras_two_stack_algorithm_num2 dijkstras_two_stack_algorithm_num1) (if (= dijkstras_two_stack_algorithm_opr "-") (- dijkstras_two_stack_algorithm_num2 dijkstras_two_stack_algorithm_num1) (if (= dijkstras_two_stack_algorithm_opr "*") (* dijkstras_two_stack_algorithm_num2 dijkstras_two_stack_algorithm_num1) (quot dijkstras_two_stack_algorithm_num2 dijkstras_two_stack_algorithm_num1))))) (set! dijkstras_two_stack_algorithm_operand_stack (conj dijkstras_two_stack_algorithm_operand_stack dijkstras_two_stack_algorithm_total)))))) (set! dijkstras_two_stack_algorithm_idx (+ dijkstras_two_stack_algorithm_idx 1)))) (throw (ex-info "return" {:v (nth dijkstras_two_stack_algorithm_operand_stack (- (count dijkstras_two_stack_algorithm_operand_stack) 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_equation "(5 + ((4 * 2) * (2 + 3)))")

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (str main_equation " = ") (str (dijkstras_two_stack_algorithm main_equation))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
