(ns main (:refer-clojure :exclude [split_custom tokenize is_digit is_operand to_int apply_op evaluate eval_rec evaluate_recursive]))

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

(declare split_custom tokenize is_digit is_operand to_int apply_op evaluate eval_rec evaluate_recursive)

(def ^:dynamic eval_rec_a nil)

(def ^:dynamic eval_rec_b nil)

(def ^:dynamic eval_rec_left nil)

(def ^:dynamic eval_rec_p1 nil)

(def ^:dynamic eval_rec_p2 nil)

(def ^:dynamic eval_rec_right nil)

(def ^:dynamic eval_rec_token nil)

(def ^:dynamic evaluate_i nil)

(def ^:dynamic evaluate_o1 nil)

(def ^:dynamic evaluate_o2 nil)

(def ^:dynamic evaluate_recursive_res nil)

(def ^:dynamic evaluate_recursive_tokens nil)

(def ^:dynamic evaluate_res nil)

(def ^:dynamic evaluate_stack nil)

(def ^:dynamic evaluate_token nil)

(def ^:dynamic evaluate_tokens nil)

(def ^:dynamic is_operand_ch nil)

(def ^:dynamic is_operand_i nil)

(def ^:dynamic next_v nil)

(def ^:dynamic split_custom_ch nil)

(def ^:dynamic split_custom_current nil)

(def ^:dynamic split_custom_i nil)

(def ^:dynamic split_custom_res nil)

(def ^:dynamic to_int_i nil)

(def ^:dynamic to_int_res nil)

(def ^:dynamic tokenize_i nil)

(def ^:dynamic tokenize_p nil)

(def ^:dynamic tokenize_parts nil)

(def ^:dynamic tokenize_res nil)

(defn split_custom [split_custom_s split_custom_sep]
  (binding [split_custom_ch nil split_custom_current nil split_custom_i nil split_custom_res nil] (try (do (set! split_custom_res []) (set! split_custom_current "") (set! split_custom_i 0) (while (< split_custom_i (count split_custom_s)) (do (set! split_custom_ch (subs split_custom_s split_custom_i (min (+ split_custom_i 1) (count split_custom_s)))) (if (= split_custom_ch split_custom_sep) (do (set! split_custom_res (conj split_custom_res split_custom_current)) (set! split_custom_current "")) (set! split_custom_current (str split_custom_current split_custom_ch))) (set! split_custom_i (+ split_custom_i 1)))) (set! split_custom_res (conj split_custom_res split_custom_current)) (throw (ex-info "return" {:v split_custom_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn tokenize [tokenize_s]
  (binding [tokenize_i nil tokenize_p nil tokenize_parts nil tokenize_res nil] (try (do (set! tokenize_parts (split_custom tokenize_s " ")) (set! tokenize_res []) (set! tokenize_i 0) (while (< tokenize_i (count tokenize_parts)) (do (set! tokenize_p (nth tokenize_parts tokenize_i)) (when (not= tokenize_p "") (set! tokenize_res (conj tokenize_res tokenize_p))) (set! tokenize_i (+ tokenize_i 1)))) (throw (ex-info "return" {:v tokenize_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_digit [is_digit_ch]
  (try (throw (ex-info "return" {:v (and (>= (compare is_digit_ch "0") 0) (<= (compare is_digit_ch "9") 0))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_operand [is_operand_token]
  (binding [is_operand_ch nil is_operand_i nil] (try (do (when (= is_operand_token "") (throw (ex-info "return" {:v false}))) (set! is_operand_i 0) (while (< is_operand_i (count is_operand_token)) (do (set! is_operand_ch (subs is_operand_token is_operand_i (min (+ is_operand_i 1) (count is_operand_token)))) (when (not (is_digit is_operand_ch)) (throw (ex-info "return" {:v false}))) (set! is_operand_i (+ is_operand_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_int [to_int_token]
  (binding [to_int_i nil to_int_res nil] (try (do (set! to_int_res 0) (set! to_int_i 0) (while (< to_int_i (count to_int_token)) (do (set! to_int_res (+ (* to_int_res 10) (Long/parseLong (subs to_int_token to_int_i (min (+ to_int_i 1) (count to_int_token)))))) (set! to_int_i (+ to_int_i 1)))) (throw (ex-info "return" {:v to_int_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn apply_op [apply_op_op apply_op_a apply_op_b]
  (try (do (when (= apply_op_op "+") (throw (ex-info "return" {:v (+ apply_op_a apply_op_b)}))) (when (= apply_op_op "-") (throw (ex-info "return" {:v (- apply_op_a apply_op_b)}))) (when (= apply_op_op "*") (throw (ex-info "return" {:v (* apply_op_a apply_op_b)}))) (if (= apply_op_op "/") (quot apply_op_a apply_op_b) 0.0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn evaluate [evaluate_expression]
  (binding [evaluate_i nil evaluate_o1 nil evaluate_o2 nil evaluate_res nil evaluate_stack nil evaluate_token nil evaluate_tokens nil] (try (do (set! evaluate_tokens (tokenize evaluate_expression)) (set! evaluate_stack []) (set! evaluate_i (- (count evaluate_tokens) 1)) (while (>= evaluate_i 0) (do (set! evaluate_token (nth evaluate_tokens evaluate_i)) (when (not= evaluate_token "") (if (is_operand evaluate_token) (set! evaluate_stack (conj evaluate_stack (double (to_int evaluate_token)))) (do (set! evaluate_o1 (nth evaluate_stack (- (count evaluate_stack) 1))) (set! evaluate_o2 (nth evaluate_stack (- (count evaluate_stack) 2))) (set! evaluate_stack (subvec evaluate_stack 0 (- (count evaluate_stack) 2))) (set! evaluate_res (apply_op evaluate_token evaluate_o1 evaluate_o2)) (set! evaluate_stack (conj evaluate_stack evaluate_res))))) (set! evaluate_i (- evaluate_i 1)))) (throw (ex-info "return" {:v (nth evaluate_stack 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn eval_rec [eval_rec_tokens eval_rec_pos]
  (binding [eval_rec_a nil eval_rec_b nil eval_rec_left nil eval_rec_p1 nil eval_rec_p2 nil eval_rec_right nil eval_rec_token nil next_v nil] (try (do (set! eval_rec_token (nth eval_rec_tokens eval_rec_pos)) (set! next_v (+ eval_rec_pos 1)) (when (is_operand eval_rec_token) (throw (ex-info "return" {:v [(double (to_int eval_rec_token)) (double next_v)]}))) (set! eval_rec_left (eval_rec eval_rec_tokens next_v)) (set! eval_rec_a (nth eval_rec_left 0)) (set! eval_rec_p1 (long (nth eval_rec_left 1))) (set! eval_rec_right (eval_rec eval_rec_tokens eval_rec_p1)) (set! eval_rec_b (nth eval_rec_right 0)) (set! eval_rec_p2 (nth eval_rec_right 1)) (throw (ex-info "return" {:v [(apply_op eval_rec_token eval_rec_a eval_rec_b) eval_rec_p2]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn evaluate_recursive [evaluate_recursive_expression]
  (binding [evaluate_recursive_res nil evaluate_recursive_tokens nil] (try (do (set! evaluate_recursive_tokens (tokenize evaluate_recursive_expression)) (set! evaluate_recursive_res (eval_rec evaluate_recursive_tokens 0)) (throw (ex-info "return" {:v (nth evaluate_recursive_res 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_test_expression "+ 9 * 2 6")

(def ^:dynamic main_test_expression2 "/ * 10 2 + 4 1 ")

(def ^:dynamic main_test_expression3 "+ * 2 3 / 8 4")

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (evaluate main_test_expression)))
      (println (str (evaluate main_test_expression2)))
      (println (str (evaluate_recursive main_test_expression3)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
