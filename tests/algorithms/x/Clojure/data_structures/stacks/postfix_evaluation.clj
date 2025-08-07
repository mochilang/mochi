(ns main (:refer-clojure :exclude [slice_without_last parse_float pow_float apply_op evaluate]))

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

(declare slice_without_last parse_float pow_float apply_op evaluate)

(def ^:dynamic evaluate_a nil)

(def ^:dynamic evaluate_b nil)

(def ^:dynamic evaluate_result nil)

(def ^:dynamic evaluate_stack nil)

(def ^:dynamic first_v nil)

(def ^:dynamic parse_float_digit nil)

(def ^:dynamic parse_float_idx nil)

(def ^:dynamic parse_float_int_part nil)

(def ^:dynamic parse_float_place nil)

(def ^:dynamic parse_float_result nil)

(def ^:dynamic parse_float_sign nil)

(def ^:dynamic pow_float_e nil)

(def ^:dynamic pow_float_i nil)

(def ^:dynamic pow_float_result nil)

(def ^:dynamic slice_without_last_i nil)

(def ^:dynamic slice_without_last_res nil)

(defn slice_without_last [slice_without_last_xs]
  (binding [slice_without_last_i nil slice_without_last_res nil] (try (do (set! slice_without_last_res []) (set! slice_without_last_i 0) (while (< slice_without_last_i (- (count slice_without_last_xs) 1)) (do (set! slice_without_last_res (conj slice_without_last_res (nth slice_without_last_xs slice_without_last_i))) (set! slice_without_last_i (+ slice_without_last_i 1)))) (throw (ex-info "return" {:v slice_without_last_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn parse_float [parse_float_token]
  (binding [first_v nil parse_float_digit nil parse_float_idx nil parse_float_int_part nil parse_float_place nil parse_float_result nil parse_float_sign nil] (try (do (set! parse_float_sign 1.0) (set! parse_float_idx 0) (when (> (count parse_float_token) 0) (do (set! first_v (subs parse_float_token 0 (min 1 (count parse_float_token)))) (if (= first_v "-") (do (set! parse_float_sign (- 1.0)) (set! parse_float_idx 1)) (when (= first_v "+") (set! parse_float_idx 1))))) (set! parse_float_int_part 0) (while (and (< parse_float_idx (count parse_float_token)) (not= (subs parse_float_token parse_float_idx (min (+ parse_float_idx 1) (count parse_float_token))) ".")) (do (set! parse_float_int_part (+ (* parse_float_int_part 10) (Integer/parseInt (subs parse_float_token parse_float_idx (min (+ parse_float_idx 1) (count parse_float_token)))))) (set! parse_float_idx (+ parse_float_idx 1)))) (set! parse_float_result (* 1.0 parse_float_int_part)) (when (and (< parse_float_idx (count parse_float_token)) (= (subs parse_float_token parse_float_idx (min (+ parse_float_idx 1) (count parse_float_token))) ".")) (do (set! parse_float_idx (+ parse_float_idx 1)) (set! parse_float_place 0.1) (while (< parse_float_idx (count parse_float_token)) (do (set! parse_float_digit (Integer/parseInt (subs parse_float_token parse_float_idx (min (+ parse_float_idx 1) (count parse_float_token))))) (set! parse_float_result (+ parse_float_result (* parse_float_place (* 1.0 parse_float_digit)))) (set! parse_float_place (/ parse_float_place 10.0)) (set! parse_float_idx (+ parse_float_idx 1)))))) (throw (ex-info "return" {:v (* parse_float_sign parse_float_result)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow_float [pow_float_base pow_float_exp]
  (binding [pow_float_e nil pow_float_i nil pow_float_result nil] (try (do (set! pow_float_result 1.0) (set! pow_float_i 0) (set! pow_float_e (Integer/parseInt pow_float_exp)) (while (< pow_float_i pow_float_e) (do (set! pow_float_result (* pow_float_result pow_float_base)) (set! pow_float_i (+ pow_float_i 1)))) (throw (ex-info "return" {:v pow_float_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn apply_op [apply_op_a apply_op_b apply_op_op]
  (try (do (when (= apply_op_op "+") (throw (ex-info "return" {:v (+ apply_op_a apply_op_b)}))) (when (= apply_op_op "-") (throw (ex-info "return" {:v (- apply_op_a apply_op_b)}))) (when (= apply_op_op "*") (throw (ex-info "return" {:v (* apply_op_a apply_op_b)}))) (when (= apply_op_op "/") (throw (ex-info "return" {:v (quot apply_op_a apply_op_b)}))) (if (= apply_op_op "^") (pow_float apply_op_a apply_op_b) 0.0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn evaluate [evaluate_tokens]
  (binding [evaluate_a nil evaluate_b nil evaluate_result nil evaluate_stack nil] (try (do (when (= (count evaluate_tokens) 0) (throw (ex-info "return" {:v 0.0}))) (set! evaluate_stack []) (doseq [token evaluate_tokens] (if (or (or (or (or (= token "+") (= token "-")) (= token "*")) (= token "/")) (= token "^")) (if (and (or (= token "+") (= token "-")) (< (count evaluate_stack) 2)) (do (set! evaluate_b (nth evaluate_stack (- (count evaluate_stack) 1))) (set! evaluate_stack (slice_without_last evaluate_stack)) (if (= token "-") (set! evaluate_stack (conj evaluate_stack (- 0.0 evaluate_b))) (set! evaluate_stack (conj evaluate_stack evaluate_b)))) (do (set! evaluate_b (nth evaluate_stack (- (count evaluate_stack) 1))) (set! evaluate_stack (slice_without_last evaluate_stack)) (set! evaluate_a (nth evaluate_stack (- (count evaluate_stack) 1))) (set! evaluate_stack (slice_without_last evaluate_stack)) (set! evaluate_result (apply_op evaluate_a evaluate_b token)) (set! evaluate_stack (conj evaluate_stack evaluate_result)))) (set! evaluate_stack (conj evaluate_stack (parse_float token))))) (when (not= (count evaluate_stack) 1) (throw (Exception. "Invalid postfix expression"))) (throw (ex-info "return" {:v (nth evaluate_stack 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (evaluate ["2" "1" "+" "3" "*"])))
      (println (str (evaluate ["4" "13" "5" "/" "+"])))
      (println (str (evaluate ["5" "6" "9" "*" "+"])))
      (println (str (evaluate ["2" "-" "3" "+"])))
      (println (str (evaluate [])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
