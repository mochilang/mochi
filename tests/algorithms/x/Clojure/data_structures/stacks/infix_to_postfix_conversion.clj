(ns main (:refer-clojure :exclude [precedence associativity balanced_parentheses is_letter is_digit is_alnum infix_to_postfix main]))

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

(declare precedence associativity balanced_parentheses is_letter is_digit is_alnum infix_to_postfix main)

(def ^:dynamic balanced_parentheses_ch nil)

(def ^:dynamic balanced_parentheses_i nil)

(def ^:dynamic count_v nil)

(def ^:dynamic infix_to_postfix_ch nil)

(def ^:dynamic infix_to_postfix_cp nil)

(def ^:dynamic infix_to_postfix_i nil)

(def ^:dynamic infix_to_postfix_j nil)

(def ^:dynamic infix_to_postfix_postfix nil)

(def ^:dynamic infix_to_postfix_res nil)

(def ^:dynamic infix_to_postfix_stack nil)

(def ^:dynamic infix_to_postfix_tp nil)

(def ^:dynamic main_expression nil)

(def ^:dynamic main_PRECEDENCES {"*" 2 "+" 1 "-" 1 "/" 2 "^" 3})

(def ^:dynamic main_ASSOCIATIVITIES {"*" "LR" "+" "LR" "-" "LR" "/" "LR" "^" "RL"})

(defn precedence [precedence_ch]
  (try (if (in precedence_ch main_PRECEDENCES) (get main_PRECEDENCES precedence_ch) (- 1)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn associativity [associativity_ch]
  (try (if (in associativity_ch main_ASSOCIATIVITIES) (get main_ASSOCIATIVITIES associativity_ch) "") (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn balanced_parentheses [balanced_parentheses_expr]
  (binding [balanced_parentheses_ch nil balanced_parentheses_i nil count_v nil] (try (do (set! count_v 0) (set! balanced_parentheses_i 0) (while (< balanced_parentheses_i (count balanced_parentheses_expr)) (do (set! balanced_parentheses_ch (subs balanced_parentheses_expr balanced_parentheses_i (min (+ balanced_parentheses_i 1) (count balanced_parentheses_expr)))) (when (= balanced_parentheses_ch "(") (set! count_v (+ count_v 1))) (when (= balanced_parentheses_ch ")") (do (set! count_v (- count_v 1)) (when (< count_v 0) (throw (ex-info "return" {:v false}))))) (set! balanced_parentheses_i (+ balanced_parentheses_i 1)))) (throw (ex-info "return" {:v (= count_v 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_letter [is_letter_ch]
  (try (throw (ex-info "return" {:v (or (and (<= (compare "a" is_letter_ch) 0) (<= (compare is_letter_ch "z") 0)) (and (<= (compare "A" is_letter_ch) 0) (<= (compare is_letter_ch "Z") 0)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_digit [is_digit_ch]
  (try (throw (ex-info "return" {:v (and (<= (compare "0" is_digit_ch) 0) (<= (compare is_digit_ch "9") 0))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_alnum [is_alnum_ch]
  (try (throw (ex-info "return" {:v (or (is_letter is_alnum_ch) (is_digit is_alnum_ch))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn infix_to_postfix [infix_to_postfix_expression]
  (binding [infix_to_postfix_ch nil infix_to_postfix_cp nil infix_to_postfix_i nil infix_to_postfix_j nil infix_to_postfix_postfix nil infix_to_postfix_res nil infix_to_postfix_stack nil infix_to_postfix_tp nil] (try (do (when (= (balanced_parentheses infix_to_postfix_expression) false) (throw (Exception. "Mismatched parentheses"))) (set! infix_to_postfix_stack []) (set! infix_to_postfix_postfix []) (set! infix_to_postfix_i 0) (while (< infix_to_postfix_i (count infix_to_postfix_expression)) (do (set! infix_to_postfix_ch (subs infix_to_postfix_expression infix_to_postfix_i (min (+ infix_to_postfix_i 1) (count infix_to_postfix_expression)))) (if (is_alnum infix_to_postfix_ch) (set! infix_to_postfix_postfix (conj infix_to_postfix_postfix infix_to_postfix_ch)) (if (= infix_to_postfix_ch "(") (set! infix_to_postfix_stack (conj infix_to_postfix_stack infix_to_postfix_ch)) (if (= infix_to_postfix_ch ")") (do (while (and (> (count infix_to_postfix_stack) 0) (not= (nth infix_to_postfix_stack (- (count infix_to_postfix_stack) 1)) "(")) (do (set! infix_to_postfix_postfix (conj infix_to_postfix_postfix (nth infix_to_postfix_stack (- (count infix_to_postfix_stack) 1)))) (set! infix_to_postfix_stack (subvec infix_to_postfix_stack 0 (min (- (count infix_to_postfix_stack) 1) (count infix_to_postfix_stack)))))) (when (> (count infix_to_postfix_stack) 0) (set! infix_to_postfix_stack (subvec infix_to_postfix_stack 0 (min (- (count infix_to_postfix_stack) 1) (count infix_to_postfix_stack)))))) (if (= infix_to_postfix_ch " ") nil (loop [while_flag_1 true] (when (and while_flag_1 true) (cond (= (count infix_to_postfix_stack) 0) (do (set! infix_to_postfix_stack (conj infix_to_postfix_stack infix_to_postfix_ch)) (recur false)) (> infix_to_postfix_cp infix_to_postfix_tp) (do (set! infix_to_postfix_stack (conj infix_to_postfix_stack infix_to_postfix_ch)) (recur false)) (< infix_to_postfix_cp infix_to_postfix_tp) (do (set! infix_to_postfix_postfix (conj infix_to_postfix_postfix (nth infix_to_postfix_stack (- (count infix_to_postfix_stack) 1)))) (set! infix_to_postfix_stack (subvec infix_to_postfix_stack 0 (min (- (count infix_to_postfix_stack) 1) (count infix_to_postfix_stack)))) (recur true)) (= (associativity infix_to_postfix_ch) "RL") (do (set! infix_to_postfix_stack (conj infix_to_postfix_stack infix_to_postfix_ch)) (recur false)) :else (do (set! infix_to_postfix_cp (precedence infix_to_postfix_ch)) (set! infix_to_postfix_tp (precedence (nth infix_to_postfix_stack (- (count infix_to_postfix_stack) 1)))) (set! infix_to_postfix_postfix (conj infix_to_postfix_postfix (nth infix_to_postfix_stack (- (count infix_to_postfix_stack) 1)))) (set! infix_to_postfix_stack (subvec infix_to_postfix_stack 0 (min (- (count infix_to_postfix_stack) 1) (count infix_to_postfix_stack)))) (recur while_flag_1))))))))) (set! infix_to_postfix_i (+ infix_to_postfix_i 1)))) (while (> (count infix_to_postfix_stack) 0) (do (set! infix_to_postfix_postfix (conj infix_to_postfix_postfix (nth infix_to_postfix_stack (- (count infix_to_postfix_stack) 1)))) (set! infix_to_postfix_stack (subvec infix_to_postfix_stack 0 (min (- (count infix_to_postfix_stack) 1) (count infix_to_postfix_stack)))))) (set! infix_to_postfix_res "") (set! infix_to_postfix_j 0) (while (< infix_to_postfix_j (count infix_to_postfix_postfix)) (do (when (> infix_to_postfix_j 0) (set! infix_to_postfix_res (str infix_to_postfix_res " "))) (set! infix_to_postfix_res (str infix_to_postfix_res (nth infix_to_postfix_postfix infix_to_postfix_j))) (set! infix_to_postfix_j (+ infix_to_postfix_j 1)))) (throw (ex-info "return" {:v infix_to_postfix_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_expression nil] (do (set! main_expression "a+b*(c^d-e)^(f+g*h)-i") (println main_expression) (println (infix_to_postfix main_expression)))))

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
