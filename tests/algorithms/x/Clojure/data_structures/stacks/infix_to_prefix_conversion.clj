(ns main (:refer-clojure :exclude [is_alpha is_digit reverse_string infix_to_postfix infix_to_prefix]))

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

(declare is_alpha is_digit reverse_string infix_to_postfix infix_to_prefix)

(def ^:dynamic infix_to_postfix_i nil)

(def ^:dynamic infix_to_postfix_j nil)

(def ^:dynamic infix_to_postfix_post nil)

(def ^:dynamic infix_to_postfix_res nil)

(def ^:dynamic infix_to_postfix_stack nil)

(def ^:dynamic infix_to_postfix_x nil)

(def ^:dynamic infix_to_prefix_ch nil)

(def ^:dynamic infix_to_prefix_i nil)

(def ^:dynamic infix_to_prefix_postfix nil)

(def ^:dynamic infix_to_prefix_prefix nil)

(def ^:dynamic infix_to_prefix_reversed nil)

(def ^:dynamic is_alpha_i nil)

(def ^:dynamic is_digit_i nil)

(def ^:dynamic reverse_string_i nil)

(def ^:dynamic reverse_string_out nil)

(def ^:dynamic main_PRIORITY {"%" 2 "*" 2 "+" 1 "-" 1 "/" 2 "^" 3})

(def ^:dynamic main_LETTERS "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def ^:dynamic main_DIGITS "0123456789")

(defn is_alpha [is_alpha_ch]
  (binding [is_alpha_i nil] (try (do (set! is_alpha_i 0) (while (< is_alpha_i (count main_LETTERS)) (do (when (= (nth main_LETTERS is_alpha_i) is_alpha_ch) (throw (ex-info "return" {:v true}))) (set! is_alpha_i (+ is_alpha_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_digit [is_digit_ch]
  (binding [is_digit_i nil] (try (do (set! is_digit_i 0) (while (< is_digit_i (count main_DIGITS)) (do (when (= (nth main_DIGITS is_digit_i) is_digit_ch) (throw (ex-info "return" {:v true}))) (set! is_digit_i (+ is_digit_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_string [reverse_string_s]
  (binding [reverse_string_i nil reverse_string_out nil] (try (do (set! reverse_string_out "") (set! reverse_string_i (- (count reverse_string_s) 1)) (while (>= reverse_string_i 0) (do (set! reverse_string_out (str reverse_string_out (nth reverse_string_s reverse_string_i))) (set! reverse_string_i (- reverse_string_i 1)))) (throw (ex-info "return" {:v reverse_string_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn infix_to_postfix [infix_to_postfix_infix]
  (binding [infix_to_postfix_i nil infix_to_postfix_j nil infix_to_postfix_post nil infix_to_postfix_res nil infix_to_postfix_stack nil infix_to_postfix_x nil] (try (do (set! infix_to_postfix_stack []) (set! infix_to_postfix_post []) (set! infix_to_postfix_i 0) (while (< infix_to_postfix_i (count infix_to_postfix_infix)) (do (set! infix_to_postfix_x (nth infix_to_postfix_infix infix_to_postfix_i)) (if (or (is_alpha infix_to_postfix_x) (is_digit infix_to_postfix_x)) (set! infix_to_postfix_post (conj infix_to_postfix_post infix_to_postfix_x)) (if (= infix_to_postfix_x "(") (set! infix_to_postfix_stack (conj infix_to_postfix_stack infix_to_postfix_x)) (if (= infix_to_postfix_x ")") (do (when (= (count infix_to_postfix_stack) 0) (throw (Exception. "list index out of range"))) (while (not= (nth infix_to_postfix_stack (- (count infix_to_postfix_stack) 1)) "(") (do (set! infix_to_postfix_post (conj infix_to_postfix_post (nth infix_to_postfix_stack (- (count infix_to_postfix_stack) 1)))) (set! infix_to_postfix_stack (subvec infix_to_postfix_stack 0 (- (count infix_to_postfix_stack) 1))))) (set! infix_to_postfix_stack (subvec infix_to_postfix_stack 0 (- (count infix_to_postfix_stack) 1)))) (if (= (count infix_to_postfix_stack) 0) (set! infix_to_postfix_stack (conj infix_to_postfix_stack infix_to_postfix_x)) (do (while (and (and (> (count infix_to_postfix_stack) 0) (not= (nth infix_to_postfix_stack (- (count infix_to_postfix_stack) 1)) "(")) (<= (get main_PRIORITY infix_to_postfix_x) (get main_PRIORITY (nth infix_to_postfix_stack (- (count infix_to_postfix_stack) 1))))) (do (set! infix_to_postfix_post (conj infix_to_postfix_post (nth infix_to_postfix_stack (- (count infix_to_postfix_stack) 1)))) (set! infix_to_postfix_stack (subvec infix_to_postfix_stack 0 (- (count infix_to_postfix_stack) 1))))) (set! infix_to_postfix_stack (conj infix_to_postfix_stack infix_to_postfix_x))))))) (set! infix_to_postfix_i (+ infix_to_postfix_i 1)))) (while (> (count infix_to_postfix_stack) 0) (do (when (= (nth infix_to_postfix_stack (- (count infix_to_postfix_stack) 1)) "(") (throw (Exception. "invalid expression"))) (set! infix_to_postfix_post (conj infix_to_postfix_post (nth infix_to_postfix_stack (- (count infix_to_postfix_stack) 1)))) (set! infix_to_postfix_stack (subvec infix_to_postfix_stack 0 (- (count infix_to_postfix_stack) 1))))) (set! infix_to_postfix_res "") (set! infix_to_postfix_j 0) (while (< infix_to_postfix_j (count infix_to_postfix_post)) (do (set! infix_to_postfix_res (str infix_to_postfix_res (nth infix_to_postfix_post infix_to_postfix_j))) (set! infix_to_postfix_j (+ infix_to_postfix_j 1)))) (throw (ex-info "return" {:v infix_to_postfix_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn infix_to_prefix [infix_to_prefix_infix]
  (binding [infix_to_prefix_ch nil infix_to_prefix_i nil infix_to_prefix_postfix nil infix_to_prefix_prefix nil infix_to_prefix_reversed nil] (try (do (set! infix_to_prefix_reversed "") (set! infix_to_prefix_i (- (count infix_to_prefix_infix) 1)) (while (>= infix_to_prefix_i 0) (do (set! infix_to_prefix_ch (nth infix_to_prefix_infix infix_to_prefix_i)) (if (= infix_to_prefix_ch "(") (set! infix_to_prefix_reversed (str infix_to_prefix_reversed ")")) (if (= infix_to_prefix_ch ")") (set! infix_to_prefix_reversed (str infix_to_prefix_reversed "(")) (set! infix_to_prefix_reversed (str infix_to_prefix_reversed infix_to_prefix_ch)))) (set! infix_to_prefix_i (- infix_to_prefix_i 1)))) (set! infix_to_prefix_postfix (infix_to_postfix infix_to_prefix_reversed)) (set! infix_to_prefix_prefix (reverse_string infix_to_prefix_postfix)) (throw (ex-info "return" {:v infix_to_prefix_prefix}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

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
