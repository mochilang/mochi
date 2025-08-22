(ns main (:refer-clojure :exclude [reverse_string is_palindrome sum_reverse solution]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare reverse_string is_palindrome sum_reverse solution)

(declare _read_file)

(def ^:dynamic count_v nil)

(def ^:dynamic is_palindrome_s nil)

(def ^:dynamic reverse_string_i nil)

(def ^:dynamic reverse_string_result nil)

(def ^:dynamic solution_current nil)

(def ^:dynamic solution_iterations nil)

(def ^:dynamic solution_num nil)

(def ^:dynamic sum_reverse_r nil)

(def ^:dynamic sum_reverse_s nil)

(defn reverse_string [reverse_string_s]
  (binding [reverse_string_i nil reverse_string_result nil] (try (do (set! reverse_string_result "") (set! reverse_string_i (- (count reverse_string_s) 1)) (while (>= reverse_string_i 0) (do (set! reverse_string_result (str reverse_string_result (subs reverse_string_s reverse_string_i (+ reverse_string_i 1)))) (set! reverse_string_i (- reverse_string_i 1)))) (throw (ex-info "return" {:v reverse_string_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_palindrome [is_palindrome_n]
  (binding [is_palindrome_s nil] (try (do (set! is_palindrome_s (mochi_str is_palindrome_n)) (throw (ex-info "return" {:v (= is_palindrome_s (reverse_string is_palindrome_s))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sum_reverse [sum_reverse_n]
  (binding [sum_reverse_r nil sum_reverse_s nil] (try (do (set! sum_reverse_s (mochi_str sum_reverse_n)) (set! sum_reverse_r (reverse_string sum_reverse_s)) (throw (ex-info "return" {:v (+ sum_reverse_n (toi sum_reverse_r))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_limit]
  (binding [count_v nil solution_current nil solution_iterations nil solution_num nil] (try (do (set! count_v 0) (set! solution_num 1) (loop [while_flag_1 true] (when (and while_flag_1 (< solution_num solution_limit)) (do (set! solution_iterations 0) (set! solution_current solution_num) (loop [while_flag_2 true] (when (and while_flag_2 (< solution_iterations 50)) (do (set! solution_current (sum_reverse solution_current)) (set! solution_iterations (+ solution_iterations 1)) (cond (is_palindrome solution_current) (recur false) :else (recur while_flag_2))))) (when (and (= solution_iterations 50) (= (is_palindrome solution_current) false)) (set! count_v (+ count_v 1))) (set! solution_num (+ solution_num 1)) (cond :else (do))))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "solution() = " (mochi_str (solution 10000))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
