(ns main (:refer-clojure :exclude [digit_factorial_sum chain_len solution]))

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

(declare digit_factorial_sum chain_len solution)

(declare _read_file)

(def ^:dynamic chain_len_cur nil)

(def ^:dynamic chain_len_length nil)

(def ^:dynamic chain_len_seen nil)

(def ^:dynamic count_v nil)

(def ^:dynamic digit_factorial_sum_digit nil)

(def ^:dynamic digit_factorial_sum_n nil)

(def ^:dynamic digit_factorial_sum_total nil)

(def ^:dynamic solution_start nil)

(def ^:dynamic main_DIGIT_FACTORIAL nil)

(defn digit_factorial_sum [digit_factorial_sum_number]
  (binding [digit_factorial_sum_digit nil digit_factorial_sum_n nil digit_factorial_sum_total nil] (try (do (when (< digit_factorial_sum_number 0) (throw (Exception. "Parameter number must be greater than or equal to 0"))) (when (= digit_factorial_sum_number 0) (throw (ex-info "return" {:v (nth main_DIGIT_FACTORIAL 0)}))) (set! digit_factorial_sum_n digit_factorial_sum_number) (set! digit_factorial_sum_total 0) (while (> digit_factorial_sum_n 0) (do (set! digit_factorial_sum_digit (mod digit_factorial_sum_n 10)) (set! digit_factorial_sum_total (+ digit_factorial_sum_total (nth main_DIGIT_FACTORIAL digit_factorial_sum_digit))) (set! digit_factorial_sum_n (/ digit_factorial_sum_n 10)))) (throw (ex-info "return" {:v digit_factorial_sum_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn chain_len [chain_len_n chain_len_limit]
  (binding [chain_len_cur nil chain_len_length nil chain_len_seen nil] (try (do (set! chain_len_seen {}) (set! chain_len_length 0) (set! chain_len_cur chain_len_n) (while (and (= (in chain_len_cur chain_len_seen) false) (<= chain_len_length chain_len_limit)) (do (set! chain_len_seen (assoc chain_len_seen chain_len_cur true)) (set! chain_len_length (+ chain_len_length 1)) (set! chain_len_cur (digit_factorial_sum chain_len_cur)))) (throw (ex-info "return" {:v chain_len_length}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_chain_length solution_number_limit]
  (binding [count_v nil solution_start nil] (try (do (when (or (<= solution_chain_length 0) (<= solution_number_limit 0)) (throw (Exception. "Parameters chain_length and number_limit must be greater than 0"))) (set! count_v 0) (set! solution_start 1) (while (< solution_start solution_number_limit) (do (when (= (chain_len solution_start solution_chain_length) solution_chain_length) (set! count_v (+ count_v 1))) (set! solution_start (+ solution_start 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_DIGIT_FACTORIAL) (constantly [1 1 2 6 24 120 720 5040 40320 362880]))
      (println (mochi_str (solution 60 1000000)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
