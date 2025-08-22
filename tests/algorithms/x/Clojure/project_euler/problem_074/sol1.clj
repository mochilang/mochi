(ns main (:refer-clojure :exclude [sum_digit_factorials chain_length solution]))

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

(declare sum_digit_factorials chain_length solution)

(declare _read_file)

(def ^:dynamic chain_length_ahead nil)

(def ^:dynamic chain_length_chain nil)

(def ^:dynamic chain_length_current nil)

(def ^:dynamic chain_length_i nil)

(def ^:dynamic chain_length_known nil)

(def ^:dynamic chain_length_loop_len nil)

(def ^:dynamic chain_length_loop_start nil)

(def ^:dynamic chain_length_seen nil)

(def ^:dynamic chain_length_total nil)

(def ^:dynamic count_v nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic sum_digit_factorials_digit nil)

(def ^:dynamic sum_digit_factorials_m nil)

(def ^:dynamic sum_digit_factorials_ret nil)

(def ^:dynamic main_DIGIT_FACTORIALS nil)

(def ^:dynamic main_cache_sum_digit_factorials nil)

(def ^:dynamic main_chain_length_cache nil)

(defn sum_digit_factorials [sum_digit_factorials_n]
  (binding [sum_digit_factorials_digit nil sum_digit_factorials_m nil sum_digit_factorials_ret nil] (try (do (when (in sum_digit_factorials_n main_cache_sum_digit_factorials) (throw (ex-info "return" {:v (get main_cache_sum_digit_factorials sum_digit_factorials_n)}))) (set! sum_digit_factorials_m sum_digit_factorials_n) (set! sum_digit_factorials_ret 0) (when (= sum_digit_factorials_m 0) (set! sum_digit_factorials_ret (nth main_DIGIT_FACTORIALS 0))) (while (> sum_digit_factorials_m 0) (do (set! sum_digit_factorials_digit (mod sum_digit_factorials_m 10)) (set! sum_digit_factorials_ret (+ sum_digit_factorials_ret (nth main_DIGIT_FACTORIALS sum_digit_factorials_digit))) (set! sum_digit_factorials_m (/ sum_digit_factorials_m 10)))) (alter-var-root (var main_cache_sum_digit_factorials) (fn [_] (assoc main_cache_sum_digit_factorials sum_digit_factorials_n sum_digit_factorials_ret))) (throw (ex-info "return" {:v sum_digit_factorials_ret}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn chain_length [chain_length_n]
  (binding [chain_length_ahead nil chain_length_chain nil chain_length_current nil chain_length_i nil chain_length_known nil chain_length_loop_len nil chain_length_loop_start nil chain_length_seen nil chain_length_total nil] (try (do (when (in chain_length_n main_chain_length_cache) (throw (ex-info "return" {:v (get main_chain_length_cache chain_length_n)}))) (set! chain_length_chain []) (set! chain_length_seen {}) (set! chain_length_current chain_length_n) (while true (do (when (in chain_length_current main_chain_length_cache) (do (set! chain_length_known (get main_chain_length_cache chain_length_current)) (set! chain_length_total chain_length_known) (set! chain_length_i (- (count chain_length_chain) 1)) (while (>= chain_length_i 0) (do (set! chain_length_total (+ chain_length_total 1)) (alter-var-root (var main_chain_length_cache) (fn [_] (assoc main_chain_length_cache (nth chain_length_chain chain_length_i) chain_length_total))) (set! chain_length_i (- chain_length_i 1)))) (throw (ex-info "return" {:v (get main_chain_length_cache chain_length_n)})))) (when (in chain_length_current chain_length_seen) (do (set! chain_length_loop_start (get chain_length_seen chain_length_current)) (set! chain_length_loop_len (- (count chain_length_chain) chain_length_loop_start)) (set! chain_length_i (- (count chain_length_chain) 1)) (set! chain_length_ahead 0) (while (>= chain_length_i 0) (do (if (>= chain_length_i chain_length_loop_start) (alter-var-root (var main_chain_length_cache) (fn [_] (assoc main_chain_length_cache (nth chain_length_chain chain_length_i) chain_length_loop_len))) (alter-var-root (var main_chain_length_cache) (fn [_] (assoc main_chain_length_cache (nth chain_length_chain chain_length_i) (+ chain_length_loop_len (+ chain_length_ahead 1)))))) (set! chain_length_ahead (+ chain_length_ahead 1)) (set! chain_length_i (- chain_length_i 1)))) (throw (ex-info "return" {:v (get main_chain_length_cache chain_length_n)})))) (set! chain_length_seen (assoc chain_length_seen chain_length_current (count chain_length_chain))) (set! chain_length_chain (conj chain_length_chain chain_length_current)) (set! chain_length_current (sum_digit_factorials chain_length_current))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_num_terms solution_max_start]
  (binding [count_v nil solution_i nil] (try (do (set! count_v 0) (set! solution_i 1) (while (< solution_i solution_max_start) (do (when (= (chain_length solution_i) solution_num_terms) (set! count_v (+ count_v 1))) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_DIGIT_FACTORIALS) (constantly [1 1 2 6 24 120 720 5040 40320 362880]))
      (alter-var-root (var main_cache_sum_digit_factorials) (constantly {145 145}))
      (alter-var-root (var main_chain_length_cache) (constantly {145 0 1454 3 169 3 36301 3 45361 2 871 2 872 2}))
      (println (str "solution() = " (mochi_str (solution 60 1000))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
