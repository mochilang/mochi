(ns main (:refer-clojure :exclude [is_prime list_truncated_nums validate compute_truncated_primes solution]))

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

(declare is_prime list_truncated_nums validate compute_truncated_primes solution)

(declare _read_file)

(def ^:dynamic compute_truncated_primes_all_prime nil)

(def ^:dynamic compute_truncated_primes_j nil)

(def ^:dynamic compute_truncated_primes_list_nums nil)

(def ^:dynamic compute_truncated_primes_list_truncated_primes nil)

(def ^:dynamic compute_truncated_primes_num nil)

(def ^:dynamic is_prime_i nil)

(def ^:dynamic list_truncated_nums_i nil)

(def ^:dynamic list_truncated_nums_left nil)

(def ^:dynamic list_truncated_nums_length nil)

(def ^:dynamic list_truncated_nums_list_nums nil)

(def ^:dynamic list_truncated_nums_right nil)

(def ^:dynamic list_truncated_nums_str_num nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_primes nil)

(def ^:dynamic solution_total nil)

(def ^:dynamic validate_first3 nil)

(def ^:dynamic validate_last3 nil)

(def ^:dynamic validate_length nil)

(def ^:dynamic validate_s nil)

(defn is_prime [is_prime_number]
  (binding [is_prime_i nil] (try (do (when (and (< 1 is_prime_number) (< is_prime_number 4)) (throw (ex-info "return" {:v true}))) (when (or (or (< is_prime_number 2) (= (mod is_prime_number 2) 0)) (= (mod is_prime_number 3) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i 5) (while (<= (* is_prime_i is_prime_i) is_prime_number) (do (when (or (= (mod is_prime_number is_prime_i) 0) (= (mod is_prime_number (+ is_prime_i 2)) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 6)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_truncated_nums [list_truncated_nums_n]
  (binding [list_truncated_nums_i nil list_truncated_nums_left nil list_truncated_nums_length nil list_truncated_nums_list_nums nil list_truncated_nums_right nil list_truncated_nums_str_num nil] (try (do (set! list_truncated_nums_str_num (mochi_str list_truncated_nums_n)) (set! list_truncated_nums_list_nums [list_truncated_nums_n]) (set! list_truncated_nums_i 1) (set! list_truncated_nums_length (count list_truncated_nums_str_num)) (while (< list_truncated_nums_i list_truncated_nums_length) (do (set! list_truncated_nums_right (toi (subs list_truncated_nums_str_num list_truncated_nums_i (min list_truncated_nums_length (count list_truncated_nums_str_num))))) (set! list_truncated_nums_left (toi (subs list_truncated_nums_str_num 0 (min (- list_truncated_nums_length list_truncated_nums_i) (count list_truncated_nums_str_num))))) (set! list_truncated_nums_list_nums (conj list_truncated_nums_list_nums list_truncated_nums_right)) (set! list_truncated_nums_list_nums (conj list_truncated_nums_list_nums list_truncated_nums_left)) (set! list_truncated_nums_i (+ list_truncated_nums_i 1)))) (throw (ex-info "return" {:v list_truncated_nums_list_nums}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn validate [validate_n]
  (binding [validate_first3 nil validate_last3 nil validate_length nil validate_s nil] (try (do (set! validate_s (mochi_str validate_n)) (set! validate_length (count validate_s)) (when (> validate_length 3) (do (set! validate_last3 (toi (subs validate_s (- validate_length 3) (min validate_length (count validate_s))))) (set! validate_first3 (toi (subs validate_s 0 (min 3 (count validate_s))))) (when (not (and (is_prime validate_last3) (is_prime validate_first3))) (throw (ex-info "return" {:v false}))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn compute_truncated_primes [count_v]
  (binding [compute_truncated_primes_all_prime nil compute_truncated_primes_j nil compute_truncated_primes_list_nums nil compute_truncated_primes_list_truncated_primes nil compute_truncated_primes_num nil] (try (do (set! compute_truncated_primes_list_truncated_primes []) (set! compute_truncated_primes_num 13) (loop [while_flag_1 true] (when (and while_flag_1 (not= (count compute_truncated_primes_list_truncated_primes) count_v)) (do (when (validate compute_truncated_primes_num) (do (set! compute_truncated_primes_list_nums (list_truncated_nums compute_truncated_primes_num)) (set! compute_truncated_primes_all_prime true) (set! compute_truncated_primes_j 0) (loop [while_flag_2 true] (when (and while_flag_2 (< compute_truncated_primes_j (count compute_truncated_primes_list_nums))) (do (if (not (is_prime (nth compute_truncated_primes_list_nums compute_truncated_primes_j))) (do (set! compute_truncated_primes_all_prime false) (recur false)) (set! compute_truncated_primes_j (+ compute_truncated_primes_j 1))) (cond :else (recur while_flag_2))))) (when compute_truncated_primes_all_prime (set! compute_truncated_primes_list_truncated_primes (conj compute_truncated_primes_list_truncated_primes compute_truncated_primes_num))))) (set! compute_truncated_primes_num (+ compute_truncated_primes_num 2)) (cond :else (recur while_flag_1))))) (throw (ex-info "return" {:v compute_truncated_primes_list_truncated_primes}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution []
  (binding [solution_i nil solution_primes nil solution_total nil] (try (do (set! solution_primes (compute_truncated_primes 11)) (set! solution_total 0) (set! solution_i 0) (while (< solution_i (count solution_primes)) (do (set! solution_total (+ solution_total (nth solution_primes solution_i))) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v solution_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "sum(compute_truncated_primes(11)) = " (mochi_str (solution))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
