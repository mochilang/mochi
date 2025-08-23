(ns main (:refer-clojure :exclude [int_sqrt is_prime compute_nums solution]))

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

(declare int_sqrt is_prime compute_nums solution)

(declare _read_file)

(def ^:dynamic compute_nums_found nil)

(def ^:dynamic compute_nums_i nil)

(def ^:dynamic compute_nums_list_nums nil)

(def ^:dynamic compute_nums_num nil)

(def ^:dynamic compute_nums_rem nil)

(def ^:dynamic int_sqrt_r nil)

(def ^:dynamic is_prime_i nil)

(def ^:dynamic is_prime_limit nil)

(defn int_sqrt [int_sqrt_n]
  (binding [int_sqrt_r nil] (try (do (set! int_sqrt_r 0) (while (<= (* (+ int_sqrt_r 1) (+ int_sqrt_r 1)) int_sqrt_n) (set! int_sqrt_r (+ int_sqrt_r 1))) (throw (ex-info "return" {:v int_sqrt_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_prime [is_prime_number]
  (binding [is_prime_i nil is_prime_limit nil] (try (do (when (and (< 1 is_prime_number) (< is_prime_number 4)) (throw (ex-info "return" {:v true}))) (when (or (or (< is_prime_number 2) (= (mod is_prime_number 2) 0)) (= (mod is_prime_number 3) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i 5) (set! is_prime_limit (int_sqrt is_prime_number)) (while (<= is_prime_i is_prime_limit) (do (when (or (= (mod is_prime_number is_prime_i) 0) (= (mod is_prime_number (+ is_prime_i 2)) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 6)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn compute_nums [compute_nums_n]
  (binding [compute_nums_found nil compute_nums_i nil compute_nums_list_nums nil compute_nums_num nil compute_nums_rem nil] (try (do (when (<= compute_nums_n 0) (throw (Exception. "n must be >= 0"))) (set! compute_nums_list_nums []) (set! compute_nums_num 3) (loop [while_flag_1 true] (when (and while_flag_1 (< (count compute_nums_list_nums) compute_nums_n)) (do (when (not (is_prime compute_nums_num)) (do (set! compute_nums_i 0) (set! compute_nums_found false) (loop [while_flag_2 true] (when (and while_flag_2 (<= (* (* 2 compute_nums_i) compute_nums_i) compute_nums_num)) (do (set! compute_nums_rem (- compute_nums_num (* (* 2 compute_nums_i) compute_nums_i))) (if (is_prime compute_nums_rem) (do (set! compute_nums_found true) (recur false)) (set! compute_nums_i (+ compute_nums_i 1))) (cond :else (recur while_flag_2))))) (when (not compute_nums_found) (set! compute_nums_list_nums (conj compute_nums_list_nums compute_nums_num))))) (set! compute_nums_num (+ compute_nums_num 2)) (cond :else (recur while_flag_1))))) (throw (ex-info "return" {:v compute_nums_list_nums}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution []
  (try (throw (ex-info "return" {:v (nth (compute_nums 1) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
