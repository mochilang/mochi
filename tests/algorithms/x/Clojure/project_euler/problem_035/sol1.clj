(ns main (:refer-clojure :exclude [is_prime contains_an_even_digit parse_int find_circular_primes solution]))

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

(declare is_prime contains_an_even_digit parse_int find_circular_primes solution)

(declare _read_file)

(def ^:dynamic contains_an_even_digit_c nil)

(def ^:dynamic contains_an_even_digit_idx nil)

(def ^:dynamic contains_an_even_digit_s nil)

(def ^:dynamic find_circular_primes_all_prime nil)

(def ^:dynamic find_circular_primes_num nil)

(def ^:dynamic find_circular_primes_result nil)

(def ^:dynamic find_circular_primes_rotated nil)

(def ^:dynamic find_circular_primes_rotated_str nil)

(def ^:dynamic find_circular_primes_s nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_j nil)

(def ^:dynamic main_p nil)

(def ^:dynamic main_sieve nil)

(def ^:dynamic parse_int_ch nil)

(def ^:dynamic parse_int_k nil)

(def ^:dynamic parse_int_value nil)

(def ^:dynamic main_LIMIT nil)

(def ^:dynamic main_sieve nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_p nil)

(defn is_prime [is_prime_n]
  (try (throw (ex-info "return" {:v (nth main_sieve is_prime_n)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn contains_an_even_digit [contains_an_even_digit_n]
  (binding [contains_an_even_digit_c nil contains_an_even_digit_idx nil contains_an_even_digit_s nil] (try (do (set! contains_an_even_digit_s (mochi_str contains_an_even_digit_n)) (set! contains_an_even_digit_idx 0) (while (< contains_an_even_digit_idx (count contains_an_even_digit_s)) (do (set! contains_an_even_digit_c (nth contains_an_even_digit_s contains_an_even_digit_idx)) (when (or (or (or (or (= contains_an_even_digit_c "0") (= contains_an_even_digit_c "2")) (= contains_an_even_digit_c "4")) (= contains_an_even_digit_c "6")) (= contains_an_even_digit_c "8")) (throw (ex-info "return" {:v true}))) (set! contains_an_even_digit_idx (+ contains_an_even_digit_idx 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn parse_int [parse_int_s]
  (binding [parse_int_ch nil parse_int_k nil parse_int_value nil] (try (do (set! parse_int_value 0) (set! parse_int_k 0) (while (< parse_int_k (count parse_int_s)) (do (set! parse_int_ch (subs parse_int_s parse_int_k (+ parse_int_k 1))) (set! parse_int_value (+ (* parse_int_value 10) (Long/parseLong parse_int_ch))) (set! parse_int_k (+ parse_int_k 1)))) (throw (ex-info "return" {:v parse_int_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn find_circular_primes [find_circular_primes_limit]
  (binding [find_circular_primes_all_prime nil find_circular_primes_num nil find_circular_primes_result nil find_circular_primes_rotated nil find_circular_primes_rotated_str nil find_circular_primes_s nil main_j nil] (try (do (set! find_circular_primes_result [2]) (set! find_circular_primes_num 3) (loop [while_flag_1 true] (when (and while_flag_1 (<= find_circular_primes_num find_circular_primes_limit)) (do (when (and (is_prime find_circular_primes_num) (= (contains_an_even_digit find_circular_primes_num) false)) (do (set! find_circular_primes_s (mochi_str find_circular_primes_num)) (set! find_circular_primes_all_prime true) (set! main_j 0) (loop [while_flag_2 true] (when (and while_flag_2 (< main_j (count find_circular_primes_s))) (do (set! find_circular_primes_rotated_str (str (subs find_circular_primes_s main_j (min (count find_circular_primes_s) (count find_circular_primes_s))) (subs find_circular_primes_s 0 (min main_j (count find_circular_primes_s))))) (set! find_circular_primes_rotated (parse_int find_circular_primes_rotated_str)) (if (not (is_prime find_circular_primes_rotated)) (do (set! find_circular_primes_all_prime false) (recur false)) (alter-var-root (var main_j) (fn [_] (+ main_j 1)))) (cond :else (recur while_flag_2))))) (when find_circular_primes_all_prime (set! find_circular_primes_result (conj find_circular_primes_result find_circular_primes_num))))) (set! find_circular_primes_num (+ find_circular_primes_num 2)) (cond :else (recur while_flag_1))))) (throw (ex-info "return" {:v find_circular_primes_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution []
  (try (throw (ex-info "return" {:v (count (find_circular_primes main_LIMIT))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_LIMIT) (constantly 10000))
      (alter-var-root (var main_sieve) (constantly []))
      (alter-var-root (var main_i) (constantly 0))
      (while (<= main_i main_LIMIT) (do (alter-var-root (var main_sieve) (constantly (conj main_sieve true))) (alter-var-root (var main_i) (constantly (+ main_i 1)))))
      (alter-var-root (var main_p) (constantly 2))
      (while (<= (* main_p main_p) main_LIMIT) (do (when (nth main_sieve main_p) (do (def ^:dynamic main_j (* main_p main_p)) (while (<= main_j main_LIMIT) (do (alter-var-root (var main_sieve) (constantly (assoc main_sieve main_j false))) (alter-var-root (var main_j) (constantly (+ main_j main_p))))))) (alter-var-root (var main_p) (constantly (+ main_p 1)))))
      (println (str "len(find_circular_primes()) = " (mochi_str (solution))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
