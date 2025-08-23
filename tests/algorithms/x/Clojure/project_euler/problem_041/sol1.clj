(ns main (:refer-clojure :exclude [is_prime remove_at collect_primes max_list solution]))

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

(declare is_prime remove_at collect_primes max_list solution)

(declare _read_file)

(def ^:dynamic collect_primes_digit nil)

(def ^:dynamic collect_primes_i nil)

(def ^:dynamic collect_primes_primes nil)

(def ^:dynamic collect_primes_remaining nil)

(def ^:dynamic collect_primes_res nil)

(def ^:dynamic is_prime_i nil)

(def ^:dynamic max_list_i nil)

(def ^:dynamic max_list_m nil)

(def ^:dynamic remove_at_i nil)

(def ^:dynamic remove_at_res nil)

(def ^:dynamic solution_digits nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_primes nil)

(defn is_prime [is_prime_number]
  (binding [is_prime_i nil] (try (do (if (and (< 1 is_prime_number) (< is_prime_number 4)) (throw (ex-info "return" {:v true})) (when (or (or (< is_prime_number 2) (= (mod is_prime_number 2) 0)) (= (mod is_prime_number 3) 0)) (throw (ex-info "return" {:v false})))) (set! is_prime_i 5) (while (<= (* is_prime_i is_prime_i) is_prime_number) (do (when (or (= (mod is_prime_number is_prime_i) 0) (= (mod is_prime_number (+ is_prime_i 2)) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 6)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_at [remove_at_xs remove_at_index]
  (binding [remove_at_i nil remove_at_res nil] (try (do (set! remove_at_res []) (set! remove_at_i 0) (while (< remove_at_i (count remove_at_xs)) (do (when (not= remove_at_i remove_at_index) (set! remove_at_res (conj remove_at_res (nth remove_at_xs remove_at_i)))) (set! remove_at_i (+ remove_at_i 1)))) (throw (ex-info "return" {:v remove_at_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn collect_primes [collect_primes_nums collect_primes_current collect_primes_primes_p]
  (binding [collect_primes_primes collect_primes_primes_p collect_primes_digit nil collect_primes_i nil collect_primes_remaining nil collect_primes_res nil] (try (do (when (= (count collect_primes_nums) 0) (do (when (is_prime collect_primes_current) (set! collect_primes_primes (conj collect_primes_primes collect_primes_current))) (throw (ex-info "return" {:v collect_primes_primes})))) (set! collect_primes_i 0) (set! collect_primes_res collect_primes_primes) (while (< collect_primes_i (count collect_primes_nums)) (do (set! collect_primes_digit (nth collect_primes_nums collect_primes_i)) (set! collect_primes_remaining (remove_at collect_primes_nums collect_primes_i)) (set! collect_primes_res (let [__res (collect_primes collect_primes_remaining (+ (* collect_primes_current 10) collect_primes_digit) collect_primes_res)] (do (set! collect_primes_res collect_primes_primes) __res))) (set! collect_primes_i (+ collect_primes_i 1)))) (throw (ex-info "return" {:v collect_primes_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var collect_primes_primes) (constantly collect_primes_primes))))))

(defn max_list [max_list_nums]
  (binding [max_list_i nil max_list_m nil] (try (do (set! max_list_m 0) (set! max_list_i 0) (while (< max_list_i (count max_list_nums)) (do (when (> (nth max_list_nums max_list_i) max_list_m) (set! max_list_m (nth max_list_nums max_list_i))) (set! max_list_i (+ max_list_i 1)))) (throw (ex-info "return" {:v max_list_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_n]
  (binding [solution_digits nil solution_i nil solution_primes nil] (try (do (set! solution_digits []) (set! solution_i 1) (while (<= solution_i solution_n) (do (set! solution_digits (conj solution_digits solution_i)) (set! solution_i (+ solution_i 1)))) (set! solution_primes (let [__res (collect_primes solution_digits 0 [])] (do __res))) (if (= (count solution_primes) 0) 0 (max_list solution_primes))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "solution() = " (mochi_str (solution 7))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
