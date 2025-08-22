(ns main (:refer-clojure :exclude [generate_primes contains partition solution]))

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

(declare generate_primes contains partition solution)

(declare _read_file)

(def ^:dynamic contains_i nil)

(def ^:dynamic generate_primes_i nil)

(def ^:dynamic generate_primes_is_prime nil)

(def ^:dynamic generate_primes_j nil)

(def ^:dynamic generate_primes_primes nil)

(def ^:dynamic partition_prime nil)

(def ^:dynamic partition_prod nil)

(def ^:dynamic partition_ret nil)

(def ^:dynamic partition_sub nil)

(def ^:dynamic partition_subs nil)

(def ^:dynamic solution_number_to_partition nil)

(def ^:dynamic solution_parts nil)

(def ^:dynamic main_NUM_PRIMES nil)

(defn generate_primes [generate_primes_limit]
  (binding [generate_primes_i nil generate_primes_is_prime nil generate_primes_j nil generate_primes_primes nil] (try (do (set! generate_primes_is_prime []) (set! generate_primes_i 0) (while (<= generate_primes_i generate_primes_limit) (do (set! generate_primes_is_prime (conj generate_primes_is_prime true)) (set! generate_primes_i (+ generate_primes_i 1)))) (set! generate_primes_is_prime (assoc generate_primes_is_prime 0 false)) (set! generate_primes_is_prime (assoc generate_primes_is_prime 1 false)) (set! generate_primes_i 2) (while (<= (* generate_primes_i generate_primes_i) generate_primes_limit) (do (when (nth generate_primes_is_prime generate_primes_i) (do (set! generate_primes_j (* generate_primes_i generate_primes_i)) (while (<= generate_primes_j generate_primes_limit) (do (set! generate_primes_is_prime (assoc generate_primes_is_prime generate_primes_j false)) (set! generate_primes_j (+ generate_primes_j generate_primes_i)))))) (set! generate_primes_i (+ generate_primes_i 1)))) (set! generate_primes_primes []) (set! generate_primes_i 2) (while (<= generate_primes_i generate_primes_limit) (do (when (nth generate_primes_is_prime generate_primes_i) (set! generate_primes_primes (conj generate_primes_primes generate_primes_i))) (set! generate_primes_i (+ generate_primes_i 1)))) (throw (ex-info "return" {:v generate_primes_primes}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_primes nil)

(defn contains [contains_xs contains_value]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_xs)) (do (when (= (nth contains_xs contains_i) contains_value) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_partition_cache nil)

(defn partition [partition_n]
  (binding [partition_prime nil partition_prod nil partition_ret nil partition_sub nil partition_subs nil] (try (do (when (< partition_n 0) (throw (ex-info "return" {:v []}))) (when (= partition_n 0) (throw (ex-info "return" {:v [1]}))) (when (in partition_n main_partition_cache) (throw (ex-info "return" {:v (get main_partition_cache partition_n)}))) (set! partition_ret []) (loop [partition_prime_seq main_primes] (when (seq partition_prime_seq) (let [partition_prime (first partition_prime_seq)] (cond (> partition_prime partition_n) (recur (rest partition_prime_seq)) :else (do (set! partition_subs (partition (- partition_n partition_prime))) (doseq [partition_sub partition_subs] (do (set! partition_prod (* partition_sub partition_prime)) (when (not (contains partition_ret partition_prod)) (set! partition_ret (conj partition_ret partition_prod))))) (recur (rest partition_prime_seq))))))) (alter-var-root (var main_partition_cache) (fn [_] (assoc main_partition_cache partition_n partition_ret))) (throw (ex-info "return" {:v partition_ret}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_threshold]
  (binding [solution_number_to_partition nil solution_parts nil] (try (do (set! solution_number_to_partition 1) (while (< solution_number_to_partition main_NUM_PRIMES) (do (set! solution_parts (partition solution_number_to_partition)) (when (> (count solution_parts) solution_threshold) (throw (ex-info "return" {:v solution_number_to_partition}))) (set! solution_number_to_partition (+ solution_number_to_partition 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_result nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_NUM_PRIMES) (constantly 100))
      (alter-var-root (var main_primes) (constantly (generate_primes main_NUM_PRIMES)))
      (alter-var-root (var main_partition_cache) (constantly {}))
      (alter-var-root (var main_result) (constantly (solution 5000)))
      (println (str "solution() = " (mochi_str main_result)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
