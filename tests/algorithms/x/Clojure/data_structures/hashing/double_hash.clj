(ns main (:refer-clojure :exclude [is_prime prev_prime create_table hash1 hash2 insert_double_hash table_keys run_example]))

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

(declare is_prime prev_prime create_table hash1 hash2 insert_double_hash table_keys run_example)

(def ^:dynamic count_v nil)

(def ^:dynamic create_table_i nil)

(def ^:dynamic create_table_vals nil)

(def ^:dynamic insert_double_hash_idx nil)

(def ^:dynamic insert_double_hash_step nil)

(def ^:dynamic insert_double_hash_vals nil)

(def ^:dynamic is_prime_i nil)

(def ^:dynamic prev_prime_p nil)

(def ^:dynamic run_example_i nil)

(def ^:dynamic run_example_prime nil)

(def ^:dynamic run_example_table nil)

(def ^:dynamic table_keys_i nil)

(def ^:dynamic table_keys_res nil)

(defn is_prime [is_prime_n]
  (binding [is_prime_i nil] (try (do (when (< is_prime_n 2) (throw (ex-info "return" {:v false}))) (set! is_prime_i 2) (while (<= (* is_prime_i is_prime_i) is_prime_n) (do (when (= (mod is_prime_n is_prime_i) 0) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn prev_prime [prev_prime_n]
  (binding [prev_prime_p nil] (try (do (set! prev_prime_p (- prev_prime_n 1)) (while (>= prev_prime_p 2) (do (when (is_prime prev_prime_p) (throw (ex-info "return" {:v prev_prime_p}))) (set! prev_prime_p (- prev_prime_p 1)))) (throw (ex-info "return" {:v 1}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn create_table [create_table_size]
  (binding [create_table_i nil create_table_vals nil] (try (do (set! create_table_vals []) (set! create_table_i 0) (while (< create_table_i create_table_size) (do (set! create_table_vals (conj create_table_vals (- 1))) (set! create_table_i (+ create_table_i 1)))) (throw (ex-info "return" {:v create_table_vals}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hash1 [hash1_size hash1_key]
  (try (throw (ex-info "return" {:v (mod hash1_key hash1_size)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hash2 [hash2_prime hash2_key]
  (try (throw (ex-info "return" {:v (- hash2_prime (mod hash2_key hash2_prime))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn insert_double_hash [insert_double_hash_values insert_double_hash_size insert_double_hash_prime insert_double_hash_value]
  (binding [count_v nil insert_double_hash_idx nil insert_double_hash_step nil insert_double_hash_vals nil] (try (do (set! insert_double_hash_vals insert_double_hash_values) (set! insert_double_hash_idx (hash1 insert_double_hash_size insert_double_hash_value)) (set! insert_double_hash_step (hash2 insert_double_hash_prime insert_double_hash_value)) (set! count_v 0) (while (and (not= (nth insert_double_hash_vals insert_double_hash_idx) (- 1)) (< count_v insert_double_hash_size)) (do (set! insert_double_hash_idx (mod (+ insert_double_hash_idx insert_double_hash_step) insert_double_hash_size)) (set! count_v (+ count_v 1)))) (when (= (nth insert_double_hash_vals insert_double_hash_idx) (- 1)) (set! insert_double_hash_vals (assoc insert_double_hash_vals insert_double_hash_idx insert_double_hash_value))) (throw (ex-info "return" {:v insert_double_hash_vals}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn table_keys [table_keys_values]
  (binding [table_keys_i nil table_keys_res nil] (try (do (set! table_keys_res {}) (set! table_keys_i 0) (while (< table_keys_i (count table_keys_values)) (do (when (not= (nth table_keys_values table_keys_i) (- 1)) (set! table_keys_res (assoc table_keys_res table_keys_i (nth table_keys_values table_keys_i)))) (set! table_keys_i (+ table_keys_i 1)))) (throw (ex-info "return" {:v table_keys_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn run_example [run_example_size run_example_data]
  (binding [run_example_i nil run_example_prime nil run_example_table nil] (do (set! run_example_prime (prev_prime run_example_size)) (set! run_example_table (create_table run_example_size)) (set! run_example_i 0) (while (< run_example_i (count run_example_data)) (do (set! run_example_table (insert_double_hash run_example_table run_example_size run_example_prime (nth run_example_data run_example_i))) (set! run_example_i (+ run_example_i 1)))) (println (str (table_keys run_example_table))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (run_example 3 [10 20 30])
      (run_example 4 [10 20 30])
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
