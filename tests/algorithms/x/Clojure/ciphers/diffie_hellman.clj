(ns main (:refer-clojure :exclude [int_to_hex rand_int mod_pow is_valid_public_key generate_private_key]))

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

(declare int_to_hex rand_int mod_pow is_valid_public_key generate_private_key)

(def ^:dynamic int_to_hex_d nil)

(def ^:dynamic int_to_hex_digits nil)

(def ^:dynamic int_to_hex_num nil)

(def ^:dynamic int_to_hex_res nil)

(def ^:dynamic mod_pow_b nil)

(def ^:dynamic mod_pow_e nil)

(def ^:dynamic mod_pow_result nil)

(defn int_to_hex [int_to_hex_n]
  (binding [int_to_hex_d nil int_to_hex_digits nil int_to_hex_num nil int_to_hex_res nil] (try (do (when (= int_to_hex_n 0) (throw (ex-info "return" {:v "0"}))) (set! int_to_hex_digits "0123456789abcdef") (set! int_to_hex_num int_to_hex_n) (set! int_to_hex_res "") (while (> int_to_hex_num 0) (do (set! int_to_hex_d (mod int_to_hex_num 16)) (set! int_to_hex_res (str (nth int_to_hex_digits int_to_hex_d) int_to_hex_res)) (set! int_to_hex_num (quot int_to_hex_num 16)))) (throw (ex-info "return" {:v int_to_hex_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_seed 123456789)

(defn rand_int []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* 1103515245 main_seed) 12345) 2147483648))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_PRIME 23)

(defn mod_pow [mod_pow_base mod_pow_exp]
  (binding [mod_pow_b nil mod_pow_e nil mod_pow_result nil] (try (do (set! mod_pow_result 1) (set! mod_pow_b (mod mod_pow_base main_PRIME)) (set! mod_pow_e mod_pow_exp) (while (> mod_pow_e 0) (do (when (= (mod mod_pow_e 2) 1) (set! mod_pow_result (mod (* mod_pow_result mod_pow_b) main_PRIME))) (set! mod_pow_b (mod (* mod_pow_b mod_pow_b) main_PRIME)) (set! mod_pow_e (quot mod_pow_e 2)))) (throw (ex-info "return" {:v mod_pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_valid_public_key [is_valid_public_key_key]
  (try (if (or (< is_valid_public_key_key 2) (> is_valid_public_key_key (- main_PRIME 2))) false (= (mod_pow is_valid_public_key_key (quot (- main_PRIME 1) 2)) 1)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn generate_private_key []
  (try (throw (ex-info "return" {:v (+ (mod (rand_int) (- main_PRIME 2)) 2)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_generator 5)

(def ^:dynamic main_alice_private (generate_private_key))

(def ^:dynamic main_alice_public (mod_pow main_generator main_alice_private))

(def ^:dynamic main_bob_private (generate_private_key))

(def ^:dynamic main_bob_public (mod_pow main_generator main_bob_private))

(def ^:dynamic main_alice_shared (mod_pow main_bob_public main_alice_private))

(def ^:dynamic main_bob_shared (mod_pow main_alice_public main_bob_private))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (when (not (is_valid_public_key main_alice_public)) (throw (Exception. "Invalid public key")))
      (when (not (is_valid_public_key main_bob_public)) (throw (Exception. "Invalid public key")))
      (println (int_to_hex main_alice_shared))
      (println (int_to_hex main_bob_shared))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
