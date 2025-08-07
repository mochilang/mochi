(ns main (:refer-clojure :exclude [rand rand_range mod_pow extended_gcd mod_inverse pow2 is_probable_prime generate_large_prime primitive_root generate_key main]))

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

(declare rand rand_range mod_pow extended_gcd mod_inverse pow2 is_probable_prime generate_large_prime primitive_root generate_key main)

(def ^:dynamic extended_gcd_res nil)

(def ^:dynamic generate_key_d nil)

(def ^:dynamic generate_key_e1 nil)

(def ^:dynamic generate_key_e2 nil)

(def ^:dynamic generate_key_p nil)

(def ^:dynamic generate_key_private_key nil)

(def ^:dynamic generate_key_public_key nil)

(def ^:dynamic generate_large_prime_max nil)

(def ^:dynamic generate_large_prime_min nil)

(def ^:dynamic generate_large_prime_p nil)

(def ^:dynamic is_probable_prime_a nil)

(def ^:dynamic is_probable_prime_d nil)

(def ^:dynamic is_probable_prime_found nil)

(def ^:dynamic is_probable_prime_i nil)

(def ^:dynamic is_probable_prime_j nil)

(def ^:dynamic is_probable_prime_r nil)

(def ^:dynamic is_probable_prime_x nil)

(def ^:dynamic main_key_size nil)

(def ^:dynamic main_kp nil)

(def ^:dynamic main_priv nil)

(def ^:dynamic main_pub nil)

(def ^:dynamic mod_inverse_r nil)

(def ^:dynamic mod_inverse_res nil)

(def ^:dynamic mod_pow_b nil)

(def ^:dynamic mod_pow_e nil)

(def ^:dynamic mod_pow_result nil)

(def ^:dynamic pow2_i nil)

(def ^:dynamic pow2_r nil)

(def ^:dynamic primitive_root_g nil)

(def ^:dynamic main_seed 123456789)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483647))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rand_range [rand_range_min rand_range_max]
  (try (throw (ex-info "return" {:v (+ rand_range_min (mod (rand) (+ (- rand_range_max rand_range_min) 1)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mod_pow [mod_pow_base mod_pow_exponent mod_pow_modulus]
  (binding [mod_pow_b nil mod_pow_e nil mod_pow_result nil] (try (do (set! mod_pow_result 1) (set! mod_pow_b (mod mod_pow_base mod_pow_modulus)) (set! mod_pow_e mod_pow_exponent) (while (> mod_pow_e 0) (do (when (= (mod mod_pow_e 2) 1) (set! mod_pow_result (mod (* mod_pow_result mod_pow_b) mod_pow_modulus))) (set! mod_pow_e (quot mod_pow_e 2)) (set! mod_pow_b (mod (* mod_pow_b mod_pow_b) mod_pow_modulus)))) (throw (ex-info "return" {:v mod_pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn extended_gcd [extended_gcd_a extended_gcd_b]
  (binding [extended_gcd_res nil] (try (do (when (= extended_gcd_b 0) (throw (ex-info "return" {:v {:g extended_gcd_a :x 1 :y 0}}))) (set! extended_gcd_res (extended_gcd extended_gcd_b (mod extended_gcd_a extended_gcd_b))) (throw (ex-info "return" {:v {:g (:g extended_gcd_res) :x (:y extended_gcd_res) :y (- (:x extended_gcd_res) (* (/ extended_gcd_a extended_gcd_b) (:y extended_gcd_res)))}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mod_inverse [mod_inverse_a mod_inverse_m]
  (binding [mod_inverse_r nil mod_inverse_res nil] (try (do (set! mod_inverse_res (extended_gcd mod_inverse_a mod_inverse_m)) (when (not= (:g mod_inverse_res) 1) (throw (Exception. "inverse does not exist"))) (set! mod_inverse_r (mod (:x mod_inverse_res) mod_inverse_m)) (if (< mod_inverse_r 0) (+ mod_inverse_r mod_inverse_m) mod_inverse_r)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow2 [pow2_n]
  (binding [pow2_i nil pow2_r nil] (try (do (set! pow2_r 1) (set! pow2_i 0) (while (< pow2_i pow2_n) (do (set! pow2_r (* pow2_r 2)) (set! pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_probable_prime [is_probable_prime_n is_probable_prime_k]
  (binding [is_probable_prime_a nil is_probable_prime_d nil is_probable_prime_found nil is_probable_prime_i nil is_probable_prime_j nil is_probable_prime_r nil is_probable_prime_x nil] (try (do (when (<= is_probable_prime_n 1) (throw (ex-info "return" {:v false}))) (when (<= is_probable_prime_n 3) (throw (ex-info "return" {:v true}))) (when (= (mod is_probable_prime_n 2) 0) (throw (ex-info "return" {:v false}))) (set! is_probable_prime_r 0) (set! is_probable_prime_d (- is_probable_prime_n 1)) (while (= (mod is_probable_prime_d 2) 0) (do (set! is_probable_prime_d (quot is_probable_prime_d 2)) (set! is_probable_prime_r (+ is_probable_prime_r 1)))) (set! is_probable_prime_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< is_probable_prime_i is_probable_prime_k)) (do (set! is_probable_prime_a (rand_range 2 (- is_probable_prime_n 2))) (set! is_probable_prime_x (mod_pow is_probable_prime_a is_probable_prime_d is_probable_prime_n)) (cond (or (= is_probable_prime_x 1) (= is_probable_prime_x (- is_probable_prime_n 1))) (do (set! is_probable_prime_i (+ is_probable_prime_i 1)) (recur true)) :else (do (set! is_probable_prime_j 1) (set! is_probable_prime_found false) (loop [while_flag_2 true] (when (and while_flag_2 (< is_probable_prime_j is_probable_prime_r)) (do (set! is_probable_prime_x (mod_pow is_probable_prime_x 2 is_probable_prime_n)) (cond (= is_probable_prime_x (- is_probable_prime_n 1)) (do (set! is_probable_prime_found true) (recur false)) :else (do (set! is_probable_prime_j (+ is_probable_prime_j 1)) (recur while_flag_2)))))) (when (not is_probable_prime_found) (throw (ex-info "return" {:v false}))) (set! is_probable_prime_i (+ is_probable_prime_i 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_large_prime [generate_large_prime_bits]
  (binding [generate_large_prime_max nil generate_large_prime_min nil generate_large_prime_p nil] (try (do (set! generate_large_prime_min (pow2 (- generate_large_prime_bits 1))) (set! generate_large_prime_max (- (pow2 generate_large_prime_bits) 1)) (set! generate_large_prime_p (rand_range generate_large_prime_min generate_large_prime_max)) (when (= (mod generate_large_prime_p 2) 0) (set! generate_large_prime_p (+ generate_large_prime_p 1))) (while (not (is_probable_prime generate_large_prime_p 5)) (do (set! generate_large_prime_p (+ generate_large_prime_p 2)) (when (> generate_large_prime_p generate_large_prime_max) (set! generate_large_prime_p (+ generate_large_prime_min 1))))) (throw (ex-info "return" {:v generate_large_prime_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn primitive_root [primitive_root_p]
  (binding [primitive_root_g nil] (try (loop [while_flag_3 true] (when (and while_flag_3 true) (do (set! primitive_root_g (rand_range 3 (- primitive_root_p 1))) (cond (= (mod_pow primitive_root_g 2 primitive_root_p) 1) (recur true) (= (mod_pow primitive_root_g primitive_root_p primitive_root_p) 1) (recur true) :else (throw (ex-info "return" {:v primitive_root_g})))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_key [generate_key_key_size]
  (binding [generate_key_d nil generate_key_e1 nil generate_key_e2 nil generate_key_p nil generate_key_private_key nil generate_key_public_key nil] (try (do (set! generate_key_p (generate_large_prime generate_key_key_size)) (set! generate_key_e1 (primitive_root generate_key_p)) (set! generate_key_d (rand_range 3 (- generate_key_p 1))) (set! generate_key_e2 (mod_inverse (mod_pow generate_key_e1 generate_key_d generate_key_p) generate_key_p)) (set! generate_key_public_key {:key_size generate_key_key_size :g generate_key_e1 :e2 generate_key_e2 :p generate_key_p}) (set! generate_key_private_key {:key_size generate_key_key_size :d generate_key_d}) (throw (ex-info "return" {:v {:public_key generate_key_public_key :private_key generate_key_private_key}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_key_size nil main_kp nil main_priv nil main_pub nil] (do (set! main_key_size 16) (set! main_kp (generate_key main_key_size)) (set! main_pub (:public_key main_kp)) (set! main_priv (:private_key main_kp)) (println (str (str (str (str (str (str (str (str "public key: (" (str (:key_size main_pub))) ", ") (str (:g main_pub))) ", ") (str (:e2 main_pub))) ", ") (str (:p main_pub))) ")")) (println (str (str (str (str "private key: (" (str (:key_size main_priv))) ", ") (str (:d main_priv))) ")")))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
