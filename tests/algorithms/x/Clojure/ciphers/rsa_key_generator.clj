(ns main (:refer-clojure :exclude [pow2 next_seed rand_range gcd mod_inverse is_prime generate_prime generate_key]))

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

(declare pow2 next_seed rand_range gcd mod_inverse is_prime generate_prime generate_key)

(def ^:dynamic gcd_temp nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic generate_key_d nil)

(def ^:dynamic generate_key_e nil)

(def ^:dynamic generate_key_n nil)

(def ^:dynamic generate_key_p nil)

(def ^:dynamic generate_key_phi nil)

(def ^:dynamic generate_key_q nil)

(def ^:dynamic generate_prime_max nil)

(def ^:dynamic generate_prime_min nil)

(def ^:dynamic generate_prime_p nil)

(def ^:dynamic is_prime_i nil)

(def ^:dynamic mod_inverse_newr nil)

(def ^:dynamic mod_inverse_newt nil)

(def ^:dynamic mod_inverse_quotient nil)

(def ^:dynamic mod_inverse_r nil)

(def ^:dynamic mod_inverse_t nil)

(def ^:dynamic mod_inverse_tmp nil)

(def ^:dynamic mod_inverse_tmp_r nil)

(def ^:dynamic pow2_i nil)

(def ^:dynamic pow2_res nil)

(def ^:dynamic rand_range_seed nil)

(defn pow2 [pow2_exp]
  (binding [pow2_i nil pow2_res nil] (try (do (set! pow2_res 1) (set! pow2_i 0) (while (< pow2_i pow2_exp) (do (set! pow2_res (* pow2_res 2)) (set! pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_seed 1)

(defn next_seed [next_seed_x]
  (try (throw (ex-info "return" {:v (mod (+ (* next_seed_x 1103515245) 12345) 2147483648)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rand_range [rand_range_min rand_range_max]
  (binding [rand_range_seed nil] (try (do (set! rand_range_seed (next_seed main_seed)) (throw (ex-info "return" {:v (+ rand_range_min (mod rand_range_seed (- rand_range_max rand_range_min)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gcd [gcd_a gcd_b]
  (binding [gcd_temp nil gcd_x nil gcd_y nil] (try (do (set! gcd_x gcd_a) (set! gcd_y gcd_b) (while (not= gcd_y 0) (do (set! gcd_temp (mod gcd_x gcd_y)) (set! gcd_x gcd_y) (set! gcd_y gcd_temp))) (throw (ex-info "return" {:v gcd_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mod_inverse [mod_inverse_e mod_inverse_phi]
  (binding [mod_inverse_newr nil mod_inverse_newt nil mod_inverse_quotient nil mod_inverse_r nil mod_inverse_t nil mod_inverse_tmp nil mod_inverse_tmp_r nil] (try (do (set! mod_inverse_t 0) (set! mod_inverse_newt 1) (set! mod_inverse_r mod_inverse_phi) (set! mod_inverse_newr mod_inverse_e) (while (not= mod_inverse_newr 0) (do (set! mod_inverse_quotient (/ mod_inverse_r mod_inverse_newr)) (set! mod_inverse_tmp mod_inverse_newt) (set! mod_inverse_newt (- mod_inverse_t (* mod_inverse_quotient mod_inverse_newt))) (set! mod_inverse_t mod_inverse_tmp) (set! mod_inverse_tmp_r mod_inverse_newr) (set! mod_inverse_newr (- mod_inverse_r (* mod_inverse_quotient mod_inverse_newr))) (set! mod_inverse_r mod_inverse_tmp_r))) (when (> mod_inverse_r 1) (throw (ex-info "return" {:v 0}))) (when (< mod_inverse_t 0) (set! mod_inverse_t (+ mod_inverse_t mod_inverse_phi))) (throw (ex-info "return" {:v mod_inverse_t}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_prime [is_prime_n]
  (binding [is_prime_i nil] (try (do (when (< is_prime_n 2) (throw (ex-info "return" {:v false}))) (set! is_prime_i 2) (while (<= (* is_prime_i is_prime_i) is_prime_n) (do (when (= (mod is_prime_n is_prime_i) 0) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_prime [generate_prime_bits]
  (binding [generate_prime_max nil generate_prime_min nil generate_prime_p nil] (try (do (set! generate_prime_min (pow2 (- generate_prime_bits 1))) (set! generate_prime_max (pow2 generate_prime_bits)) (set! generate_prime_p (rand_range generate_prime_min generate_prime_max)) (when (= (mod generate_prime_p 2) 0) (set! generate_prime_p (+ generate_prime_p 1))) (while (not (is_prime generate_prime_p)) (do (set! generate_prime_p (+ generate_prime_p 2)) (when (>= generate_prime_p generate_prime_max) (set! generate_prime_p (+ generate_prime_min 1))))) (throw (ex-info "return" {:v generate_prime_p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_key [generate_key_bits]
  (binding [generate_key_d nil generate_key_e nil generate_key_n nil generate_key_p nil generate_key_phi nil generate_key_q nil] (try (do (set! generate_key_p (generate_prime generate_key_bits)) (set! generate_key_q (generate_prime generate_key_bits)) (set! generate_key_n (* generate_key_p generate_key_q)) (set! generate_key_phi (* (- generate_key_p 1) (- generate_key_q 1))) (set! generate_key_e (rand_range 2 generate_key_phi)) (while (not= (gcd generate_key_e generate_key_phi) 1) (do (set! generate_key_e (+ generate_key_e 1)) (when (>= generate_key_e generate_key_phi) (set! generate_key_e 2)))) (set! generate_key_d (mod_inverse generate_key_e generate_key_phi)) (throw (ex-info "return" {:v {:public_key [generate_key_n generate_key_e] :private_key [generate_key_n generate_key_d]}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_keys (generate_key 8))

(def ^:dynamic main_pub (:public_key main_keys))

(def ^:dynamic main_priv (:private_key main_keys))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (str (str (str "Public key: (" (str (nth main_pub 0))) ", ") (str (nth main_pub 1))) ")"))
      (println (str (str (str (str "Private key: (" (str (nth main_priv 0))) ", ") (str (nth main_priv 1))) ")"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
