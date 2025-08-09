(ns main (:refer-clojure :exclude [mod greatest_common_divisor extended_gcd extended_euclid invert_modulo modular_division modular_division2 tests main]))

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
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare mod greatest_common_divisor extended_gcd extended_euclid invert_modulo modular_division modular_division2 tests main)

(def ^:dynamic extended_euclid_res nil)

(def ^:dynamic extended_euclid_x nil)

(def ^:dynamic extended_euclid_y nil)

(def ^:dynamic extended_gcd_d nil)

(def ^:dynamic extended_gcd_p nil)

(def ^:dynamic extended_gcd_q nil)

(def ^:dynamic extended_gcd_res nil)

(def ^:dynamic extended_gcd_x nil)

(def ^:dynamic extended_gcd_y nil)

(def ^:dynamic greatest_common_divisor_t nil)

(def ^:dynamic greatest_common_divisor_x nil)

(def ^:dynamic greatest_common_divisor_y nil)

(def ^:dynamic invert_modulo_inv nil)

(def ^:dynamic invert_modulo_res nil)

(def ^:dynamic mod_r nil)

(def ^:dynamic modular_division2_s nil)

(def ^:dynamic modular_division_eg nil)

(def ^:dynamic modular_division_s nil)

(def ^:dynamic tests_eg nil)

(def ^:dynamic tests_eu nil)

(defn mod [mod_a mod_n]
  (binding [mod_r nil] (try (do (set! mod_r (mod mod_a mod_n)) (if (< mod_r 0) (+ mod_r mod_n) mod_r)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn greatest_common_divisor [greatest_common_divisor_a greatest_common_divisor_b]
  (binding [greatest_common_divisor_t nil greatest_common_divisor_x nil greatest_common_divisor_y nil] (try (do (set! greatest_common_divisor_x (if (< greatest_common_divisor_a 0) (- greatest_common_divisor_a) greatest_common_divisor_a)) (set! greatest_common_divisor_y (if (< greatest_common_divisor_b 0) (- greatest_common_divisor_b) greatest_common_divisor_b)) (while (not= greatest_common_divisor_y 0) (do (set! greatest_common_divisor_t (mod greatest_common_divisor_x greatest_common_divisor_y)) (set! greatest_common_divisor_x greatest_common_divisor_y) (set! greatest_common_divisor_y greatest_common_divisor_t))) (throw (ex-info "return" {:v greatest_common_divisor_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn extended_gcd [extended_gcd_a extended_gcd_b]
  (binding [extended_gcd_d nil extended_gcd_p nil extended_gcd_q nil extended_gcd_res nil extended_gcd_x nil extended_gcd_y nil] (try (do (when (= extended_gcd_b 0) (throw (ex-info "return" {:v [extended_gcd_a 1 0]}))) (set! extended_gcd_res (extended_gcd extended_gcd_b (mod extended_gcd_a extended_gcd_b))) (set! extended_gcd_d (nth extended_gcd_res 0)) (set! extended_gcd_p (nth extended_gcd_res 1)) (set! extended_gcd_q (nth extended_gcd_res 2)) (set! extended_gcd_x extended_gcd_q) (set! extended_gcd_y (- extended_gcd_p (* extended_gcd_q (quot extended_gcd_a extended_gcd_b)))) (throw (ex-info "return" {:v [extended_gcd_d extended_gcd_x extended_gcd_y]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn extended_euclid [extended_euclid_a extended_euclid_b]
  (binding [extended_euclid_res nil extended_euclid_x nil extended_euclid_y nil] (try (do (when (= extended_euclid_b 0) (throw (ex-info "return" {:v [1 0]}))) (set! extended_euclid_res (extended_euclid extended_euclid_b (mod extended_euclid_a extended_euclid_b))) (set! extended_euclid_x (nth extended_euclid_res 1)) (set! extended_euclid_y (- (nth extended_euclid_res 0) (* (quot extended_euclid_a extended_euclid_b) (nth extended_euclid_res 1)))) (throw (ex-info "return" {:v [extended_euclid_x extended_euclid_y]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn invert_modulo [invert_modulo_a invert_modulo_n]
  (binding [invert_modulo_inv nil invert_modulo_res nil] (try (do (set! invert_modulo_res (extended_euclid invert_modulo_a invert_modulo_n)) (set! invert_modulo_inv (nth invert_modulo_res 0)) (throw (ex-info "return" {:v (mod invert_modulo_inv invert_modulo_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn modular_division [modular_division_a modular_division_b modular_division_n]
  (binding [modular_division_eg nil modular_division_s nil] (try (do (when (<= modular_division_n 1) (throw (Exception. "n must be > 1"))) (when (<= modular_division_a 0) (throw (Exception. "a must be > 0"))) (when (not= (greatest_common_divisor modular_division_a modular_division_n) 1) (throw (Exception. "gcd(a,n) != 1"))) (set! modular_division_eg (extended_gcd modular_division_n modular_division_a)) (set! modular_division_s (nth modular_division_eg 2)) (throw (ex-info "return" {:v (mod (* modular_division_b modular_division_s) modular_division_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn modular_division2 [modular_division2_a modular_division2_b modular_division2_n]
  (binding [modular_division2_s nil] (try (do (set! modular_division2_s (invert_modulo modular_division2_a modular_division2_n)) (throw (ex-info "return" {:v (mod (* modular_division2_b modular_division2_s) modular_division2_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn tests []
  (binding [tests_eg nil tests_eu nil] (do (when (not= (modular_division 4 8 5) 2) (throw (Exception. "md1"))) (when (not= (modular_division 3 8 5) 1) (throw (Exception. "md2"))) (when (not= (modular_division 4 11 5) 4) (throw (Exception. "md3"))) (when (not= (modular_division2 4 8 5) 2) (throw (Exception. "md21"))) (when (not= (modular_division2 3 8 5) 1) (throw (Exception. "md22"))) (when (not= (modular_division2 4 11 5) 4) (throw (Exception. "md23"))) (when (not= (invert_modulo 2 5) 3) (throw (Exception. "inv"))) (set! tests_eg (extended_gcd 10 6)) (when (or (or (not= (nth tests_eg 0) 2) (not= (nth tests_eg 1) (- 1))) (not= (nth tests_eg 2) 2)) (throw (Exception. "eg"))) (set! tests_eu (extended_euclid 10 6)) (when (or (not= (nth tests_eu 0) (- 1)) (not= (nth tests_eu 1) 2)) (throw (Exception. "eu"))) (when (not= (greatest_common_divisor 121 11) 11) (throw (Exception. "gcd"))))))

(defn main []
  (do (tests) (println (str (modular_division 4 8 5)))))

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
