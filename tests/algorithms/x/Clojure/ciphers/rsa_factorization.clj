(ns main (:refer-clojure :exclude [gcd pow_mod rsa_factor]))

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

(declare gcd pow_mod rsa_factor)

(def ^:dynamic gcd_t nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic pow_mod_b nil)

(def ^:dynamic pow_mod_e nil)

(def ^:dynamic pow_mod_result nil)

(def ^:dynamic rsa_factor_g nil)

(def ^:dynamic rsa_factor_k nil)

(def ^:dynamic rsa_factor_p nil)

(def ^:dynamic rsa_factor_q nil)

(def ^:dynamic rsa_factor_t nil)

(def ^:dynamic rsa_factor_x nil)

(def ^:dynamic rsa_factor_y nil)

(defn gcd [gcd_a gcd_b]
  (binding [gcd_t nil gcd_x nil gcd_y nil] (try (do (set! gcd_x gcd_a) (set! gcd_y gcd_b) (while (not= gcd_y 0) (do (set! gcd_t (mod gcd_x gcd_y)) (set! gcd_x gcd_y) (set! gcd_y gcd_t))) (if (< gcd_x 0) (- gcd_x) gcd_x)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow_mod [pow_mod_base pow_mod_exp pow_mod_mod]
  (binding [pow_mod_b nil pow_mod_e nil pow_mod_result nil] (try (do (set! pow_mod_result 1) (set! pow_mod_b (mod pow_mod_base pow_mod_mod)) (set! pow_mod_e pow_mod_exp) (while (> pow_mod_e 0) (do (when (= (mod pow_mod_e 2) 1) (set! pow_mod_result (mod (* pow_mod_result pow_mod_b) pow_mod_mod))) (set! pow_mod_e (quot pow_mod_e 2)) (set! pow_mod_b (mod (* pow_mod_b pow_mod_b) pow_mod_mod)))) (throw (ex-info "return" {:v pow_mod_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rsa_factor [rsa_factor_d rsa_factor_e rsa_factor_n]
  (binding [rsa_factor_g nil rsa_factor_k nil rsa_factor_p nil rsa_factor_q nil rsa_factor_t nil rsa_factor_x nil rsa_factor_y nil] (try (do (set! rsa_factor_k (- (* rsa_factor_d rsa_factor_e) 1)) (set! rsa_factor_p 0) (set! rsa_factor_q 0) (set! rsa_factor_g 2) (while (and (= rsa_factor_p 0) (< rsa_factor_g rsa_factor_n)) (do (set! rsa_factor_t rsa_factor_k) (loop [while_flag_1 true] (when (and while_flag_1 (= (mod rsa_factor_t 2) 0)) (do (set! rsa_factor_t (quot rsa_factor_t 2)) (set! rsa_factor_x (pow_mod rsa_factor_g rsa_factor_t rsa_factor_n)) (set! rsa_factor_y (gcd (- rsa_factor_x 1) rsa_factor_n)) (cond (and (> rsa_factor_x 1) (> rsa_factor_y 1)) (do (set! rsa_factor_p rsa_factor_y) (set! rsa_factor_q (/ rsa_factor_n rsa_factor_y)) (recur false)) :else (recur while_flag_1))))) (set! rsa_factor_g (+ rsa_factor_g 1)))) (if (> rsa_factor_p rsa_factor_q) [rsa_factor_q rsa_factor_p] [rsa_factor_p rsa_factor_q])) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (rsa_factor 3 16971 25777))
      (println (rsa_factor 7331 11 27233))
      (println (rsa_factor 4021 13 17711))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
