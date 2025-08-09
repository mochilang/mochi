(ns main (:refer-clojure :exclude [set_seed randint jacobi_symbol pow_mod solovay_strassen main]))

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

(declare set_seed randint jacobi_symbol pow_mod solovay_strassen main)

(def ^:dynamic jacobi_symbol_number nil)

(def ^:dynamic jacobi_symbol_r nil)

(def ^:dynamic jacobi_symbol_random_a nil)

(def ^:dynamic jacobi_symbol_t nil)

(def ^:dynamic jacobi_symbol_temp nil)

(def ^:dynamic pow_mod_b nil)

(def ^:dynamic pow_mod_e nil)

(def ^:dynamic pow_mod_result nil)

(def ^:dynamic solovay_strassen_a nil)

(def ^:dynamic solovay_strassen_i nil)

(def ^:dynamic solovay_strassen_mod_x nil)

(def ^:dynamic solovay_strassen_x nil)

(def ^:dynamic solovay_strassen_y nil)

(def ^:dynamic main_seed 1)

(defn set_seed [set_seed_s]
  (do (alter-var-root (var main_seed) (fn [_] set_seed_s)) set_seed_s))

(defn randint [randint_a randint_b]
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v (+ (mod main_seed (+ (- randint_b randint_a) 1)) randint_a)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn jacobi_symbol [jacobi_symbol_random_a_p jacobi_symbol_number_p]
  (binding [jacobi_symbol_number nil jacobi_symbol_r nil jacobi_symbol_random_a nil jacobi_symbol_t nil jacobi_symbol_temp nil] (try (do (set! jacobi_symbol_random_a jacobi_symbol_random_a_p) (set! jacobi_symbol_number jacobi_symbol_number_p) (when (or (= jacobi_symbol_random_a 0) (= jacobi_symbol_random_a 1)) (throw (ex-info "return" {:v jacobi_symbol_random_a}))) (set! jacobi_symbol_random_a (mod jacobi_symbol_random_a jacobi_symbol_number)) (set! jacobi_symbol_t 1) (while (not= jacobi_symbol_random_a 0) (do (while (= (mod jacobi_symbol_random_a 2) 0) (do (set! jacobi_symbol_random_a (quot jacobi_symbol_random_a 2)) (set! jacobi_symbol_r (mod jacobi_symbol_number 8)) (when (or (= jacobi_symbol_r 3) (= jacobi_symbol_r 5)) (set! jacobi_symbol_t (- jacobi_symbol_t))))) (set! jacobi_symbol_temp jacobi_symbol_random_a) (set! jacobi_symbol_random_a jacobi_symbol_number) (set! jacobi_symbol_number jacobi_symbol_temp) (when (and (= (mod jacobi_symbol_random_a 4) 3) (= (mod jacobi_symbol_number 4) 3)) (set! jacobi_symbol_t (- jacobi_symbol_t))) (set! jacobi_symbol_random_a (mod jacobi_symbol_random_a jacobi_symbol_number)))) (if (= jacobi_symbol_number 1) jacobi_symbol_t 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow_mod [pow_mod_base pow_mod_exp pow_mod_mod]
  (binding [pow_mod_b nil pow_mod_e nil pow_mod_result nil] (try (do (set! pow_mod_result 1) (set! pow_mod_b (mod pow_mod_base pow_mod_mod)) (set! pow_mod_e pow_mod_exp) (while (> pow_mod_e 0) (do (when (= (mod pow_mod_e 2) 1) (set! pow_mod_result (mod (* pow_mod_result pow_mod_b) pow_mod_mod))) (set! pow_mod_b (mod (* pow_mod_b pow_mod_b) pow_mod_mod)) (set! pow_mod_e (quot pow_mod_e 2)))) (throw (ex-info "return" {:v pow_mod_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solovay_strassen [solovay_strassen_number solovay_strassen_iterations]
  (binding [solovay_strassen_a nil solovay_strassen_i nil solovay_strassen_mod_x nil solovay_strassen_x nil solovay_strassen_y nil] (try (do (when (<= solovay_strassen_number 1) (throw (ex-info "return" {:v false}))) (when (<= solovay_strassen_number 3) (throw (ex-info "return" {:v true}))) (set! solovay_strassen_i 0) (while (< solovay_strassen_i solovay_strassen_iterations) (do (set! solovay_strassen_a (randint 2 (- solovay_strassen_number 2))) (set! solovay_strassen_x (jacobi_symbol solovay_strassen_a solovay_strassen_number)) (set! solovay_strassen_y (pow_mod solovay_strassen_a (quot (- solovay_strassen_number 1) 2) solovay_strassen_number)) (set! solovay_strassen_mod_x (mod solovay_strassen_x solovay_strassen_number)) (when (< solovay_strassen_mod_x 0) (set! solovay_strassen_mod_x (+ solovay_strassen_mod_x solovay_strassen_number))) (when (or (= solovay_strassen_x 0) (not= solovay_strassen_y solovay_strassen_mod_x)) (throw (ex-info "return" {:v false}))) (set! solovay_strassen_i (+ solovay_strassen_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (set_seed 10) (println (str (solovay_strassen 13 5))) (println (str (solovay_strassen 9 10))) (println (str (solovay_strassen 17 15)))))

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
