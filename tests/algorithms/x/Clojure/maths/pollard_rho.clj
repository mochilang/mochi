(ns main (:refer-clojure :exclude [gcd rand_fn pollard_rho test_pollard_rho main]))

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

(declare gcd rand_fn pollard_rho test_pollard_rho main)

(def ^:dynamic gcd_t nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic main_a nil)

(def ^:dynamic main_b nil)

(def ^:dynamic pollard_rho_divisor nil)

(def ^:dynamic pollard_rho_hare nil)

(def ^:dynamic pollard_rho_i nil)

(def ^:dynamic pollard_rho_s nil)

(def ^:dynamic pollard_rho_st nil)

(def ^:dynamic pollard_rho_tortoise nil)

(def ^:dynamic test_pollard_rho_r1 nil)

(def ^:dynamic test_pollard_rho_r2 nil)

(def ^:dynamic test_pollard_rho_r3 nil)

(def ^:dynamic test_pollard_rho_r4 nil)

(def ^:dynamic test_pollard_rho_r5 nil)

(def ^:dynamic test_pollard_rho_r6 nil)

(def ^:dynamic test_pollard_rho_r7 nil)

(defn gcd [gcd_a gcd_b]
  (binding [gcd_t nil gcd_x nil gcd_y nil] (try (do (set! gcd_x (if (< gcd_a 0) (- gcd_a) gcd_a)) (set! gcd_y (if (< gcd_b 0) (- gcd_b) gcd_b)) (while (not= gcd_y 0) (do (set! gcd_t (mod gcd_x gcd_y)) (set! gcd_x gcd_y) (set! gcd_y gcd_t))) (throw (ex-info "return" {:v gcd_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rand_fn [rand_fn_value rand_fn_step rand_fn_modulus]
  (try (throw (ex-info "return" {:v (mod (+ (* rand_fn_value rand_fn_value) rand_fn_step) rand_fn_modulus)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pollard_rho [pollard_rho_num pollard_rho_seed pollard_rho_step pollard_rho_attempts]
  (binding [pollard_rho_divisor nil pollard_rho_hare nil pollard_rho_i nil pollard_rho_s nil pollard_rho_st nil pollard_rho_tortoise nil] (try (do (when (< pollard_rho_num 2) (throw (Exception. "The input value cannot be less than 2"))) (when (and (> pollard_rho_num 2) (= (mod pollard_rho_num 2) 0)) (throw (ex-info "return" {:v {:factor 2 :ok true}}))) (set! pollard_rho_s pollard_rho_seed) (set! pollard_rho_st pollard_rho_step) (set! pollard_rho_i 0) (while (< pollard_rho_i pollard_rho_attempts) (do (set! pollard_rho_tortoise pollard_rho_s) (set! pollard_rho_hare pollard_rho_s) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! pollard_rho_tortoise (rand_fn pollard_rho_tortoise pollard_rho_st pollard_rho_num)) (set! pollard_rho_hare (rand_fn pollard_rho_hare pollard_rho_st pollard_rho_num)) (set! pollard_rho_hare (rand_fn pollard_rho_hare pollard_rho_st pollard_rho_num)) (set! pollard_rho_divisor (gcd (- pollard_rho_hare pollard_rho_tortoise) pollard_rho_num)) (do (when (= pollard_rho_divisor 1) (recur true)) (do (when (= pollard_rho_divisor pollard_rho_num) (recur false)) (throw (ex-info "return" {:v {:factor pollard_rho_divisor :ok true}})))) (cond :else (recur while_flag_1))))) (set! pollard_rho_s pollard_rho_hare) (set! pollard_rho_st (+ pollard_rho_st 1)) (set! pollard_rho_i (+ pollard_rho_i 1)))) (throw (ex-info "return" {:v {:factor 0 :ok false}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_pollard_rho []
  (binding [test_pollard_rho_r1 nil test_pollard_rho_r2 nil test_pollard_rho_r3 nil test_pollard_rho_r4 nil test_pollard_rho_r5 nil test_pollard_rho_r6 nil test_pollard_rho_r7 nil] (do (set! test_pollard_rho_r1 (pollard_rho 8051 2 1 5)) (when (or (not (:ok test_pollard_rho_r1)) (and (not= (:factor test_pollard_rho_r1) 83) (not= (:factor test_pollard_rho_r1) 97))) (throw (Exception. "test1 failed"))) (set! test_pollard_rho_r2 (pollard_rho 10403 2 1 5)) (when (or (not (:ok test_pollard_rho_r2)) (and (not= (:factor test_pollard_rho_r2) 101) (not= (:factor test_pollard_rho_r2) 103))) (throw (Exception. "test2 failed"))) (set! test_pollard_rho_r3 (pollard_rho 100 2 1 3)) (when (or (not (:ok test_pollard_rho_r3)) (not= (:factor test_pollard_rho_r3) 2)) (throw (Exception. "test3 failed"))) (set! test_pollard_rho_r4 (pollard_rho 17 2 1 3)) (when (:ok test_pollard_rho_r4) (throw (Exception. "test4 failed"))) (set! test_pollard_rho_r5 (pollard_rho (* (* 17 17) 17) 2 1 3)) (when (or (not (:ok test_pollard_rho_r5)) (not= (:factor test_pollard_rho_r5) 17)) (throw (Exception. "test5 failed"))) (set! test_pollard_rho_r6 (pollard_rho (* (* 17 17) 17) 2 1 1)) (when (:ok test_pollard_rho_r6) (throw (Exception. "test6 failed"))) (set! test_pollard_rho_r7 (pollard_rho (* (* 3 5) 7) 2 1 3)) (when (or (not (:ok test_pollard_rho_r7)) (not= (:factor test_pollard_rho_r7) 21)) (throw (Exception. "test7 failed"))))))

(defn main []
  (binding [main_a nil main_b nil] (do (test_pollard_rho) (set! main_a (pollard_rho 100 2 1 3)) (if (:ok main_a) (println (str (:factor main_a))) (println "None")) (set! main_b (pollard_rho 17 2 1 3)) (if (:ok main_b) (println (str (:factor main_b))) (println "None")))))

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
