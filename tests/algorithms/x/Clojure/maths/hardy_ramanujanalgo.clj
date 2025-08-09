(ns main (:refer-clojure :exclude [exact_prime_factor_count ln floor round4 main]))

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

(declare exact_prime_factor_count ln floor round4 main)

(def ^:dynamic count_v nil)

(def ^:dynamic exact_prime_factor_count_i nil)

(def ^:dynamic exact_prime_factor_count_num nil)

(def ^:dynamic floor_i nil)

(def ^:dynamic ln_k nil)

(def ^:dynamic ln_ln2 nil)

(def ^:dynamic ln_n nil)

(def ^:dynamic ln_sum nil)

(def ^:dynamic ln_t nil)

(def ^:dynamic ln_term nil)

(def ^:dynamic ln_y nil)

(def ^:dynamic main_loglog nil)

(def ^:dynamic main_n nil)

(def ^:dynamic round4_m nil)

(defn exact_prime_factor_count [exact_prime_factor_count_n]
  (binding [count_v nil exact_prime_factor_count_i nil exact_prime_factor_count_num nil] (try (do (set! count_v 0) (set! exact_prime_factor_count_num exact_prime_factor_count_n) (when (= (mod exact_prime_factor_count_num 2) 0) (do (set! count_v (+ count_v 1)) (while (= (mod exact_prime_factor_count_num 2) 0) (set! exact_prime_factor_count_num (quot exact_prime_factor_count_num 2))))) (set! exact_prime_factor_count_i 3) (while (<= (* exact_prime_factor_count_i exact_prime_factor_count_i) exact_prime_factor_count_num) (do (when (= (mod exact_prime_factor_count_num exact_prime_factor_count_i) 0) (do (set! count_v (+ count_v 1)) (while (= (mod exact_prime_factor_count_num exact_prime_factor_count_i) 0) (set! exact_prime_factor_count_num (quot exact_prime_factor_count_num exact_prime_factor_count_i))))) (set! exact_prime_factor_count_i (+ exact_prime_factor_count_i 2)))) (when (> exact_prime_factor_count_num 2) (set! count_v (+ count_v 1))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ln [ln_x]
  (binding [ln_k nil ln_ln2 nil ln_n nil ln_sum nil ln_t nil ln_term nil ln_y nil] (try (do (set! ln_ln2 0.6931471805599453) (set! ln_y ln_x) (set! ln_k 0.0) (while (> ln_y 2.0) (do (set! ln_y (/ ln_y 2.0)) (set! ln_k (+ ln_k ln_ln2)))) (while (< ln_y 1.0) (do (set! ln_y (* ln_y 2.0)) (set! ln_k (- ln_k ln_ln2)))) (set! ln_t (quot (- ln_y 1.0) (+ ln_y 1.0))) (set! ln_term ln_t) (set! ln_sum 0.0) (set! ln_n 1) (while (<= ln_n 19) (do (set! ln_sum (+ ln_sum (quot ln_term (double ln_n)))) (set! ln_term (* (* ln_term ln_t) ln_t)) (set! ln_n (+ ln_n 2)))) (throw (ex-info "return" {:v (+ ln_k (* 2.0 ln_sum))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round4 [round4_x]
  (binding [round4_m nil] (try (do (set! round4_m 10000.0) (throw (ex-info "return" {:v (quot (floor (+ (* round4_x round4_m) 0.5)) round4_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [count_v nil main_loglog nil main_n nil] (do (set! main_n 51242183) (set! count_v (exact_prime_factor_count main_n)) (println (str "The number of distinct prime factors is/are " (str count_v))) (set! main_loglog (ln (ln (double main_n)))) (println (str "The value of log(log(n)) is " (str (round4 main_loglog)))))))

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
