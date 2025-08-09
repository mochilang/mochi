(ns main (:refer-clojure :exclude [binary_exp_recursive binary_exp_iterative binary_exp_mod_recursive binary_exp_mod_iterative]))

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

(declare binary_exp_recursive binary_exp_iterative binary_exp_mod_recursive binary_exp_mod_iterative)

(def ^:dynamic binary_exp_iterative_b nil)

(def ^:dynamic binary_exp_iterative_e nil)

(def ^:dynamic binary_exp_iterative_result nil)

(def ^:dynamic binary_exp_mod_iterative_b nil)

(def ^:dynamic binary_exp_mod_iterative_e nil)

(def ^:dynamic binary_exp_mod_iterative_result nil)

(def ^:dynamic binary_exp_mod_recursive_r nil)

(def ^:dynamic binary_exp_recursive_half nil)

(defn binary_exp_recursive [binary_exp_recursive_base binary_exp_recursive_exponent]
  (binding [binary_exp_recursive_half nil] (try (do (when (< binary_exp_recursive_exponent 0) (throw (Exception. "exponent must be non-negative"))) (when (= binary_exp_recursive_exponent 0) (throw (ex-info "return" {:v 1.0}))) (when (= (mod binary_exp_recursive_exponent 2) 1) (throw (ex-info "return" {:v (* (binary_exp_recursive binary_exp_recursive_base (- binary_exp_recursive_exponent 1)) binary_exp_recursive_base)}))) (set! binary_exp_recursive_half (binary_exp_recursive binary_exp_recursive_base (quot binary_exp_recursive_exponent 2))) (throw (ex-info "return" {:v (* binary_exp_recursive_half binary_exp_recursive_half)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_exp_iterative [binary_exp_iterative_base binary_exp_iterative_exponent]
  (binding [binary_exp_iterative_b nil binary_exp_iterative_e nil binary_exp_iterative_result nil] (try (do (when (< binary_exp_iterative_exponent 0) (throw (Exception. "exponent must be non-negative"))) (set! binary_exp_iterative_result 1.0) (set! binary_exp_iterative_b binary_exp_iterative_base) (set! binary_exp_iterative_e binary_exp_iterative_exponent) (while (> binary_exp_iterative_e 0) (do (when (= (mod binary_exp_iterative_e 2) 1) (set! binary_exp_iterative_result (* binary_exp_iterative_result binary_exp_iterative_b))) (set! binary_exp_iterative_b (* binary_exp_iterative_b binary_exp_iterative_b)) (set! binary_exp_iterative_e (quot binary_exp_iterative_e 2)))) (throw (ex-info "return" {:v binary_exp_iterative_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_exp_mod_recursive [binary_exp_mod_recursive_base binary_exp_mod_recursive_exponent binary_exp_mod_recursive_modulus]
  (binding [binary_exp_mod_recursive_r nil] (try (do (when (< binary_exp_mod_recursive_exponent 0) (throw (Exception. "exponent must be non-negative"))) (when (<= binary_exp_mod_recursive_modulus 0) (throw (Exception. "modulus must be positive"))) (when (= binary_exp_mod_recursive_exponent 0) (throw (ex-info "return" {:v (mod 1 binary_exp_mod_recursive_modulus)}))) (when (= (mod binary_exp_mod_recursive_exponent 2) 1) (throw (ex-info "return" {:v (mod (* (binary_exp_mod_recursive binary_exp_mod_recursive_base (- binary_exp_mod_recursive_exponent 1) binary_exp_mod_recursive_modulus) (mod binary_exp_mod_recursive_base binary_exp_mod_recursive_modulus)) binary_exp_mod_recursive_modulus)}))) (set! binary_exp_mod_recursive_r (binary_exp_mod_recursive binary_exp_mod_recursive_base (quot binary_exp_mod_recursive_exponent 2) binary_exp_mod_recursive_modulus)) (throw (ex-info "return" {:v (mod (* binary_exp_mod_recursive_r binary_exp_mod_recursive_r) binary_exp_mod_recursive_modulus)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_exp_mod_iterative [binary_exp_mod_iterative_base binary_exp_mod_iterative_exponent binary_exp_mod_iterative_modulus]
  (binding [binary_exp_mod_iterative_b nil binary_exp_mod_iterative_e nil binary_exp_mod_iterative_result nil] (try (do (when (< binary_exp_mod_iterative_exponent 0) (throw (Exception. "exponent must be non-negative"))) (when (<= binary_exp_mod_iterative_modulus 0) (throw (Exception. "modulus must be positive"))) (set! binary_exp_mod_iterative_result (mod 1 binary_exp_mod_iterative_modulus)) (set! binary_exp_mod_iterative_b (mod binary_exp_mod_iterative_base binary_exp_mod_iterative_modulus)) (set! binary_exp_mod_iterative_e binary_exp_mod_iterative_exponent) (while (> binary_exp_mod_iterative_e 0) (do (when (= (mod binary_exp_mod_iterative_e 2) 1) (set! binary_exp_mod_iterative_result (mod (* binary_exp_mod_iterative_result binary_exp_mod_iterative_b) binary_exp_mod_iterative_modulus))) (set! binary_exp_mod_iterative_b (mod (* binary_exp_mod_iterative_b binary_exp_mod_iterative_b) binary_exp_mod_iterative_modulus)) (set! binary_exp_mod_iterative_e (quot binary_exp_mod_iterative_e 2)))) (throw (ex-info "return" {:v binary_exp_mod_iterative_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (binary_exp_recursive 3.0 5))
      (println (binary_exp_iterative 1.5 4))
      (println (binary_exp_mod_recursive 3 4 5))
      (println (binary_exp_mod_iterative 11 13 7))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
