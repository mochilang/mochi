(ns main (:refer-clojure :exclude [binary_exponentiation naive_exponent_mod print_bool]))

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

(declare binary_exponentiation naive_exponent_mod print_bool)

(def ^:dynamic binary_exponentiation_b nil)

(def ^:dynamic naive_exponent_mod_i nil)

(def ^:dynamic naive_exponent_mod_result nil)

(defn binary_exponentiation [binary_exponentiation_a binary_exponentiation_n binary_exponentiation_mod]
  (binding [binary_exponentiation_b nil] (try (do (when (= binary_exponentiation_n 0) (throw (ex-info "return" {:v 1}))) (when (= (mod binary_exponentiation_n 2) 1) (throw (ex-info "return" {:v (mod (* (binary_exponentiation binary_exponentiation_a (- binary_exponentiation_n 1) binary_exponentiation_mod) binary_exponentiation_a) binary_exponentiation_mod)}))) (set! binary_exponentiation_b (binary_exponentiation binary_exponentiation_a (quot binary_exponentiation_n 2) binary_exponentiation_mod)) (throw (ex-info "return" {:v (mod (* binary_exponentiation_b binary_exponentiation_b) binary_exponentiation_mod)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn naive_exponent_mod [naive_exponent_mod_a naive_exponent_mod_n naive_exponent_mod_mod]
  (binding [naive_exponent_mod_i nil naive_exponent_mod_result nil] (try (do (set! naive_exponent_mod_result 1) (set! naive_exponent_mod_i 0) (while (< naive_exponent_mod_i naive_exponent_mod_n) (do (set! naive_exponent_mod_result (mod (* naive_exponent_mod_result naive_exponent_mod_a) naive_exponent_mod_mod)) (set! naive_exponent_mod_i (+ naive_exponent_mod_i 1)))) (throw (ex-info "return" {:v naive_exponent_mod_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_bool [print_bool_b]
  (do (if print_bool_b (println true) (println false)) print_bool_b))

(def ^:dynamic main_p 701)

(def ^:dynamic main_a 1000000000)

(def ^:dynamic main_b 10)

(def ^:dynamic main_left (mod (quot main_a main_b) main_p))

(def ^:dynamic main_right_fast (mod (* main_a (binary_exponentiation main_b (- main_p 2) main_p)) main_p))

(def ^:dynamic main_right_naive (mod (* main_a (naive_exponent_mod main_b (- main_p 2) main_p)) main_p))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (print_bool (= main_left main_right_fast))
      (print_bool (= main_left main_right_naive))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
