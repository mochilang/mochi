(ns main (:refer-clojure :exclude [int_pow decimal_to_octal]))

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

(declare int_pow decimal_to_octal)

(def ^:dynamic decimal_to_octal_counter nil)

(def ^:dynamic decimal_to_octal_octal nil)

(def ^:dynamic decimal_to_octal_remainder nil)

(def ^:dynamic decimal_to_octal_value nil)

(def ^:dynamic int_pow_i nil)

(def ^:dynamic int_pow_result nil)

(defn int_pow [int_pow_base int_pow_exp]
  (binding [int_pow_i nil int_pow_result nil] (try (do (set! int_pow_result 1) (set! int_pow_i 0) (while (< int_pow_i int_pow_exp) (do (set! int_pow_result (* int_pow_result int_pow_base)) (set! int_pow_i (+ int_pow_i 1)))) (throw (ex-info "return" {:v int_pow_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decimal_to_octal [decimal_to_octal_num]
  (binding [decimal_to_octal_counter nil decimal_to_octal_octal nil decimal_to_octal_remainder nil decimal_to_octal_value nil] (try (do (when (= decimal_to_octal_num 0) (throw (ex-info "return" {:v "0o0"}))) (set! decimal_to_octal_octal 0) (set! decimal_to_octal_counter 0) (set! decimal_to_octal_value decimal_to_octal_num) (while (> decimal_to_octal_value 0) (do (set! decimal_to_octal_remainder (mod decimal_to_octal_value 8)) (set! decimal_to_octal_octal (+ decimal_to_octal_octal (* decimal_to_octal_remainder (int_pow 10 decimal_to_octal_counter)))) (set! decimal_to_octal_counter (+ decimal_to_octal_counter 1)) (set! decimal_to_octal_value (quot decimal_to_octal_value 8)))) (throw (ex-info "return" {:v (str "0o" (str decimal_to_octal_octal))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (decimal_to_octal 2))
      (println (decimal_to_octal 8))
      (println (decimal_to_octal 65))
      (println (decimal_to_octal 216))
      (println (decimal_to_octal 512))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
