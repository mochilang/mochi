(ns main (:refer-clojure :exclude [octal_to_binary]))

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

(declare octal_to_binary)

(def ^:dynamic octal_to_binary_binary_digit nil)

(def ^:dynamic octal_to_binary_binary_number nil)

(def ^:dynamic octal_to_binary_digit nil)

(def ^:dynamic octal_to_binary_i nil)

(def ^:dynamic octal_to_binary_j nil)

(def ^:dynamic octal_to_binary_k nil)

(def ^:dynamic octal_to_binary_octal_digits nil)

(def ^:dynamic octal_to_binary_valid nil)

(def ^:dynamic octal_to_binary_value nil)

(defn octal_to_binary [octal_to_binary_octal_number]
  (binding [octal_to_binary_binary_digit nil octal_to_binary_binary_number nil octal_to_binary_digit nil octal_to_binary_i nil octal_to_binary_j nil octal_to_binary_k nil octal_to_binary_octal_digits nil octal_to_binary_valid nil octal_to_binary_value nil] (try (do (when (= (count octal_to_binary_octal_number) 0) (throw (Exception. "Empty string was passed to the function"))) (set! octal_to_binary_octal_digits "01234567") (set! octal_to_binary_binary_number "") (set! octal_to_binary_i 0) (while (< octal_to_binary_i (count octal_to_binary_octal_number)) (do (set! octal_to_binary_digit (nth octal_to_binary_octal_number octal_to_binary_i)) (set! octal_to_binary_valid false) (set! octal_to_binary_j 0) (loop [while_flag_1 true] (when (and while_flag_1 (< octal_to_binary_j (count octal_to_binary_octal_digits))) (cond (= octal_to_binary_digit (nth octal_to_binary_octal_digits octal_to_binary_j)) (do (set! octal_to_binary_valid true) (recur false)) :else (do (set! octal_to_binary_j (+ octal_to_binary_j 1)) (recur while_flag_1))))) (when (not octal_to_binary_valid) (throw (Exception. "Non-octal value was passed to the function"))) (set! octal_to_binary_value (long octal_to_binary_digit)) (set! octal_to_binary_k 0) (set! octal_to_binary_binary_digit "") (while (< octal_to_binary_k 3) (do (set! octal_to_binary_binary_digit (str (str (mod octal_to_binary_value 2)) octal_to_binary_binary_digit)) (set! octal_to_binary_value (quot octal_to_binary_value 2)) (set! octal_to_binary_k (+ octal_to_binary_k 1)))) (set! octal_to_binary_binary_number (str octal_to_binary_binary_number octal_to_binary_binary_digit)) (set! octal_to_binary_i (+ octal_to_binary_i 1)))) (throw (ex-info "return" {:v octal_to_binary_binary_number}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (octal_to_binary "17"))
      (println (octal_to_binary "7"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
