(ns main (:refer-clojure :exclude [to_binary4 binary_coded_decimal]))

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

(declare to_binary4 binary_coded_decimal)

(def ^:dynamic binary_coded_decimal_d nil)

(def ^:dynamic binary_coded_decimal_d_int nil)

(def ^:dynamic binary_coded_decimal_digits nil)

(def ^:dynamic binary_coded_decimal_i nil)

(def ^:dynamic binary_coded_decimal_n nil)

(def ^:dynamic binary_coded_decimal_out nil)

(def ^:dynamic to_binary4_result nil)

(def ^:dynamic to_binary4_x nil)

(defn to_binary4 [to_binary4_n]
  (binding [to_binary4_result nil to_binary4_x nil] (try (do (set! to_binary4_result "") (set! to_binary4_x to_binary4_n) (while (> to_binary4_x 0) (do (set! to_binary4_result (str (str (mod to_binary4_x 2)) to_binary4_result)) (set! to_binary4_x (quot to_binary4_x 2)))) (while (< (count to_binary4_result) 4) (set! to_binary4_result (str "0" to_binary4_result))) (throw (ex-info "return" {:v to_binary4_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn binary_coded_decimal [binary_coded_decimal_number]
  (binding [binary_coded_decimal_d nil binary_coded_decimal_d_int nil binary_coded_decimal_digits nil binary_coded_decimal_i nil binary_coded_decimal_n nil binary_coded_decimal_out nil] (try (do (set! binary_coded_decimal_n binary_coded_decimal_number) (when (< binary_coded_decimal_n 0) (set! binary_coded_decimal_n 0)) (set! binary_coded_decimal_digits (str binary_coded_decimal_n)) (set! binary_coded_decimal_out "0b") (set! binary_coded_decimal_i 0) (while (< binary_coded_decimal_i (count binary_coded_decimal_digits)) (do (set! binary_coded_decimal_d (nth binary_coded_decimal_digits binary_coded_decimal_i)) (set! binary_coded_decimal_d_int (long binary_coded_decimal_d)) (set! binary_coded_decimal_out (str binary_coded_decimal_out (to_binary4 binary_coded_decimal_d_int))) (set! binary_coded_decimal_i (+ binary_coded_decimal_i 1)))) (throw (ex-info "return" {:v binary_coded_decimal_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (binary_coded_decimal (- 2)))
      (println (binary_coded_decimal (- 1)))
      (println (binary_coded_decimal 0))
      (println (binary_coded_decimal 3))
      (println (binary_coded_decimal 2))
      (println (binary_coded_decimal 12))
      (println (binary_coded_decimal 987))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
