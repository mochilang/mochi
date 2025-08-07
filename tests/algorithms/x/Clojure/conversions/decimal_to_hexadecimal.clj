(ns main (:refer-clojure :exclude [decimal_to_hexadecimal]))

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

(declare decimal_to_hexadecimal)

(def ^:dynamic decimal_to_hexadecimal_hex nil)

(def ^:dynamic decimal_to_hexadecimal_negative nil)

(def ^:dynamic decimal_to_hexadecimal_num nil)

(def ^:dynamic decimal_to_hexadecimal_remainder nil)

(def ^:dynamic main_values ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "a" "b" "c" "d" "e" "f"])

(defn decimal_to_hexadecimal [decimal_to_hexadecimal_decimal]
  (binding [decimal_to_hexadecimal_hex nil decimal_to_hexadecimal_negative nil decimal_to_hexadecimal_num nil decimal_to_hexadecimal_remainder nil] (try (do (set! decimal_to_hexadecimal_num decimal_to_hexadecimal_decimal) (set! decimal_to_hexadecimal_negative false) (when (< decimal_to_hexadecimal_num 0) (do (set! decimal_to_hexadecimal_negative true) (set! decimal_to_hexadecimal_num (- decimal_to_hexadecimal_num)))) (when (= decimal_to_hexadecimal_num 0) (do (when decimal_to_hexadecimal_negative (throw (ex-info "return" {:v "-0x0"}))) (throw (ex-info "return" {:v "0x0"})))) (set! decimal_to_hexadecimal_hex "") (while (> decimal_to_hexadecimal_num 0) (do (set! decimal_to_hexadecimal_remainder (mod decimal_to_hexadecimal_num 16)) (set! decimal_to_hexadecimal_hex (str (nth main_values decimal_to_hexadecimal_remainder) decimal_to_hexadecimal_hex)) (set! decimal_to_hexadecimal_num (quot decimal_to_hexadecimal_num 16)))) (if decimal_to_hexadecimal_negative (str "-0x" decimal_to_hexadecimal_hex) (str "0x" decimal_to_hexadecimal_hex))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (decimal_to_hexadecimal 5))
      (println (decimal_to_hexadecimal 15))
      (println (decimal_to_hexadecimal 37))
      (println (decimal_to_hexadecimal 255))
      (println (decimal_to_hexadecimal 4096))
      (println (decimal_to_hexadecimal 999098))
      (println (decimal_to_hexadecimal (- 256)))
      (println (decimal_to_hexadecimal 0))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
