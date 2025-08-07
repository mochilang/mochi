(ns main (:refer-clojure :exclude [octal_to_hex]))

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

(declare octal_to_hex)

(def ^:dynamic main_t nil)

(def ^:dynamic octal_to_hex_c nil)

(def ^:dynamic octal_to_hex_d nil)

(def ^:dynamic octal_to_hex_decimal nil)

(def ^:dynamic octal_to_hex_hex nil)

(def ^:dynamic octal_to_hex_hex_chars nil)

(def ^:dynamic octal_to_hex_idx nil)

(def ^:dynamic octal_to_hex_j nil)

(def ^:dynamic octal_to_hex_k nil)

(def ^:dynamic octal_to_hex_s nil)

(defn octal_to_hex [octal_to_hex_octal]
  (binding [octal_to_hex_c nil octal_to_hex_d nil octal_to_hex_decimal nil octal_to_hex_hex nil octal_to_hex_hex_chars nil octal_to_hex_idx nil octal_to_hex_j nil octal_to_hex_k nil octal_to_hex_s nil] (try (do (set! octal_to_hex_s octal_to_hex_octal) (when (and (and (>= (count octal_to_hex_s) 2) (= (nth octal_to_hex_s 0) "0")) (= (nth octal_to_hex_s 1) "o")) (set! octal_to_hex_s (subs octal_to_hex_s 2 (min (count octal_to_hex_s) (count octal_to_hex_s))))) (when (= (count octal_to_hex_s) 0) (throw (Exception. "Empty string was passed to the function"))) (set! octal_to_hex_j 0) (while (< octal_to_hex_j (count octal_to_hex_s)) (do (set! octal_to_hex_c (nth octal_to_hex_s octal_to_hex_j)) (when (and (and (and (and (and (and (and (not= octal_to_hex_c "0") (not= octal_to_hex_c "1")) (not= octal_to_hex_c "2")) (not= octal_to_hex_c "3")) (not= octal_to_hex_c "4")) (not= octal_to_hex_c "5")) (not= octal_to_hex_c "6")) (not= octal_to_hex_c "7")) (throw (Exception. "Not a Valid Octal Number"))) (set! octal_to_hex_j (+ octal_to_hex_j 1)))) (set! octal_to_hex_decimal 0) (set! octal_to_hex_k 0) (while (< octal_to_hex_k (count octal_to_hex_s)) (do (set! octal_to_hex_d (long (nth octal_to_hex_s octal_to_hex_k))) (set! octal_to_hex_decimal (+ (* octal_to_hex_decimal 8) octal_to_hex_d)) (set! octal_to_hex_k (+ octal_to_hex_k 1)))) (set! octal_to_hex_hex_chars "0123456789ABCDEF") (when (= octal_to_hex_decimal 0) (throw (ex-info "return" {:v "0x"}))) (set! octal_to_hex_hex "") (while (> octal_to_hex_decimal 0) (do (set! octal_to_hex_idx (mod octal_to_hex_decimal 16)) (set! octal_to_hex_hex (str (nth octal_to_hex_hex_chars octal_to_hex_idx) octal_to_hex_hex)) (set! octal_to_hex_decimal (quot octal_to_hex_decimal 16)))) (throw (ex-info "return" {:v (str "0x" octal_to_hex_hex)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_nums ["030" "100" "247" "235" "007"])

(def ^:dynamic main_t 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_t (count main_nums)) (do (def ^:dynamic main_num (nth main_nums main_t)) (println (octal_to_hex main_num)) (def main_t (+ main_t 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
