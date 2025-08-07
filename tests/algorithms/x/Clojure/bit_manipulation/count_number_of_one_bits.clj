(ns main (:refer-clojure :exclude [bit_and count_bits_kernighan count_bits_modulo main]))

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

(declare bit_and count_bits_kernighan count_bits_modulo main)

(def ^:dynamic bit_and_bit nil)

(def ^:dynamic bit_and_res nil)

(def ^:dynamic bit_and_ua nil)

(def ^:dynamic bit_and_ub nil)

(def ^:dynamic count_bits_kernighan_num nil)

(def ^:dynamic count_bits_kernighan_result nil)

(def ^:dynamic count_bits_modulo_num nil)

(def ^:dynamic count_bits_modulo_result nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_numbers nil)

(defn bit_and [bit_and_a bit_and_b]
  (binding [bit_and_bit nil bit_and_res nil bit_and_ua nil bit_and_ub nil] (try (do (set! bit_and_ua bit_and_a) (set! bit_and_ub bit_and_b) (set! bit_and_res 0) (set! bit_and_bit 1) (while (or (> bit_and_ua 0) (> bit_and_ub 0)) (do (when (and (= (mod bit_and_ua 2) 1) (= (mod bit_and_ub 2) 1)) (set! bit_and_res (+ bit_and_res bit_and_bit))) (set! bit_and_ua (long (quot bit_and_ua 2))) (set! bit_and_ub (long (quot bit_and_ub 2))) (set! bit_and_bit (* bit_and_bit 2)))) (throw (ex-info "return" {:v bit_and_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_bits_kernighan [count_bits_kernighan_n]
  (binding [count_bits_kernighan_num nil count_bits_kernighan_result nil] (try (do (when (< count_bits_kernighan_n 0) (throw (Exception. "the value of input must not be negative"))) (set! count_bits_kernighan_num count_bits_kernighan_n) (set! count_bits_kernighan_result 0) (while (not= count_bits_kernighan_num 0) (do (set! count_bits_kernighan_num (bit_and count_bits_kernighan_num (- count_bits_kernighan_num 1))) (set! count_bits_kernighan_result (+ count_bits_kernighan_result 1)))) (throw (ex-info "return" {:v count_bits_kernighan_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_bits_modulo [count_bits_modulo_n]
  (binding [count_bits_modulo_num nil count_bits_modulo_result nil] (try (do (when (< count_bits_modulo_n 0) (throw (Exception. "the value of input must not be negative"))) (set! count_bits_modulo_num count_bits_modulo_n) (set! count_bits_modulo_result 0) (while (not= count_bits_modulo_num 0) (do (when (= (mod count_bits_modulo_num 2) 1) (set! count_bits_modulo_result (+ count_bits_modulo_result 1))) (set! count_bits_modulo_num (long (quot count_bits_modulo_num 2))))) (throw (ex-info "return" {:v count_bits_modulo_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_i nil main_numbers nil] (do (set! main_numbers [25 37 21 58 0 256]) (set! main_i 0) (while (< main_i (count main_numbers)) (do (println (str (count_bits_kernighan (nth main_numbers main_i)))) (set! main_i (+ main_i 1)))) (set! main_i 0) (while (< main_i (count main_numbers)) (do (println (str (count_bits_modulo (nth main_numbers main_i)))) (set! main_i (+ main_i 1)))))))

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
