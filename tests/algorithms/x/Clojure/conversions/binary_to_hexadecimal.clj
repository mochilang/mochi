(ns main (:refer-clojure :exclude [strip_spaces repeat_char slice bits_to_int bin_to_hexadecimal]))

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

(declare strip_spaces repeat_char slice bits_to_int bin_to_hexadecimal)

(def ^:dynamic bin_to_hexadecimal_c nil)

(def ^:dynamic bin_to_hexadecimal_chunk nil)

(def ^:dynamic bin_to_hexadecimal_digits nil)

(def ^:dynamic bin_to_hexadecimal_groups nil)

(def ^:dynamic bin_to_hexadecimal_i nil)

(def ^:dynamic bin_to_hexadecimal_is_negative nil)

(def ^:dynamic bin_to_hexadecimal_j nil)

(def ^:dynamic bin_to_hexadecimal_pad_len nil)

(def ^:dynamic bin_to_hexadecimal_res nil)

(def ^:dynamic bin_to_hexadecimal_s nil)

(def ^:dynamic bin_to_hexadecimal_val nil)

(def ^:dynamic bits_to_int_i nil)

(def ^:dynamic bits_to_int_value nil)

(def ^:dynamic repeat_char_i nil)

(def ^:dynamic repeat_char_res nil)

(def ^:dynamic slice_i nil)

(def ^:dynamic slice_res nil)

(def ^:dynamic strip_spaces_end nil)

(def ^:dynamic strip_spaces_i nil)

(def ^:dynamic strip_spaces_res nil)

(def ^:dynamic strip_spaces_start nil)

(defn strip_spaces [strip_spaces_s]
  (binding [strip_spaces_end nil strip_spaces_i nil strip_spaces_res nil strip_spaces_start nil] (try (do (set! strip_spaces_start 0) (set! strip_spaces_end (- (count strip_spaces_s) 1)) (while (and (< strip_spaces_start (count strip_spaces_s)) (= (nth strip_spaces_s strip_spaces_start) " ")) (set! strip_spaces_start (+ strip_spaces_start 1))) (while (and (>= strip_spaces_end strip_spaces_start) (= (nth strip_spaces_s strip_spaces_end) " ")) (set! strip_spaces_end (- strip_spaces_end 1))) (set! strip_spaces_res "") (set! strip_spaces_i strip_spaces_start) (while (<= strip_spaces_i strip_spaces_end) (do (set! strip_spaces_res (str strip_spaces_res (nth strip_spaces_s strip_spaces_i))) (set! strip_spaces_i (+ strip_spaces_i 1)))) (throw (ex-info "return" {:v strip_spaces_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn repeat_char [repeat_char_ch count_v]
  (binding [repeat_char_i nil repeat_char_res nil] (try (do (set! repeat_char_res "") (set! repeat_char_i 0) (while (< repeat_char_i count_v) (do (set! repeat_char_res (str repeat_char_res repeat_char_ch)) (set! repeat_char_i (+ repeat_char_i 1)))) (throw (ex-info "return" {:v repeat_char_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn slice [slice_s slice_start slice_end]
  (binding [slice_i nil slice_res nil] (try (do (set! slice_res "") (set! slice_i slice_start) (while (< slice_i slice_end) (do (set! slice_res (str slice_res (nth slice_s slice_i))) (set! slice_i (+ slice_i 1)))) (throw (ex-info "return" {:v slice_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bits_to_int [bits_to_int_bits]
  (binding [bits_to_int_i nil bits_to_int_value nil] (try (do (set! bits_to_int_value 0) (set! bits_to_int_i 0) (while (< bits_to_int_i (count bits_to_int_bits)) (do (set! bits_to_int_value (* bits_to_int_value 2)) (when (= (nth bits_to_int_bits bits_to_int_i) "1") (set! bits_to_int_value (+ bits_to_int_value 1))) (set! bits_to_int_i (+ bits_to_int_i 1)))) (throw (ex-info "return" {:v bits_to_int_value}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bin_to_hexadecimal [bin_to_hexadecimal_binary_str]
  (binding [bin_to_hexadecimal_c nil bin_to_hexadecimal_chunk nil bin_to_hexadecimal_digits nil bin_to_hexadecimal_groups nil bin_to_hexadecimal_i nil bin_to_hexadecimal_is_negative nil bin_to_hexadecimal_j nil bin_to_hexadecimal_pad_len nil bin_to_hexadecimal_res nil bin_to_hexadecimal_s nil bin_to_hexadecimal_val nil] (try (do (set! bin_to_hexadecimal_s (strip_spaces bin_to_hexadecimal_binary_str)) (when (= (count bin_to_hexadecimal_s) 0) (throw (Exception. "Empty string was passed to the function"))) (set! bin_to_hexadecimal_is_negative false) (when (= (nth bin_to_hexadecimal_s 0) "-") (do (set! bin_to_hexadecimal_is_negative true) (set! bin_to_hexadecimal_s (slice bin_to_hexadecimal_s 1 (count bin_to_hexadecimal_s))))) (set! bin_to_hexadecimal_i 0) (while (< bin_to_hexadecimal_i (count bin_to_hexadecimal_s)) (do (set! bin_to_hexadecimal_c (nth bin_to_hexadecimal_s bin_to_hexadecimal_i)) (when (and (not= bin_to_hexadecimal_c "0") (not= bin_to_hexadecimal_c "1")) (throw (Exception. "Non-binary value was passed to the function"))) (set! bin_to_hexadecimal_i (+ bin_to_hexadecimal_i 1)))) (set! bin_to_hexadecimal_groups (+ (quot (count bin_to_hexadecimal_s) 4) 1)) (set! bin_to_hexadecimal_pad_len (- (* bin_to_hexadecimal_groups 4) (count bin_to_hexadecimal_s))) (set! bin_to_hexadecimal_s (str (repeat_char "0" bin_to_hexadecimal_pad_len) bin_to_hexadecimal_s)) (set! bin_to_hexadecimal_digits "0123456789abcdef") (set! bin_to_hexadecimal_res "0x") (set! bin_to_hexadecimal_j 0) (while (< bin_to_hexadecimal_j (count bin_to_hexadecimal_s)) (do (set! bin_to_hexadecimal_chunk (slice bin_to_hexadecimal_s bin_to_hexadecimal_j (+ bin_to_hexadecimal_j 4))) (set! bin_to_hexadecimal_val (bits_to_int bin_to_hexadecimal_chunk)) (set! bin_to_hexadecimal_res (str bin_to_hexadecimal_res (nth bin_to_hexadecimal_digits bin_to_hexadecimal_val))) (set! bin_to_hexadecimal_j (+ bin_to_hexadecimal_j 4)))) (if bin_to_hexadecimal_is_negative (str "-" bin_to_hexadecimal_res) bin_to_hexadecimal_res)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (bin_to_hexadecimal "101011111"))
      (println (bin_to_hexadecimal " 1010   "))
      (println (bin_to_hexadecimal "-11101"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
