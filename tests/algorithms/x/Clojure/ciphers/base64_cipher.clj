(ns main (:refer-clojure :exclude [to_binary zfill from_binary repeat char_index base64_encode base64_decode main]))

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

(declare to_binary zfill from_binary repeat char_index base64_encode base64_decode main)

(def ^:dynamic base64_decode_bits nil)

(def ^:dynamic base64_decode_byte nil)

(def ^:dynamic base64_decode_bytes nil)

(def ^:dynamic base64_decode_c nil)

(def ^:dynamic base64_decode_end nil)

(def ^:dynamic base64_decode_idx nil)

(def ^:dynamic base64_decode_k nil)

(def ^:dynamic base64_decode_m nil)

(def ^:dynamic base64_decode_padding nil)

(def ^:dynamic base64_encode_bits nil)

(def ^:dynamic base64_encode_chunk nil)

(def ^:dynamic base64_encode_encoded nil)

(def ^:dynamic base64_encode_i nil)

(def ^:dynamic base64_encode_idx nil)

(def ^:dynamic base64_encode_j nil)

(def ^:dynamic base64_encode_pad nil)

(def ^:dynamic base64_encode_pad_bits nil)

(def ^:dynamic char_index_i nil)

(def ^:dynamic from_binary_i nil)

(def ^:dynamic from_binary_result nil)

(def ^:dynamic main_data nil)

(def ^:dynamic main_encoded nil)

(def ^:dynamic repeat_i nil)

(def ^:dynamic repeat_res nil)

(def ^:dynamic to_binary_bit nil)

(def ^:dynamic to_binary_num nil)

(def ^:dynamic to_binary_res nil)

(def ^:dynamic zfill_pad nil)

(def ^:dynamic zfill_res nil)

(def ^:dynamic main_B64_CHARSET "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

(defn to_binary [to_binary_n]
  (binding [to_binary_bit nil to_binary_num nil to_binary_res nil] (try (do (when (= to_binary_n 0) (throw (ex-info "return" {:v "0"}))) (set! to_binary_num to_binary_n) (set! to_binary_res "") (while (> to_binary_num 0) (do (set! to_binary_bit (mod to_binary_num 2)) (set! to_binary_res (str (str to_binary_bit) to_binary_res)) (set! to_binary_num (quot to_binary_num 2)))) (throw (ex-info "return" {:v to_binary_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn zfill [zfill_s zfill_width]
  (binding [zfill_pad nil zfill_res nil] (try (do (set! zfill_res zfill_s) (set! zfill_pad (- zfill_width (count zfill_s))) (while (> zfill_pad 0) (do (set! zfill_res (str "0" zfill_res)) (set! zfill_pad (- zfill_pad 1)))) (throw (ex-info "return" {:v zfill_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn from_binary [from_binary_s]
  (binding [from_binary_i nil from_binary_result nil] (try (do (set! from_binary_i 0) (set! from_binary_result 0) (while (< from_binary_i (count from_binary_s)) (do (set! from_binary_result (* from_binary_result 2)) (when (= (subs from_binary_s from_binary_i (min (+ from_binary_i 1) (count from_binary_s))) "1") (set! from_binary_result (+ from_binary_result 1))) (set! from_binary_i (+ from_binary_i 1)))) (throw (ex-info "return" {:v from_binary_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn repeat [repeat_ch repeat_times]
  (binding [repeat_i nil repeat_res nil] (try (do (set! repeat_res "") (set! repeat_i 0) (while (< repeat_i repeat_times) (do (set! repeat_res (str repeat_res repeat_ch)) (set! repeat_i (+ repeat_i 1)))) (throw (ex-info "return" {:v repeat_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn char_index [char_index_s char_index_c]
  (binding [char_index_i nil] (try (do (set! char_index_i 0) (while (< char_index_i (count char_index_s)) (do (when (= (subs char_index_s char_index_i (min (+ char_index_i 1) (count char_index_s))) char_index_c) (throw (ex-info "return" {:v char_index_i}))) (set! char_index_i (+ char_index_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn base64_encode [base64_encode_data]
  (binding [base64_encode_bits nil base64_encode_chunk nil base64_encode_encoded nil base64_encode_i nil base64_encode_idx nil base64_encode_j nil base64_encode_pad nil base64_encode_pad_bits nil] (try (do (set! base64_encode_bits "") (set! base64_encode_i 0) (while (< base64_encode_i (count base64_encode_data)) (do (set! base64_encode_bits (str base64_encode_bits (zfill (to_binary (nth base64_encode_data base64_encode_i)) 8))) (set! base64_encode_i (+ base64_encode_i 1)))) (set! base64_encode_pad_bits 0) (when (not= (mod (count base64_encode_bits) 6) 0) (do (set! base64_encode_pad_bits (- 6 (mod (count base64_encode_bits) 6))) (set! base64_encode_bits (str base64_encode_bits (repeat "0" base64_encode_pad_bits))))) (set! base64_encode_j 0) (set! base64_encode_encoded "") (while (< base64_encode_j (count base64_encode_bits)) (do (set! base64_encode_chunk (subs base64_encode_bits base64_encode_j (min (+ base64_encode_j 6) (count base64_encode_bits)))) (set! base64_encode_idx (from_binary base64_encode_chunk)) (set! base64_encode_encoded (str base64_encode_encoded (subs main_B64_CHARSET base64_encode_idx (min (+ base64_encode_idx 1) (count main_B64_CHARSET))))) (set! base64_encode_j (+ base64_encode_j 6)))) (set! base64_encode_pad (quot base64_encode_pad_bits 2)) (while (> base64_encode_pad 0) (do (set! base64_encode_encoded (str base64_encode_encoded "=")) (set! base64_encode_pad (- base64_encode_pad 1)))) (throw (ex-info "return" {:v base64_encode_encoded}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn base64_decode [base64_decode_s]
  (binding [base64_decode_bits nil base64_decode_byte nil base64_decode_bytes nil base64_decode_c nil base64_decode_end nil base64_decode_idx nil base64_decode_k nil base64_decode_m nil base64_decode_padding nil] (try (do (set! base64_decode_padding 0) (set! base64_decode_end (count base64_decode_s)) (while (and (> base64_decode_end 0) (= (subs base64_decode_s (- base64_decode_end 1) (min base64_decode_end (count base64_decode_s))) "=")) (do (set! base64_decode_padding (+ base64_decode_padding 1)) (set! base64_decode_end (- base64_decode_end 1)))) (set! base64_decode_bits "") (set! base64_decode_k 0) (while (< base64_decode_k base64_decode_end) (do (set! base64_decode_c (subs base64_decode_s base64_decode_k (min (+ base64_decode_k 1) (count base64_decode_s)))) (set! base64_decode_idx (char_index main_B64_CHARSET base64_decode_c)) (set! base64_decode_bits (str base64_decode_bits (zfill (to_binary base64_decode_idx) 6))) (set! base64_decode_k (+ base64_decode_k 1)))) (when (> base64_decode_padding 0) (set! base64_decode_bits (subs base64_decode_bits 0 (min (- (count base64_decode_bits) (* base64_decode_padding 2)) (count base64_decode_bits))))) (set! base64_decode_bytes []) (set! base64_decode_m 0) (while (< base64_decode_m (count base64_decode_bits)) (do (set! base64_decode_byte (from_binary (subs base64_decode_bits base64_decode_m (min (+ base64_decode_m 8) (count base64_decode_bits))))) (set! base64_decode_bytes (conj base64_decode_bytes base64_decode_byte)) (set! base64_decode_m (+ base64_decode_m 8)))) (throw (ex-info "return" {:v base64_decode_bytes}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_data nil main_encoded nil] (do (set! main_data [77 111 99 104 105]) (set! main_encoded (base64_encode main_data)) (println main_encoded) (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) (base64_decode main_encoded))))))

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
