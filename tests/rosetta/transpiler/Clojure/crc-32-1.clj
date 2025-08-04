(ns main (:refer-clojure :exclude [xor rshift ord toHex crc32Table crc32 main]))

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

(declare xor rshift ord toHex crc32Table crc32 main)

(def ^:dynamic crc32Table_i nil)

(def ^:dynamic crc32Table_j nil)

(def ^:dynamic crc32Table_table nil)

(def ^:dynamic crc32Table_word nil)

(def ^:dynamic crc32_c nil)

(def ^:dynamic crc32_crc nil)

(def ^:dynamic crc32_i nil)

(def ^:dynamic crc32_idx nil)

(def ^:dynamic main_hex nil)

(def ^:dynamic main_result nil)

(def ^:dynamic main_s nil)

(def ^:dynamic ord_idx nil)

(def ^:dynamic ord_lower nil)

(def ^:dynamic ord_upper nil)

(def ^:dynamic rshift_i nil)

(def ^:dynamic rshift_v nil)

(def ^:dynamic toHex_d nil)

(def ^:dynamic toHex_digits nil)

(def ^:dynamic toHex_out nil)

(def ^:dynamic toHex_v nil)

(def ^:dynamic xor_abit nil)

(def ^:dynamic xor_bbit nil)

(def ^:dynamic xor_bit nil)

(def ^:dynamic xor_res nil)

(def ^:dynamic xor_x nil)

(def ^:dynamic xor_y nil)

(defn xor [xor_a xor_b]
  (binding [xor_abit nil xor_bbit nil xor_bit nil xor_res nil xor_x nil xor_y nil] (try (do (set! xor_res 0) (set! xor_bit 1) (set! xor_x xor_a) (set! xor_y xor_b) (while (or (> xor_x 0) (> xor_y 0)) (do (set! xor_abit (mod xor_x 2)) (set! xor_bbit (mod xor_y 2)) (when (not= xor_abit xor_bbit) (set! xor_res (+ xor_res xor_bit))) (set! xor_x (quot xor_x 2)) (set! xor_y (quot xor_y 2)) (set! xor_bit (* xor_bit 2)))) (throw (ex-info "return" {:v xor_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rshift [rshift_x rshift_n]
  (binding [rshift_i nil rshift_v nil] (try (do (set! rshift_v rshift_x) (set! rshift_i 0) (while (< rshift_i rshift_n) (do (set! rshift_v (quot rshift_v 2)) (set! rshift_i (+ rshift_i 1)))) (throw (ex-info "return" {:v rshift_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ord [ord_ch]
  (binding [ord_idx nil ord_lower nil ord_upper nil] (try (do (set! ord_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! ord_lower "abcdefghijklmnopqrstuvwxyz") (set! ord_idx (indexOf ord_upper ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 65 ord_idx)}))) (set! ord_idx (indexOf ord_lower ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 97 ord_idx)}))) (if (= ord_ch " ") 32 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn toHex [toHex_n]
  (binding [toHex_d nil toHex_digits nil toHex_out nil toHex_v nil] (try (do (set! toHex_digits "0123456789ABCDEF") (when (= toHex_n 0) (throw (ex-info "return" {:v "0"}))) (set! toHex_v toHex_n) (set! toHex_out "") (while (> toHex_v 0) (do (set! toHex_d (mod toHex_v 16)) (set! toHex_out (str (subs toHex_digits toHex_d (+ toHex_d 1)) toHex_out)) (set! toHex_v (quot toHex_v 16)))) (throw (ex-info "return" {:v toHex_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn crc32Table []
  (binding [crc32Table_i nil crc32Table_j nil crc32Table_table nil crc32Table_word nil] (try (do (set! crc32Table_table []) (set! crc32Table_i 0) (while (< crc32Table_i 256) (do (set! crc32Table_word crc32Table_i) (set! crc32Table_j 0) (while (< crc32Table_j 8) (do (if (= (mod crc32Table_word 2) 1) (set! crc32Table_word (xor (rshift crc32Table_word 1) 3988292384)) (set! crc32Table_word (rshift crc32Table_word 1))) (set! crc32Table_j (+ crc32Table_j 1)))) (set! crc32Table_table (conj crc32Table_table crc32Table_word)) (set! crc32Table_i (+ crc32Table_i 1)))) (throw (ex-info "return" {:v crc32Table_table}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_table (crc32Table))

(defn crc32 [crc32_s]
  (binding [crc32_c nil crc32_crc nil crc32_i nil crc32_idx nil] (try (do (set! crc32_crc 4294967295) (set! crc32_i 0) (while (< crc32_i (count crc32_s)) (do (set! crc32_c (ord (subs crc32_s crc32_i (+ crc32_i 1)))) (set! crc32_idx (xor (mod crc32_crc 256) crc32_c)) (set! crc32_crc (xor (nth main_table crc32_idx) (rshift crc32_crc 8))) (set! crc32_i (+ crc32_i 1)))) (throw (ex-info "return" {:v (- 4294967295 crc32_crc)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_hex nil main_result nil main_s nil] (do (set! main_s "The quick brown fox jumps over the lazy dog") (set! main_result (crc32 main_s)) (set! main_hex (toHex main_result)) (println main_hex))))

(defn -main []
  (main))

(-main)
