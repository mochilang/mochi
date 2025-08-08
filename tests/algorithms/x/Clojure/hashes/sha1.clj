(ns main (:refer-clojure :exclude [ord pow2 bit_and bit_or bit_xor bit_not rotate_left to_hex32 sha1 main]))

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

(declare ord pow2 bit_and bit_or bit_xor bit_not rotate_left to_hex32 sha1 main)

(def ^:dynamic bit_and_bit nil)

(def ^:dynamic bit_and_i nil)

(def ^:dynamic bit_and_res nil)

(def ^:dynamic bit_and_x nil)

(def ^:dynamic bit_and_y nil)

(def ^:dynamic bit_or_abit nil)

(def ^:dynamic bit_or_bbit nil)

(def ^:dynamic bit_or_bit nil)

(def ^:dynamic bit_or_i nil)

(def ^:dynamic bit_or_res nil)

(def ^:dynamic bit_or_x nil)

(def ^:dynamic bit_or_y nil)

(def ^:dynamic bit_xor_abit nil)

(def ^:dynamic bit_xor_bbit nil)

(def ^:dynamic bit_xor_bit nil)

(def ^:dynamic bit_xor_i nil)

(def ^:dynamic bit_xor_res nil)

(def ^:dynamic bit_xor_x nil)

(def ^:dynamic bit_xor_y nil)

(def ^:dynamic ord_i nil)

(def ^:dynamic pow2_i nil)

(def ^:dynamic pow2_res nil)

(def ^:dynamic rotate_left_left nil)

(def ^:dynamic rotate_left_right nil)

(def ^:dynamic sha1_a nil)

(def ^:dynamic sha1_b nil)

(def ^:dynamic sha1_bindex nil)

(def ^:dynamic sha1_bit_len nil)

(def ^:dynamic sha1_bl nil)

(def ^:dynamic sha1_block nil)

(def ^:dynamic sha1_blocks nil)

(def ^:dynamic sha1_bytes nil)

(def ^:dynamic sha1_c nil)

(def ^:dynamic sha1_d nil)

(def ^:dynamic sha1_e nil)

(def ^:dynamic sha1_f nil)

(def ^:dynamic sha1_h0 nil)

(def ^:dynamic sha1_h1 nil)

(def ^:dynamic sha1_h2 nil)

(def ^:dynamic sha1_h3 nil)

(def ^:dynamic sha1_h4 nil)

(def ^:dynamic sha1_i nil)

(def ^:dynamic sha1_i2 nil)

(def ^:dynamic sha1_j nil)

(def ^:dynamic sha1_j2 nil)

(def ^:dynamic sha1_j3 nil)

(def ^:dynamic sha1_k nil)

(def ^:dynamic sha1_kconst nil)

(def ^:dynamic sha1_len_bytes nil)

(def ^:dynamic sha1_pos nil)

(def ^:dynamic sha1_t nil)

(def ^:dynamic sha1_temp nil)

(def ^:dynamic sha1_tmp nil)

(def ^:dynamic sha1_w nil)

(def ^:dynamic sha1_word nil)

(def ^:dynamic to_hex32_d nil)

(def ^:dynamic to_hex32_digits nil)

(def ^:dynamic to_hex32_num nil)

(def ^:dynamic to_hex32_s nil)

(def ^:dynamic main_MOD 4294967296)

(def ^:dynamic main_ASCII " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defn ord [ord_ch]
  (binding [ord_i nil] (try (do (set! ord_i 0) (while (< ord_i (count main_ASCII)) (do (when (= (subs main_ASCII ord_i (min (+ ord_i 1) (count main_ASCII))) ord_ch) (throw (ex-info "return" {:v (+ 32 ord_i)}))) (set! ord_i (+ ord_i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow2 [pow2_n]
  (binding [pow2_i nil pow2_res nil] (try (do (set! pow2_res 1) (set! pow2_i 0) (while (< pow2_i pow2_n) (do (set! pow2_res (* pow2_res 2)) (set! pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bit_and [bit_and_a bit_and_b]
  (binding [bit_and_bit nil bit_and_i nil bit_and_res nil bit_and_x nil bit_and_y nil] (try (do (set! bit_and_x bit_and_a) (set! bit_and_y bit_and_b) (set! bit_and_res 0) (set! bit_and_bit 1) (set! bit_and_i 0) (while (< bit_and_i 32) (do (when (and (= (mod bit_and_x 2) 1) (= (mod bit_and_y 2) 1)) (set! bit_and_res (+ bit_and_res bit_and_bit))) (set! bit_and_x (quot bit_and_x 2)) (set! bit_and_y (quot bit_and_y 2)) (set! bit_and_bit (* bit_and_bit 2)) (set! bit_and_i (+ bit_and_i 1)))) (throw (ex-info "return" {:v bit_and_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bit_or [bit_or_a bit_or_b]
  (binding [bit_or_abit nil bit_or_bbit nil bit_or_bit nil bit_or_i nil bit_or_res nil bit_or_x nil bit_or_y nil] (try (do (set! bit_or_x bit_or_a) (set! bit_or_y bit_or_b) (set! bit_or_res 0) (set! bit_or_bit 1) (set! bit_or_i 0) (while (< bit_or_i 32) (do (set! bit_or_abit (mod bit_or_x 2)) (set! bit_or_bbit (mod bit_or_y 2)) (when (or (= bit_or_abit 1) (= bit_or_bbit 1)) (set! bit_or_res (+ bit_or_res bit_or_bit))) (set! bit_or_x (quot bit_or_x 2)) (set! bit_or_y (quot bit_or_y 2)) (set! bit_or_bit (* bit_or_bit 2)) (set! bit_or_i (+ bit_or_i 1)))) (throw (ex-info "return" {:v bit_or_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bit_xor [bit_xor_a bit_xor_b]
  (binding [bit_xor_abit nil bit_xor_bbit nil bit_xor_bit nil bit_xor_i nil bit_xor_res nil bit_xor_x nil bit_xor_y nil] (try (do (set! bit_xor_x bit_xor_a) (set! bit_xor_y bit_xor_b) (set! bit_xor_res 0) (set! bit_xor_bit 1) (set! bit_xor_i 0) (while (< bit_xor_i 32) (do (set! bit_xor_abit (mod bit_xor_x 2)) (set! bit_xor_bbit (mod bit_xor_y 2)) (when (or (and (= bit_xor_abit 1) (= bit_xor_bbit 0)) (and (= bit_xor_abit 0) (= bit_xor_bbit 1))) (set! bit_xor_res (+ bit_xor_res bit_xor_bit))) (set! bit_xor_x (quot bit_xor_x 2)) (set! bit_xor_y (quot bit_xor_y 2)) (set! bit_xor_bit (* bit_xor_bit 2)) (set! bit_xor_i (+ bit_xor_i 1)))) (throw (ex-info "return" {:v bit_xor_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bit_not [bit_not_a]
  (try (throw (ex-info "return" {:v (- (- main_MOD 1) bit_not_a)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rotate_left [rotate_left_n rotate_left_b]
  (binding [rotate_left_left nil rotate_left_right nil] (try (do (set! rotate_left_left (mod (* rotate_left_n (pow2 rotate_left_b)) main_MOD)) (set! rotate_left_right (quot rotate_left_n (pow2 (- 32 rotate_left_b)))) (throw (ex-info "return" {:v (mod (+ rotate_left_left rotate_left_right) main_MOD)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_hex32 [to_hex32_n]
  (binding [to_hex32_d nil to_hex32_digits nil to_hex32_num nil to_hex32_s nil] (try (do (set! to_hex32_digits "0123456789abcdef") (set! to_hex32_num to_hex32_n) (set! to_hex32_s "") (when (= to_hex32_num 0) (set! to_hex32_s "0")) (while (> to_hex32_num 0) (do (set! to_hex32_d (mod to_hex32_num 16)) (set! to_hex32_s (str (subs to_hex32_digits to_hex32_d (min (+ to_hex32_d 1) (count to_hex32_digits))) to_hex32_s)) (set! to_hex32_num (quot to_hex32_num 16)))) (while (< (count to_hex32_s) 8) (set! to_hex32_s (str "0" to_hex32_s))) (when (> (count to_hex32_s) 8) (set! to_hex32_s (subs to_hex32_s (- (count to_hex32_s) 8) (min (count to_hex32_s) (count to_hex32_s))))) (throw (ex-info "return" {:v to_hex32_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sha1 [sha1_message]
  (binding [sha1_a nil sha1_b nil sha1_bindex nil sha1_bit_len nil sha1_bl nil sha1_block nil sha1_blocks nil sha1_bytes nil sha1_c nil sha1_d nil sha1_e nil sha1_f nil sha1_h0 nil sha1_h1 nil sha1_h2 nil sha1_h3 nil sha1_h4 nil sha1_i nil sha1_i2 nil sha1_j nil sha1_j2 nil sha1_j3 nil sha1_k nil sha1_kconst nil sha1_len_bytes nil sha1_pos nil sha1_t nil sha1_temp nil sha1_tmp nil sha1_w nil sha1_word nil] (try (do (set! sha1_bytes []) (set! sha1_i 0) (while (< sha1_i (count sha1_message)) (do (set! sha1_bytes (conj sha1_bytes (ord (subs sha1_message sha1_i (min (+ sha1_i 1) (count sha1_message)))))) (set! sha1_i (+ sha1_i 1)))) (set! sha1_bytes (conj sha1_bytes 128)) (while (not= (mod (+ (count sha1_bytes) 8) 64) 0) (set! sha1_bytes (conj sha1_bytes 0))) (set! sha1_bit_len (* (count sha1_message) 8)) (set! sha1_len_bytes [0 0 0 0 0 0 0 0]) (set! sha1_bl sha1_bit_len) (set! sha1_k 7) (while (>= sha1_k 0) (do (set! sha1_len_bytes (assoc sha1_len_bytes sha1_k (mod sha1_bl 256))) (set! sha1_bl (quot sha1_bl 256)) (set! sha1_k (- sha1_k 1)))) (set! sha1_j 0) (while (< sha1_j 8) (do (set! sha1_bytes (conj sha1_bytes (nth sha1_len_bytes sha1_j))) (set! sha1_j (+ sha1_j 1)))) (set! sha1_blocks []) (set! sha1_pos 0) (while (< sha1_pos (count sha1_bytes)) (do (set! sha1_block []) (set! sha1_j2 0) (while (< sha1_j2 64) (do (set! sha1_block (conj sha1_block (nth sha1_bytes (+ sha1_pos sha1_j2)))) (set! sha1_j2 (+ sha1_j2 1)))) (set! sha1_blocks (conj sha1_blocks sha1_block)) (set! sha1_pos (+ sha1_pos 64)))) (set! sha1_h0 1732584193) (set! sha1_h1 4023233417) (set! sha1_h2 2562383102) (set! sha1_h3 271733878) (set! sha1_h4 3285377520) (set! sha1_bindex 0) (while (< sha1_bindex (count sha1_blocks)) (do (set! sha1_block (nth sha1_blocks sha1_bindex)) (set! sha1_w []) (set! sha1_t 0) (while (< sha1_t 16) (do (set! sha1_j3 (* sha1_t 4)) (set! sha1_word (+ (* (+ (* (+ (* (nth sha1_block sha1_j3) 256) (nth sha1_block (+ sha1_j3 1))) 256) (nth sha1_block (+ sha1_j3 2))) 256) (nth sha1_block (+ sha1_j3 3)))) (set! sha1_w (conj sha1_w sha1_word)) (set! sha1_t (+ sha1_t 1)))) (while (< sha1_t 80) (do (set! sha1_tmp (bit_xor (bit_xor (bit_xor (nth sha1_w (- sha1_t 3)) (nth sha1_w (- sha1_t 8))) (nth sha1_w (- sha1_t 14))) (nth sha1_w (- sha1_t 16)))) (set! sha1_w (conj sha1_w (rotate_left sha1_tmp 1))) (set! sha1_t (+ sha1_t 1)))) (set! sha1_a sha1_h0) (set! sha1_b sha1_h1) (set! sha1_c sha1_h2) (set! sha1_d sha1_h3) (set! sha1_e sha1_h4) (set! sha1_i2 0) (while (< sha1_i2 80) (do (set! sha1_f 0) (set! sha1_kconst 0) (if (< sha1_i2 20) (do (set! sha1_f (bit_or (bit_and sha1_b sha1_c) (bit_and (bit_not sha1_b) sha1_d))) (set! sha1_kconst 1518500249)) (if (< sha1_i2 40) (do (set! sha1_f (bit_xor (bit_xor sha1_b sha1_c) sha1_d)) (set! sha1_kconst 1859775393)) (if (< sha1_i2 60) (do (set! sha1_f (bit_or (bit_or (bit_and sha1_b sha1_c) (bit_and sha1_b sha1_d)) (bit_and sha1_c sha1_d))) (set! sha1_kconst 2400959708)) (do (set! sha1_f (bit_xor (bit_xor sha1_b sha1_c) sha1_d)) (set! sha1_kconst 3395469782))))) (set! sha1_temp (mod (+ (+ (+ (+ (rotate_left sha1_a 5) sha1_f) sha1_e) sha1_kconst) (nth sha1_w sha1_i2)) main_MOD)) (set! sha1_e sha1_d) (set! sha1_d sha1_c) (set! sha1_c (rotate_left sha1_b 30)) (set! sha1_b sha1_a) (set! sha1_a sha1_temp) (set! sha1_i2 (+ sha1_i2 1)))) (set! sha1_h0 (mod (+ sha1_h0 sha1_a) main_MOD)) (set! sha1_h1 (mod (+ sha1_h1 sha1_b) main_MOD)) (set! sha1_h2 (mod (+ sha1_h2 sha1_c) main_MOD)) (set! sha1_h3 (mod (+ sha1_h3 sha1_d) main_MOD)) (set! sha1_h4 (mod (+ sha1_h4 sha1_e) main_MOD)) (set! sha1_bindex (+ sha1_bindex 1)))) (throw (ex-info "return" {:v (str (str (str (str (to_hex32 sha1_h0) (to_hex32 sha1_h1)) (to_hex32 sha1_h2)) (to_hex32 sha1_h3)) (to_hex32 sha1_h4))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (println (sha1 "Test String")))

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
