(ns main (:refer-clojure :exclude [index_of_char ord rabin_karp test_rabin_karp]))

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

(declare index_of_char ord rabin_karp test_rabin_karp)

(def ^:dynamic index_of_char_i nil)

(def ^:dynamic ord_digits nil)

(def ^:dynamic ord_idx nil)

(def ^:dynamic ord_lower nil)

(def ^:dynamic ord_upper nil)

(def ^:dynamic rabin_karp_i nil)

(def ^:dynamic rabin_karp_j nil)

(def ^:dynamic rabin_karp_modulus_power nil)

(def ^:dynamic rabin_karp_p_hash nil)

(def ^:dynamic rabin_karp_p_len nil)

(def ^:dynamic rabin_karp_t_hash nil)

(def ^:dynamic rabin_karp_t_len nil)

(def ^:dynamic test_rabin_karp_pattern1 nil)

(def ^:dynamic test_rabin_karp_pattern2 nil)

(def ^:dynamic test_rabin_karp_pattern3 nil)

(def ^:dynamic test_rabin_karp_pattern4 nil)

(def ^:dynamic test_rabin_karp_pattern5 nil)

(def ^:dynamic test_rabin_karp_pattern6 nil)

(def ^:dynamic test_rabin_karp_text1 nil)

(def ^:dynamic test_rabin_karp_text2 nil)

(def ^:dynamic test_rabin_karp_text3 nil)

(def ^:dynamic test_rabin_karp_text4 nil)

(def ^:dynamic test_rabin_karp_text5 nil)

(def ^:dynamic test_rabin_karp_text6 nil)

(def ^:dynamic main_alphabet_size 256)

(def ^:dynamic main_modulus 1000003)

(defn index_of_char [index_of_char_s index_of_char_ch]
  (binding [index_of_char_i nil] (try (do (set! index_of_char_i 0) (while (< index_of_char_i (count index_of_char_s)) (do (when (= (subs index_of_char_s index_of_char_i (+ index_of_char_i 1)) index_of_char_ch) (throw (ex-info "return" {:v index_of_char_i}))) (set! index_of_char_i (+ index_of_char_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ord [ord_ch]
  (binding [ord_digits nil ord_idx nil ord_lower nil ord_upper nil] (try (do (set! ord_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! ord_lower "abcdefghijklmnopqrstuvwxyz") (set! ord_digits "0123456789") (set! ord_idx (index_of_char ord_upper ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 65 ord_idx)}))) (set! ord_idx (index_of_char ord_lower ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 97 ord_idx)}))) (set! ord_idx (index_of_char ord_digits ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 48 ord_idx)}))) (when (= ord_ch "ü") (throw (ex-info "return" {:v 252}))) (when (= ord_ch "Ü") (throw (ex-info "return" {:v 220}))) (if (= ord_ch " ") 32 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rabin_karp [rabin_karp_pattern rabin_karp_text]
  (binding [rabin_karp_i nil rabin_karp_j nil rabin_karp_modulus_power nil rabin_karp_p_hash nil rabin_karp_p_len nil rabin_karp_t_hash nil rabin_karp_t_len nil] (try (do (set! rabin_karp_p_len (count rabin_karp_pattern)) (set! rabin_karp_t_len (count rabin_karp_text)) (when (> rabin_karp_p_len rabin_karp_t_len) (throw (ex-info "return" {:v false}))) (set! rabin_karp_p_hash 0) (set! rabin_karp_t_hash 0) (set! rabin_karp_modulus_power 1) (set! rabin_karp_i 0) (while (< rabin_karp_i rabin_karp_p_len) (do (set! rabin_karp_p_hash (mod (+ (ord (subs rabin_karp_pattern rabin_karp_i (+ rabin_karp_i 1))) (* rabin_karp_p_hash main_alphabet_size)) main_modulus)) (set! rabin_karp_t_hash (mod (+ (ord (subs rabin_karp_text rabin_karp_i (+ rabin_karp_i 1))) (* rabin_karp_t_hash main_alphabet_size)) main_modulus)) (when (not= rabin_karp_i (- rabin_karp_p_len 1)) (set! rabin_karp_modulus_power (mod (* rabin_karp_modulus_power main_alphabet_size) main_modulus))) (set! rabin_karp_i (+ rabin_karp_i 1)))) (set! rabin_karp_j 0) (loop [while_flag_1 true] (when (and while_flag_1 (<= rabin_karp_j (- rabin_karp_t_len rabin_karp_p_len))) (do (when (and (= rabin_karp_t_hash rabin_karp_p_hash) (= (subs rabin_karp_text rabin_karp_j (min (+ rabin_karp_j rabin_karp_p_len) (count rabin_karp_text))) rabin_karp_pattern)) (throw (ex-info "return" {:v true}))) (cond (= rabin_karp_j (- rabin_karp_t_len rabin_karp_p_len)) (do (set! rabin_karp_j (+ rabin_karp_j 1)) (recur true)) :else (do (set! rabin_karp_t_hash (mod (+ (* (- rabin_karp_t_hash (* (ord (subs rabin_karp_text rabin_karp_j (+ rabin_karp_j 1))) rabin_karp_modulus_power)) main_alphabet_size) (ord (subs rabin_karp_text (+ rabin_karp_j rabin_karp_p_len) (+ (+ rabin_karp_j rabin_karp_p_len) 1)))) main_modulus)) (when (< rabin_karp_t_hash 0) (set! rabin_karp_t_hash (+ rabin_karp_t_hash main_modulus))) (set! rabin_karp_j (+ rabin_karp_j 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_rabin_karp []
  (binding [test_rabin_karp_pattern1 nil test_rabin_karp_pattern2 nil test_rabin_karp_pattern3 nil test_rabin_karp_pattern4 nil test_rabin_karp_pattern5 nil test_rabin_karp_pattern6 nil test_rabin_karp_text1 nil test_rabin_karp_text2 nil test_rabin_karp_text3 nil test_rabin_karp_text4 nil test_rabin_karp_text5 nil test_rabin_karp_text6 nil] (try (do (set! test_rabin_karp_pattern1 "abc1abc12") (set! test_rabin_karp_text1 "alskfjaldsabc1abc1abc12k23adsfabcabc") (set! test_rabin_karp_text2 "alskfjaldsk23adsfabcabc") (when (or (not (rabin_karp test_rabin_karp_pattern1 test_rabin_karp_text1)) (rabin_karp test_rabin_karp_pattern1 test_rabin_karp_text2)) (do (println "Failure") (throw (ex-info "return" {:v nil})))) (set! test_rabin_karp_pattern2 "ABABX") (set! test_rabin_karp_text3 "ABABZABABYABABX") (when (not (rabin_karp test_rabin_karp_pattern2 test_rabin_karp_text3)) (do (println "Failure") (throw (ex-info "return" {:v nil})))) (set! test_rabin_karp_pattern3 "AAAB") (set! test_rabin_karp_text4 "ABAAAAAB") (when (not (rabin_karp test_rabin_karp_pattern3 test_rabin_karp_text4)) (do (println "Failure") (throw (ex-info "return" {:v nil})))) (set! test_rabin_karp_pattern4 "abcdabcy") (set! test_rabin_karp_text5 "abcxabcdabxabcdabcdabcy") (when (not (rabin_karp test_rabin_karp_pattern4 test_rabin_karp_text5)) (do (println "Failure") (throw (ex-info "return" {:v nil})))) (set! test_rabin_karp_pattern5 "Lü") (set! test_rabin_karp_text6 "Lüsai") (when (not (rabin_karp test_rabin_karp_pattern5 test_rabin_karp_text6)) (do (println "Failure") (throw (ex-info "return" {:v nil})))) (set! test_rabin_karp_pattern6 "Lue") (when (rabin_karp test_rabin_karp_pattern6 test_rabin_karp_text6) (do (println "Failure") (throw (ex-info "return" {:v nil})))) (println "Success.")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (test_rabin_karp)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
