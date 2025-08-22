(ns main (:refer-clojure :exclude [xor chr ord is_valid_ascii try_key filter_valid_chars contains filter_common_word solution]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare xor chr ord is_valid_ascii try_key filter_valid_chars contains filter_common_word solution)

(declare _read_file)

(def ^:dynamic contains_is_match nil)

(def ^:dynamic contains_j nil)

(def ^:dynamic contains_m nil)

(def ^:dynamic contains_n nil)

(def ^:dynamic filter_common_word_p nil)

(def ^:dynamic filter_common_word_res nil)

(def ^:dynamic filter_valid_chars_decoded nil)

(def ^:dynamic filter_valid_chars_j nil)

(def ^:dynamic filter_valid_chars_k nil)

(def ^:dynamic filter_valid_chars_key nil)

(def ^:dynamic filter_valid_chars_possibles nil)

(def ^:dynamic main_LOWERCASE_INTS nil)

(def ^:dynamic main_i nil)

(def ^:dynamic ord_i nil)

(def ^:dynamic solution_decoded_text nil)

(def ^:dynamic solution_j nil)

(def ^:dynamic solution_possibles nil)

(def ^:dynamic solution_sum nil)

(def ^:dynamic solution_word nil)

(def ^:dynamic try_key_decoded nil)

(def ^:dynamic try_key_decodedchar nil)

(def ^:dynamic try_key_klen nil)

(def ^:dynamic xor_abit nil)

(def ^:dynamic xor_bbit nil)

(def ^:dynamic xor_bit nil)

(def ^:dynamic xor_res nil)

(def ^:dynamic xor_x nil)

(def ^:dynamic xor_y nil)

(defn xor [xor_a xor_b]
  (binding [xor_abit nil xor_bbit nil xor_bit nil xor_res nil xor_x nil xor_y nil] (try (do (set! xor_res 0) (set! xor_bit 1) (set! xor_x xor_a) (set! xor_y xor_b) (while (or (> xor_x 0) (> xor_y 0)) (do (set! xor_abit (mod xor_x 2)) (set! xor_bbit (mod xor_y 2)) (when (not= xor_abit xor_bbit) (set! xor_res (+ xor_res xor_bit))) (set! xor_x (/ xor_x 2)) (set! xor_y (/ xor_y 2)) (set! xor_bit (* xor_bit 2)))) (throw (ex-info "return" {:v xor_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_ascii_chars nil)

(defn chr [chr_code]
  (try (do (when (= chr_code 10) (throw (ex-info "return" {:v "\n"}))) (when (= chr_code 13) (throw (ex-info "return" {:v "\r"}))) (when (= chr_code 9) (throw (ex-info "return" {:v "\t"}))) (if (and (>= chr_code 32) (< chr_code 127)) (subvec main_ascii_chars (- chr_code 32) (- chr_code 31)) "")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ord [ord_ch]
  (binding [ord_i nil] (try (do (when (= ord_ch "\n") (throw (ex-info "return" {:v 10}))) (when (= ord_ch "\r") (throw (ex-info "return" {:v 13}))) (when (= ord_ch "\t") (throw (ex-info "return" {:v 9}))) (set! ord_i 0) (while (< ord_i (count main_ascii_chars)) (do (when (= (subvec main_ascii_chars ord_i (+ ord_i 1)) ord_ch) (throw (ex-info "return" {:v (+ 32 ord_i)}))) (set! ord_i (+ ord_i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_valid_ascii [is_valid_ascii_code]
  (try (do (when (and (>= is_valid_ascii_code 32) (<= is_valid_ascii_code 126)) (throw (ex-info "return" {:v true}))) (if (or (or (= is_valid_ascii_code 9) (= is_valid_ascii_code 10)) (= is_valid_ascii_code 13)) true false)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_LOWERCASE_INTS nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_COMMON_WORDS nil)

(defn try_key [try_key_ciphertext try_key_key]
  (binding [main_i nil try_key_decoded nil try_key_decodedchar nil try_key_klen nil] (try (do (set! try_key_decoded "") (set! main_i 0) (set! try_key_klen (count try_key_key)) (while (< main_i (count try_key_ciphertext)) (do (set! try_key_decodedchar (xor (nth try_key_ciphertext main_i) (nth try_key_key (mod main_i try_key_klen)))) (when (not (is_valid_ascii try_key_decodedchar)) (throw (ex-info "return" {:v nil}))) (set! try_key_decoded (str try_key_decoded (chr try_key_decodedchar))) (alter-var-root (var main_i) (fn [_] (+ main_i 1))))) (throw (ex-info "return" {:v try_key_decoded}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn filter_valid_chars [filter_valid_chars_ciphertext]
  (binding [filter_valid_chars_decoded nil filter_valid_chars_j nil filter_valid_chars_k nil filter_valid_chars_key nil filter_valid_chars_possibles nil main_i nil] (try (do (set! filter_valid_chars_possibles []) (set! main_i 0) (while (< main_i (count main_LOWERCASE_INTS)) (do (set! filter_valid_chars_j 0) (while (< filter_valid_chars_j (count main_LOWERCASE_INTS)) (do (set! filter_valid_chars_k 0) (while (< filter_valid_chars_k (count main_LOWERCASE_INTS)) (do (set! filter_valid_chars_key [(nth main_LOWERCASE_INTS main_i) (nth main_LOWERCASE_INTS filter_valid_chars_j) (nth main_LOWERCASE_INTS filter_valid_chars_k)]) (set! filter_valid_chars_decoded (try_key filter_valid_chars_ciphertext filter_valid_chars_key)) (when (not= filter_valid_chars_decoded nil) (set! filter_valid_chars_possibles (conj filter_valid_chars_possibles filter_valid_chars_decoded))) (set! filter_valid_chars_k (+ filter_valid_chars_k 1)))) (set! filter_valid_chars_j (+ filter_valid_chars_j 1)))) (alter-var-root (var main_i) (fn [_] (+ main_i 1))))) (throw (ex-info "return" {:v filter_valid_chars_possibles}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains [contains_s contains_sub]
  (binding [contains_is_match nil contains_j nil contains_m nil contains_n nil main_i nil] (try (do (set! contains_n (count contains_s)) (set! contains_m (count contains_sub)) (when (= contains_m 0) (throw (ex-info "return" {:v true}))) (set! main_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (<= main_i (- contains_n contains_m))) (do (set! contains_j 0) (set! contains_is_match true) (loop [while_flag_2 true] (when (and while_flag_2 (< contains_j contains_m)) (cond (not= (subs contains_s (+ main_i contains_j) (+ (+ main_i contains_j) 1)) (subs contains_sub contains_j (+ contains_j 1))) (do (set! contains_is_match false) (recur false)) :else (do (set! contains_j (+ contains_j 1)) (recur while_flag_2))))) (when contains_is_match (throw (ex-info "return" {:v true}))) (alter-var-root (var main_i) (fn [_] (+ main_i 1))) (cond :else (do))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn filter_common_word [filter_common_word_possibles filter_common_word_common_word]
  (binding [filter_common_word_p nil filter_common_word_res nil main_i nil] (try (do (set! filter_common_word_res []) (set! main_i 0) (while (< main_i (count filter_common_word_possibles)) (do (set! filter_common_word_p (nth filter_common_word_possibles main_i)) (when (contains (clojure.string/lower-case filter_common_word_p) filter_common_word_common_word) (set! filter_common_word_res (conj filter_common_word_res filter_common_word_p))) (alter-var-root (var main_i) (fn [_] (+ main_i 1))))) (throw (ex-info "return" {:v filter_common_word_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_ciphertext]
  (binding [main_i nil solution_decoded_text nil solution_j nil solution_possibles nil solution_sum nil solution_word nil] (try (do (set! solution_possibles (filter_valid_chars solution_ciphertext)) (set! main_i 0) (loop [while_flag_3 true] (when (and while_flag_3 (< main_i (count main_COMMON_WORDS))) (do (set! solution_word (nth main_COMMON_WORDS main_i)) (set! solution_possibles (filter_common_word solution_possibles solution_word)) (cond (= (count solution_possibles) 1) (recur false) :else (do (alter-var-root (var main_i) (fn [_] (+ main_i 1))) (recur while_flag_3)))))) (set! solution_decoded_text (nth solution_possibles 0)) (set! solution_sum 0) (set! solution_j 0) (while (< solution_j (count solution_decoded_text)) (do (set! solution_sum (+ solution_sum (ord (subs solution_decoded_text solution_j (min (+ solution_j 1) (count solution_decoded_text)))))) (set! solution_j (+ solution_j 1)))) (throw (ex-info "return" {:v solution_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_ciphertext nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_ascii_chars) (constantly " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"))
      (alter-var-root (var main_LOWERCASE_INTS) (constantly []))
      (alter-var-root (var main_i) (constantly 97))
      (while (<= main_i 122) (do (alter-var-root (var main_LOWERCASE_INTS) (constantly (conj main_LOWERCASE_INTS main_i))) (alter-var-root (var main_i) (constantly (+ main_i 1)))))
      (alter-var-root (var main_COMMON_WORDS) (constantly ["the" "be" "to" "of" "and" "in" "that" "have"]))
      (alter-var-root (var main_ciphertext) (constantly [17 6 1 69 12 1 69 26 11 69 1 2 69 15 10 1 78 13 11 78 16 13 15 16 69 6 5 19 11]))
      (println (mochi_str (solution main_ciphertext)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
