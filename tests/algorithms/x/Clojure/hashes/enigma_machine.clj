(ns main (:refer-clojure :exclude [build_alphabet range_list reversed_range_list index_of_char index_of_int enigma_encrypt]))

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

(declare build_alphabet range_list reversed_range_list index_of_char index_of_int enigma_encrypt)

(def ^:dynamic build_alphabet_i nil)

(def ^:dynamic build_alphabet_result nil)

(def ^:dynamic engine_target nil)

(def ^:dynamic enigma_encrypt_alphabets nil)

(def ^:dynamic enigma_encrypt_gear_one nil)

(def ^:dynamic enigma_encrypt_gear_one_pos nil)

(def ^:dynamic enigma_encrypt_gear_three nil)

(def ^:dynamic enigma_encrypt_gear_three_pos nil)

(def ^:dynamic enigma_encrypt_gear_two nil)

(def ^:dynamic enigma_encrypt_gear_two_pos nil)

(def ^:dynamic enigma_encrypt_idx nil)

(def ^:dynamic enigma_encrypt_n nil)

(def ^:dynamic enigma_encrypt_reflector nil)

(def ^:dynamic enigma_encrypt_result nil)

(def ^:dynamic enigma_encrypt_t nil)

(def ^:dynamic index_of_char_i nil)

(def ^:dynamic index_of_int_i nil)

(def ^:dynamic range_list_i nil)

(def ^:dynamic range_list_lst nil)

(def ^:dynamic reversed_range_list_i nil)

(def ^:dynamic reversed_range_list_lst nil)

(def ^:dynamic rotator_i nil)

(def ^:dynamic main_ASCII " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}")

(defn build_alphabet []
  (binding [build_alphabet_i nil build_alphabet_result nil] (try (do (set! build_alphabet_result []) (set! build_alphabet_i 0) (while (< build_alphabet_i (count main_ASCII)) (do (set! build_alphabet_result (conj build_alphabet_result (nth main_ASCII build_alphabet_i))) (set! build_alphabet_i (+ build_alphabet_i 1)))) (throw (ex-info "return" {:v build_alphabet_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn range_list [range_list_n]
  (binding [range_list_i nil range_list_lst nil] (try (do (set! range_list_lst []) (set! range_list_i 0) (while (< range_list_i range_list_n) (do (set! range_list_lst (conj range_list_lst range_list_i)) (set! range_list_i (+ range_list_i 1)))) (throw (ex-info "return" {:v range_list_lst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reversed_range_list [reversed_range_list_n]
  (binding [reversed_range_list_i nil reversed_range_list_lst nil] (try (do (set! reversed_range_list_lst []) (set! reversed_range_list_i (- reversed_range_list_n 1)) (while (>= reversed_range_list_i 0) (do (set! reversed_range_list_lst (conj reversed_range_list_lst reversed_range_list_i)) (set! reversed_range_list_i (- reversed_range_list_i 1)))) (throw (ex-info "return" {:v reversed_range_list_lst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn index_of_char [index_of_char_lst index_of_char_ch]
  (binding [index_of_char_i nil] (try (do (set! index_of_char_i 0) (while (< index_of_char_i (count index_of_char_lst)) (do (when (= (nth index_of_char_lst index_of_char_i) index_of_char_ch) (throw (ex-info "return" {:v index_of_char_i}))) (set! index_of_char_i (+ index_of_char_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn index_of_int [index_of_int_lst index_of_int_value]
  (binding [index_of_int_i nil] (try (do (set! index_of_int_i 0) (while (< index_of_int_i (count index_of_int_lst)) (do (when (= (nth index_of_int_lst index_of_int_i) index_of_int_value) (throw (ex-info "return" {:v index_of_int_i}))) (set! index_of_int_i (+ index_of_int_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rotator [enigma_encrypt_message enigma_encrypt_token]
  (binding [rotator_i nil] (do (set! rotator_i (nth enigma_encrypt_gear_one 0)) (alter-var-root (var enigma_encrypt_gear_one) (fn [_] (subvec enigma_encrypt_gear_one 1 (count enigma_encrypt_gear_one)))) (alter-var-root (var enigma_encrypt_gear_one) (fn [_] (conj enigma_encrypt_gear_one rotator_i))) (alter-var-root (var enigma_encrypt_gear_one_pos) (fn [_] (+ enigma_encrypt_gear_one_pos 1))) (when (= (mod enigma_encrypt_gear_one_pos enigma_encrypt_n) 0) (do (set! rotator_i (nth enigma_encrypt_gear_two 0)) (alter-var-root (var enigma_encrypt_gear_two) (fn [_] (subvec enigma_encrypt_gear_two 1 (count enigma_encrypt_gear_two)))) (alter-var-root (var enigma_encrypt_gear_two) (fn [_] (conj enigma_encrypt_gear_two rotator_i))) (alter-var-root (var enigma_encrypt_gear_two_pos) (fn [_] (+ enigma_encrypt_gear_two_pos 1))) (when (= (mod enigma_encrypt_gear_two_pos enigma_encrypt_n) 0) (do (set! rotator_i (nth enigma_encrypt_gear_three 0)) (alter-var-root (var enigma_encrypt_gear_three) (fn [_] (subvec enigma_encrypt_gear_three 1 (count enigma_encrypt_gear_three)))) (alter-var-root (var enigma_encrypt_gear_three) (fn [_] (conj enigma_encrypt_gear_three rotator_i))) (alter-var-root (var enigma_encrypt_gear_three_pos) (fn [_] (+ enigma_encrypt_gear_three_pos 1))))))))))

(defn engine [enigma_encrypt_message enigma_encrypt_token engine_ch]
  (binding [engine_target nil] (try (do (set! engine_target (index_of_char enigma_encrypt_alphabets engine_ch)) (set! engine_target (nth enigma_encrypt_gear_one engine_target)) (set! engine_target (nth enigma_encrypt_gear_two engine_target)) (set! engine_target (nth enigma_encrypt_gear_three engine_target)) (set! engine_target (nth enigma_encrypt_reflector engine_target)) (set! engine_target (index_of_int enigma_encrypt_gear_three engine_target)) (set! engine_target (index_of_int enigma_encrypt_gear_two engine_target)) (set! engine_target (index_of_int enigma_encrypt_gear_one engine_target)) (rotator enigma_encrypt_message enigma_encrypt_token) (throw (ex-info "return" {:v (nth enigma_encrypt_alphabets engine_target)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn enigma_encrypt [enigma_encrypt_message enigma_encrypt_token]
  (binding [enigma_encrypt_alphabets nil enigma_encrypt_gear_one nil enigma_encrypt_gear_one_pos nil enigma_encrypt_gear_three nil enigma_encrypt_gear_three_pos nil enigma_encrypt_gear_two nil enigma_encrypt_gear_two_pos nil enigma_encrypt_idx nil enigma_encrypt_n nil enigma_encrypt_reflector nil enigma_encrypt_result nil enigma_encrypt_t nil] (try (do (set! enigma_encrypt_alphabets (build_alphabet)) (set! enigma_encrypt_n (count enigma_encrypt_alphabets)) (set! enigma_encrypt_gear_one (range_list enigma_encrypt_n)) (set! enigma_encrypt_gear_two (range_list enigma_encrypt_n)) (set! enigma_encrypt_gear_three (range_list enigma_encrypt_n)) (set! enigma_encrypt_reflector (reversed_range_list enigma_encrypt_n)) (set! enigma_encrypt_gear_one_pos 0) (set! enigma_encrypt_gear_two_pos 0) (set! enigma_encrypt_gear_three_pos 0) (set! enigma_encrypt_t 0) (while (< enigma_encrypt_t enigma_encrypt_token) (do (rotator enigma_encrypt_message enigma_encrypt_token) (set! enigma_encrypt_t (+ enigma_encrypt_t 1)))) (set! enigma_encrypt_result "") (set! enigma_encrypt_idx 0) (while (< enigma_encrypt_idx (count enigma_encrypt_message)) (do (set! enigma_encrypt_result (+ enigma_encrypt_result (engine enigma_encrypt_message enigma_encrypt_token (nth enigma_encrypt_message enigma_encrypt_idx)))) (set! enigma_encrypt_idx (+ enigma_encrypt_idx 1)))) (throw (ex-info "return" {:v enigma_encrypt_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_message "HELLO WORLD")

(def ^:dynamic main_token 123)

(def ^:dynamic main_encoded (enigma_encrypt main_message main_token))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_encoded)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
