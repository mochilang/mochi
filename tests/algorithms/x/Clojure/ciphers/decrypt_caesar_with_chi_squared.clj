(ns main (:refer-clojure :exclude [default_alphabet default_frequencies index_of count_char decrypt_caesar_with_chi_squared]))

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

(declare default_alphabet default_frequencies index_of count_char decrypt_caesar_with_chi_squared)

(def ^:dynamic count_char_i nil)

(def ^:dynamic count_v nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_alphabet_letters nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_best_chi nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_best_shift nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_best_text nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_ch nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_chi nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_ciphertext nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_decrypted nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_diff nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_expected nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_frequencies nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_i nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_idx nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_j nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_letter nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_lowered nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_m nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_new_char nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_new_idx nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_occ nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_occf nil)

(def ^:dynamic decrypt_caesar_with_chi_squared_shift nil)

(def ^:dynamic index_of_i nil)

(defn default_alphabet []
  (try (throw (ex-info "return" {:v ["a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn default_frequencies []
  (try (throw (ex-info "return" {:v {"a" 0.08497 "b" 0.01492 "c" 0.02202 "d" 0.04253 "e" 0.11162 "f" 0.02228 "g" 0.02015 "h" 0.06094 "i" 0.07546 "j" 0.00153 "k" 0.01292 "l" 0.04025 "m" 0.02406 "n" 0.06749 "o" 0.07507 "p" 0.01929 "q" 0.00095 "r" 0.07587 "s" 0.06327 "t" 0.09356 "u" 0.02758 "v" 0.00978 "w" 0.0256 "x" 0.0015 "y" 0.01994 "z" 0.00077}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn index_of [index_of_xs index_of_ch]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_xs)) (do (when (= (nth index_of_xs index_of_i) index_of_ch) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_char [count_char_s count_char_ch]
  (binding [count_char_i nil count_v nil] (try (do (set! count_v 0) (set! count_char_i 0) (while (< count_char_i (count count_char_s)) (do (when (= (subs count_char_s count_char_i (min (+ count_char_i 1) (count count_char_s))) count_char_ch) (set! count_v (+ count_v 1))) (set! count_char_i (+ count_char_i 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt_caesar_with_chi_squared [decrypt_caesar_with_chi_squared_ciphertext_p decrypt_caesar_with_chi_squared_cipher_alphabet decrypt_caesar_with_chi_squared_frequencies_dict decrypt_caesar_with_chi_squared_case_sensitive]
  (binding [decrypt_caesar_with_chi_squared_alphabet_letters nil decrypt_caesar_with_chi_squared_best_chi nil decrypt_caesar_with_chi_squared_best_shift nil decrypt_caesar_with_chi_squared_best_text nil decrypt_caesar_with_chi_squared_ch nil decrypt_caesar_with_chi_squared_chi nil decrypt_caesar_with_chi_squared_ciphertext nil decrypt_caesar_with_chi_squared_decrypted nil decrypt_caesar_with_chi_squared_diff nil decrypt_caesar_with_chi_squared_expected nil decrypt_caesar_with_chi_squared_frequencies nil decrypt_caesar_with_chi_squared_i nil decrypt_caesar_with_chi_squared_idx nil decrypt_caesar_with_chi_squared_j nil decrypt_caesar_with_chi_squared_letter nil decrypt_caesar_with_chi_squared_lowered nil decrypt_caesar_with_chi_squared_m nil decrypt_caesar_with_chi_squared_new_char nil decrypt_caesar_with_chi_squared_new_idx nil decrypt_caesar_with_chi_squared_occ nil decrypt_caesar_with_chi_squared_occf nil decrypt_caesar_with_chi_squared_shift nil] (try (do (set! decrypt_caesar_with_chi_squared_ciphertext decrypt_caesar_with_chi_squared_ciphertext_p) (set! decrypt_caesar_with_chi_squared_alphabet_letters decrypt_caesar_with_chi_squared_cipher_alphabet) (when (= (count decrypt_caesar_with_chi_squared_alphabet_letters) 0) (set! decrypt_caesar_with_chi_squared_alphabet_letters (default_alphabet))) (set! decrypt_caesar_with_chi_squared_frequencies decrypt_caesar_with_chi_squared_frequencies_dict) (when (= (count decrypt_caesar_with_chi_squared_frequencies) 0) (set! decrypt_caesar_with_chi_squared_frequencies (default_frequencies))) (when (not decrypt_caesar_with_chi_squared_case_sensitive) (set! decrypt_caesar_with_chi_squared_ciphertext (clojure.string/lower-case decrypt_caesar_with_chi_squared_ciphertext))) (set! decrypt_caesar_with_chi_squared_best_shift 0) (set! decrypt_caesar_with_chi_squared_best_chi 0.0) (set! decrypt_caesar_with_chi_squared_best_text "") (set! decrypt_caesar_with_chi_squared_shift 0) (while (< decrypt_caesar_with_chi_squared_shift (count decrypt_caesar_with_chi_squared_alphabet_letters)) (do (set! decrypt_caesar_with_chi_squared_decrypted "") (set! decrypt_caesar_with_chi_squared_i 0) (while (< decrypt_caesar_with_chi_squared_i (count decrypt_caesar_with_chi_squared_ciphertext)) (do (set! decrypt_caesar_with_chi_squared_ch (subs decrypt_caesar_with_chi_squared_ciphertext decrypt_caesar_with_chi_squared_i (min (+ decrypt_caesar_with_chi_squared_i 1) (count decrypt_caesar_with_chi_squared_ciphertext)))) (set! decrypt_caesar_with_chi_squared_idx (index_of decrypt_caesar_with_chi_squared_alphabet_letters (clojure.string/lower-case decrypt_caesar_with_chi_squared_ch))) (if (>= decrypt_caesar_with_chi_squared_idx 0) (do (set! decrypt_caesar_with_chi_squared_m (count decrypt_caesar_with_chi_squared_alphabet_letters)) (set! decrypt_caesar_with_chi_squared_new_idx (mod (- decrypt_caesar_with_chi_squared_idx decrypt_caesar_with_chi_squared_shift) decrypt_caesar_with_chi_squared_m)) (when (< decrypt_caesar_with_chi_squared_new_idx 0) (set! decrypt_caesar_with_chi_squared_new_idx (+ decrypt_caesar_with_chi_squared_new_idx decrypt_caesar_with_chi_squared_m))) (set! decrypt_caesar_with_chi_squared_new_char (nth decrypt_caesar_with_chi_squared_alphabet_letters decrypt_caesar_with_chi_squared_new_idx)) (if (and decrypt_caesar_with_chi_squared_case_sensitive (not= decrypt_caesar_with_chi_squared_ch (clojure.string/lower-case decrypt_caesar_with_chi_squared_ch))) (set! decrypt_caesar_with_chi_squared_decrypted (str decrypt_caesar_with_chi_squared_decrypted (clojure.string/upper-case decrypt_caesar_with_chi_squared_new_char))) (set! decrypt_caesar_with_chi_squared_decrypted (str decrypt_caesar_with_chi_squared_decrypted decrypt_caesar_with_chi_squared_new_char)))) (set! decrypt_caesar_with_chi_squared_decrypted (str decrypt_caesar_with_chi_squared_decrypted decrypt_caesar_with_chi_squared_ch))) (set! decrypt_caesar_with_chi_squared_i (+ decrypt_caesar_with_chi_squared_i 1)))) (set! decrypt_caesar_with_chi_squared_chi 0.0) (set! decrypt_caesar_with_chi_squared_lowered (if decrypt_caesar_with_chi_squared_case_sensitive (clojure.string/lower-case decrypt_caesar_with_chi_squared_decrypted) decrypt_caesar_with_chi_squared_decrypted)) (set! decrypt_caesar_with_chi_squared_j 0) (while (< decrypt_caesar_with_chi_squared_j (count decrypt_caesar_with_chi_squared_alphabet_letters)) (do (set! decrypt_caesar_with_chi_squared_letter (nth decrypt_caesar_with_chi_squared_alphabet_letters decrypt_caesar_with_chi_squared_j)) (set! decrypt_caesar_with_chi_squared_occ (count_char decrypt_caesar_with_chi_squared_lowered decrypt_caesar_with_chi_squared_letter)) (when (> decrypt_caesar_with_chi_squared_occ 0) (do (set! decrypt_caesar_with_chi_squared_occf (double decrypt_caesar_with_chi_squared_occ)) (set! decrypt_caesar_with_chi_squared_expected (* (get decrypt_caesar_with_chi_squared_frequencies decrypt_caesar_with_chi_squared_letter) decrypt_caesar_with_chi_squared_occf)) (set! decrypt_caesar_with_chi_squared_diff (- decrypt_caesar_with_chi_squared_occf decrypt_caesar_with_chi_squared_expected)) (set! decrypt_caesar_with_chi_squared_chi (+ decrypt_caesar_with_chi_squared_chi (* (/ (* decrypt_caesar_with_chi_squared_diff decrypt_caesar_with_chi_squared_diff) decrypt_caesar_with_chi_squared_expected) decrypt_caesar_with_chi_squared_occf))))) (set! decrypt_caesar_with_chi_squared_j (+ decrypt_caesar_with_chi_squared_j 1)))) (when (or (= decrypt_caesar_with_chi_squared_shift 0) (< decrypt_caesar_with_chi_squared_chi decrypt_caesar_with_chi_squared_best_chi)) (do (set! decrypt_caesar_with_chi_squared_best_shift decrypt_caesar_with_chi_squared_shift) (set! decrypt_caesar_with_chi_squared_best_chi decrypt_caesar_with_chi_squared_chi) (set! decrypt_caesar_with_chi_squared_best_text decrypt_caesar_with_chi_squared_decrypted))) (set! decrypt_caesar_with_chi_squared_shift (+ decrypt_caesar_with_chi_squared_shift 1)))) (throw (ex-info "return" {:v {:shift decrypt_caesar_with_chi_squared_best_shift :chi decrypt_caesar_with_chi_squared_best_chi :decoded decrypt_caesar_with_chi_squared_best_text}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_r1 (decrypt_caesar_with_chi_squared "dof pz aol jhlzhy jpwoly zv wvwbshy? pa pz avv lhzf av jyhjr!" [] {} false))

(def ^:dynamic main_r2 (decrypt_caesar_with_chi_squared "crybd cdbsxq" [] {} false))

(def ^:dynamic main_r3 (decrypt_caesar_with_chi_squared "Crybd Cdbsxq" [] {} true))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (str (str (str (str (:shift main_r1)) ", ") (str (:chi main_r1))) ", ") (:decoded main_r1)))
      (println (str (str (str (str (str (:shift main_r2)) ", ") (str (:chi main_r2))) ", ") (:decoded main_r2)))
      (println (str (str (str (str (str (:shift main_r3)) ", ") (str (:chi main_r3))) ", ") (:decoded main_r3)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
