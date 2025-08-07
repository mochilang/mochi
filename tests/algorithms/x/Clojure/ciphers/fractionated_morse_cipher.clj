(ns main (:refer-clojure :exclude [encodeToMorse encryptFractionatedMorse decryptFractionatedMorse]))

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

(declare encodeToMorse encryptFractionatedMorse decryptFractionatedMorse)

(def ^:dynamic decryptFractionatedMorse_ch nil)

(def ^:dynamic decryptFractionatedMorse_code nil)

(def ^:dynamic decryptFractionatedMorse_codes nil)

(def ^:dynamic decryptFractionatedMorse_combinedKey nil)

(def ^:dynamic decryptFractionatedMorse_current nil)

(def ^:dynamic decryptFractionatedMorse_decrypted nil)

(def ^:dynamic decryptFractionatedMorse_dedupKey nil)

(def ^:dynamic decryptFractionatedMorse_end nil)

(def ^:dynamic decryptFractionatedMorse_i nil)

(def ^:dynamic decryptFractionatedMorse_idx nil)

(def ^:dynamic decryptFractionatedMorse_inv nil)

(def ^:dynamic decryptFractionatedMorse_j nil)

(def ^:dynamic decryptFractionatedMorse_k nil)

(def ^:dynamic decryptFractionatedMorse_letter nil)

(def ^:dynamic decryptFractionatedMorse_m nil)

(def ^:dynamic decryptFractionatedMorse_morse nil)

(def ^:dynamic decryptFractionatedMorse_start nil)

(def ^:dynamic encodeToMorse_ch nil)

(def ^:dynamic encodeToMorse_code nil)

(def ^:dynamic encodeToMorse_i nil)

(def ^:dynamic encodeToMorse_morse nil)

(def ^:dynamic encryptFractionatedMorse_ch nil)

(def ^:dynamic encryptFractionatedMorse_combinedKey nil)

(def ^:dynamic encryptFractionatedMorse_combo nil)

(def ^:dynamic encryptFractionatedMorse_dedupKey nil)

(def ^:dynamic encryptFractionatedMorse_dict nil)

(def ^:dynamic encryptFractionatedMorse_encrypted nil)

(def ^:dynamic encryptFractionatedMorse_group nil)

(def ^:dynamic encryptFractionatedMorse_i nil)

(def ^:dynamic encryptFractionatedMorse_j nil)

(def ^:dynamic encryptFractionatedMorse_k nil)

(def ^:dynamic encryptFractionatedMorse_letter nil)

(def ^:dynamic encryptFractionatedMorse_morseCode nil)

(def ^:dynamic encryptFractionatedMorse_p nil)

(def ^:dynamic encryptFractionatedMorse_paddingLength nil)

(def ^:dynamic main_MORSE_CODE_DICT {"A" ".-" "B" "-..." "C" "-.-." "D" "-.." "E" "." "F" "..-." "G" "--." "H" "...." "I" ".." "J" ".---" "K" "-.-" "L" ".-.." "M" "--" "N" "-." "O" "---" "P" ".--." "Q" "--.-" "R" ".-." "S" "..." "T" "-" "U" "..-" "V" "...-" "W" ".--" "X" "-..-" "Y" "-.--" "Z" "--.." " " ""})

(def ^:dynamic main_MORSE_COMBINATIONS ["..." "..-" "..x" ".-." ".--" ".-x" ".x." ".x-" ".xx" "-.." "-.-" "-.x" "--." "---" "--x" "-x." "-x-" "-xx" "x.." "x.-" "x.x" "x-." "x--" "x-x" "xx." "xx-" "xxx"])

(def ^:dynamic main_REVERSE_DICT {".-" "A" "-..." "B" "-.-." "C" "-.." "D" "." "E" "..-." "F" "--." "G" "...." "H" ".." "I" ".---" "J" "-.-" "K" ".-.." "L" "--" "M" "-." "N" "---" "O" ".--." "P" "--.-" "Q" ".-." "R" "..." "S" "-" "T" "..-" "U" "...-" "V" ".--" "W" "-..-" "X" "-.--" "Y" "--.." "Z" "" " "})

(defn encodeToMorse [encodeToMorse_plaintext]
  (binding [encodeToMorse_ch nil encodeToMorse_code nil encodeToMorse_i nil encodeToMorse_morse nil] (try (do (set! encodeToMorse_morse "") (set! encodeToMorse_i 0) (while (< encodeToMorse_i (count encodeToMorse_plaintext)) (do (set! encodeToMorse_ch (clojure.string/upper-case (subs encodeToMorse_plaintext encodeToMorse_i (min (+ encodeToMorse_i 1) (count encodeToMorse_plaintext))))) (set! encodeToMorse_code "") (when (in encodeToMorse_ch main_MORSE_CODE_DICT) (set! encodeToMorse_code (nth main_MORSE_CODE_DICT encodeToMorse_ch))) (when (> encodeToMorse_i 0) (set! encodeToMorse_morse (str encodeToMorse_morse "x"))) (set! encodeToMorse_morse (str encodeToMorse_morse encodeToMorse_code)) (set! encodeToMorse_i (+ encodeToMorse_i 1)))) (throw (ex-info "return" {:v encodeToMorse_morse}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encryptFractionatedMorse [encryptFractionatedMorse_plaintext encryptFractionatedMorse_key]
  (binding [encryptFractionatedMorse_ch nil encryptFractionatedMorse_combinedKey nil encryptFractionatedMorse_combo nil encryptFractionatedMorse_dedupKey nil encryptFractionatedMorse_dict nil encryptFractionatedMorse_encrypted nil encryptFractionatedMorse_group nil encryptFractionatedMorse_i nil encryptFractionatedMorse_j nil encryptFractionatedMorse_k nil encryptFractionatedMorse_letter nil encryptFractionatedMorse_morseCode nil encryptFractionatedMorse_p nil encryptFractionatedMorse_paddingLength nil] (try (do (set! encryptFractionatedMorse_morseCode (encodeToMorse encryptFractionatedMorse_plaintext)) (set! encryptFractionatedMorse_combinedKey (str (clojure.string/upper-case encryptFractionatedMorse_key) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) (set! encryptFractionatedMorse_dedupKey "") (set! encryptFractionatedMorse_i 0) (while (< encryptFractionatedMorse_i (count encryptFractionatedMorse_combinedKey)) (do (set! encryptFractionatedMorse_ch (subs encryptFractionatedMorse_combinedKey encryptFractionatedMorse_i (min (+ encryptFractionatedMorse_i 1) (count encryptFractionatedMorse_combinedKey)))) (when (not (in encryptFractionatedMorse_ch encryptFractionatedMorse_dedupKey)) (set! encryptFractionatedMorse_dedupKey (str encryptFractionatedMorse_dedupKey encryptFractionatedMorse_ch))) (set! encryptFractionatedMorse_i (+ encryptFractionatedMorse_i 1)))) (set! encryptFractionatedMorse_paddingLength (- 3 (mod (count encryptFractionatedMorse_morseCode) 3))) (set! encryptFractionatedMorse_p 0) (while (< encryptFractionatedMorse_p encryptFractionatedMorse_paddingLength) (do (set! encryptFractionatedMorse_morseCode (str encryptFractionatedMorse_morseCode "x")) (set! encryptFractionatedMorse_p (+ encryptFractionatedMorse_p 1)))) (set! encryptFractionatedMorse_dict {}) (set! encryptFractionatedMorse_j 0) (while (< encryptFractionatedMorse_j 26) (do (set! encryptFractionatedMorse_combo (nth main_MORSE_COMBINATIONS encryptFractionatedMorse_j)) (set! encryptFractionatedMorse_letter (subs encryptFractionatedMorse_dedupKey encryptFractionatedMorse_j (min (+ encryptFractionatedMorse_j 1) (count encryptFractionatedMorse_dedupKey)))) (set! encryptFractionatedMorse_dict (assoc encryptFractionatedMorse_dict encryptFractionatedMorse_combo encryptFractionatedMorse_letter)) (set! encryptFractionatedMorse_j (+ encryptFractionatedMorse_j 1)))) (set! encryptFractionatedMorse_dict (assoc encryptFractionatedMorse_dict "xxx" "")) (set! encryptFractionatedMorse_encrypted "") (set! encryptFractionatedMorse_k 0) (while (< encryptFractionatedMorse_k (count encryptFractionatedMorse_morseCode)) (do (set! encryptFractionatedMorse_group (subs encryptFractionatedMorse_morseCode encryptFractionatedMorse_k (min (+ encryptFractionatedMorse_k 3) (count encryptFractionatedMorse_morseCode)))) (set! encryptFractionatedMorse_encrypted (str encryptFractionatedMorse_encrypted (get encryptFractionatedMorse_dict encryptFractionatedMorse_group))) (set! encryptFractionatedMorse_k (+ encryptFractionatedMorse_k 3)))) (throw (ex-info "return" {:v encryptFractionatedMorse_encrypted}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decryptFractionatedMorse [decryptFractionatedMorse_ciphertext decryptFractionatedMorse_key]
  (binding [decryptFractionatedMorse_ch nil decryptFractionatedMorse_code nil decryptFractionatedMorse_codes nil decryptFractionatedMorse_combinedKey nil decryptFractionatedMorse_current nil decryptFractionatedMorse_decrypted nil decryptFractionatedMorse_dedupKey nil decryptFractionatedMorse_end nil decryptFractionatedMorse_i nil decryptFractionatedMorse_idx nil decryptFractionatedMorse_inv nil decryptFractionatedMorse_j nil decryptFractionatedMorse_k nil decryptFractionatedMorse_letter nil decryptFractionatedMorse_m nil decryptFractionatedMorse_morse nil decryptFractionatedMorse_start nil] (try (do (set! decryptFractionatedMorse_combinedKey (str (clojure.string/upper-case decryptFractionatedMorse_key) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) (set! decryptFractionatedMorse_dedupKey "") (set! decryptFractionatedMorse_i 0) (while (< decryptFractionatedMorse_i (count decryptFractionatedMorse_combinedKey)) (do (set! decryptFractionatedMorse_ch (subs decryptFractionatedMorse_combinedKey decryptFractionatedMorse_i (min (+ decryptFractionatedMorse_i 1) (count decryptFractionatedMorse_combinedKey)))) (when (not (in decryptFractionatedMorse_ch decryptFractionatedMorse_dedupKey)) (set! decryptFractionatedMorse_dedupKey (str decryptFractionatedMorse_dedupKey decryptFractionatedMorse_ch))) (set! decryptFractionatedMorse_i (+ decryptFractionatedMorse_i 1)))) (set! decryptFractionatedMorse_inv {}) (set! decryptFractionatedMorse_j 0) (while (< decryptFractionatedMorse_j 26) (do (set! decryptFractionatedMorse_letter (subs decryptFractionatedMorse_dedupKey decryptFractionatedMorse_j (min (+ decryptFractionatedMorse_j 1) (count decryptFractionatedMorse_dedupKey)))) (set! decryptFractionatedMorse_inv (assoc decryptFractionatedMorse_inv decryptFractionatedMorse_letter (nth main_MORSE_COMBINATIONS decryptFractionatedMorse_j))) (set! decryptFractionatedMorse_j (+ decryptFractionatedMorse_j 1)))) (set! decryptFractionatedMorse_morse "") (set! decryptFractionatedMorse_k 0) (while (< decryptFractionatedMorse_k (count decryptFractionatedMorse_ciphertext)) (do (set! decryptFractionatedMorse_ch (subs decryptFractionatedMorse_ciphertext decryptFractionatedMorse_k (min (+ decryptFractionatedMorse_k 1) (count decryptFractionatedMorse_ciphertext)))) (when (in decryptFractionatedMorse_ch decryptFractionatedMorse_inv) (set! decryptFractionatedMorse_morse (str decryptFractionatedMorse_morse (get decryptFractionatedMorse_inv decryptFractionatedMorse_ch)))) (set! decryptFractionatedMorse_k (+ decryptFractionatedMorse_k 1)))) (set! decryptFractionatedMorse_codes []) (set! decryptFractionatedMorse_current "") (set! decryptFractionatedMorse_m 0) (while (< decryptFractionatedMorse_m (count decryptFractionatedMorse_morse)) (do (set! decryptFractionatedMorse_ch (subs decryptFractionatedMorse_morse decryptFractionatedMorse_m (min (+ decryptFractionatedMorse_m 1) (count decryptFractionatedMorse_morse)))) (if (= decryptFractionatedMorse_ch "x") (do (set! decryptFractionatedMorse_codes (conj decryptFractionatedMorse_codes decryptFractionatedMorse_current)) (set! decryptFractionatedMorse_current "")) (set! decryptFractionatedMorse_current (str decryptFractionatedMorse_current decryptFractionatedMorse_ch))) (set! decryptFractionatedMorse_m (+ decryptFractionatedMorse_m 1)))) (set! decryptFractionatedMorse_codes (conj decryptFractionatedMorse_codes decryptFractionatedMorse_current)) (set! decryptFractionatedMorse_decrypted "") (set! decryptFractionatedMorse_idx 0) (while (< decryptFractionatedMorse_idx (count decryptFractionatedMorse_codes)) (do (set! decryptFractionatedMorse_code (nth decryptFractionatedMorse_codes decryptFractionatedMorse_idx)) (set! decryptFractionatedMorse_decrypted (str decryptFractionatedMorse_decrypted (nth main_REVERSE_DICT decryptFractionatedMorse_code))) (set! decryptFractionatedMorse_idx (+ decryptFractionatedMorse_idx 1)))) (set! decryptFractionatedMorse_start 0) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (when (< decryptFractionatedMorse_start (count decryptFractionatedMorse_decrypted)) (when (= (subs decryptFractionatedMorse_decrypted decryptFractionatedMorse_start (min (+ decryptFractionatedMorse_start 1) (count decryptFractionatedMorse_decrypted))) " ") (do (set! decryptFractionatedMorse_start (+ decryptFractionatedMorse_start 1)) (recur true)))) (cond true (recur false) :else (recur while_flag_1))))) (set! decryptFractionatedMorse_end (count decryptFractionatedMorse_decrypted)) (loop [while_flag_2 true] (when (and while_flag_2 true) (do (when (> decryptFractionatedMorse_end decryptFractionatedMorse_start) (when (= (subs decryptFractionatedMorse_decrypted (- decryptFractionatedMorse_end 1) (min decryptFractionatedMorse_end (count decryptFractionatedMorse_decrypted))) " ") (do (set! decryptFractionatedMorse_end (- decryptFractionatedMorse_end 1)) (recur true)))) (cond true (recur false) :else (recur while_flag_2))))) (throw (ex-info "return" {:v (subs decryptFractionatedMorse_decrypted decryptFractionatedMorse_start (min decryptFractionatedMorse_end (count decryptFractionatedMorse_decrypted)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_plaintext "defend the east")

(def ^:dynamic main_key "ROUNDTABLE")

(def ^:dynamic main_ciphertext (encryptFractionatedMorse main_plaintext main_key))

(def ^:dynamic main_decrypted (decryptFractionatedMorse main_ciphertext main_key))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Plain Text:" main_plaintext)
      (println "Encrypted:" main_ciphertext)
      (println "Decrypted:" main_decrypted)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
