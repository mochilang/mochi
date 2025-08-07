(ns main (:refer-clojure :exclude [remove_spaces char_to_trigram trigram_to_char encrypt_part encrypt_message decrypt_part decrypt_message main]))

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

(declare remove_spaces char_to_trigram trigram_to_char encrypt_part encrypt_message decrypt_part decrypt_message main)

(def ^:dynamic char_to_trigram_i nil)

(def ^:dynamic decrypt_message_alpha nil)

(def ^:dynamic decrypt_message_decrypted nil)

(def ^:dynamic decrypt_message_decrypted_numeric nil)

(def ^:dynamic decrypt_message_end nil)

(def ^:dynamic decrypt_message_groups nil)

(def ^:dynamic decrypt_message_i nil)

(def ^:dynamic decrypt_message_j nil)

(def ^:dynamic decrypt_message_k nil)

(def ^:dynamic decrypt_message_msg nil)

(def ^:dynamic decrypt_message_part nil)

(def ^:dynamic decrypt_message_tri nil)

(def ^:dynamic decrypt_part_converted nil)

(def ^:dynamic decrypt_part_i nil)

(def ^:dynamic decrypt_part_j nil)

(def ^:dynamic decrypt_part_result nil)

(def ^:dynamic decrypt_part_tmp nil)

(def ^:dynamic decrypt_part_tri nil)

(def ^:dynamic encrypt_message_alpha nil)

(def ^:dynamic encrypt_message_encrypted nil)

(def ^:dynamic encrypt_message_encrypted_numeric nil)

(def ^:dynamic encrypt_message_end nil)

(def ^:dynamic encrypt_message_i nil)

(def ^:dynamic encrypt_message_j nil)

(def ^:dynamic encrypt_message_msg nil)

(def ^:dynamic encrypt_message_part nil)

(def ^:dynamic encrypt_message_tri nil)

(def ^:dynamic encrypt_part_i nil)

(def ^:dynamic encrypt_part_one nil)

(def ^:dynamic encrypt_part_three nil)

(def ^:dynamic encrypt_part_tri nil)

(def ^:dynamic encrypt_part_two nil)

(def ^:dynamic main_alphabet nil)

(def ^:dynamic main_decrypted nil)

(def ^:dynamic main_encrypted nil)

(def ^:dynamic main_msg nil)

(def ^:dynamic remove_spaces_c nil)

(def ^:dynamic remove_spaces_i nil)

(def ^:dynamic remove_spaces_res nil)

(def ^:dynamic trigram_to_char_i nil)

(def ^:dynamic main_triagrams ["111" "112" "113" "121" "122" "123" "131" "132" "133" "211" "212" "213" "221" "222" "223" "231" "232" "233" "311" "312" "313" "321" "322" "323" "331" "332" "333"])

(defn remove_spaces [remove_spaces_s]
  (binding [remove_spaces_c nil remove_spaces_i nil remove_spaces_res nil] (try (do (set! remove_spaces_res "") (set! remove_spaces_i 0) (while (< remove_spaces_i (count remove_spaces_s)) (do (set! remove_spaces_c (subs remove_spaces_s remove_spaces_i (min (+ remove_spaces_i 1) (count remove_spaces_s)))) (when (not= remove_spaces_c " ") (set! remove_spaces_res (str remove_spaces_res remove_spaces_c))) (set! remove_spaces_i (+ remove_spaces_i 1)))) (throw (ex-info "return" {:v remove_spaces_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn char_to_trigram [char_to_trigram_ch char_to_trigram_alphabet]
  (binding [char_to_trigram_i nil] (try (do (set! char_to_trigram_i 0) (while (< char_to_trigram_i (count char_to_trigram_alphabet)) (do (when (= (subs char_to_trigram_alphabet char_to_trigram_i (min (+ char_to_trigram_i 1) (count char_to_trigram_alphabet))) char_to_trigram_ch) (throw (ex-info "return" {:v (nth main_triagrams char_to_trigram_i)}))) (set! char_to_trigram_i (+ char_to_trigram_i 1)))) (throw (ex-info "return" {:v ""}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn trigram_to_char [trigram_to_char_tri trigram_to_char_alphabet]
  (binding [trigram_to_char_i nil] (try (do (set! trigram_to_char_i 0) (while (< trigram_to_char_i (count main_triagrams)) (do (when (= (nth main_triagrams trigram_to_char_i) trigram_to_char_tri) (throw (ex-info "return" {:v (subs trigram_to_char_alphabet trigram_to_char_i (min (+ trigram_to_char_i 1) (count trigram_to_char_alphabet)))}))) (set! trigram_to_char_i (+ trigram_to_char_i 1)))) (throw (ex-info "return" {:v ""}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encrypt_part [encrypt_part_part encrypt_part_alphabet]
  (binding [encrypt_part_i nil encrypt_part_one nil encrypt_part_three nil encrypt_part_tri nil encrypt_part_two nil] (try (do (set! encrypt_part_one "") (set! encrypt_part_two "") (set! encrypt_part_three "") (set! encrypt_part_i 0) (while (< encrypt_part_i (count encrypt_part_part)) (do (set! encrypt_part_tri (char_to_trigram (subs encrypt_part_part encrypt_part_i (min (+ encrypt_part_i 1) (count encrypt_part_part))) encrypt_part_alphabet)) (set! encrypt_part_one (str encrypt_part_one (subs encrypt_part_tri 0 (min 1 (count encrypt_part_tri))))) (set! encrypt_part_two (str encrypt_part_two (subs encrypt_part_tri 1 (min 2 (count encrypt_part_tri))))) (set! encrypt_part_three (str encrypt_part_three (subs encrypt_part_tri 2 (min 3 (count encrypt_part_tri))))) (set! encrypt_part_i (+ encrypt_part_i 1)))) (throw (ex-info "return" {:v (str (str encrypt_part_one encrypt_part_two) encrypt_part_three)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encrypt_message [encrypt_message_message encrypt_message_alphabet encrypt_message_period]
  (binding [encrypt_message_alpha nil encrypt_message_encrypted nil encrypt_message_encrypted_numeric nil encrypt_message_end nil encrypt_message_i nil encrypt_message_j nil encrypt_message_msg nil encrypt_message_part nil encrypt_message_tri nil] (try (do (set! encrypt_message_msg (remove_spaces encrypt_message_message)) (set! encrypt_message_alpha (remove_spaces encrypt_message_alphabet)) (when (not= (count encrypt_message_alpha) 27) (throw (ex-info "return" {:v ""}))) (set! encrypt_message_encrypted_numeric "") (set! encrypt_message_i 0) (while (< encrypt_message_i (count encrypt_message_msg)) (do (set! encrypt_message_end (+ encrypt_message_i encrypt_message_period)) (when (> encrypt_message_end (count encrypt_message_msg)) (set! encrypt_message_end (count encrypt_message_msg))) (set! encrypt_message_part (subs encrypt_message_msg encrypt_message_i (min encrypt_message_end (count encrypt_message_msg)))) (set! encrypt_message_encrypted_numeric (str encrypt_message_encrypted_numeric (encrypt_part encrypt_message_part encrypt_message_alpha))) (set! encrypt_message_i (+ encrypt_message_i encrypt_message_period)))) (set! encrypt_message_encrypted "") (set! encrypt_message_j 0) (while (< encrypt_message_j (count encrypt_message_encrypted_numeric)) (do (set! encrypt_message_tri (subs encrypt_message_encrypted_numeric encrypt_message_j (min (+ encrypt_message_j 3) (count encrypt_message_encrypted_numeric)))) (set! encrypt_message_encrypted (str encrypt_message_encrypted (trigram_to_char encrypt_message_tri encrypt_message_alpha))) (set! encrypt_message_j (+ encrypt_message_j 3)))) (throw (ex-info "return" {:v encrypt_message_encrypted}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt_part [decrypt_part_part decrypt_part_alphabet]
  (binding [decrypt_part_converted nil decrypt_part_i nil decrypt_part_j nil decrypt_part_result nil decrypt_part_tmp nil decrypt_part_tri nil] (try (do (set! decrypt_part_converted "") (set! decrypt_part_i 0) (while (< decrypt_part_i (count decrypt_part_part)) (do (set! decrypt_part_tri (char_to_trigram (subs decrypt_part_part decrypt_part_i (min (+ decrypt_part_i 1) (count decrypt_part_part))) decrypt_part_alphabet)) (set! decrypt_part_converted (str decrypt_part_converted decrypt_part_tri)) (set! decrypt_part_i (+ decrypt_part_i 1)))) (set! decrypt_part_result []) (set! decrypt_part_tmp "") (set! decrypt_part_j 0) (while (< decrypt_part_j (count decrypt_part_converted)) (do (set! decrypt_part_tmp (str decrypt_part_tmp (subs decrypt_part_converted decrypt_part_j (min (+ decrypt_part_j 1) (count decrypt_part_converted))))) (when (= (count decrypt_part_tmp) (count decrypt_part_part)) (do (set! decrypt_part_result (conj decrypt_part_result decrypt_part_tmp)) (set! decrypt_part_tmp ""))) (set! decrypt_part_j (+ decrypt_part_j 1)))) (throw (ex-info "return" {:v decrypt_part_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt_message [decrypt_message_message decrypt_message_alphabet decrypt_message_period]
  (binding [decrypt_message_alpha nil decrypt_message_decrypted nil decrypt_message_decrypted_numeric nil decrypt_message_end nil decrypt_message_groups nil decrypt_message_i nil decrypt_message_j nil decrypt_message_k nil decrypt_message_msg nil decrypt_message_part nil decrypt_message_tri nil] (try (do (set! decrypt_message_msg (remove_spaces decrypt_message_message)) (set! decrypt_message_alpha (remove_spaces decrypt_message_alphabet)) (when (not= (count decrypt_message_alpha) 27) (throw (ex-info "return" {:v ""}))) (set! decrypt_message_decrypted_numeric []) (set! decrypt_message_i 0) (while (< decrypt_message_i (count decrypt_message_msg)) (do (set! decrypt_message_end (+ decrypt_message_i decrypt_message_period)) (when (> decrypt_message_end (count decrypt_message_msg)) (set! decrypt_message_end (count decrypt_message_msg))) (set! decrypt_message_part (subs decrypt_message_msg decrypt_message_i (min decrypt_message_end (count decrypt_message_msg)))) (set! decrypt_message_groups (decrypt_part decrypt_message_part decrypt_message_alpha)) (set! decrypt_message_k 0) (while (< decrypt_message_k (count (nth decrypt_message_groups 0))) (do (set! decrypt_message_tri (str (str (subs (nth decrypt_message_groups 0) decrypt_message_k (min (+ decrypt_message_k 1) (count (nth decrypt_message_groups 0)))) (subs (nth decrypt_message_groups 1) decrypt_message_k (min (+ decrypt_message_k 1) (count (nth decrypt_message_groups 1))))) (subs (nth decrypt_message_groups 2) decrypt_message_k (min (+ decrypt_message_k 1) (count (nth decrypt_message_groups 2)))))) (set! decrypt_message_decrypted_numeric (conj decrypt_message_decrypted_numeric decrypt_message_tri)) (set! decrypt_message_k (+ decrypt_message_k 1)))) (set! decrypt_message_i (+ decrypt_message_i decrypt_message_period)))) (set! decrypt_message_decrypted "") (set! decrypt_message_j 0) (while (< decrypt_message_j (count decrypt_message_decrypted_numeric)) (do (set! decrypt_message_decrypted (str decrypt_message_decrypted (trigram_to_char (nth decrypt_message_decrypted_numeric decrypt_message_j) decrypt_message_alpha))) (set! decrypt_message_j (+ decrypt_message_j 1)))) (throw (ex-info "return" {:v decrypt_message_decrypted}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_alphabet nil main_decrypted nil main_encrypted nil main_msg nil] (do (set! main_msg "DEFEND THE EAST WALL OF THE CASTLE.") (set! main_alphabet "EPSDUCVWYM.ZLKXNBTFGORIJHAQ") (set! main_encrypted (encrypt_message main_msg main_alphabet 5)) (set! main_decrypted (decrypt_message main_encrypted main_alphabet 5)) (println (str "Encrypted: " main_encrypted)) (println (str "Decrypted: " main_decrypted)))))

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
