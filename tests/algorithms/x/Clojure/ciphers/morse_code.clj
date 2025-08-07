(ns main (:refer-clojure :exclude [to_upper_char to_upper index_of encrypt split_spaces decrypt]))

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

(declare to_upper_char to_upper index_of encrypt split_spaces decrypt)

(def ^:dynamic decrypt_idx nil)

(def ^:dynamic decrypt_parts nil)

(def ^:dynamic decrypt_res nil)

(def ^:dynamic encrypt_c nil)

(def ^:dynamic encrypt_i nil)

(def ^:dynamic encrypt_idx nil)

(def ^:dynamic encrypt_msg nil)

(def ^:dynamic encrypt_res nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic split_spaces_ch nil)

(def ^:dynamic split_spaces_current nil)

(def ^:dynamic split_spaces_i nil)

(def ^:dynamic split_spaces_res nil)

(def ^:dynamic to_upper_i nil)

(def ^:dynamic to_upper_res nil)

(def ^:dynamic main_CHARS ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "&" "@" ":" "," "." "'" "\"" "?" "/" "=" "+" "-" "(" ")" "!" " "])

(def ^:dynamic main_CODES [".-" "-..." "-.-." "-.." "." "..-." "--." "...." ".." ".---" "-.-" ".-.." "--" "-." "---" ".--." "--.-" ".-." "..." "-" "..-" "...-" ".--" "-..-" "-.--" "--.." ".----" "..---" "...--" "....-" "....." "-...." "--..." "---.." "----." "-----" ".-..." ".--.-." "---..." "--..--" ".-.-.-" ".----." ".-..-." "..--.." "-..-." "-...-" ".-.-." "-....-" "-.--." "-.--.-" "-.-.--" "/"])

(defn to_upper_char [to_upper_char_c]
  (try (do (when (= to_upper_char_c "a") (throw (ex-info "return" {:v "A"}))) (when (= to_upper_char_c "b") (throw (ex-info "return" {:v "B"}))) (when (= to_upper_char_c "c") (throw (ex-info "return" {:v "C"}))) (when (= to_upper_char_c "d") (throw (ex-info "return" {:v "D"}))) (when (= to_upper_char_c "e") (throw (ex-info "return" {:v "E"}))) (when (= to_upper_char_c "f") (throw (ex-info "return" {:v "F"}))) (when (= to_upper_char_c "g") (throw (ex-info "return" {:v "G"}))) (when (= to_upper_char_c "h") (throw (ex-info "return" {:v "H"}))) (when (= to_upper_char_c "i") (throw (ex-info "return" {:v "I"}))) (when (= to_upper_char_c "j") (throw (ex-info "return" {:v "J"}))) (when (= to_upper_char_c "k") (throw (ex-info "return" {:v "K"}))) (when (= to_upper_char_c "l") (throw (ex-info "return" {:v "L"}))) (when (= to_upper_char_c "m") (throw (ex-info "return" {:v "M"}))) (when (= to_upper_char_c "n") (throw (ex-info "return" {:v "N"}))) (when (= to_upper_char_c "o") (throw (ex-info "return" {:v "O"}))) (when (= to_upper_char_c "p") (throw (ex-info "return" {:v "P"}))) (when (= to_upper_char_c "q") (throw (ex-info "return" {:v "Q"}))) (when (= to_upper_char_c "r") (throw (ex-info "return" {:v "R"}))) (when (= to_upper_char_c "s") (throw (ex-info "return" {:v "S"}))) (when (= to_upper_char_c "t") (throw (ex-info "return" {:v "T"}))) (when (= to_upper_char_c "u") (throw (ex-info "return" {:v "U"}))) (when (= to_upper_char_c "v") (throw (ex-info "return" {:v "V"}))) (when (= to_upper_char_c "w") (throw (ex-info "return" {:v "W"}))) (when (= to_upper_char_c "x") (throw (ex-info "return" {:v "X"}))) (when (= to_upper_char_c "y") (throw (ex-info "return" {:v "Y"}))) (if (= to_upper_char_c "z") "Z" to_upper_char_c)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_upper [to_upper_s]
  (binding [to_upper_i nil to_upper_res nil] (try (do (set! to_upper_res "") (set! to_upper_i 0) (while (< to_upper_i (count to_upper_s)) (do (set! to_upper_res (str to_upper_res (to_upper_char (nth to_upper_s to_upper_i)))) (set! to_upper_i (+ to_upper_i 1)))) (throw (ex-info "return" {:v to_upper_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn index_of [index_of_xs index_of_target]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_xs)) (do (when (= (nth index_of_xs index_of_i) index_of_target) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encrypt [encrypt_message]
  (binding [encrypt_c nil encrypt_i nil encrypt_idx nil encrypt_msg nil encrypt_res nil] (try (do (set! encrypt_msg (to_upper encrypt_message)) (set! encrypt_res "") (set! encrypt_i 0) (while (< encrypt_i (count encrypt_msg)) (do (set! encrypt_c (nth encrypt_msg encrypt_i)) (set! encrypt_idx (index_of main_CHARS encrypt_c)) (when (>= encrypt_idx 0) (do (when (not= encrypt_res "") (set! encrypt_res (str encrypt_res " "))) (set! encrypt_res (str encrypt_res (nth main_CODES encrypt_idx))))) (set! encrypt_i (+ encrypt_i 1)))) (throw (ex-info "return" {:v encrypt_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn split_spaces [split_spaces_s]
  (binding [split_spaces_ch nil split_spaces_current nil split_spaces_i nil split_spaces_res nil] (try (do (set! split_spaces_res []) (set! split_spaces_current "") (set! split_spaces_i 0) (while (< split_spaces_i (count split_spaces_s)) (do (set! split_spaces_ch (nth split_spaces_s split_spaces_i)) (if (= split_spaces_ch " ") (when (not= split_spaces_current "") (do (set! split_spaces_res (conj split_spaces_res split_spaces_current)) (set! split_spaces_current ""))) (set! split_spaces_current (str split_spaces_current split_spaces_ch))) (set! split_spaces_i (+ split_spaces_i 1)))) (when (not= split_spaces_current "") (set! split_spaces_res (conj split_spaces_res split_spaces_current))) (throw (ex-info "return" {:v split_spaces_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt [decrypt_message]
  (binding [decrypt_idx nil decrypt_parts nil decrypt_res nil] (try (do (set! decrypt_parts (split_spaces decrypt_message)) (set! decrypt_res "") (doseq [code decrypt_parts] (do (set! decrypt_idx (index_of main_CODES code)) (when (>= decrypt_idx 0) (set! decrypt_res (str decrypt_res (nth main_CHARS decrypt_idx)))))) (throw (ex-info "return" {:v decrypt_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_msg "Morse code here!")

(def ^:dynamic main_enc (encrypt main_msg))

(def ^:dynamic main_dec (decrypt main_enc))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_msg)
      (println main_enc)
      (println main_dec)
      (println (encrypt "Sos!"))
      (println (decrypt "... --- ... -.-.--"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
