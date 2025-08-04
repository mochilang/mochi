(ns main (:refer-clojure :exclude [indexOf ord chr shiftRune encipher decipher main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare indexOf ord chr shiftRune encipher decipher main)

(declare chr_lower chr_upper encipher_i encipher_out indexOf_i main_ct main_pt ord_idx ord_lower ord_upper)

(defn indexOf [indexOf_s indexOf_ch]
  (try (do (def indexOf_i 0) (while (< indexOf_i (count indexOf_s)) (do (when (= (subs indexOf_s indexOf_i (+ indexOf_i 1)) indexOf_ch) (throw (ex-info "return" {:v indexOf_i}))) (def indexOf_i (+ indexOf_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ord [ord_ch]
  (try (do (def ord_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (def ord_lower "abcdefghijklmnopqrstuvwxyz") (def ord_idx (indexOf ord_upper ord_ch)) (when (>= ord_idx 0) (throw (ex-info "return" {:v (+ 65 ord_idx)}))) (def ord_idx (indexOf ord_lower ord_ch)) (if (>= ord_idx 0) (+ 97 ord_idx) 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn chr [chr_n]
  (try (do (def chr_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (def chr_lower "abcdefghijklmnopqrstuvwxyz") (when (and (>= chr_n 65) (< chr_n 91)) (throw (ex-info "return" {:v (subs chr_upper (- chr_n 65) (- chr_n 64))}))) (if (and (>= chr_n 97) (< chr_n 123)) (subs chr_lower (- chr_n 97) (- chr_n 96)) "?")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn shiftRune [shiftRune_r shiftRune_k]
  (try (do (when (and (>= (compare shiftRune_r "a") 0) (<= (compare shiftRune_r "z") 0)) (throw (ex-info "return" {:v (chr (+ (mod (+ (- (ord shiftRune_r) 97) shiftRune_k) 26) 97))}))) (if (and (>= (compare shiftRune_r "A") 0) (<= (compare shiftRune_r "Z") 0)) (chr (+ (mod (+ (- (ord shiftRune_r) 65) shiftRune_k) 26) 65)) shiftRune_r)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn encipher [encipher_s encipher_k]
  (try (do (def encipher_out "") (def encipher_i 0) (while (< encipher_i (count encipher_s)) (do (def encipher_out (str encipher_out (shiftRune (subs encipher_s encipher_i (+ encipher_i 1)) encipher_k))) (def encipher_i (+ encipher_i 1)))) (throw (ex-info "return" {:v encipher_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn decipher [decipher_s decipher_k]
  (try (throw (ex-info "return" {:v (encipher decipher_s (mod (- 26 (mod decipher_k 26)) 26))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def main_pt "The five boxing wizards jump quickly") (println (str "Plaintext: " main_pt)) (loop [key_seq [0 1 7 25 26]] (when (seq key_seq) (let [key (first key_seq)] (cond (or (< key 1) (> key 25)) (do (println (str (str "Key " (str key)) " invalid")) (recur (rest key_seq))) :else (do (def main_ct (encipher main_pt key)) (println (str "Key " (str key))) (println (str "  Enciphered: " main_ct)) (println (str "  Deciphered: " (decipher main_ct key))) (recur (rest key_seq)))))))))

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
