(ns main (:refer-clojure :exclude [index_of to_lower_without_spaces letter_to_numbers numbers_to_letter encode decode]))

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

(declare index_of to_lower_without_spaces letter_to_numbers numbers_to_letter encode decode)

(def ^:dynamic decode_bottom nil)

(def ^:dynamic decode_c nil)

(def ^:dynamic decode_clean nil)

(def ^:dynamic decode_decoded nil)

(def ^:dynamic decode_i nil)

(def ^:dynamic decode_l nil)

(def ^:dynamic decode_nums nil)

(def ^:dynamic decode_r nil)

(def ^:dynamic decode_top nil)

(def ^:dynamic encode_c nil)

(def ^:dynamic encode_clean nil)

(def ^:dynamic encode_cols nil)

(def ^:dynamic encode_encoded nil)

(def ^:dynamic encode_i nil)

(def ^:dynamic encode_l nil)

(def ^:dynamic encode_nums nil)

(def ^:dynamic encode_r nil)

(def ^:dynamic encode_rows nil)

(def ^:dynamic encode_seq nil)

(def ^:dynamic first_v nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic letter_to_numbers_c nil)

(def ^:dynamic letter_to_numbers_r nil)

(def ^:dynamic to_lower_without_spaces_ch nil)

(def ^:dynamic to_lower_without_spaces_i nil)

(def ^:dynamic to_lower_without_spaces_lower nil)

(def ^:dynamic to_lower_without_spaces_pos nil)

(def ^:dynamic to_lower_without_spaces_res nil)

(def ^:dynamic to_lower_without_spaces_upper nil)

(def ^:dynamic main_SQUARE [["a" "b" "c" "d" "e"] ["f" "g" "h" "i" "k"] ["l" "m" "n" "o" "p"] ["q" "r" "s" "t" "u"] ["v" "w" "x" "y" "z"]])

(defn index_of [index_of_s index_of_ch]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (nth index_of_s index_of_i) index_of_ch) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_lower_without_spaces [to_lower_without_spaces_message to_lower_without_spaces_replace_j]
  (binding [to_lower_without_spaces_ch nil to_lower_without_spaces_i nil to_lower_without_spaces_lower nil to_lower_without_spaces_pos nil to_lower_without_spaces_res nil to_lower_without_spaces_upper nil] (try (do (set! to_lower_without_spaces_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! to_lower_without_spaces_lower "abcdefghijklmnopqrstuvwxyz") (set! to_lower_without_spaces_res "") (set! to_lower_without_spaces_i 0) (while (< to_lower_without_spaces_i (count to_lower_without_spaces_message)) (do (set! to_lower_without_spaces_ch (nth to_lower_without_spaces_message to_lower_without_spaces_i)) (set! to_lower_without_spaces_pos (index_of to_lower_without_spaces_upper to_lower_without_spaces_ch)) (when (>= to_lower_without_spaces_pos 0) (set! to_lower_without_spaces_ch (nth to_lower_without_spaces_lower to_lower_without_spaces_pos))) (when (not= to_lower_without_spaces_ch " ") (do (when (and to_lower_without_spaces_replace_j (= to_lower_without_spaces_ch "j")) (set! to_lower_without_spaces_ch "i")) (set! to_lower_without_spaces_res (str to_lower_without_spaces_res to_lower_without_spaces_ch)))) (set! to_lower_without_spaces_i (+ to_lower_without_spaces_i 1)))) (throw (ex-info "return" {:v to_lower_without_spaces_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn letter_to_numbers [letter_to_numbers_letter]
  (binding [letter_to_numbers_c nil letter_to_numbers_r nil] (try (do (set! letter_to_numbers_r 0) (while (< letter_to_numbers_r (count main_SQUARE)) (do (set! letter_to_numbers_c 0) (while (< letter_to_numbers_c (count (nth main_SQUARE letter_to_numbers_r))) (do (when (= (nth (nth main_SQUARE letter_to_numbers_r) letter_to_numbers_c) letter_to_numbers_letter) (throw (ex-info "return" {:v [(+ letter_to_numbers_r 1) (+ letter_to_numbers_c 1)]}))) (set! letter_to_numbers_c (+ letter_to_numbers_c 1)))) (set! letter_to_numbers_r (+ letter_to_numbers_r 1)))) (throw (ex-info "return" {:v [0 0]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn numbers_to_letter [numbers_to_letter_row numbers_to_letter_col]
  (try (throw (ex-info "return" {:v (nth (nth main_SQUARE (- numbers_to_letter_row 1)) (- numbers_to_letter_col 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn encode [encode_message]
  (binding [encode_c nil encode_clean nil encode_cols nil encode_encoded nil encode_i nil encode_l nil encode_nums nil encode_r nil encode_rows nil encode_seq nil] (try (do (set! encode_clean (to_lower_without_spaces encode_message true)) (set! encode_l (count encode_clean)) (set! encode_rows []) (set! encode_cols []) (set! encode_i 0) (while (< encode_i encode_l) (do (set! encode_nums (letter_to_numbers (nth encode_clean encode_i))) (set! encode_rows (conj encode_rows (nth encode_nums 0))) (set! encode_cols (conj encode_cols (nth encode_nums 1))) (set! encode_i (+ encode_i 1)))) (set! encode_seq []) (set! encode_i 0) (while (< encode_i encode_l) (do (set! encode_seq (conj encode_seq (nth encode_rows encode_i))) (set! encode_i (+ encode_i 1)))) (set! encode_i 0) (while (< encode_i encode_l) (do (set! encode_seq (conj encode_seq (nth encode_cols encode_i))) (set! encode_i (+ encode_i 1)))) (set! encode_encoded "") (set! encode_i 0) (while (< encode_i encode_l) (do (set! encode_r (nth encode_seq (* 2 encode_i))) (set! encode_c (nth encode_seq (+ (* 2 encode_i) 1))) (set! encode_encoded (str encode_encoded (numbers_to_letter encode_r encode_c))) (set! encode_i (+ encode_i 1)))) (throw (ex-info "return" {:v encode_encoded}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decode [decode_message]
  (binding [decode_bottom nil decode_c nil decode_clean nil decode_decoded nil decode_i nil decode_l nil decode_nums nil decode_r nil decode_top nil first_v nil] (try (do (set! decode_clean (to_lower_without_spaces decode_message false)) (set! decode_l (count decode_clean)) (set! first_v []) (set! decode_i 0) (while (< decode_i decode_l) (do (set! decode_nums (letter_to_numbers (nth decode_clean decode_i))) (set! first_v (conj first_v (nth decode_nums 0))) (set! first_v (conj first_v (nth decode_nums 1))) (set! decode_i (+ decode_i 1)))) (set! decode_top []) (set! decode_bottom []) (set! decode_i 0) (while (< decode_i decode_l) (do (set! decode_top (conj decode_top (nth first_v decode_i))) (set! decode_bottom (conj decode_bottom (nth first_v (+ decode_i decode_l)))) (set! decode_i (+ decode_i 1)))) (set! decode_decoded "") (set! decode_i 0) (while (< decode_i decode_l) (do (set! decode_r (nth decode_top decode_i)) (set! decode_c (nth decode_bottom decode_i)) (set! decode_decoded (str decode_decoded (numbers_to_letter decode_r decode_c))) (set! decode_i (+ decode_i 1)))) (throw (ex-info "return" {:v decode_decoded}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (encode "testmessage"))
      (println (encode "Test Message"))
      (println (encode "test j"))
      (println (encode "test i"))
      (println (decode "qtltbdxrxlk"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
