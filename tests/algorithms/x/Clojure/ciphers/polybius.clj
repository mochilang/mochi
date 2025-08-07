(ns main (:refer-clojure :exclude [letter_to_numbers numbers_to_letter char_to_int encode decode]))

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

(declare letter_to_numbers numbers_to_letter char_to_int encode decode)

(def ^:dynamic decode_decoded nil)

(def ^:dynamic decode_i nil)

(def ^:dynamic decode_index1 nil)

(def ^:dynamic decode_index2 nil)

(def ^:dynamic decode_letter nil)

(def ^:dynamic encode_ch nil)

(def ^:dynamic encode_encoded nil)

(def ^:dynamic encode_i nil)

(def ^:dynamic encode_message nil)

(def ^:dynamic encode_nums nil)

(def ^:dynamic letter_to_numbers_i nil)

(def ^:dynamic letter_to_numbers_j nil)

(def ^:dynamic main_square [["a" "b" "c" "d" "e"] ["f" "g" "h" "i" "k"] ["l" "m" "n" "o" "p"] ["q" "r" "s" "t" "u"] ["v" "w" "x" "y" "z"]])

(defn letter_to_numbers [letter_to_numbers_letter]
  (binding [letter_to_numbers_i nil letter_to_numbers_j nil] (try (do (set! letter_to_numbers_i 0) (while (< letter_to_numbers_i (count main_square)) (do (set! letter_to_numbers_j 0) (while (< letter_to_numbers_j (count (nth main_square letter_to_numbers_i))) (do (when (= (nth (nth main_square letter_to_numbers_i) letter_to_numbers_j) letter_to_numbers_letter) (throw (ex-info "return" {:v [(+ letter_to_numbers_i 1) (+ letter_to_numbers_j 1)]}))) (set! letter_to_numbers_j (+ letter_to_numbers_j 1)))) (set! letter_to_numbers_i (+ letter_to_numbers_i 1)))) (throw (ex-info "return" {:v [0 0]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn numbers_to_letter [numbers_to_letter_index1 numbers_to_letter_index2]
  (try (throw (ex-info "return" {:v (nth (nth main_square (- numbers_to_letter_index1 1)) (- numbers_to_letter_index2 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn char_to_int [char_to_int_ch]
  (try (do (when (= char_to_int_ch "1") (throw (ex-info "return" {:v 1}))) (when (= char_to_int_ch "2") (throw (ex-info "return" {:v 2}))) (when (= char_to_int_ch "3") (throw (ex-info "return" {:v 3}))) (when (= char_to_int_ch "4") (throw (ex-info "return" {:v 4}))) (if (= char_to_int_ch "5") 5 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn encode [encode_message_p]
  (binding [encode_ch nil encode_encoded nil encode_i nil encode_message nil encode_nums nil] (try (do (set! encode_message encode_message_p) (set! encode_message (clojure.string/lower-case encode_message)) (set! encode_encoded "") (set! encode_i 0) (while (< encode_i (count encode_message)) (do (set! encode_ch (nth encode_message encode_i)) (when (= encode_ch "j") (set! encode_ch "i")) (if (not= encode_ch " ") (do (set! encode_nums (letter_to_numbers encode_ch)) (set! encode_encoded (str (str encode_encoded (str (nth encode_nums 0))) (str (nth encode_nums 1))))) (set! encode_encoded (str encode_encoded " "))) (set! encode_i (+ encode_i 1)))) (throw (ex-info "return" {:v encode_encoded}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decode [decode_message]
  (binding [decode_decoded nil decode_i nil decode_index1 nil decode_index2 nil decode_letter nil] (try (do (set! decode_decoded "") (set! decode_i 0) (while (< decode_i (count decode_message)) (if (= (nth decode_message decode_i) " ") (do (set! decode_decoded (str decode_decoded " ")) (set! decode_i (+ decode_i 1))) (do (set! decode_index1 (char_to_int (nth decode_message decode_i))) (set! decode_index2 (char_to_int (nth decode_message (+ decode_i 1)))) (set! decode_letter (numbers_to_letter decode_index1 decode_index2)) (set! decode_decoded (str decode_decoded decode_letter)) (set! decode_i (+ decode_i 2))))) (throw (ex-info "return" {:v decode_decoded}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (encode "test message"))
      (println (encode "Test Message"))
      (println (decode "44154344 32154343112215"))
      (println (decode "4415434432154343112215"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
