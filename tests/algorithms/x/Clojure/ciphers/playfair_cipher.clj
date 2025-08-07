(ns main (:refer-clojure :exclude [contains index_of prepare_input generate_table encode decode main]))

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

(declare contains index_of prepare_input generate_table encode decode main)

(def ^:dynamic contains_i nil)

(def ^:dynamic decode_c1 nil)

(def ^:dynamic decode_c2 nil)

(def ^:dynamic decode_col1 nil)

(def ^:dynamic decode_col2 nil)

(def ^:dynamic decode_i nil)

(def ^:dynamic decode_idx1 nil)

(def ^:dynamic decode_idx2 nil)

(def ^:dynamic decode_plain nil)

(def ^:dynamic decode_row1 nil)

(def ^:dynamic decode_row2 nil)

(def ^:dynamic decode_table nil)

(def ^:dynamic encode_c1 nil)

(def ^:dynamic encode_c2 nil)

(def ^:dynamic encode_cipher nil)

(def ^:dynamic encode_col1 nil)

(def ^:dynamic encode_col2 nil)

(def ^:dynamic encode_i nil)

(def ^:dynamic encode_idx1 nil)

(def ^:dynamic encode_idx2 nil)

(def ^:dynamic encode_row1 nil)

(def ^:dynamic encode_row2 nil)

(def ^:dynamic encode_table nil)

(def ^:dynamic encode_text nil)

(def ^:dynamic generate_table_alphabet nil)

(def ^:dynamic generate_table_c nil)

(def ^:dynamic generate_table_i nil)

(def ^:dynamic generate_table_table nil)

(def ^:dynamic generate_table_upper_key nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic prepare_input_c nil)

(def ^:dynamic prepare_input_c1 nil)

(def ^:dynamic prepare_input_c2 nil)

(def ^:dynamic prepare_input_clean nil)

(def ^:dynamic prepare_input_filtered nil)

(def ^:dynamic prepare_input_i nil)

(def ^:dynamic prepare_input_letters nil)

(def ^:dynamic prepare_input_upper_dirty nil)

(defn contains [contains_xs contains_x]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count contains_xs)) (do (when (= (nth contains_xs contains_i) contains_x) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn index_of [index_of_xs index_of_x]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_xs)) (do (when (= (nth index_of_xs index_of_i) index_of_x) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn prepare_input [prepare_input_dirty]
  (binding [prepare_input_c nil prepare_input_c1 nil prepare_input_c2 nil prepare_input_clean nil prepare_input_filtered nil prepare_input_i nil prepare_input_letters nil prepare_input_upper_dirty nil] (try (do (set! prepare_input_letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! prepare_input_upper_dirty (clojure.string/upper-case prepare_input_dirty)) (set! prepare_input_filtered "") (set! prepare_input_i 0) (while (< prepare_input_i (count prepare_input_upper_dirty)) (do (set! prepare_input_c (subs prepare_input_upper_dirty prepare_input_i (min (+ prepare_input_i 1) (count prepare_input_upper_dirty)))) (when (in prepare_input_c prepare_input_letters) (set! prepare_input_filtered (str prepare_input_filtered prepare_input_c))) (set! prepare_input_i (+ prepare_input_i 1)))) (when (< (count prepare_input_filtered) 2) (throw (ex-info "return" {:v prepare_input_filtered}))) (set! prepare_input_clean "") (set! prepare_input_i 0) (while (< prepare_input_i (- (count prepare_input_filtered) 1)) (do (set! prepare_input_c1 (subs prepare_input_filtered prepare_input_i (min (+ prepare_input_i 1) (count prepare_input_filtered)))) (set! prepare_input_c2 (subs prepare_input_filtered (+ prepare_input_i 1) (min (+ prepare_input_i 2) (count prepare_input_filtered)))) (set! prepare_input_clean (str prepare_input_clean prepare_input_c1)) (when (= prepare_input_c1 prepare_input_c2) (set! prepare_input_clean (str prepare_input_clean "X"))) (set! prepare_input_i (+ prepare_input_i 1)))) (set! prepare_input_clean (str prepare_input_clean (subs prepare_input_filtered (- (count prepare_input_filtered) 1) (min (count prepare_input_filtered) (count prepare_input_filtered))))) (when (= (mod (count prepare_input_clean) 2) 1) (set! prepare_input_clean (str prepare_input_clean "X"))) (throw (ex-info "return" {:v prepare_input_clean}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_table [generate_table_key]
  (binding [generate_table_alphabet nil generate_table_c nil generate_table_i nil generate_table_table nil generate_table_upper_key nil] (try (do (set! generate_table_alphabet "ABCDEFGHIKLMNOPQRSTUVWXYZ") (set! generate_table_table []) (set! generate_table_upper_key (clojure.string/upper-case generate_table_key)) (set! generate_table_i 0) (while (< generate_table_i (count generate_table_upper_key)) (do (set! generate_table_c (subs generate_table_upper_key generate_table_i (min (+ generate_table_i 1) (count generate_table_upper_key)))) (when (in generate_table_c generate_table_alphabet) (when (not (contains generate_table_table generate_table_c)) (set! generate_table_table (conj generate_table_table generate_table_c)))) (set! generate_table_i (+ generate_table_i 1)))) (set! generate_table_i 0) (while (< generate_table_i (count generate_table_alphabet)) (do (set! generate_table_c (subs generate_table_alphabet generate_table_i (min (+ generate_table_i 1) (count generate_table_alphabet)))) (when (not (contains generate_table_table generate_table_c)) (set! generate_table_table (conj generate_table_table generate_table_c))) (set! generate_table_i (+ generate_table_i 1)))) (throw (ex-info "return" {:v generate_table_table}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encode [encode_plaintext encode_key]
  (binding [encode_c1 nil encode_c2 nil encode_cipher nil encode_col1 nil encode_col2 nil encode_i nil encode_idx1 nil encode_idx2 nil encode_row1 nil encode_row2 nil encode_table nil encode_text nil] (try (do (set! encode_table (generate_table encode_key)) (set! encode_text (prepare_input encode_plaintext)) (set! encode_cipher "") (set! encode_i 0) (while (< encode_i (count encode_text)) (do (set! encode_c1 (subs encode_text encode_i (min (+ encode_i 1) (count encode_text)))) (set! encode_c2 (subs encode_text (+ encode_i 1) (min (+ encode_i 2) (count encode_text)))) (set! encode_idx1 (index_of encode_table encode_c1)) (set! encode_idx2 (index_of encode_table encode_c2)) (set! encode_row1 (quot encode_idx1 5)) (set! encode_col1 (mod encode_idx1 5)) (set! encode_row2 (quot encode_idx2 5)) (set! encode_col2 (mod encode_idx2 5)) (if (= encode_row1 encode_row2) (do (set! encode_cipher (str encode_cipher (nth encode_table (+ (* encode_row1 5) (mod (+ encode_col1 1) 5))))) (set! encode_cipher (str encode_cipher (nth encode_table (+ (* encode_row2 5) (mod (+ encode_col2 1) 5)))))) (if (= encode_col1 encode_col2) (do (set! encode_cipher (str encode_cipher (nth encode_table (+ (* (mod (+ encode_row1 1) 5) 5) encode_col1)))) (set! encode_cipher (str encode_cipher (nth encode_table (+ (* (mod (+ encode_row2 1) 5) 5) encode_col2))))) (do (set! encode_cipher (str encode_cipher (nth encode_table (+ (* encode_row1 5) encode_col2)))) (set! encode_cipher (str encode_cipher (nth encode_table (+ (* encode_row2 5) encode_col1))))))) (set! encode_i (+ encode_i 2)))) (throw (ex-info "return" {:v encode_cipher}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decode [decode_cipher decode_key]
  (binding [decode_c1 nil decode_c2 nil decode_col1 nil decode_col2 nil decode_i nil decode_idx1 nil decode_idx2 nil decode_plain nil decode_row1 nil decode_row2 nil decode_table nil] (try (do (set! decode_table (generate_table decode_key)) (set! decode_plain "") (set! decode_i 0) (while (< decode_i (count decode_cipher)) (do (set! decode_c1 (subs decode_cipher decode_i (min (+ decode_i 1) (count decode_cipher)))) (set! decode_c2 (subs decode_cipher (+ decode_i 1) (min (+ decode_i 2) (count decode_cipher)))) (set! decode_idx1 (index_of decode_table decode_c1)) (set! decode_idx2 (index_of decode_table decode_c2)) (set! decode_row1 (quot decode_idx1 5)) (set! decode_col1 (mod decode_idx1 5)) (set! decode_row2 (quot decode_idx2 5)) (set! decode_col2 (mod decode_idx2 5)) (if (= decode_row1 decode_row2) (do (set! decode_plain (str decode_plain (nth decode_table (+ (* decode_row1 5) (mod (+ decode_col1 4) 5))))) (set! decode_plain (str decode_plain (nth decode_table (+ (* decode_row2 5) (mod (+ decode_col2 4) 5)))))) (if (= decode_col1 decode_col2) (do (set! decode_plain (str decode_plain (nth decode_table (+ (* (mod (+ decode_row1 4) 5) 5) decode_col1)))) (set! decode_plain (str decode_plain (nth decode_table (+ (* (mod (+ decode_row2 4) 5) 5) decode_col2))))) (do (set! decode_plain (str decode_plain (nth decode_table (+ (* decode_row1 5) decode_col2)))) (set! decode_plain (str decode_plain (nth decode_table (+ (* decode_row2 5) decode_col1))))))) (set! decode_i (+ decode_i 2)))) (throw (ex-info "return" {:v decode_plain}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println "Encoded:" (encode "BYE AND THANKS" "GREETING")) (println "Decoded:" (decode "CXRBANRLBALQ" "GREETING"))))

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
