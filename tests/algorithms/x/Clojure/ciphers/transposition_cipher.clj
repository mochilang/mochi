(ns main (:refer-clojure :exclude [join_strings encrypt_message decrypt_message main]))

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

(declare join_strings encrypt_message decrypt_message main)

(def ^:dynamic decrypt_message_col nil)

(def ^:dynamic decrypt_message_i nil)

(def ^:dynamic decrypt_message_index nil)

(def ^:dynamic decrypt_message_num_cols nil)

(def ^:dynamic decrypt_message_num_rows nil)

(def ^:dynamic decrypt_message_num_shaded_boxes nil)

(def ^:dynamic decrypt_message_plain_text nil)

(def ^:dynamic decrypt_message_row nil)

(def ^:dynamic encrypt_message_col nil)

(def ^:dynamic encrypt_message_pointer nil)

(def ^:dynamic encrypt_message_result nil)

(def ^:dynamic first_v nil)

(def ^:dynamic join_strings_i nil)

(def ^:dynamic join_strings_res nil)

(def ^:dynamic main_key nil)

(def ^:dynamic main_max_key nil)

(def ^:dynamic main_message nil)

(def ^:dynamic main_mode nil)

(def ^:dynamic main_text nil)

(defn join_strings [join_strings_xs]
  (binding [join_strings_i nil join_strings_res nil] (try (do (set! join_strings_res "") (set! join_strings_i 0) (while (< join_strings_i (count join_strings_xs)) (do (set! join_strings_res (str join_strings_res (nth join_strings_xs join_strings_i))) (set! join_strings_i (+ join_strings_i 1)))) (throw (ex-info "return" {:v join_strings_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encrypt_message [encrypt_message_key encrypt_message_message]
  (binding [encrypt_message_col nil encrypt_message_pointer nil encrypt_message_result nil] (try (do (set! encrypt_message_result "") (set! encrypt_message_col 0) (while (< encrypt_message_col encrypt_message_key) (do (set! encrypt_message_pointer encrypt_message_col) (while (< encrypt_message_pointer (count encrypt_message_message)) (do (set! encrypt_message_result (str encrypt_message_result (subs encrypt_message_message encrypt_message_pointer (min (+ encrypt_message_pointer 1) (count encrypt_message_message))))) (set! encrypt_message_pointer (+ encrypt_message_pointer encrypt_message_key)))) (set! encrypt_message_col (+ encrypt_message_col 1)))) (throw (ex-info "return" {:v encrypt_message_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt_message [decrypt_message_key decrypt_message_message]
  (binding [decrypt_message_col nil decrypt_message_i nil decrypt_message_index nil decrypt_message_num_cols nil decrypt_message_num_rows nil decrypt_message_num_shaded_boxes nil decrypt_message_plain_text nil decrypt_message_row nil] (try (do (set! decrypt_message_num_cols (/ (- (+ (count decrypt_message_message) decrypt_message_key) 1) decrypt_message_key)) (set! decrypt_message_num_rows decrypt_message_key) (set! decrypt_message_num_shaded_boxes (- (* decrypt_message_num_cols decrypt_message_num_rows) (count decrypt_message_message))) (set! decrypt_message_plain_text []) (set! decrypt_message_i 0) (while (< decrypt_message_i decrypt_message_num_cols) (do (set! decrypt_message_plain_text (conj decrypt_message_plain_text "")) (set! decrypt_message_i (+ decrypt_message_i 1)))) (set! decrypt_message_col 0) (set! decrypt_message_row 0) (set! decrypt_message_index 0) (while (< decrypt_message_index (count decrypt_message_message)) (do (set! decrypt_message_plain_text (assoc decrypt_message_plain_text decrypt_message_col (str (nth decrypt_message_plain_text decrypt_message_col) (subs decrypt_message_message decrypt_message_index (min (+ decrypt_message_index 1) (count decrypt_message_message)))))) (set! decrypt_message_col (+ decrypt_message_col 1)) (when (or (= decrypt_message_col decrypt_message_num_cols) (and (= decrypt_message_col (- decrypt_message_num_cols 1)) (>= decrypt_message_row (- decrypt_message_num_rows decrypt_message_num_shaded_boxes)))) (do (set! decrypt_message_col 0) (set! decrypt_message_row (+ decrypt_message_row 1)))) (set! decrypt_message_index (+ decrypt_message_index 1)))) (throw (ex-info "return" {:v (join_strings decrypt_message_plain_text)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [first_v nil main_key nil main_max_key nil main_message nil main_mode nil main_text nil] (do (println "Enter message: ") (set! main_message (read-line)) (set! main_max_key (- (count main_message) 1)) (println (str (str "Enter key [2-" (str main_max_key)) "]: ")) (set! main_key (Integer/parseInt (read-line))) (println "Encryption/Decryption [e/d]: ") (set! main_mode (read-line)) (set! main_text "") (set! first_v (subs main_mode 0 (min 1 (count main_mode)))) (if (or (= first_v "e") (= first_v "E")) (set! main_text (encrypt_message main_key main_message)) (when (or (= first_v "d") (= first_v "D")) (set! main_text (decrypt_message main_key main_message)))) (println (str (str "Output:\n" main_text) "|")))))

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
