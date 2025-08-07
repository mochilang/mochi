(ns main (:refer-clojure :exclude [encrypt_message decrypt_message]))

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

(declare encrypt_message decrypt_message)

(def ^:dynamic decrypt_message_ch nil)

(def ^:dynamic decrypt_message_col nil)

(def ^:dynamic decrypt_message_i nil)

(def ^:dynamic decrypt_message_idx nil)

(def ^:dynamic decrypt_message_msg_len nil)

(def ^:dynamic decrypt_message_num_cols nil)

(def ^:dynamic decrypt_message_num_rows nil)

(def ^:dynamic decrypt_message_num_shaded_boxes nil)

(def ^:dynamic decrypt_message_plain nil)

(def ^:dynamic decrypt_message_result nil)

(def ^:dynamic decrypt_message_row nil)

(def ^:dynamic encrypt_message_col nil)

(def ^:dynamic encrypt_message_pointer nil)

(def ^:dynamic encrypt_message_result nil)

(defn encrypt_message [encrypt_message_key encrypt_message_message]
  (binding [encrypt_message_col nil encrypt_message_pointer nil encrypt_message_result nil] (try (do (set! encrypt_message_result "") (set! encrypt_message_col 0) (while (< encrypt_message_col encrypt_message_key) (do (set! encrypt_message_pointer encrypt_message_col) (while (< encrypt_message_pointer (count encrypt_message_message)) (do (set! encrypt_message_result (str encrypt_message_result (nth encrypt_message_message encrypt_message_pointer))) (set! encrypt_message_pointer (+ encrypt_message_pointer encrypt_message_key)))) (set! encrypt_message_col (+ encrypt_message_col 1)))) (throw (ex-info "return" {:v encrypt_message_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt_message [decrypt_message_key decrypt_message_message]
  (binding [decrypt_message_ch nil decrypt_message_col nil decrypt_message_i nil decrypt_message_idx nil decrypt_message_msg_len nil decrypt_message_num_cols nil decrypt_message_num_rows nil decrypt_message_num_shaded_boxes nil decrypt_message_plain nil decrypt_message_result nil decrypt_message_row nil] (try (do (set! decrypt_message_msg_len (count decrypt_message_message)) (set! decrypt_message_num_cols (/ decrypt_message_msg_len decrypt_message_key)) (when (not= (mod decrypt_message_msg_len decrypt_message_key) 0) (set! decrypt_message_num_cols (+ decrypt_message_num_cols 1))) (set! decrypt_message_num_rows decrypt_message_key) (set! decrypt_message_num_shaded_boxes (- (* decrypt_message_num_cols decrypt_message_num_rows) decrypt_message_msg_len)) (set! decrypt_message_plain []) (set! decrypt_message_i 0) (while (< decrypt_message_i decrypt_message_num_cols) (do (set! decrypt_message_plain (conj decrypt_message_plain "")) (set! decrypt_message_i (+ decrypt_message_i 1)))) (set! decrypt_message_col 0) (set! decrypt_message_row 0) (set! decrypt_message_idx 0) (while (< decrypt_message_idx decrypt_message_msg_len) (do (set! decrypt_message_ch (nth decrypt_message_message decrypt_message_idx)) (set! decrypt_message_plain (assoc decrypt_message_plain decrypt_message_col (+ (nth decrypt_message_plain decrypt_message_col) decrypt_message_ch))) (set! decrypt_message_col (+ decrypt_message_col 1)) (when (or (= decrypt_message_col decrypt_message_num_cols) (and (= decrypt_message_col (- decrypt_message_num_cols 1)) (>= decrypt_message_row (- decrypt_message_num_rows decrypt_message_num_shaded_boxes)))) (do (set! decrypt_message_col 0) (set! decrypt_message_row (+ decrypt_message_row 1)))) (set! decrypt_message_idx (+ decrypt_message_idx 1)))) (set! decrypt_message_result "") (set! decrypt_message_i 0) (while (< decrypt_message_i decrypt_message_num_cols) (do (set! decrypt_message_result (str decrypt_message_result (nth decrypt_message_plain decrypt_message_i))) (set! decrypt_message_i (+ decrypt_message_i 1)))) (throw (ex-info "return" {:v decrypt_message_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_key 6)

(def ^:dynamic main_message "Harshil Darji")

(def ^:dynamic main_encrypted (encrypt_message main_key main_message))

(def ^:dynamic main_decrypted (decrypt_message main_key main_encrypted))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_encrypted)
      (println main_decrypted)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
