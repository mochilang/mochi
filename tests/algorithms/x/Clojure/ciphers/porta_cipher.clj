(ns main (:refer-clojure :exclude [to_upper char_index rotate_right table_for generate_table str_index get_position get_opponent encrypt decrypt main]))

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

(declare to_upper char_index rotate_right table_for generate_table str_index get_position get_opponent encrypt decrypt main)

(def ^:dynamic char_index_i nil)

(def ^:dynamic count_v nil)

(def ^:dynamic decrypt_res nil)

(def ^:dynamic encrypt_ch nil)

(def ^:dynamic encrypt_cipher nil)

(def ^:dynamic encrypt_i nil)

(def ^:dynamic encrypt_table nil)

(def ^:dynamic encrypt_up_words nil)

(def ^:dynamic generate_table_ch nil)

(def ^:dynamic generate_table_i nil)

(def ^:dynamic generate_table_pair nil)

(def ^:dynamic generate_table_result nil)

(def ^:dynamic generate_table_up nil)

(def ^:dynamic get_opponent_col nil)

(def ^:dynamic get_opponent_pos nil)

(def ^:dynamic get_opponent_row nil)

(def ^:dynamic get_position_col nil)

(def ^:dynamic get_position_row nil)

(def ^:dynamic rotate_right_n nil)

(def ^:dynamic rotate_right_shift nil)

(def ^:dynamic str_index_i nil)

(def ^:dynamic table_for_idx nil)

(def ^:dynamic table_for_pair nil)

(def ^:dynamic table_for_row1 nil)

(def ^:dynamic table_for_shift nil)

(def ^:dynamic to_upper_ch nil)

(def ^:dynamic to_upper_i nil)

(def ^:dynamic to_upper_j nil)

(def ^:dynamic to_upper_replaced nil)

(def ^:dynamic to_upper_res nil)

(def ^:dynamic main_UPPER "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def ^:dynamic main_LOWER "abcdefghijklmnopqrstuvwxyz")

(def ^:dynamic main_BASE_TOP "ABCDEFGHIJKLM")

(def ^:dynamic main_BASE_BOTTOM "NOPQRSTUVWXYZ")

(defn to_upper [to_upper_s]
  (binding [to_upper_ch nil to_upper_i nil to_upper_j nil to_upper_replaced nil to_upper_res nil] (try (do (set! to_upper_res "") (set! to_upper_i 0) (while (< to_upper_i (count to_upper_s)) (do (set! to_upper_ch (subs to_upper_s to_upper_i (min (+ to_upper_i 1) (count to_upper_s)))) (set! to_upper_j 0) (set! to_upper_replaced false) (loop [while_flag_1 true] (when (and while_flag_1 (< to_upper_j (count main_LOWER))) (cond (= (subs main_LOWER to_upper_j (min (+ to_upper_j 1) (count main_LOWER))) to_upper_ch) (do (set! to_upper_res (str to_upper_res (subs main_UPPER to_upper_j (min (+ to_upper_j 1) (count main_UPPER))))) (set! to_upper_replaced true) (recur false)) :else (do (set! to_upper_j (+ to_upper_j 1)) (recur while_flag_1))))) (when (not to_upper_replaced) (set! to_upper_res (str to_upper_res to_upper_ch))) (set! to_upper_i (+ to_upper_i 1)))) (throw (ex-info "return" {:v to_upper_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn char_index [char_index_c]
  (binding [char_index_i nil] (try (do (set! char_index_i 0) (while (< char_index_i (count main_UPPER)) (do (when (= (subs main_UPPER char_index_i (min (+ char_index_i 1) (count main_UPPER))) char_index_c) (throw (ex-info "return" {:v char_index_i}))) (set! char_index_i (+ char_index_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rotate_right [rotate_right_s rotate_right_k]
  (binding [rotate_right_n nil rotate_right_shift nil] (try (do (set! rotate_right_n (count rotate_right_s)) (set! rotate_right_shift (mod rotate_right_k rotate_right_n)) (throw (ex-info "return" {:v (str (subs rotate_right_s (- rotate_right_n rotate_right_shift) (min rotate_right_n (count rotate_right_s))) (subs rotate_right_s 0 (min (- rotate_right_n rotate_right_shift) (count rotate_right_s))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn table_for [table_for_c]
  (binding [table_for_idx nil table_for_pair nil table_for_row1 nil table_for_shift nil] (try (do (set! table_for_idx (char_index table_for_c)) (set! table_for_shift (quot table_for_idx 2)) (set! table_for_row1 (rotate_right main_BASE_BOTTOM table_for_shift)) (set! table_for_pair [main_BASE_TOP table_for_row1]) (throw (ex-info "return" {:v table_for_pair}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_table [generate_table_key]
  (binding [generate_table_ch nil generate_table_i nil generate_table_pair nil generate_table_result nil generate_table_up nil] (try (do (set! generate_table_up (to_upper generate_table_key)) (set! generate_table_i 0) (set! generate_table_result []) (while (< generate_table_i (count generate_table_up)) (do (set! generate_table_ch (subs generate_table_up generate_table_i (min (+ generate_table_i 1) (count generate_table_up)))) (set! generate_table_pair (table_for generate_table_ch)) (set! generate_table_result (conj generate_table_result generate_table_pair)) (set! generate_table_i (+ generate_table_i 1)))) (throw (ex-info "return" {:v generate_table_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn str_index [str_index_s str_index_ch]
  (binding [str_index_i nil] (try (do (set! str_index_i 0) (while (< str_index_i (count str_index_s)) (do (when (= (subs str_index_s str_index_i (min (+ str_index_i 1) (count str_index_s))) str_index_ch) (throw (ex-info "return" {:v str_index_i}))) (set! str_index_i (+ str_index_i 1)))) (throw (ex-info "return" {:v (- 0 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_position [get_position_table get_position_ch]
  (binding [get_position_col nil get_position_row nil] (try (do (set! get_position_row 0) (when (= (str_index (nth get_position_table 0) get_position_ch) (- 0 1)) (set! get_position_row 1)) (set! get_position_col (str_index (nth get_position_table get_position_row) get_position_ch)) (throw (ex-info "return" {:v [get_position_row get_position_col]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_opponent [get_opponent_table get_opponent_ch]
  (binding [get_opponent_col nil get_opponent_pos nil get_opponent_row nil] (try (do (set! get_opponent_pos (get_position get_opponent_table get_opponent_ch)) (set! get_opponent_row (nth get_opponent_pos 0)) (set! get_opponent_col (nth get_opponent_pos 1)) (when (= get_opponent_col (- 0 1)) (throw (ex-info "return" {:v get_opponent_ch}))) (if (= get_opponent_row 1) (subs (nth get_opponent_table 0) get_opponent_col (min (+ get_opponent_col 1) (count (nth get_opponent_table 0)))) (subs (nth get_opponent_table 1) get_opponent_col (min (+ get_opponent_col 1) (count (nth get_opponent_table 1)))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encrypt [encrypt_key encrypt_words]
  (binding [count_v nil encrypt_ch nil encrypt_cipher nil encrypt_i nil encrypt_table nil encrypt_up_words nil] (try (do (set! encrypt_table (generate_table encrypt_key)) (set! encrypt_up_words (to_upper encrypt_words)) (set! encrypt_cipher "") (set! count_v 0) (set! encrypt_i 0) (while (< encrypt_i (count encrypt_up_words)) (do (set! encrypt_ch (subs encrypt_up_words encrypt_i (min (+ encrypt_i 1) (count encrypt_up_words)))) (set! encrypt_cipher (str encrypt_cipher (get_opponent (nth encrypt_table count_v) encrypt_ch))) (set! count_v (mod (+ count_v 1) (count encrypt_table))) (set! encrypt_i (+ encrypt_i 1)))) (throw (ex-info "return" {:v encrypt_cipher}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt [decrypt_key decrypt_words]
  (binding [decrypt_res nil] (try (do (set! decrypt_res (encrypt decrypt_key decrypt_words)) (throw (ex-info "return" {:v decrypt_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (encrypt "marvin" "jessica")) (println (decrypt "marvin" "QRACRWU"))))

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
