(ns main (:refer-clojure :exclude [index_of to_uppercase gronsfeld]))

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

(declare index_of to_uppercase gronsfeld)

(def ^:dynamic gronsfeld_ascii_len nil)

(def ^:dynamic gronsfeld_ch nil)

(def ^:dynamic gronsfeld_encrypted nil)

(def ^:dynamic gronsfeld_i nil)

(def ^:dynamic gronsfeld_idx nil)

(def ^:dynamic gronsfeld_key_idx nil)

(def ^:dynamic gronsfeld_key_len nil)

(def ^:dynamic gronsfeld_new_position nil)

(def ^:dynamic gronsfeld_shift nil)

(def ^:dynamic gronsfeld_upper_text nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic to_uppercase_ch nil)

(def ^:dynamic to_uppercase_i nil)

(def ^:dynamic to_uppercase_idx nil)

(def ^:dynamic to_uppercase_result nil)

(def ^:dynamic main_ASCII_UPPERCASE "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def ^:dynamic main_ASCII_LOWERCASE "abcdefghijklmnopqrstuvwxyz")

(def ^:dynamic main_NEG_ONE (- 0 1))

(defn index_of [index_of_s index_of_ch]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (subs index_of_s index_of_i (min (+ index_of_i 1) (count index_of_s))) index_of_ch) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v main_NEG_ONE}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_uppercase [to_uppercase_s]
  (binding [to_uppercase_ch nil to_uppercase_i nil to_uppercase_idx nil to_uppercase_result nil] (try (do (set! to_uppercase_result "") (set! to_uppercase_i 0) (while (< to_uppercase_i (count to_uppercase_s)) (do (set! to_uppercase_ch (subs to_uppercase_s to_uppercase_i (min (+ to_uppercase_i 1) (count to_uppercase_s)))) (set! to_uppercase_idx (index_of main_ASCII_LOWERCASE to_uppercase_ch)) (if (= to_uppercase_idx main_NEG_ONE) (set! to_uppercase_result (str to_uppercase_result to_uppercase_ch)) (set! to_uppercase_result (str to_uppercase_result (subs main_ASCII_UPPERCASE to_uppercase_idx (min (+ to_uppercase_idx 1) (count main_ASCII_UPPERCASE)))))) (set! to_uppercase_i (+ to_uppercase_i 1)))) (throw (ex-info "return" {:v to_uppercase_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gronsfeld [gronsfeld_text gronsfeld_key]
  (binding [gronsfeld_ascii_len nil gronsfeld_ch nil gronsfeld_encrypted nil gronsfeld_i nil gronsfeld_idx nil gronsfeld_key_idx nil gronsfeld_key_len nil gronsfeld_new_position nil gronsfeld_shift nil gronsfeld_upper_text nil] (try (do (set! gronsfeld_ascii_len (count main_ASCII_UPPERCASE)) (set! gronsfeld_key_len (count gronsfeld_key)) (when (= gronsfeld_key_len 0) (throw (Exception. "integer modulo by zero"))) (set! gronsfeld_upper_text (to_uppercase gronsfeld_text)) (set! gronsfeld_encrypted "") (set! gronsfeld_i 0) (while (< gronsfeld_i (count gronsfeld_upper_text)) (do (set! gronsfeld_ch (subs gronsfeld_upper_text gronsfeld_i (min (+ gronsfeld_i 1) (count gronsfeld_upper_text)))) (set! gronsfeld_idx (index_of main_ASCII_UPPERCASE gronsfeld_ch)) (if (= gronsfeld_idx main_NEG_ONE) (set! gronsfeld_encrypted (str gronsfeld_encrypted gronsfeld_ch)) (do (set! gronsfeld_key_idx (mod gronsfeld_i gronsfeld_key_len)) (set! gronsfeld_shift (Integer/parseInt (subs gronsfeld_key gronsfeld_key_idx (min (+ gronsfeld_key_idx 1) (count gronsfeld_key))))) (set! gronsfeld_new_position (mod (+ gronsfeld_idx gronsfeld_shift) gronsfeld_ascii_len)) (set! gronsfeld_encrypted (str gronsfeld_encrypted (subs main_ASCII_UPPERCASE gronsfeld_new_position (min (+ gronsfeld_new_position 1) (count main_ASCII_UPPERCASE))))))) (set! gronsfeld_i (+ gronsfeld_i 1)))) (throw (ex-info "return" {:v gronsfeld_encrypted}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (gronsfeld "hello" "412"))
      (println (gronsfeld "hello" "123"))
      (println (gronsfeld "" "123"))
      (println (gronsfeld "yes, ¥€$ - _!@#%?" "0"))
      (println (gronsfeld "yes, ¥€$ - _!@#%?" "01"))
      (println (gronsfeld "yes, ¥€$ - _!@#%?" "012"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
