(ns main (:refer-clojure :exclude [index_of generate_key cipher_text original_text]))

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

(declare index_of generate_key cipher_text original_text)

(def ^:dynamic cipher_text_ch nil)

(def ^:dynamic cipher_text_i nil)

(def ^:dynamic cipher_text_res nil)

(def ^:dynamic cipher_text_x nil)

(def ^:dynamic generate_key_i nil)

(def ^:dynamic generate_key_key_new nil)

(def ^:dynamic original_text_ch nil)

(def ^:dynamic original_text_i nil)

(def ^:dynamic original_text_res nil)

(def ^:dynamic original_text_x nil)

(def ^:dynamic main_ALPHABET "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn index_of [index_of_ch]
  (try (do (dotimes [i (count main_ALPHABET)] (when (= (nth main_ALPHABET i) index_of_ch) (throw (ex-info "return" {:v i})))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn generate_key [generate_key_message generate_key_key]
  (binding [generate_key_i nil generate_key_key_new nil] (try (do (set! generate_key_key_new generate_key_key) (set! generate_key_i 0) (while (< (count generate_key_key_new) (count generate_key_message)) (do (set! generate_key_key_new (str generate_key_key_new (nth generate_key_key generate_key_i))) (set! generate_key_i (+ generate_key_i 1)) (when (= generate_key_i (count generate_key_key)) (set! generate_key_i 0)))) (throw (ex-info "return" {:v generate_key_key_new}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cipher_text [cipher_text_message cipher_text_key_new]
  (binding [cipher_text_ch nil cipher_text_i nil cipher_text_res nil cipher_text_x nil] (try (do (set! cipher_text_res "") (set! cipher_text_i 0) (dotimes [idx (count cipher_text_message)] (do (set! cipher_text_ch (nth cipher_text_message idx)) (if (= cipher_text_ch " ") (set! cipher_text_res (str cipher_text_res " ")) (do (set! cipher_text_x (mod (+ (- (index_of cipher_text_ch) (index_of (nth cipher_text_key_new cipher_text_i))) 26) 26)) (set! cipher_text_i (+ cipher_text_i 1)) (set! cipher_text_res (str cipher_text_res (nth main_ALPHABET cipher_text_x))))))) (throw (ex-info "return" {:v cipher_text_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn original_text [original_text_cipher original_text_key_new]
  (binding [original_text_ch nil original_text_i nil original_text_res nil original_text_x nil] (try (do (set! original_text_res "") (set! original_text_i 0) (dotimes [idx (count original_text_cipher)] (do (set! original_text_ch (nth original_text_cipher idx)) (if (= original_text_ch " ") (set! original_text_res (str original_text_res " ")) (do (set! original_text_x (mod (+ (+ (index_of original_text_ch) (index_of (nth original_text_key_new original_text_i))) 26) 26)) (set! original_text_i (+ original_text_i 1)) (set! original_text_res (str original_text_res (nth main_ALPHABET original_text_x))))))) (throw (ex-info "return" {:v original_text_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_message "THE GERMAN ATTACK")

(def ^:dynamic main_key "SECRET")

(def ^:dynamic main_key_new (generate_key main_message main_key))

(def ^:dynamic main_encrypted (cipher_text main_message main_key_new))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "Encrypted Text = " main_encrypted))
      (println (str "Original Text = " (original_text main_encrypted main_key_new)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
