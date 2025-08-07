(ns main (:refer-clojure :exclude [set_seed randint ord chr encrypt decrypt]))

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

(declare set_seed randint ord chr encrypt decrypt)

(def ^:dynamic decrypt_i nil)

(def ^:dynamic decrypt_p nil)

(def ^:dynamic decrypt_plain nil)

(def ^:dynamic encrypt_c nil)

(def ^:dynamic encrypt_cipher nil)

(def ^:dynamic encrypt_i nil)

(def ^:dynamic encrypt_k nil)

(def ^:dynamic encrypt_key nil)

(def ^:dynamic encrypt_p nil)

(def ^:dynamic encrypt_res nil)

(def ^:dynamic ord_i nil)

(def ^:dynamic main_seed 1)

(defn set_seed [set_seed_s]
  (alter-var-root (var main_seed) (fn [_] set_seed_s)))

(defn randint [randint_a randint_b]
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v (+ (mod main_seed (+ (- randint_b randint_a) 1)) randint_a)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_ascii_chars " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defn ord [ord_ch]
  (binding [ord_i nil] (try (do (set! ord_i 0) (while (< ord_i (count main_ascii_chars)) (do (when (= (nth main_ascii_chars ord_i) ord_ch) (throw (ex-info "return" {:v (+ 32 ord_i)}))) (set! ord_i (+ ord_i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn chr [chr_code]
  (try (if (or (< chr_code 32) (> chr_code 126)) "" (nth main_ascii_chars (- chr_code 32))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn encrypt [encrypt_text]
  (binding [encrypt_c nil encrypt_cipher nil encrypt_i nil encrypt_k nil encrypt_key nil encrypt_p nil encrypt_res nil] (try (do (set! encrypt_cipher []) (set! encrypt_key []) (set! encrypt_i 0) (while (< encrypt_i (count encrypt_text)) (do (set! encrypt_p (ord (nth encrypt_text encrypt_i))) (set! encrypt_k (randint 1 300)) (set! encrypt_c (* (+ encrypt_p encrypt_k) encrypt_k)) (set! encrypt_cipher (conj encrypt_cipher encrypt_c)) (set! encrypt_key (conj encrypt_key encrypt_k)) (set! encrypt_i (+ encrypt_i 1)))) (set! encrypt_res {}) (set! encrypt_res (assoc encrypt_res "cipher" encrypt_cipher)) (set! encrypt_res (assoc encrypt_res "key" encrypt_key)) (throw (ex-info "return" {:v encrypt_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt [decrypt_cipher decrypt_key]
  (binding [decrypt_i nil decrypt_p nil decrypt_plain nil] (try (do (set! decrypt_plain "") (set! decrypt_i 0) (while (< decrypt_i (count decrypt_key)) (do (set! decrypt_p (/ (- (nth decrypt_cipher decrypt_i) (* (nth decrypt_key decrypt_i) (nth decrypt_key decrypt_i))) (nth decrypt_key decrypt_i))) (set! decrypt_plain (str decrypt_plain (chr decrypt_p))) (set! decrypt_i (+ decrypt_i 1)))) (throw (ex-info "return" {:v decrypt_plain}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_res (encrypt "Hello"))

(def ^:dynamic main_cipher (get main_res "cipher"))

(def ^:dynamic main_key (get main_res "key"))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (set_seed 1)
      (println main_cipher)
      (println main_key)
      (println (decrypt main_cipher main_key))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
