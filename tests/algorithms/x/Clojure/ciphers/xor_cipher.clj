(ns main (:refer-clojure :exclude [xor ord chr normalize_key encrypt encrypt_string]))

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

(declare xor ord chr normalize_key encrypt encrypt_string)

(def ^:dynamic encrypt_c nil)

(def ^:dynamic encrypt_e nil)

(def ^:dynamic encrypt_i nil)

(def ^:dynamic encrypt_k nil)

(def ^:dynamic encrypt_result nil)

(def ^:dynamic encrypt_string_chars nil)

(def ^:dynamic encrypt_string_out nil)

(def ^:dynamic normalize_key_k nil)

(def ^:dynamic ord_i nil)

(def ^:dynamic xor_abit nil)

(def ^:dynamic xor_bbit nil)

(def ^:dynamic xor_bit nil)

(def ^:dynamic xor_res nil)

(def ^:dynamic xor_x nil)

(def ^:dynamic xor_y nil)

(defn xor [xor_a xor_b]
  (binding [xor_abit nil xor_bbit nil xor_bit nil xor_res nil xor_x nil xor_y nil] (try (do (set! xor_res 0) (set! xor_bit 1) (set! xor_x xor_a) (set! xor_y xor_b) (while (or (> xor_x 0) (> xor_y 0)) (do (set! xor_abit (mod xor_x 2)) (set! xor_bbit (mod xor_y 2)) (when (not= xor_abit xor_bbit) (set! xor_res (+ xor_res xor_bit))) (set! xor_x (quot xor_x 2)) (set! xor_y (quot xor_y 2)) (set! xor_bit (* xor_bit 2)))) (throw (ex-info "return" {:v xor_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_ascii " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defn ord [ord_ch]
  (binding [ord_i nil] (try (do (set! ord_i 0) (while (< ord_i (count main_ascii)) (do (when (= (subvec main_ascii ord_i (+ ord_i 1)) ord_ch) (throw (ex-info "return" {:v (+ 32 ord_i)}))) (set! ord_i (+ ord_i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn chr [chr_n]
  (try (if (and (>= chr_n 32) (< chr_n 127)) (subvec main_ascii (- chr_n 32) (- chr_n 31)) "") (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn normalize_key [normalize_key_key]
  (binding [normalize_key_k nil] (try (do (set! normalize_key_k normalize_key_key) (when (= normalize_key_k 0) (set! normalize_key_k 1)) (set! normalize_key_k (mod normalize_key_k 256)) (when (< normalize_key_k 0) (set! normalize_key_k (+ normalize_key_k 256))) (throw (ex-info "return" {:v normalize_key_k}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encrypt [encrypt_content encrypt_key]
  (binding [encrypt_c nil encrypt_e nil encrypt_i nil encrypt_k nil encrypt_result nil] (try (do (set! encrypt_k (normalize_key encrypt_key)) (set! encrypt_result []) (set! encrypt_i 0) (while (< encrypt_i (count encrypt_content)) (do (set! encrypt_c (ord (subs encrypt_content encrypt_i (min (+ encrypt_i 1) (count encrypt_content))))) (set! encrypt_e (xor encrypt_c encrypt_k)) (set! encrypt_result (conj encrypt_result (chr encrypt_e))) (set! encrypt_i (+ encrypt_i 1)))) (throw (ex-info "return" {:v encrypt_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encrypt_string [encrypt_string_content encrypt_string_key]
  (binding [encrypt_string_chars nil encrypt_string_out nil] (try (do (set! encrypt_string_chars (encrypt encrypt_string_content encrypt_string_key)) (set! encrypt_string_out "") (doseq [ch encrypt_string_chars] (set! encrypt_string_out (str encrypt_string_out ch))) (throw (ex-info "return" {:v encrypt_string_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_sample "hallo welt")

(def ^:dynamic main_enc (encrypt_string main_sample 1))

(def ^:dynamic main_dec (encrypt_string main_enc 1))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (encrypt main_sample 1)))
      (println main_enc)
      (println main_dec)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
