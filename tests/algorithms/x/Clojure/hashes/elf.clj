(ns main (:refer-clojure :exclude [ord bit_and bit_xor bit_not32 elf_hash]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare ord bit_and bit_xor bit_not32 elf_hash)

(def ^:dynamic bit_and_bit nil)

(def ^:dynamic bit_and_res nil)

(def ^:dynamic bit_and_ua nil)

(def ^:dynamic bit_and_ub nil)

(def ^:dynamic bit_not32_bit nil)

(def ^:dynamic bit_not32_res nil)

(def ^:dynamic bit_not32_ux nil)

(def ^:dynamic bit_xor_abit nil)

(def ^:dynamic bit_xor_bbit nil)

(def ^:dynamic bit_xor_bit nil)

(def ^:dynamic bit_xor_res nil)

(def ^:dynamic bit_xor_ua nil)

(def ^:dynamic bit_xor_ub nil)

(def ^:dynamic count_v nil)

(def ^:dynamic elf_hash_c nil)

(def ^:dynamic elf_hash_hash_ nil)

(def ^:dynamic elf_hash_i nil)

(def ^:dynamic elf_hash_x nil)

(def ^:dynamic ord_i nil)

(def ^:dynamic main_ascii " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defn ord [ord_ch]
  (binding [ord_i nil] (try (do (set! ord_i 0) (while (< ord_i (count main_ascii)) (do (when (= (subs main_ascii ord_i (min (+ ord_i 1) (count main_ascii))) ord_ch) (throw (ex-info "return" {:v (+ 32 ord_i)}))) (set! ord_i (+ ord_i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bit_and [bit_and_a bit_and_b]
  (binding [bit_and_bit nil bit_and_res nil bit_and_ua nil bit_and_ub nil] (try (do (set! bit_and_ua bit_and_a) (set! bit_and_ub bit_and_b) (set! bit_and_res 0) (set! bit_and_bit 1) (while (or (> bit_and_ua 0) (> bit_and_ub 0)) (do (when (and (= (mod bit_and_ua 2) 1) (= (mod bit_and_ub 2) 1)) (set! bit_and_res (+ bit_and_res bit_and_bit))) (set! bit_and_ua (long (quot bit_and_ua 2))) (set! bit_and_ub (long (quot bit_and_ub 2))) (set! bit_and_bit (* bit_and_bit 2)))) (throw (ex-info "return" {:v bit_and_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bit_xor [bit_xor_a bit_xor_b]
  (binding [bit_xor_abit nil bit_xor_bbit nil bit_xor_bit nil bit_xor_res nil bit_xor_ua nil bit_xor_ub nil] (try (do (set! bit_xor_ua bit_xor_a) (set! bit_xor_ub bit_xor_b) (set! bit_xor_res 0) (set! bit_xor_bit 1) (while (or (> bit_xor_ua 0) (> bit_xor_ub 0)) (do (set! bit_xor_abit (mod bit_xor_ua 2)) (set! bit_xor_bbit (mod bit_xor_ub 2)) (when (not= bit_xor_abit bit_xor_bbit) (set! bit_xor_res (+ bit_xor_res bit_xor_bit))) (set! bit_xor_ua (long (quot bit_xor_ua 2))) (set! bit_xor_ub (long (quot bit_xor_ub 2))) (set! bit_xor_bit (* bit_xor_bit 2)))) (throw (ex-info "return" {:v bit_xor_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bit_not32 [bit_not32_x]
  (binding [bit_not32_bit nil bit_not32_res nil bit_not32_ux nil count_v nil] (try (do (set! bit_not32_ux bit_not32_x) (set! bit_not32_res 0) (set! bit_not32_bit 1) (set! count_v 0) (while (< count_v 32) (do (when (= (mod bit_not32_ux 2) 0) (set! bit_not32_res (+ bit_not32_res bit_not32_bit))) (set! bit_not32_ux (long (quot bit_not32_ux 2))) (set! bit_not32_bit (* bit_not32_bit 2)) (set! count_v (+ count_v 1)))) (throw (ex-info "return" {:v bit_not32_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn elf_hash [elf_hash_data]
  (binding [elf_hash_c nil elf_hash_hash_ nil elf_hash_i nil elf_hash_x nil] (try (do (set! elf_hash_hash_ 0) (set! elf_hash_i 0) (while (< elf_hash_i (count elf_hash_data)) (do (set! elf_hash_c (ord (subs elf_hash_data elf_hash_i (min (+ elf_hash_i 1) (count elf_hash_data))))) (set! elf_hash_hash_ (+ (* elf_hash_hash_ 16) elf_hash_c)) (set! elf_hash_x (bit_and elf_hash_hash_ 4026531840)) (when (not= elf_hash_x 0) (set! elf_hash_hash_ (bit_xor elf_hash_hash_ (long (quot elf_hash_x 16777216))))) (set! elf_hash_hash_ (bit_and elf_hash_hash_ (bit_not32 elf_hash_x))) (set! elf_hash_i (+ elf_hash_i 1)))) (throw (ex-info "return" {:v elf_hash_hash_}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (elf_hash "lorem ipsum")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
