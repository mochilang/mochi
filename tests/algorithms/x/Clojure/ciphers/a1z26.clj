(ns main (:refer-clojure :exclude [indexOf charToNum numToChar encode decode main]))

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

(declare indexOf charToNum numToChar encode decode main)

(def ^:dynamic charToNum_idx nil)

(def ^:dynamic charToNum_letters nil)

(def ^:dynamic decode_out nil)

(def ^:dynamic encode_ch nil)

(def ^:dynamic encode_i nil)

(def ^:dynamic encode_res nil)

(def ^:dynamic encode_val nil)

(def ^:dynamic indexOf_i nil)

(def ^:dynamic main_enc nil)

(def ^:dynamic main_text nil)

(def ^:dynamic numToChar_letters nil)

(defn indexOf [indexOf_s indexOf_ch]
  (binding [indexOf_i nil] (try (do (set! indexOf_i 0) (while (< indexOf_i (count indexOf_s)) (do (when (= (subs indexOf_s indexOf_i (min (+ indexOf_i 1) (count indexOf_s))) indexOf_ch) (throw (ex-info "return" {:v indexOf_i}))) (set! indexOf_i (+ indexOf_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn charToNum [charToNum_ch]
  (binding [charToNum_idx nil charToNum_letters nil] (try (do (set! charToNum_letters "abcdefghijklmnopqrstuvwxyz") (set! charToNum_idx (indexOf charToNum_letters charToNum_ch)) (if (>= charToNum_idx 0) (+ charToNum_idx 1) 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn numToChar [numToChar_n]
  (binding [numToChar_letters nil] (try (do (set! numToChar_letters "abcdefghijklmnopqrstuvwxyz") (if (and (>= numToChar_n 1) (<= numToChar_n 26)) (subs numToChar_letters (- numToChar_n 1) (min numToChar_n (count numToChar_letters))) "?")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn encode [encode_plain]
  (binding [encode_ch nil encode_i nil encode_res nil encode_val nil] (try (do (set! encode_res []) (set! encode_i 0) (while (< encode_i (count encode_plain)) (do (set! encode_ch (clojure.string/lower-case (subs encode_plain encode_i (min (+ encode_i 1) (count encode_plain))))) (set! encode_val (charToNum encode_ch)) (when (> encode_val 0) (set! encode_res (conj encode_res encode_val))) (set! encode_i (+ encode_i 1)))) (throw (ex-info "return" {:v encode_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decode [decode_encoded]
  (binding [decode_out nil] (try (do (set! decode_out "") (doseq [n decode_encoded] (set! decode_out (str decode_out (numToChar n)))) (throw (ex-info "return" {:v decode_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_enc nil main_text nil] (do (println "-> ") (set! main_text (clojure.string/lower-case (read-line))) (set! main_enc (encode main_text)) (println (str "Encoded: " (str main_enc))) (println (str "Decoded: " (decode main_enc))))))

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
