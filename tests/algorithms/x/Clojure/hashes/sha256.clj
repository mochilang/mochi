(ns main (:refer-clojure :exclude [byte_to_hex bytes_to_hex]))

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

(declare byte_to_hex bytes_to_hex)

(def ^:dynamic byte_to_hex_hi nil)

(def ^:dynamic byte_to_hex_lo nil)

(def ^:dynamic bytes_to_hex_i nil)

(def ^:dynamic bytes_to_hex_res nil)

(def ^:dynamic main_HEX "0123456789abcdef")

(defn byte_to_hex [byte_to_hex_b]
  (binding [byte_to_hex_hi nil byte_to_hex_lo nil] (try (do (set! byte_to_hex_hi (quot byte_to_hex_b 16)) (set! byte_to_hex_lo (mod byte_to_hex_b 16)) (throw (ex-info "return" {:v (+ (nth main_HEX byte_to_hex_hi) (nth main_HEX byte_to_hex_lo))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bytes_to_hex [bytes_to_hex_bs]
  (binding [bytes_to_hex_i nil bytes_to_hex_res nil] (try (do (set! bytes_to_hex_res "") (set! bytes_to_hex_i 0) (while (< bytes_to_hex_i (count bytes_to_hex_bs)) (do (set! bytes_to_hex_res (str bytes_to_hex_res (byte_to_hex (nth bytes_to_hex_bs bytes_to_hex_i)))) (set! bytes_to_hex_i (+ bytes_to_hex_i 1)))) (throw (ex-info "return" {:v bytes_to_hex_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (bytes_to_hex (sha256 "Python")))
      (println (bytes_to_hex (sha256 "hello world")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
