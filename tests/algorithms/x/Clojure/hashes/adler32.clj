(ns main (:refer-clojure :exclude [ord adler32 main]))

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

(declare ord adler32 main)

(def ^:dynamic adler32_a nil)

(def ^:dynamic adler32_b nil)

(def ^:dynamic adler32_code nil)

(def ^:dynamic adler32_i nil)

(def ^:dynamic ord_digits nil)

(def ^:dynamic ord_i nil)

(def ^:dynamic ord_lower nil)

(def ^:dynamic ord_upper nil)

(def ^:dynamic main_MOD_ADLER 65521)

(defn ord [ord_ch]
  (binding [ord_digits nil ord_i nil ord_lower nil ord_upper nil] (try (do (set! ord_lower "abcdefghijklmnopqrstuvwxyz") (set! ord_upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set! ord_digits "0123456789") (set! ord_i 0) (while (< ord_i (count ord_lower)) (do (when (= (nth ord_lower ord_i) ord_ch) (throw (ex-info "return" {:v (+ 97 ord_i)}))) (set! ord_i (+ ord_i 1)))) (set! ord_i 0) (while (< ord_i (count ord_upper)) (do (when (= (nth ord_upper ord_i) ord_ch) (throw (ex-info "return" {:v (+ 65 ord_i)}))) (set! ord_i (+ ord_i 1)))) (set! ord_i 0) (while (< ord_i (count ord_digits)) (do (when (= (nth ord_digits ord_i) ord_ch) (throw (ex-info "return" {:v (+ 48 ord_i)}))) (set! ord_i (+ ord_i 1)))) (if (= ord_ch " ") 32 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn adler32 [adler32_plain_text]
  (binding [adler32_a nil adler32_b nil adler32_code nil adler32_i nil] (try (do (set! adler32_a 1) (set! adler32_b 0) (set! adler32_i 0) (while (< adler32_i (count adler32_plain_text)) (do (set! adler32_code (ord (nth adler32_plain_text adler32_i))) (set! adler32_a (mod (+ adler32_a adler32_code) main_MOD_ADLER)) (set! adler32_b (mod (+ adler32_b adler32_a) main_MOD_ADLER)) (set! adler32_i (+ adler32_i 1)))) (throw (ex-info "return" {:v (+ (* adler32_b 65536) adler32_a)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (str (adler32 "Algorithms"))) (println (str (adler32 "go adler em all")))))

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
