(ns main (:refer-clojure :exclude [ord fletcher16]))

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

(declare ord fletcher16)

(def ^:dynamic fletcher16_code nil)

(def ^:dynamic fletcher16_i nil)

(def ^:dynamic fletcher16_sum1 nil)

(def ^:dynamic fletcher16_sum2 nil)

(def ^:dynamic ord_i nil)

(def ^:dynamic main_ascii_chars " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defn ord [ord_ch]
  (binding [ord_i nil] (try (do (set! ord_i 0) (while (< ord_i (count main_ascii_chars)) (do (when (= (nth main_ascii_chars ord_i) ord_ch) (throw (ex-info "return" {:v (+ 32 ord_i)}))) (set! ord_i (+ ord_i 1)))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fletcher16 [fletcher16_text]
  (binding [fletcher16_code nil fletcher16_i nil fletcher16_sum1 nil fletcher16_sum2 nil] (try (do (set! fletcher16_sum1 0) (set! fletcher16_sum2 0) (set! fletcher16_i 0) (while (< fletcher16_i (count fletcher16_text)) (do (set! fletcher16_code (ord (nth fletcher16_text fletcher16_i))) (set! fletcher16_sum1 (mod (+ fletcher16_sum1 fletcher16_code) 255)) (set! fletcher16_sum2 (mod (+ fletcher16_sum1 fletcher16_sum2) 255)) (set! fletcher16_i (+ fletcher16_i 1)))) (throw (ex-info "return" {:v (+ (* fletcher16_sum2 256) fletcher16_sum1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
