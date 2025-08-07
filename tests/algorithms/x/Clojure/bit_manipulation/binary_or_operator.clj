(ns main (:refer-clojure :exclude [binary_or]))

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

(declare binary_or)

(def ^:dynamic binary_or_bit_a nil)

(def ^:dynamic binary_or_bit_b nil)

(def ^:dynamic binary_or_res nil)

(def ^:dynamic binary_or_x nil)

(def ^:dynamic binary_or_y nil)

(defn binary_or [binary_or_a binary_or_b]
  (binding [binary_or_bit_a nil binary_or_bit_b nil binary_or_res nil binary_or_x nil binary_or_y nil] (try (do (when (or (< binary_or_a 0) (< binary_or_b 0)) (throw (ex-info "return" {:v "ValueError"}))) (set! binary_or_res "") (set! binary_or_x binary_or_a) (set! binary_or_y binary_or_b) (while (or (> binary_or_x 0) (> binary_or_y 0)) (do (set! binary_or_bit_a (mod binary_or_x 2)) (set! binary_or_bit_b (mod binary_or_y 2)) (if (or (= binary_or_bit_a 1) (= binary_or_bit_b 1)) (set! binary_or_res (str "1" binary_or_res)) (set! binary_or_res (str "0" binary_or_res))) (set! binary_or_x (quot binary_or_x 2)) (set! binary_or_y (quot binary_or_y 2)))) (when (= binary_or_res "") (set! binary_or_res "0")) (throw (ex-info "return" {:v (str "0b" binary_or_res)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (binary_or 25 32))
      (println (binary_or 37 50))
      (println (binary_or 21 30))
      (println (binary_or 58 73))
      (println (binary_or 0 255))
      (println (binary_or 0 256))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
