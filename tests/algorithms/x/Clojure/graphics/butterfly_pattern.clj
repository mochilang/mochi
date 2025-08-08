(ns main (:refer-clojure :exclude [repeat_char butterfly_pattern]))

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

(declare repeat_char butterfly_pattern)

(def ^:dynamic butterfly_pattern_i nil)

(def ^:dynamic butterfly_pattern_j nil)

(def ^:dynamic butterfly_pattern_k nil)

(def ^:dynamic butterfly_pattern_left nil)

(def ^:dynamic butterfly_pattern_lines nil)

(def ^:dynamic butterfly_pattern_mid nil)

(def ^:dynamic butterfly_pattern_out nil)

(def ^:dynamic butterfly_pattern_right nil)

(def ^:dynamic repeat_char_i nil)

(def ^:dynamic repeat_char_result nil)

(defn repeat_char [repeat_char_ch count_v]
  (binding [repeat_char_i nil repeat_char_result nil] (try (do (set! repeat_char_result "") (set! repeat_char_i 0) (while (< repeat_char_i count_v) (do (set! repeat_char_result (str repeat_char_result repeat_char_ch)) (set! repeat_char_i (+ repeat_char_i 1)))) (throw (ex-info "return" {:v repeat_char_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn butterfly_pattern [butterfly_pattern_n]
  (binding [butterfly_pattern_i nil butterfly_pattern_j nil butterfly_pattern_k nil butterfly_pattern_left nil butterfly_pattern_lines nil butterfly_pattern_mid nil butterfly_pattern_out nil butterfly_pattern_right nil] (try (do (set! butterfly_pattern_lines []) (set! butterfly_pattern_i 1) (while (< butterfly_pattern_i butterfly_pattern_n) (do (set! butterfly_pattern_left (repeat_char "*" butterfly_pattern_i)) (set! butterfly_pattern_mid (repeat_char " " (- (* 2 (- butterfly_pattern_n butterfly_pattern_i)) 1))) (set! butterfly_pattern_right (repeat_char "*" butterfly_pattern_i)) (set! butterfly_pattern_lines (conj butterfly_pattern_lines (str (str butterfly_pattern_left butterfly_pattern_mid) butterfly_pattern_right))) (set! butterfly_pattern_i (+ butterfly_pattern_i 1)))) (set! butterfly_pattern_lines (conj butterfly_pattern_lines (repeat_char "*" (- (* 2 butterfly_pattern_n) 1)))) (set! butterfly_pattern_j (- butterfly_pattern_n 1)) (while (> butterfly_pattern_j 0) (do (set! butterfly_pattern_left (repeat_char "*" butterfly_pattern_j)) (set! butterfly_pattern_mid (repeat_char " " (- (* 2 (- butterfly_pattern_n butterfly_pattern_j)) 1))) (set! butterfly_pattern_right (repeat_char "*" butterfly_pattern_j)) (set! butterfly_pattern_lines (conj butterfly_pattern_lines (str (str butterfly_pattern_left butterfly_pattern_mid) butterfly_pattern_right))) (set! butterfly_pattern_j (- butterfly_pattern_j 1)))) (set! butterfly_pattern_out "") (set! butterfly_pattern_k 0) (while (< butterfly_pattern_k (count butterfly_pattern_lines)) (do (when (> butterfly_pattern_k 0) (set! butterfly_pattern_out (str butterfly_pattern_out "\n"))) (set! butterfly_pattern_out (str butterfly_pattern_out (nth butterfly_pattern_lines butterfly_pattern_k))) (set! butterfly_pattern_k (+ butterfly_pattern_k 1)))) (throw (ex-info "return" {:v butterfly_pattern_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (butterfly_pattern 3))
      (println (butterfly_pattern 5))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
