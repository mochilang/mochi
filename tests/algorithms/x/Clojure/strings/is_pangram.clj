(ns main (:refer-clojure :exclude [is_pangram is_pangram_faster is_pangram_fastest]))

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

(declare is_pangram is_pangram_faster is_pangram_fastest)

(def ^:dynamic is_pangram_c nil)

(def ^:dynamic is_pangram_faster_alphabet nil)

(def ^:dynamic is_pangram_faster_c nil)

(def ^:dynamic is_pangram_faster_flag nil)

(def ^:dynamic is_pangram_faster_i nil)

(def ^:dynamic is_pangram_faster_j nil)

(def ^:dynamic is_pangram_faster_k nil)

(def ^:dynamic is_pangram_faster_t nil)

(def ^:dynamic is_pangram_fastest_alphabet nil)

(def ^:dynamic is_pangram_fastest_i nil)

(def ^:dynamic is_pangram_fastest_letter nil)

(def ^:dynamic is_pangram_fastest_s nil)

(def ^:dynamic is_pangram_i nil)

(def ^:dynamic is_pangram_is_new nil)

(def ^:dynamic is_pangram_letters nil)

(defn is_pangram [is_pangram_input_str]
  (binding [is_pangram_c nil is_pangram_i nil is_pangram_is_new nil is_pangram_letters nil] (try (do (set! is_pangram_letters []) (set! is_pangram_i 0) (while (< is_pangram_i (count is_pangram_input_str)) (do (set! is_pangram_c (clojure.string/lower-case (subs is_pangram_input_str is_pangram_i (+ is_pangram_i 1)))) (set! is_pangram_is_new (not (in is_pangram_c is_pangram_letters))) (when (and (and (and (not= is_pangram_c " ") (<= (compare "a" is_pangram_c) 0)) (<= (compare is_pangram_c "z") 0)) is_pangram_is_new) (set! is_pangram_letters (conj is_pangram_letters is_pangram_c))) (set! is_pangram_i (+ is_pangram_i 1)))) (throw (ex-info "return" {:v (= (count is_pangram_letters) 26)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_pangram_faster [is_pangram_faster_input_str]
  (binding [is_pangram_faster_alphabet nil is_pangram_faster_c nil is_pangram_faster_flag nil is_pangram_faster_i nil is_pangram_faster_j nil is_pangram_faster_k nil is_pangram_faster_t nil] (try (do (set! is_pangram_faster_alphabet "abcdefghijklmnopqrstuvwxyz") (set! is_pangram_faster_flag []) (set! is_pangram_faster_i 0) (while (< is_pangram_faster_i 26) (do (set! is_pangram_faster_flag (conj is_pangram_faster_flag false)) (set! is_pangram_faster_i (+ is_pangram_faster_i 1)))) (set! is_pangram_faster_j 0) (while (< is_pangram_faster_j (count is_pangram_faster_input_str)) (do (set! is_pangram_faster_c (clojure.string/lower-case (subs is_pangram_faster_input_str is_pangram_faster_j (+ is_pangram_faster_j 1)))) (set! is_pangram_faster_k 0) (loop [while_flag_1 true] (when (and while_flag_1 (< is_pangram_faster_k 26)) (cond (= (subs is_pangram_faster_alphabet is_pangram_faster_k (+ is_pangram_faster_k 1)) is_pangram_faster_c) (do (set! is_pangram_faster_flag (assoc is_pangram_faster_flag is_pangram_faster_k true)) (recur false)) :else (do (set! is_pangram_faster_k (+ is_pangram_faster_k 1)) (recur while_flag_1))))) (set! is_pangram_faster_j (+ is_pangram_faster_j 1)))) (set! is_pangram_faster_t 0) (while (< is_pangram_faster_t 26) (do (when (not (nth is_pangram_faster_flag is_pangram_faster_t)) (throw (ex-info "return" {:v false}))) (set! is_pangram_faster_t (+ is_pangram_faster_t 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_pangram_fastest [is_pangram_fastest_input_str]
  (binding [is_pangram_fastest_alphabet nil is_pangram_fastest_i nil is_pangram_fastest_letter nil is_pangram_fastest_s nil] (try (do (set! is_pangram_fastest_s (clojure.string/lower-case is_pangram_fastest_input_str)) (set! is_pangram_fastest_alphabet "abcdefghijklmnopqrstuvwxyz") (set! is_pangram_fastest_i 0) (while (< is_pangram_fastest_i (count is_pangram_fastest_alphabet)) (do (set! is_pangram_fastest_letter (subs is_pangram_fastest_alphabet is_pangram_fastest_i (+ is_pangram_fastest_i 1))) (when (not (in is_pangram_fastest_letter is_pangram_fastest_s)) (throw (ex-info "return" {:v false}))) (set! is_pangram_fastest_i (+ is_pangram_fastest_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_s1 "The quick brown fox jumps over the lazy dog")

(def ^:dynamic main_s2 "My name is Unknown")

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_pangram main_s1)))
      (println (str (is_pangram main_s2)))
      (println (str (is_pangram_faster main_s1)))
      (println (str (is_pangram_faster main_s2)))
      (println (str (is_pangram_fastest main_s1)))
      (println (str (is_pangram_fastest main_s2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
