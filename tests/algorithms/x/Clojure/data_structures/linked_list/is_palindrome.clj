(ns main (:refer-clojure :exclude [is_palindrome main]))

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

(declare is_palindrome main)

(def ^:dynamic is_palindrome_fast nil)

(def ^:dynamic is_palindrome_i nil)

(def ^:dynamic is_palindrome_n nil)

(def ^:dynamic is_palindrome_slow nil)

(def ^:dynamic is_palindrome_stack nil)

(defn is_palindrome [is_palindrome_values]
  (binding [is_palindrome_fast nil is_palindrome_i nil is_palindrome_n nil is_palindrome_slow nil is_palindrome_stack nil] (try (do (set! is_palindrome_stack []) (set! is_palindrome_fast 0) (set! is_palindrome_slow 0) (set! is_palindrome_n (count is_palindrome_values)) (while (and (< is_palindrome_fast is_palindrome_n) (< (+ is_palindrome_fast 1) is_palindrome_n)) (do (set! is_palindrome_stack (conj is_palindrome_stack (nth is_palindrome_values is_palindrome_slow))) (set! is_palindrome_slow (+ is_palindrome_slow 1)) (set! is_palindrome_fast (+ is_palindrome_fast 2)))) (when (= is_palindrome_fast (- is_palindrome_n 1)) (set! is_palindrome_slow (+ is_palindrome_slow 1))) (set! is_palindrome_i (- (count is_palindrome_stack) 1)) (while (< is_palindrome_slow is_palindrome_n) (do (when (not= (nth is_palindrome_stack is_palindrome_i) (nth is_palindrome_values is_palindrome_slow)) (throw (ex-info "return" {:v false}))) (set! is_palindrome_i (- is_palindrome_i 1)) (set! is_palindrome_slow (+ is_palindrome_slow 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (is_palindrome [])) (println (is_palindrome [1])) (println (is_palindrome [1 2])) (println (is_palindrome [1 2 1])) (println (is_palindrome [1 2 2 1]))))

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
