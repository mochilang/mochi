(ns main (:refer-clojure :exclude [is_int_palindrome main]))

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

(declare is_int_palindrome main)

(def ^:dynamic is_int_palindrome_n nil)

(def ^:dynamic is_int_palindrome_rev nil)

(defn is_int_palindrome [is_int_palindrome_num]
  (binding [is_int_palindrome_n nil is_int_palindrome_rev nil] (try (do (when (< is_int_palindrome_num 0) (throw (ex-info "return" {:v false}))) (set! is_int_palindrome_n is_int_palindrome_num) (set! is_int_palindrome_rev 0) (while (> is_int_palindrome_n 0) (do (set! is_int_palindrome_rev (+ (* is_int_palindrome_rev 10) (mod is_int_palindrome_n 10))) (set! is_int_palindrome_n (quot is_int_palindrome_n 10)))) (throw (ex-info "return" {:v (= is_int_palindrome_rev is_int_palindrome_num)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (is_int_palindrome (- 121))) (println (is_int_palindrome 0)) (println (is_int_palindrome 10)) (println (is_int_palindrome 11)) (println (is_int_palindrome 101)) (println (is_int_palindrome 120))))

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
