(ns main (:refer-clojure :exclude [is_palindrome solution]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare is_palindrome solution)

(def ^:dynamic is_palindrome_i nil)

(def ^:dynamic is_palindrome_j nil)

(def ^:dynamic is_palindrome_s nil)

(def ^:dynamic solution_divisor nil)

(def ^:dynamic solution_number nil)

(def ^:dynamic solution_other nil)

(defn is_palindrome [is_palindrome_num]
  (binding [is_palindrome_i nil is_palindrome_j nil is_palindrome_s nil] (try (do (set! is_palindrome_s (str is_palindrome_num)) (set! is_palindrome_i 0) (set! is_palindrome_j (- (count is_palindrome_s) 1)) (while (< is_palindrome_i is_palindrome_j) (do (when (not= (subs is_palindrome_s is_palindrome_i (min (+ is_palindrome_i 1) (count is_palindrome_s))) (subs is_palindrome_s is_palindrome_j (min (+ is_palindrome_j 1) (count is_palindrome_s)))) (throw (ex-info "return" {:v false}))) (set! is_palindrome_i (+ is_palindrome_i 1)) (set! is_palindrome_j (- is_palindrome_j 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_n]
  (binding [solution_divisor nil solution_number nil solution_other nil] (try (do (set! solution_number (- solution_n 1)) (while (> solution_number 9999) (do (when (is_palindrome solution_number) (do (set! solution_divisor 999) (while (> solution_divisor 99) (do (when (= (mod solution_number solution_divisor) 0) (do (set! solution_other (/ solution_number solution_divisor)) (when (= (count (str solution_other)) 3) (throw (ex-info "return" {:v solution_number}))))) (set! solution_divisor (- solution_divisor 1)))))) (set! solution_number (- solution_number 1)))) (println "That number is larger than our acceptable range.") (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "solution() = " (str (solution 998001))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
