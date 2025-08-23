(ns main (:refer-clojure :exclude [fibonacci fibonacci_digits_index solution main]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare fibonacci fibonacci_digits_index solution main)

(declare _read_file)

(def ^:dynamic fibonacci_a nil)

(def ^:dynamic fibonacci_b nil)

(def ^:dynamic fibonacci_c nil)

(def ^:dynamic fibonacci_digits_index_digits nil)

(def ^:dynamic fibonacci_digits_index_fib nil)

(def ^:dynamic fibonacci_digits_index_index nil)

(def ^:dynamic fibonacci_i nil)

(def ^:dynamic main_n nil)

(defn fibonacci [fibonacci_n]
  (binding [fibonacci_a nil fibonacci_b nil fibonacci_c nil fibonacci_i nil] (try (do (when (= fibonacci_n 1) (throw (ex-info "return" {:v 0}))) (when (= fibonacci_n 2) (throw (ex-info "return" {:v 1}))) (set! fibonacci_a 0) (set! fibonacci_b 1) (set! fibonacci_i 2) (while (<= fibonacci_i fibonacci_n) (do (set! fibonacci_c (+ fibonacci_a fibonacci_b)) (set! fibonacci_a fibonacci_b) (set! fibonacci_b fibonacci_c) (set! fibonacci_i (+ fibonacci_i 1)))) (throw (ex-info "return" {:v fibonacci_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fibonacci_digits_index [fibonacci_digits_index_n]
  (binding [fibonacci_digits_index_digits nil fibonacci_digits_index_fib nil fibonacci_digits_index_index nil] (try (do (set! fibonacci_digits_index_digits 0) (set! fibonacci_digits_index_index 2) (while (< fibonacci_digits_index_digits fibonacci_digits_index_n) (do (set! fibonacci_digits_index_index (+ fibonacci_digits_index_index 1)) (set! fibonacci_digits_index_fib (fibonacci fibonacci_digits_index_index)) (set! fibonacci_digits_index_digits (count (mochi_str fibonacci_digits_index_fib))))) (throw (ex-info "return" {:v fibonacci_digits_index_index}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution [solution_n]
  (try (throw (ex-info "return" {:v (fibonacci_digits_index solution_n)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_n nil] (do (set! main_n (toi (read-line))) (println (solution main_n)))))

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
