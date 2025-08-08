(ns main (:refer-clojure :exclude [fizz_buzz]))

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

(declare fizz_buzz)

(def ^:dynamic fizz_buzz_n nil)

(def ^:dynamic fizz_buzz_out nil)

(defn fizz_buzz [fizz_buzz_number fizz_buzz_iterations]
  (binding [fizz_buzz_n nil fizz_buzz_out nil] (try (do (when (< fizz_buzz_number 1) (throw (Exception. "starting number must be an integer and be more than 0"))) (when (< fizz_buzz_iterations 1) (throw (Exception. "Iterations must be done more than 0 times to play FizzBuzz"))) (set! fizz_buzz_out "") (set! fizz_buzz_n fizz_buzz_number) (while (<= fizz_buzz_n fizz_buzz_iterations) (do (when (= (mod fizz_buzz_n 3) 0) (set! fizz_buzz_out (str fizz_buzz_out "Fizz"))) (when (= (mod fizz_buzz_n 5) 0) (set! fizz_buzz_out (str fizz_buzz_out "Buzz"))) (when (and (not= (mod fizz_buzz_n 3) 0) (not= (mod fizz_buzz_n 5) 0)) (set! fizz_buzz_out (str fizz_buzz_out (str fizz_buzz_n)))) (set! fizz_buzz_out (str fizz_buzz_out " ")) (set! fizz_buzz_n (+ fizz_buzz_n 1)))) (throw (ex-info "return" {:v fizz_buzz_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (fizz_buzz 1 7))
      (println (fizz_buzz 1 15))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
