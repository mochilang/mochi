(ns main (:refer-clojure :exclude [_fib fibonacci]))

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

(declare _fib fibonacci)

(def ^:dynamic _fib_a nil)

(def ^:dynamic _fib_b nil)

(def ^:dynamic _fib_c nil)

(def ^:dynamic _fib_d nil)

(def ^:dynamic _fib_half nil)

(def ^:dynamic fibonacci_res nil)

(def ^:dynamic main_i nil)

(defn _fib [_fib_n]
  (binding [_fib_a nil _fib_b nil _fib_c nil _fib_d nil _fib_half nil] (try (do (when (= _fib_n 0) (throw (ex-info "return" {:v {:fn 0 :fn1 1}}))) (set! _fib_half (_fib (quot _fib_n 2))) (set! _fib_a (:fn _fib_half)) (set! _fib_b (:fn1 _fib_half)) (set! _fib_c (* _fib_a (- (* _fib_b 2) _fib_a))) (set! _fib_d (+ (* _fib_a _fib_a) (* _fib_b _fib_b))) (if (= (mod _fib_n 2) 0) {:fn _fib_c :fn1 _fib_d} {:fn _fib_d :fn1 (+ _fib_c _fib_d)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fibonacci [fibonacci_n]
  (binding [fibonacci_res nil] (try (do (when (< fibonacci_n 0) (throw (Exception. "Negative arguments are not supported"))) (set! fibonacci_res (_fib fibonacci_n)) (throw (ex-info "return" {:v (:fn fibonacci_res)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_i 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_i 13) (do (println (str (fibonacci main_i))) (def main_i (+ main_i 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
