(ns main (:refer-clojure :exclude [factorial is_krishnamurthy]))

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

(declare factorial is_krishnamurthy)

(def ^:dynamic is_krishnamurthy_digit nil)

(def ^:dynamic is_krishnamurthy_duplicate nil)

(def ^:dynamic is_krishnamurthy_fact_sum nil)

(defn factorial [factorial_digit]
  (try (if (or (= factorial_digit 0) (= factorial_digit 1)) 1 (* factorial_digit (factorial (- factorial_digit 1)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_krishnamurthy [is_krishnamurthy_n]
  (binding [is_krishnamurthy_digit nil is_krishnamurthy_duplicate nil is_krishnamurthy_fact_sum nil] (try (do (set! is_krishnamurthy_duplicate is_krishnamurthy_n) (set! is_krishnamurthy_fact_sum 0) (while (> is_krishnamurthy_duplicate 0) (do (set! is_krishnamurthy_digit (mod is_krishnamurthy_duplicate 10)) (set! is_krishnamurthy_fact_sum (+ is_krishnamurthy_fact_sum (factorial is_krishnamurthy_digit))) (set! is_krishnamurthy_duplicate (quot is_krishnamurthy_duplicate 10)))) (throw (ex-info "return" {:v (= is_krishnamurthy_fact_sum is_krishnamurthy_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_krishnamurthy 145)))
      (println (str (is_krishnamurthy 240)))
      (println (str (is_krishnamurthy 1)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
