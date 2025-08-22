(ns main (:refer-clojure :exclude [solution]))

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

(declare solution)

(declare _read_file)

(def ^:dynamic solution_currentDenominator nil)

(def ^:dynamic solution_currentNumerator nil)

(def ^:dynamic solution_maxDenominator nil)

(def ^:dynamic solution_maxNumerator nil)

(defn solution [solution_numerator solution_denominator solution_limit]
  (binding [solution_currentDenominator nil solution_currentNumerator nil solution_maxDenominator nil solution_maxNumerator nil] (try (do (set! solution_maxNumerator 0) (set! solution_maxDenominator 1) (set! solution_currentDenominator 1) (while (<= solution_currentDenominator solution_limit) (do (set! solution_currentNumerator (/ (* solution_currentDenominator solution_numerator) solution_denominator)) (when (= (mod solution_currentDenominator solution_denominator) 0) (set! solution_currentNumerator (- solution_currentNumerator 1))) (when (> (* solution_currentNumerator solution_maxDenominator) (* solution_currentDenominator solution_maxNumerator)) (do (set! solution_maxNumerator solution_currentNumerator) (set! solution_maxDenominator solution_currentDenominator))) (set! solution_currentDenominator (+ solution_currentDenominator 1)))) (throw (ex-info "return" {:v solution_maxNumerator}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (solution 3 7 1000000)))
      (println (mochi_str (solution 3 7 8)))
      (println (mochi_str (solution 6 7 60)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
