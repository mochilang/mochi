(ns main (:refer-clojure :exclude [indexOf fmt3 pad smaSeries main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare indexOf fmt3 pad smaSeries main)

(defn indexOf [s ch]
  (try (do (def i 0) (while (< i (count s)) (do (when (= (subs s i (+ i 1)) ch) (throw (ex-info "return" {:v i}))) (def i (+ i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fmt3 [x]
  (try (do (def y (/ (double (int (+ (* x 1000.0) 0.5))) 1000.0)) (def s (str y)) (def dot (indexOf s ".")) (if (= dot (- 0 1)) (def s (str s ".000")) (do (def decs (- (- (count s) dot) 1)) (if (> decs 3) (def s (subs s 0 (+ dot 4))) (while (< decs 3) (do (def s (str s "0")) (def decs (+ decs 1))))))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [s width]
  (try (do (def out s) (while (< (count out) width) (def out (str " " out))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn smaSeries [xs period]
  (try (do (def res []) (def sum 0.0) (def i 0) (while (< i (count xs)) (do (def sum (+ sum (nth xs i))) (when (>= i period) (def sum (- sum (nth xs (- i period))))) (def denom (+ i 1)) (when (> denom period) (def denom period)) (def res (conj res (/ sum (double denom)))) (def i (+ i 1)))) (throw (ex-info "return" {:v res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def xs [1.0 2.0 3.0 4.0 5.0 5.0 4.0 3.0 2.0 1.0]) (def sma3 (smaSeries xs 3)) (def sma5 (smaSeries xs 5)) (println "x       sma3   sma5") (def i 0) (while (< i (count xs)) (do (def line (str (str (str (str (pad (fmt3 (nth xs i)) 5) "  ") (pad (fmt3 (nth sma3 i)) 5)) "  ") (pad (fmt3 (nth sma5 i)) 5))) (println line) (def i (+ i 1))))))

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
