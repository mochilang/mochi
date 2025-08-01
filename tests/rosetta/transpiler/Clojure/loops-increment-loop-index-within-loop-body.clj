(ns main (:refer-clojure :exclude [isPrime commatize padLeft padRight main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare isPrime commatize padLeft padRight main)

(declare d i limit n nStr out pStr s)

(defn isPrime [n]
  (try (do (when (< n 2) (throw (ex-info "return" {:v false}))) (when (= (mod n 2) 0) (throw (ex-info "return" {:v (= n 2)}))) (when (= (mod n 3) 0) (throw (ex-info "return" {:v (= n 3)}))) (def d 5) (while (<= (* d d) n) (do (when (= (mod n d) 0) (throw (ex-info "return" {:v false}))) (def d (+' d 2)) (when (= (mod n d) 0) (throw (ex-info "return" {:v false}))) (def d (+' d 4)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn commatize [n]
  (try (do (def s (str n)) (def i (- (count s) 3)) (while (>= i 1) (do (def s (str (str (subs s 0 i) ",") (subs s i (count s)))) (def i (- i 3)))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padLeft [s w]
  (try (do (def out s) (while (< (count out) w) (def out (str " " out))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padRight [s w]
  (try (do (def out s) (while (< (count out) w) (def out (str out " "))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def limit 42)

(defn main []
  (do (def i limit) (def n 0) (while (< n limit) (do (when (isPrime i) (do (def n (+' n 1)) (def nStr (padRight (str n) 2)) (def pStr (padLeft (commatize i) 19)) (println (str (str (str "n = " nStr) "  ") pStr)) (def i (- (+' i i) 1)))) (def i (+' i 1))))))

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
