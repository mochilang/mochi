(ns main (:refer-clojure :exclude [fuscVal firstFusc commatize padLeft main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare fuscVal firstFusc commatize padLeft main)

(declare a arr b i idx idxStr idxs neg numStr out s val x)

(defn fuscVal [n]
  (try (do (def a 1) (def b 0) (def x n) (while (> x 0) (if (= (mod x 2) 0) (do (def x (/ x 2)) (def a (+' a b))) (do (def x (/ (- x 1) 2)) (def b (+' a b))))) (if (= n 0) 0 b)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn firstFusc [n]
  (try (do (def arr []) (def i 0) (while (< i n) (do (def arr (conj arr (fuscVal i))) (def i (+' i 1)))) (throw (ex-info "return" {:v arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn commatize [n]
  (try (do (def s (str n)) (def neg false) (when (< n 0) (do (def neg true) (def s (subs s 1 (count s))))) (def i (- (count s) 3)) (while (>= i 1) (do (def s (str (str (subs s 0 i) ",") (subs s i (count s)))) (def i (- i 3)))) (if neg (str "-" s) s)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padLeft [s w]
  (try (do (def out s) (while (< (count out) w) (def out (str " " out))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (println "The first 61 fusc numbers are:") (println (str (firstFusc 61))) (println "\nThe fusc numbers whose length > any previous fusc number length are:") (def idxs [0 37 1173 35499 699051 19573419]) (def i 0) (while (< i (count idxs)) (do (def idx (nth idxs i)) (def val (fuscVal idx)) (def numStr (padLeft (commatize val) 7)) (def idxStr (padLeft (commatize idx) 10)) (println (str (str (str numStr " (index ") idxStr) ")")) (def i (+' i 1))))))

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
