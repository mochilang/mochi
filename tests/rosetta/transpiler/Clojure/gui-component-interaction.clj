(ns main (:refer-clojure :exclude [parseInt rand10000]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare parseInt rand10000)

(declare ans digits done i line n neg value)

(defn parseInt [str]
  (try (do (def i 0) (def neg false) (when (and (> (count str) 0) (= (subs str 0 1) "-")) (do (def neg true) (def i 1))) (def n 0) (def digits {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9}) (while (< i (count str)) (do (def n (+' (* n 10) (get digits (subs str i (+' i 1))))) (def i (+' i 1)))) (when neg (def n (- n))) (throw (ex-info "return" {:v n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rand10000 []
  (try (throw (ex-info "return" {:v (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) 10000)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def value 0)

(def done false)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Value:" value)
      (while (not done) (do (println "i=increment, r=random, s num=set, q=quit:") (def line (read-line)) (if (= line "i") (do (def value (+' value 1)) (println "Value:" value)) (if (= line "r") (do (println "Set random value? (y/n)") (def ans (read-line)) (when (= ans "y") (do (def value (rand10000)) (println "Value:" value)))) (if (and (> (count line) 2) (= (subvec line 0 2) "s ")) (do (def value (parseInt (subvec line 2 (count line)))) (println "Value:" value)) (if (= line "q") (def done true) (println "Unknown command")))))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
