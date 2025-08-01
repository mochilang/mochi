(ns main (:refer-clojure :exclude [parseIntStr showTokens main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare parseIntStr showTokens main)

(declare ct digits done i line n neg s t tokens)

(defn parseIntStr [str]
  (try (do (def i 0) (def neg false) (when (and (> (count str) 0) (= (subs str 0 1) "-")) (do (def neg true) (def i 1))) (def n 0) (def digits {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9}) (while (< i (count str)) (do (def n (+' (* n 10) (get digits (subs str i (+' i 1))))) (def i (+' i 1)))) (when neg (def n (- n))) (throw (ex-info "return" {:v n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn showTokens [tokens]
  (println (str "Tokens remaining " (str tokens))))

(defn main []
  (do (def tokens 12) (def done false) (while (not done) (do (showTokens tokens) (println "") (println "How many tokens 1, 2 or 3?") (def line (read-line)) (def t 0) (when (> (count line) 0) (def t (parseIntStr line))) (if (or (< t 1) (> t 3)) (println "\nMust be a number between 1 and 3, try again.\n") (do (def ct (- 4 t)) (def s "s") (when (= ct 1) (def s "")) (println (str (str (str (str "  Computer takes " (str ct)) " token") s) "\n\n")) (def tokens (- tokens 4)))) (when (= tokens 0) (do (showTokens 0) (println "  Computer wins!") (def done true)))))))

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
