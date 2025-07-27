(ns main (:refer-clojure :exclude [poly aks main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare poly aks main)

(defn poly [p]
  (try (do (def s "") (def coef 1) (def i p) (when (not= coef 1) (def s (str s (str coef)))) (while (> i 0) (do (def s (str s "x")) (when (not= i 1) (def s (str (str s "^") (str i)))) (def coef (int (/ (* coef i) (+ (- p i) 1)))) (def d coef) (when (= (mod (- p (- i 1)) 2) 1) (def d (- d))) (if (< d 0) (def s (str (str s " - ") (str (- d)))) (def s (str (str s " + ") (str d)))) (def i (- i 1)))) (when (= s "") (def s "1")) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn aks [n]
  (try (do (when (< n 2) (throw (ex-info "return" {:v false}))) (def c n) (def i 1) (while (< i n) (do (when (not= (mod c n) 0) (throw (ex-info "return" {:v false}))) (def c (int (/ (* c (- n i)) (+ i 1)))) (def i (+ i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def p 0) (while (<= p 7) (do (println (str (str (str p) ":  ") (poly p))) (def p (+ p 1)))) (def first true) (def p 2) (def line "") (while (< p 50) (do (when (aks p) (if first (do (def line (str line (str p))) (def first false)) (def line (str (str line " ") (str p))))) (def p (+ p 1)))) (println line)))

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
