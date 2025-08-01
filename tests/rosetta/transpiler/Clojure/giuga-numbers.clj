(ns main (:refer-clojure :exclude [primeFactors isGiuga main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare primeFactors isGiuga main)

(declare facs factors known last nums p x)

(defn primeFactors [n]
  (try (do (def factors []) (def last 0) (def x n) (while (= (mod x 2) 0) (do (when (= last 2) (throw (ex-info "return" {:v []}))) (def factors (conj factors 2)) (def last 2) (def x (/ x 2)))) (def p 3) (while (<= (* p p) x) (do (while (= (mod x p) 0) (do (when (= last p) (throw (ex-info "return" {:v []}))) (def factors (conj factors p)) (def last p) (def x (/ x p)))) (def p (+' p 2)))) (when (> x 1) (do (when (= last x) (throw (ex-info "return" {:v []}))) (def factors (conj factors x)))) (throw (ex-info "return" {:v factors}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn isGiuga [n]
  (try (do (def facs (primeFactors n)) (when (<= (count facs) 2) (throw (ex-info "return" {:v false}))) (doseq [f facs] (when (not= (mod (- (/ n f) 1) f) 0) (throw (ex-info "return" {:v false})))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def known [30 858 1722 66198]) (def nums []) (doseq [n known] (when (isGiuga n) (def nums (conj nums n)))) (println "The first 4 Giuga numbers are:") (println nums)))

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
