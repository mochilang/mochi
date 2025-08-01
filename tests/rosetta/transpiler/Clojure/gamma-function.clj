(ns main (:refer-clojure :exclude [ln expf powf lanczos7]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare ln expf powf lanczos7)

(declare i k ln2 sum t term v x xs z zpow)

(defn ln [x]
  (try (do (def k 0.0) (def v x) (while (>= v 2.0) (do (def v (/ v 2.0)) (def k (+' k 1.0)))) (while (< v 1.0) (do (def v (* v 2.0)) (def k (- k 1.0)))) (def z (/ (- v 1.0) (+' v 1.0))) (def zpow z) (def sum z) (def i 3) (while (<= i 9) (do (def zpow (* (* zpow z) z)) (def sum (+' sum (/ zpow (double i)))) (def i (+' i 2)))) (def ln2 0.6931471805599453) (throw (ex-info "return" {:v (+' (* k ln2) (* 2.0 sum))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn expf [x]
  (try (do (def term 1.0) (def sum 1.0) (def i 1) (while (< i 20) (do (def term (/ (* term x) (float i))) (def sum (+' sum term)) (def i (+' i 1)))) (throw (ex-info "return" {:v sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn powf [base exp]
  (try (throw (ex-info "return" {:v (expf (* exp (ln base)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn lanczos7 [z]
  (try (do (def t (+' z 6.5)) (def x (+' (+' (- (+' (- (+' (- (+' 0.9999999999998099 (/ 676.5203681218851 z)) (/ 1259.1392167224028 (+' z 1.0))) (/ 771.3234287776531 (+' z 2.0))) (/ 176.6150291621406 (+' z 3.0))) (/ 12.507343278686905 (+' z 4.0))) (/ 0.13857109526572012 (+' z 5.0))) (/ 0.000009984369578019572 (+' z 6.0))) (/ 0.00000015056327351493116 (+' z 7.0)))) (throw (ex-info "return" {:v (* (* (* 2.5066282746310002 (powf t (- z 0.5))) (powf 2.718281828459045 (- t))) x)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def xs [(- 0.5) 0.1 0.5 1.0 1.5 2.0 3.0 10.0 140.0 170.0])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (doseq [x xs] (println (str (str (str x) " ") (str (lanczos7 x)))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
