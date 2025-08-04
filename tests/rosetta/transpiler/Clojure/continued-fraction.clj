(ns main (:refer-clojure :exclude [newTerm cfSqrt2 cfNap cfPi real main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare newTerm cfSqrt2 cfNap cfPi real main)

(def ^:dynamic cfNap_f nil)

(def ^:dynamic cfNap_n nil)

(def ^:dynamic cfPi_f nil)

(def ^:dynamic cfPi_g nil)

(def ^:dynamic cfPi_n nil)

(def ^:dynamic cfSqrt2_f nil)

(def ^:dynamic cfSqrt2_n nil)

(def ^:dynamic real_i nil)

(def ^:dynamic real_r nil)

(defn newTerm [newTerm_a newTerm_b]
  (try (throw (ex-info "return" {:v {"a" newTerm_a "b" newTerm_b}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn cfSqrt2 [cfSqrt2_nTerms]
  (binding [cfSqrt2_f nil cfSqrt2_n nil] (try (do (set! cfSqrt2_f []) (set! cfSqrt2_n 0) (while (< cfSqrt2_n cfSqrt2_nTerms) (do (set! cfSqrt2_f (conj cfSqrt2_f (newTerm 2 1))) (set! cfSqrt2_n (+ cfSqrt2_n 1)))) (when (> cfSqrt2_nTerms 0) (set! cfSqrt2_f (assoc-in cfSqrt2_f [0 "a"] 1))) (throw (ex-info "return" {:v cfSqrt2_f}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cfNap [cfNap_nTerms]
  (binding [cfNap_f nil cfNap_n nil] (try (do (set! cfNap_f []) (set! cfNap_n 0) (while (< cfNap_n cfNap_nTerms) (do (set! cfNap_f (conj cfNap_f (newTerm cfNap_n (- cfNap_n 1)))) (set! cfNap_n (+ cfNap_n 1)))) (when (> cfNap_nTerms 0) (set! cfNap_f (assoc-in cfNap_f [0 "a"] 2))) (when (> cfNap_nTerms 1) (set! cfNap_f (assoc-in cfNap_f [1 "b"] 1))) (throw (ex-info "return" {:v cfNap_f}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cfPi [cfPi_nTerms]
  (binding [cfPi_f nil cfPi_g nil cfPi_n nil] (try (do (set! cfPi_f []) (set! cfPi_n 0) (while (< cfPi_n cfPi_nTerms) (do (set! cfPi_g (- (* 2 cfPi_n) 1)) (set! cfPi_f (conj cfPi_f (newTerm 6 (* cfPi_g cfPi_g)))) (set! cfPi_n (+ cfPi_n 1)))) (when (> cfPi_nTerms 0) (set! cfPi_f (assoc-in cfPi_f [0 "a"] 3))) (throw (ex-info "return" {:v cfPi_f}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn real [real_f]
  (binding [real_i nil real_r nil] (try (do (set! real_r 0.0) (set! real_i (- (count real_f) 1)) (while (> real_i 0) (do (set! real_r (/ (double (get (nth real_f real_i) "b")) (+ (double (get (nth real_f real_i) "a")) real_r))) (set! real_i (- real_i 1)))) (when (> (count real_f) 0) (set! real_r (+ real_r (double (get (nth real_f 0) "a"))))) (throw (ex-info "return" {:v real_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (str "sqrt2: " (str (real (cfSqrt2 20))))) (println (str "nap:   " (str (real (cfNap 20))))) (println (str "pi:    " (str (real (cfPi 20)))))))

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
