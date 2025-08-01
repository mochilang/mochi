(ns main (:refer-clojure :exclude [mod isPrime pad carmichael]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare mod isPrime pad carmichael)

(declare carmichael_c carmichael_p2 carmichael_p3 isPrime_d pad_s)

(defn mod [mod_n mod_m]
  (try (throw (ex-info "return" {:v (mod (+ (mod mod_n mod_m) mod_m) mod_m)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn isPrime [isPrime_n]
  (try (do (when (< isPrime_n 2) (throw (ex-info "return" {:v false}))) (when (= (mod isPrime_n 2) 0) (throw (ex-info "return" {:v (= isPrime_n 2)}))) (when (= (mod isPrime_n 3) 0) (throw (ex-info "return" {:v (= isPrime_n 3)}))) (def isPrime_d 5) (while (<= (* isPrime_d isPrime_d) isPrime_n) (do (when (= (mod isPrime_n isPrime_d) 0) (throw (ex-info "return" {:v false}))) (def isPrime_d (+ isPrime_d 2)) (when (= (mod isPrime_n isPrime_d) 0) (throw (ex-info "return" {:v false}))) (def isPrime_d (+ isPrime_d 4)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [pad_n pad_width]
  (try (do (def pad_s (str pad_n)) (while (< (count pad_s) pad_width) (def pad_s (str " " pad_s))) (throw (ex-info "return" {:v pad_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn carmichael [carmichael_p1]
  (doseq [h3 (range 2 carmichael_p1)] (loop [d_seq (range 1 (+ h3 carmichael_p1))] (when (seq d_seq) (do (when (and (= (mod (* (+ h3 carmichael_p1) (- carmichael_p1 1)) d) 0) (= (mod (* (- carmichael_p1) carmichael_p1) h3) (mod d h3))) (do (def carmichael_p2 (+ 1 (/ (* (- carmichael_p1 1) (+ h3 carmichael_p1)) d))) (when (not (isPrime carmichael_p2)) (recur (rest d_seq))) (def carmichael_p3 (+ 1 (/ (* carmichael_p1 carmichael_p2) h3))) (when (not (isPrime carmichael_p3)) (recur (rest d_seq))) (when (not= (mod (* carmichael_p2 carmichael_p3) (- carmichael_p1 1)) 1) (recur (rest d_seq))) (def carmichael_c (* (* carmichael_p1 carmichael_p2) carmichael_p3)) (println (str (str (str (str (str (str (pad carmichael_p1 2) "   ") (pad carmichael_p2 4)) "   ") (pad carmichael_p3 5)) "     ") (str carmichael_c))))) (let [d (first d_seq)] (cond :else (recur (rest d_seq)))))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "The following are Carmichael munbers for p1 <= 61:\n")
      (println "p1     p2      p3     product")
      (println "==     ==      ==     =======")
      (doseq [p1 (range 2 62)] (when (isPrime p1) (carmichael p1)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
