(ns main (:refer-clojure :exclude [isPrime gen pad main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare isPrime gen pad main)

(defn isPrime [n]
  (try (do (when (< n 2) (throw (ex-info "return" {:v false}))) (when (= (mod n 2) 0) (throw (ex-info "return" {:v (= n 2)}))) (when (= (mod n 3) 0) (throw (ex-info "return" {:v (= n 3)}))) (def d 5) (while (<= (* d d) n) (do (when (= (mod n d) 0) (throw (ex-info "return" {:v false}))) (def d (+ d 2)) (when (= (mod n d) 0) (throw (ex-info "return" {:v false}))) (def d (+ d 4)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn gen [first cand digits]
  (try (do (when (= digits 0) (do (when (isPrime cand) (def asc (+ asc [cand]))) (throw (ex-info "return" {:v nil})))) (def i first) (while (< i 10) (do (gen (+ i 1) (+ (* cand 10) i) (- digits 1)) (def i (+ i 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [n width]
  (try (do (def s (str n)) (while (< (count s) width) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def digits 1) (while (< digits 10) (do (gen 1 0 digits) (def digits (+ digits 1)))) (println (str (str "There are " (str (count asc))) " ascending primes, namely:")) (def i 0) (def line "") (while (< i (count asc)) (do (def line (str (str line (pad (nth asc i) 8)) " ")) (when (= (mod (+ i 1) 10) 0) (do (println (subs line 0 (- (count line) 1))) (def line ""))) (def i (+ i 1)))) (when (> (count line) 0) (println (subs line 0 (- (count line) 1))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def asc [])
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
