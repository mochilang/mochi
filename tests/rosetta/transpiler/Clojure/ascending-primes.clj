(ns main (:refer-clojure :exclude [isPrime gen pad main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare isPrime gen pad main)

(defn isPrime [n_v0]
  (try (do (when (< n_v0 2) (throw (ex-info "return" {:v false}))) (when (= (mod n_v0 2) 0) (throw (ex-info "return" {:v (= n_v0 2)}))) (when (= (mod n_v0 3) 0) (throw (ex-info "return" {:v (= n_v0 3)}))) (def d_v1 5) (while (<= (* d_v1 d_v1) n_v0) (do (when (= (mod n_v0 d_v1) 0) (throw (ex-info "return" {:v false}))) (def d_v1 (+ d_v1 2)) (when (= (mod n_v0 d_v1) 0) (throw (ex-info "return" {:v false}))) (def d_v1 (+ d_v1 4)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def asc [])

(defn gen [first_v0 cand_v1 digits_v2]
  (try (do (when (= digits_v2 0) (do (when (isPrime cand_v1) (def asc (vec (concat asc [cand_v1])))) (throw (ex-info "return" {:v nil})))) (def i_v3 first_v0) (while (< i_v3 10) (do (gen (+ i_v3 1) (+ (* cand_v1 10) i_v3) (- digits_v2 1)) (def i_v3 (+ i_v3 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pad [n_v0 width_v1]
  (try (do (def s_v2 (str n_v0)) (while (< (count s_v2) width_v1) (def s_v2 (str " " s_v2))) (throw (ex-info "return" {:v s_v2}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def digits_v0 1) (while (< digits_v0 10) (do (gen 1 0 digits_v0) (def digits_v0 (+ digits_v0 1)))) (println (str (str "There are " (str (count asc))) " ascending primes, namely:")) (def i_v1 0) (def line_v2 "") (while (< i_v1 (count asc)) (do (def line_v2 (str (str line_v2 (pad (nth asc i_v1) 8)) " ")) (when (= (mod (+ i_v1 1) 10) 0) (do (println (subs line_v2 0 (- (count line_v2) 1))) (def line_v2 ""))) (def i_v1 (+ i_v1 1)))) (when (> (count line_v2) 0) (println (subs line_v2 0 (- (count line_v2) 1))))))

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
