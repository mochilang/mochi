(ns main (:refer-clojure :exclude [kPrime gen main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare kPrime gen main)

(defn kPrime [n k]
  (try (do (def nf 0) (def i 2) (while (<= i n) (do (while (= (mod n i) 0) (do (when (= nf k) (throw (ex-info "return" {:v false}))) (def nf (+ nf 1)) (def n (/ n i)))) (def i (+ i 1)))) (throw (ex-info "return" {:v (= nf k)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn gen [k count]
  (try (do (def r []) (def n 2) (while (< (count r) count_v) (do (when (kPrime n k) (def r (conj r n))) (def n (+ n 1)))) (throw (ex-info "return" {:v r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def k 1) (while (<= k 5) (do (println (str (str (str k) " ") (str (gen k 10)))) (def k (+ k 1))))))

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
