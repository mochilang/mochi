(ns main (:refer-clojure :exclude [primesUpTo longestSeq main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare primesUpTo longestSeq main)

(def ^:dynamic longestSeq_currSeq nil)

(def ^:dynamic longestSeq_d nil)

(def ^:dynamic longestSeq_diffs nil)

(def ^:dynamic longestSeq_i nil)

(def ^:dynamic longestSeq_j nil)

(def ^:dynamic longestSeq_k nil)

(def ^:dynamic longestSeq_longSeqs nil)

(def ^:dynamic longestSeq_pd nil)

(def ^:dynamic primesUpTo_i nil)

(def ^:dynamic primesUpTo_m nil)

(def ^:dynamic primesUpTo_p nil)

(def ^:dynamic primesUpTo_res nil)

(def ^:dynamic primesUpTo_sieve nil)

(def ^:dynamic primesUpTo_x nil)

(defn primesUpTo [primesUpTo_n]
  (binding [primesUpTo_i nil primesUpTo_m nil primesUpTo_p nil primesUpTo_res nil primesUpTo_sieve nil primesUpTo_x nil] (try (do (set! primesUpTo_sieve []) (set! primesUpTo_i 0) (while (<= primesUpTo_i primesUpTo_n) (do (set! primesUpTo_sieve (conj primesUpTo_sieve true)) (set! primesUpTo_i (+ primesUpTo_i 1)))) (set! primesUpTo_p 2) (while (<= (* primesUpTo_p primesUpTo_p) primesUpTo_n) (do (when (nth primesUpTo_sieve primesUpTo_p) (do (set! primesUpTo_m (* primesUpTo_p primesUpTo_p)) (while (<= primesUpTo_m primesUpTo_n) (do (set! primesUpTo_sieve (assoc primesUpTo_sieve primesUpTo_m false)) (set! primesUpTo_m (+ primesUpTo_m primesUpTo_p)))))) (set! primesUpTo_p (+ primesUpTo_p 1)))) (set! primesUpTo_res []) (set! primesUpTo_x 2) (while (<= primesUpTo_x primesUpTo_n) (do (when (nth primesUpTo_sieve primesUpTo_x) (set! primesUpTo_res (conj primesUpTo_res primesUpTo_x))) (set! primesUpTo_x (+ primesUpTo_x 1)))) (throw (ex-info "return" {:v primesUpTo_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_LIMIT 999999)

(def ^:dynamic main_primes (primesUpTo main_LIMIT))

(defn longestSeq [longestSeq_dir]
  (binding [longestSeq_currSeq nil longestSeq_d nil longestSeq_diffs nil longestSeq_i nil longestSeq_j nil longestSeq_k nil longestSeq_longSeqs nil longestSeq_pd nil] (do (set! longestSeq_pd 0) (set! longestSeq_longSeqs [[2]]) (set! longestSeq_currSeq [2]) (set! longestSeq_i 1) (while (< longestSeq_i (count main_primes)) (do (set! longestSeq_d (- (nth main_primes longestSeq_i) (nth main_primes (- longestSeq_i 1)))) (if (or (and (= longestSeq_dir "ascending") (<= longestSeq_d longestSeq_pd)) (and (= longestSeq_dir "descending") (>= longestSeq_d longestSeq_pd))) (do (if (> (count longestSeq_currSeq) (count (nth longestSeq_longSeqs 0))) (set! longestSeq_longSeqs [longestSeq_currSeq]) (when (= (count longestSeq_currSeq) (count (nth longestSeq_longSeqs 0))) (set! longestSeq_longSeqs (conj longestSeq_longSeqs longestSeq_currSeq)))) (set! longestSeq_currSeq [(nth main_primes (- longestSeq_i 1)) (nth main_primes longestSeq_i)])) (set! longestSeq_currSeq (conj longestSeq_currSeq (nth main_primes longestSeq_i)))) (set! longestSeq_pd longestSeq_d) (set! longestSeq_i (+ longestSeq_i 1)))) (if (> (count longestSeq_currSeq) (count (nth longestSeq_longSeqs 0))) (set! longestSeq_longSeqs [longestSeq_currSeq]) (when (= (count longestSeq_currSeq) (count (nth longestSeq_longSeqs 0))) (set! longestSeq_longSeqs (conj longestSeq_longSeqs longestSeq_currSeq)))) (println (str (str (str (str "Longest run(s) of primes with " longestSeq_dir) " differences is ") (str (count (nth longestSeq_longSeqs 0)))) " :")) (doseq [ls longestSeq_longSeqs] (do (set! longestSeq_diffs []) (set! longestSeq_j 1) (while (< longestSeq_j (count ls)) (do (set! longestSeq_diffs (conj longestSeq_diffs (- (nth ls longestSeq_j) (nth ls (- longestSeq_j 1))))) (set! longestSeq_j (+ longestSeq_j 1)))) (set! longestSeq_k 0) (while (< longestSeq_k (- (count ls) 1)) (do (println (str (str (str (str (nth ls longestSeq_k)) " (") (str (nth longestSeq_diffs longestSeq_k))) ") ") false) (set! longestSeq_k (+ longestSeq_k 1)))) (println (str (nth ls (- (count ls) 1)))))) (println ""))))

(defn main []
  (do (println "For primes < 1 million:\n") (doseq [dir ["ascending" "descending"]] (longestSeq dir))))

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
