(ns main (:refer-clojure :exclude [min_partitions]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare min_partitions)

(def ^:dynamic min_partitions_candidate nil)

(def ^:dynamic min_partitions_cut nil)

(def ^:dynamic min_partitions_i nil)

(def ^:dynamic min_partitions_j nil)

(def ^:dynamic min_partitions_mincut nil)

(def ^:dynamic min_partitions_n nil)

(def ^:dynamic min_partitions_pal nil)

(def ^:dynamic min_partitions_row nil)

(defn min_partitions [min_partitions_s]
  (binding [min_partitions_candidate nil min_partitions_cut nil min_partitions_i nil min_partitions_j nil min_partitions_mincut nil min_partitions_n nil min_partitions_pal nil min_partitions_row nil] (try (do (set! min_partitions_n (count min_partitions_s)) (set! min_partitions_cut []) (set! min_partitions_i 0) (while (< min_partitions_i min_partitions_n) (do (set! min_partitions_cut (conj min_partitions_cut 0)) (set! min_partitions_i (+ min_partitions_i 1)))) (set! min_partitions_pal []) (set! min_partitions_i 0) (while (< min_partitions_i min_partitions_n) (do (set! min_partitions_row []) (set! min_partitions_j 0) (while (< min_partitions_j min_partitions_n) (do (set! min_partitions_row (conj min_partitions_row false)) (set! min_partitions_j (+ min_partitions_j 1)))) (set! min_partitions_pal (conj min_partitions_pal min_partitions_row)) (set! min_partitions_i (+ min_partitions_i 1)))) (set! min_partitions_i 0) (while (< min_partitions_i min_partitions_n) (do (set! min_partitions_mincut min_partitions_i) (set! min_partitions_j 0) (while (<= min_partitions_j min_partitions_i) (do (when (and (= (nth min_partitions_s min_partitions_i) (nth min_partitions_s min_partitions_j)) (or (< (- min_partitions_i min_partitions_j) 2) (nth (nth min_partitions_pal (+ min_partitions_j 1)) (- min_partitions_i 1)))) (do (set! min_partitions_pal (assoc-in min_partitions_pal [min_partitions_j min_partitions_i] true)) (if (= min_partitions_j 0) (set! min_partitions_mincut 0) (do (set! min_partitions_candidate (+ (nth min_partitions_cut (- min_partitions_j 1)) 1)) (when (< min_partitions_candidate min_partitions_mincut) (set! min_partitions_mincut min_partitions_candidate)))))) (set! min_partitions_j (+ min_partitions_j 1)))) (set! min_partitions_cut (assoc min_partitions_cut min_partitions_i min_partitions_mincut)) (set! min_partitions_i (+ min_partitions_i 1)))) (throw (ex-info "return" {:v (nth min_partitions_cut (- min_partitions_n 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (min_partitions "aab"))
      (println (min_partitions "aaa"))
      (println (min_partitions "ababbbabbababa"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
