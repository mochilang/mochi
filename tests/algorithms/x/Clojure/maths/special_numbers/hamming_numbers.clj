(ns main (:refer-clojure :exclude [hamming]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare hamming)

(def ^:dynamic hamming_hamming_list nil)

(def ^:dynamic hamming_i nil)

(def ^:dynamic hamming_index nil)

(def ^:dynamic hamming_j nil)

(def ^:dynamic hamming_k nil)

(def ^:dynamic hamming_m1 nil)

(def ^:dynamic hamming_m2 nil)

(def ^:dynamic hamming_m3 nil)

(def ^:dynamic next_v nil)

(defn hamming [hamming_n]
  (binding [hamming_hamming_list nil hamming_i nil hamming_index nil hamming_j nil hamming_k nil hamming_m1 nil hamming_m2 nil hamming_m3 nil next_v nil] (try (do (when (< hamming_n 1) (throw (Exception. "n_element should be a positive number"))) (set! hamming_hamming_list [1]) (set! hamming_i 0) (set! hamming_j 0) (set! hamming_k 0) (set! hamming_index 1) (while (< hamming_index hamming_n) (do (while (<= (* (nth hamming_hamming_list hamming_i) 2) (nth hamming_hamming_list (- (count hamming_hamming_list) 1))) (set! hamming_i (+ hamming_i 1))) (while (<= (* (nth hamming_hamming_list hamming_j) 3) (nth hamming_hamming_list (- (count hamming_hamming_list) 1))) (set! hamming_j (+ hamming_j 1))) (while (<= (* (nth hamming_hamming_list hamming_k) 5) (nth hamming_hamming_list (- (count hamming_hamming_list) 1))) (set! hamming_k (+ hamming_k 1))) (set! hamming_m1 (* (nth hamming_hamming_list hamming_i) 2)) (set! hamming_m2 (* (nth hamming_hamming_list hamming_j) 3)) (set! hamming_m3 (* (nth hamming_hamming_list hamming_k) 5)) (set! next_v hamming_m1) (when (< hamming_m2 next_v) (set! next_v hamming_m2)) (when (< hamming_m3 next_v) (set! next_v hamming_m3)) (set! hamming_hamming_list (conj hamming_hamming_list next_v)) (set! hamming_index (+ hamming_index 1)))) (throw (ex-info "return" {:v hamming_hamming_list}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (hamming 5))
      (println (hamming 10))
      (println (hamming 15))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
