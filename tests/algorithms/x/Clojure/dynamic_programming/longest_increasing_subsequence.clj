(ns main (:refer-clojure :exclude [longest_subsequence]))

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

(declare longest_subsequence)

(def ^:dynamic longest_subsequence_candidate nil)

(def ^:dynamic longest_subsequence_filtered nil)

(def ^:dynamic longest_subsequence_i nil)

(def ^:dynamic longest_subsequence_is_found nil)

(def ^:dynamic longest_subsequence_j nil)

(def ^:dynamic longest_subsequence_longest_subseq nil)

(def ^:dynamic longest_subsequence_n nil)

(def ^:dynamic longest_subsequence_pivot nil)

(def ^:dynamic longest_subsequence_temp_array nil)

(defn longest_subsequence [longest_subsequence_xs]
  (binding [longest_subsequence_candidate nil longest_subsequence_filtered nil longest_subsequence_i nil longest_subsequence_is_found nil longest_subsequence_j nil longest_subsequence_longest_subseq nil longest_subsequence_n nil longest_subsequence_pivot nil longest_subsequence_temp_array nil] (try (do (set! longest_subsequence_n (count longest_subsequence_xs)) (when (<= longest_subsequence_n 1) (throw (ex-info "return" {:v longest_subsequence_xs}))) (set! longest_subsequence_pivot (nth longest_subsequence_xs 0)) (set! longest_subsequence_is_found false) (set! longest_subsequence_i 1) (set! longest_subsequence_longest_subseq []) (while (and (not longest_subsequence_is_found) (< longest_subsequence_i longest_subsequence_n)) (if (< (nth longest_subsequence_xs longest_subsequence_i) longest_subsequence_pivot) (do (set! longest_subsequence_is_found true) (set! longest_subsequence_temp_array (subvec longest_subsequence_xs longest_subsequence_i (min longest_subsequence_n (count longest_subsequence_xs)))) (set! longest_subsequence_temp_array (longest_subsequence longest_subsequence_temp_array)) (when (> (count longest_subsequence_temp_array) (count longest_subsequence_longest_subseq)) (set! longest_subsequence_longest_subseq longest_subsequence_temp_array))) (set! longest_subsequence_i (+ longest_subsequence_i 1)))) (set! longest_subsequence_filtered []) (set! longest_subsequence_j 1) (while (< longest_subsequence_j longest_subsequence_n) (do (when (>= (nth longest_subsequence_xs longest_subsequence_j) longest_subsequence_pivot) (set! longest_subsequence_filtered (conj longest_subsequence_filtered (nth longest_subsequence_xs longest_subsequence_j)))) (set! longest_subsequence_j (+ longest_subsequence_j 1)))) (set! longest_subsequence_candidate []) (set! longest_subsequence_candidate (conj longest_subsequence_candidate longest_subsequence_pivot)) (set! longest_subsequence_candidate (concat longest_subsequence_candidate (longest_subsequence longest_subsequence_filtered))) (if (> (count longest_subsequence_candidate) (count longest_subsequence_longest_subseq)) (throw (ex-info "return" {:v longest_subsequence_candidate})) (throw (ex-info "return" {:v longest_subsequence_longest_subseq})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
