(ns main (:refer-clojure :exclude [copy_list subset_combinations]))

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

(declare copy_list subset_combinations)

(def ^:dynamic copy_list_i nil)

(def ^:dynamic copy_list_result nil)

(def ^:dynamic subset_combinations_comb nil)

(def ^:dynamic subset_combinations_dp nil)

(def ^:dynamic subset_combinations_i nil)

(def ^:dynamic subset_combinations_j nil)

(def ^:dynamic subset_combinations_k nil)

(def ^:dynamic subset_combinations_prev nil)

(def ^:dynamic subset_combinations_prevs nil)

(def ^:dynamic subset_combinations_r nil)

(defn copy_list [copy_list_src]
  (binding [copy_list_i nil copy_list_result nil] (try (do (set! copy_list_result []) (set! copy_list_i 0) (while (< copy_list_i (count copy_list_src)) (do (set! copy_list_result (conj copy_list_result (nth copy_list_src copy_list_i))) (set! copy_list_i (+ copy_list_i 1)))) (throw (ex-info "return" {:v copy_list_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn subset_combinations [subset_combinations_elements subset_combinations_n]
  (binding [subset_combinations_comb nil subset_combinations_dp nil subset_combinations_i nil subset_combinations_j nil subset_combinations_k nil subset_combinations_prev nil subset_combinations_prevs nil subset_combinations_r nil] (try (do (set! subset_combinations_r (count subset_combinations_elements)) (when (> subset_combinations_n subset_combinations_r) (throw (ex-info "return" {:v []}))) (set! subset_combinations_dp []) (set! subset_combinations_i 0) (while (<= subset_combinations_i subset_combinations_r) (do (set! subset_combinations_dp (conj subset_combinations_dp [])) (set! subset_combinations_i (+ subset_combinations_i 1)))) (set! subset_combinations_dp (assoc subset_combinations_dp 0 (conj (nth subset_combinations_dp 0) []))) (set! subset_combinations_i 1) (while (<= subset_combinations_i subset_combinations_r) (do (set! subset_combinations_j subset_combinations_i) (while (> subset_combinations_j 0) (do (set! subset_combinations_prevs (nth subset_combinations_dp (- subset_combinations_j 1))) (set! subset_combinations_k 0) (while (< subset_combinations_k (count subset_combinations_prevs)) (do (set! subset_combinations_prev (nth subset_combinations_prevs subset_combinations_k)) (set! subset_combinations_comb (copy_list subset_combinations_prev)) (set! subset_combinations_comb (conj subset_combinations_comb (nth subset_combinations_elements (- subset_combinations_i 1)))) (set! subset_combinations_dp (assoc subset_combinations_dp subset_combinations_j (conj (nth subset_combinations_dp subset_combinations_j) subset_combinations_comb))) (set! subset_combinations_k (+ subset_combinations_k 1)))) (set! subset_combinations_j (- subset_combinations_j 1)))) (set! subset_combinations_i (+ subset_combinations_i 1)))) (throw (ex-info "return" {:v (nth subset_combinations_dp subset_combinations_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (subset_combinations [10 20 30 40] 2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
