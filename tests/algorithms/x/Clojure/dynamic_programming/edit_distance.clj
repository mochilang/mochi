(ns main (:refer-clojure :exclude [min3 helper_top_down min_dist_top_down min_dist_bottom_up]))

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

(declare min3 helper_top_down min_dist_top_down min_dist_bottom_up)

(def ^:dynamic helper_top_down_delete nil)

(def ^:dynamic helper_top_down_dp nil)

(def ^:dynamic helper_top_down_insert nil)

(def ^:dynamic helper_top_down_replace nil)

(def ^:dynamic min3_m nil)

(def ^:dynamic min_dist_bottom_up_delete nil)

(def ^:dynamic min_dist_bottom_up_dp nil)

(def ^:dynamic min_dist_bottom_up_insert nil)

(def ^:dynamic min_dist_bottom_up_m nil)

(def ^:dynamic min_dist_bottom_up_n nil)

(def ^:dynamic min_dist_bottom_up_replace nil)

(def ^:dynamic min_dist_bottom_up_row nil)

(def ^:dynamic min_dist_top_down_dp nil)

(def ^:dynamic min_dist_top_down_m nil)

(def ^:dynamic min_dist_top_down_n nil)

(def ^:dynamic min_dist_top_down_row nil)

(defn min3 [min3_a min3_b min3_c]
  (binding [min3_m nil] (try (do (set! min3_m min3_a) (when (< min3_b min3_m) (set! min3_m min3_b)) (when (< min3_c min3_m) (set! min3_m min3_c)) (throw (ex-info "return" {:v min3_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn helper_top_down [helper_top_down_word1 helper_top_down_word2 helper_top_down_dp_p helper_top_down_i helper_top_down_j]
  (binding [helper_top_down_delete nil helper_top_down_dp nil helper_top_down_insert nil helper_top_down_replace nil] (try (do (set! helper_top_down_dp helper_top_down_dp_p) (when (< helper_top_down_i 0) (throw (ex-info "return" {:v (+ helper_top_down_j 1)}))) (when (< helper_top_down_j 0) (throw (ex-info "return" {:v (+ helper_top_down_i 1)}))) (when (not= (nth (nth helper_top_down_dp helper_top_down_i) helper_top_down_j) (- 0 1)) (throw (ex-info "return" {:v (nth (nth helper_top_down_dp helper_top_down_i) helper_top_down_j)}))) (if (= (subs helper_top_down_word1 helper_top_down_i (min (+ helper_top_down_i 1) (count helper_top_down_word1))) (subs helper_top_down_word2 helper_top_down_j (min (+ helper_top_down_j 1) (count helper_top_down_word2)))) (set! helper_top_down_dp (assoc-in helper_top_down_dp [helper_top_down_i helper_top_down_j] (helper_top_down helper_top_down_word1 helper_top_down_word2 helper_top_down_dp (- helper_top_down_i 1) (- helper_top_down_j 1)))) (do (set! helper_top_down_insert (helper_top_down helper_top_down_word1 helper_top_down_word2 helper_top_down_dp helper_top_down_i (- helper_top_down_j 1))) (set! helper_top_down_delete (helper_top_down helper_top_down_word1 helper_top_down_word2 helper_top_down_dp (- helper_top_down_i 1) helper_top_down_j)) (set! helper_top_down_replace (helper_top_down helper_top_down_word1 helper_top_down_word2 helper_top_down_dp (- helper_top_down_i 1) (- helper_top_down_j 1))) (set! helper_top_down_dp (assoc-in helper_top_down_dp [helper_top_down_i helper_top_down_j] (+ 1 (min3 helper_top_down_insert helper_top_down_delete helper_top_down_replace)))))) (throw (ex-info "return" {:v (nth (nth helper_top_down_dp helper_top_down_i) helper_top_down_j)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn min_dist_top_down [min_dist_top_down_word1 min_dist_top_down_word2]
  (binding [min_dist_top_down_dp nil min_dist_top_down_m nil min_dist_top_down_n nil min_dist_top_down_row nil] (try (do (set! min_dist_top_down_m (count min_dist_top_down_word1)) (set! min_dist_top_down_n (count min_dist_top_down_word2)) (set! min_dist_top_down_dp []) (dotimes [_ min_dist_top_down_m] (do (set! min_dist_top_down_row []) (dotimes [_2 min_dist_top_down_n] (set! min_dist_top_down_row (conj min_dist_top_down_row (- 0 1)))) (set! min_dist_top_down_dp (conj min_dist_top_down_dp min_dist_top_down_row)))) (throw (ex-info "return" {:v (helper_top_down min_dist_top_down_word1 min_dist_top_down_word2 min_dist_top_down_dp (- min_dist_top_down_m 1) (- min_dist_top_down_n 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn min_dist_bottom_up [min_dist_bottom_up_word1 min_dist_bottom_up_word2]
  (binding [min_dist_bottom_up_delete nil min_dist_bottom_up_dp nil min_dist_bottom_up_insert nil min_dist_bottom_up_m nil min_dist_bottom_up_n nil min_dist_bottom_up_replace nil min_dist_bottom_up_row nil] (try (do (set! min_dist_bottom_up_m (count min_dist_bottom_up_word1)) (set! min_dist_bottom_up_n (count min_dist_bottom_up_word2)) (set! min_dist_bottom_up_dp []) (dotimes [_ (+ min_dist_bottom_up_m 1)] (do (set! min_dist_bottom_up_row []) (dotimes [_2 (+ min_dist_bottom_up_n 1)] (set! min_dist_bottom_up_row (conj min_dist_bottom_up_row 0))) (set! min_dist_bottom_up_dp (conj min_dist_bottom_up_dp min_dist_bottom_up_row)))) (dotimes [i (+ min_dist_bottom_up_m 1)] (dotimes [j (+ min_dist_bottom_up_n 1)] (if (= i 0) (set! min_dist_bottom_up_dp (assoc-in min_dist_bottom_up_dp [i j] j)) (if (= j 0) (set! min_dist_bottom_up_dp (assoc-in min_dist_bottom_up_dp [i j] i)) (if (= (subs min_dist_bottom_up_word1 (- i 1) (min i (count min_dist_bottom_up_word1))) (subs min_dist_bottom_up_word2 (- j 1) (min j (count min_dist_bottom_up_word2)))) (set! min_dist_bottom_up_dp (assoc-in min_dist_bottom_up_dp [i j] (nth (nth min_dist_bottom_up_dp (- i 1)) (- j 1)))) (do (set! min_dist_bottom_up_insert (nth (nth min_dist_bottom_up_dp i) (- j 1))) (set! min_dist_bottom_up_delete (nth (nth min_dist_bottom_up_dp (- i 1)) j)) (set! min_dist_bottom_up_replace (nth (nth min_dist_bottom_up_dp (- i 1)) (- j 1))) (set! min_dist_bottom_up_dp (assoc-in min_dist_bottom_up_dp [i j] (+ 1 (min3 min_dist_bottom_up_insert min_dist_bottom_up_delete min_dist_bottom_up_replace)))))))))) (throw (ex-info "return" {:v (nth (nth min_dist_bottom_up_dp min_dist_bottom_up_m) min_dist_bottom_up_n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (min_dist_top_down "intention" "execution")))
      (println (str (min_dist_top_down "intention" "")))
      (println (str (min_dist_top_down "" "")))
      (println (str (min_dist_bottom_up "intention" "execution")))
      (println (str (min_dist_bottom_up "intention" "")))
      (println (str (min_dist_bottom_up "" "")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
