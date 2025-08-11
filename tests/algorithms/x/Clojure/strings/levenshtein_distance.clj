(ns main (:refer-clojure :exclude [range_list min3 levenshtein_distance levenshtein_distance_optimized main]))

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

(declare range_list min3 levenshtein_distance levenshtein_distance_optimized main)

(def ^:dynamic levenshtein_distance_c1 nil)

(def ^:dynamic levenshtein_distance_c2 nil)

(def ^:dynamic levenshtein_distance_current_row nil)

(def ^:dynamic levenshtein_distance_deletions nil)

(def ^:dynamic levenshtein_distance_i nil)

(def ^:dynamic levenshtein_distance_insertions nil)

(def ^:dynamic levenshtein_distance_j nil)

(def ^:dynamic levenshtein_distance_min_val nil)

(def ^:dynamic levenshtein_distance_optimized_c1 nil)

(def ^:dynamic levenshtein_distance_optimized_c2 nil)

(def ^:dynamic levenshtein_distance_optimized_current_row nil)

(def ^:dynamic levenshtein_distance_optimized_deletions nil)

(def ^:dynamic levenshtein_distance_optimized_i nil)

(def ^:dynamic levenshtein_distance_optimized_insertions nil)

(def ^:dynamic levenshtein_distance_optimized_j nil)

(def ^:dynamic levenshtein_distance_optimized_k nil)

(def ^:dynamic levenshtein_distance_optimized_min_val nil)

(def ^:dynamic levenshtein_distance_optimized_previous_row nil)

(def ^:dynamic levenshtein_distance_optimized_substitutions nil)

(def ^:dynamic levenshtein_distance_previous_row nil)

(def ^:dynamic levenshtein_distance_substitutions nil)

(def ^:dynamic main_a nil)

(def ^:dynamic main_b nil)

(def ^:dynamic min3_m nil)

(def ^:dynamic range_list_i nil)

(def ^:dynamic range_list_lst nil)

(defn range_list [range_list_n]
  (binding [range_list_i nil range_list_lst nil] (try (do (set! range_list_lst []) (set! range_list_i 0) (while (< range_list_i range_list_n) (do (set! range_list_lst (conj range_list_lst range_list_i)) (set! range_list_i (+ range_list_i 1)))) (throw (ex-info "return" {:v range_list_lst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn min3 [min3_a min3_b min3_c]
  (binding [min3_m nil] (try (do (set! min3_m min3_a) (when (< min3_b min3_m) (set! min3_m min3_b)) (when (< min3_c min3_m) (set! min3_m min3_c)) (throw (ex-info "return" {:v min3_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn levenshtein_distance [levenshtein_distance_first_word levenshtein_distance_second_word]
  (binding [levenshtein_distance_c1 nil levenshtein_distance_c2 nil levenshtein_distance_current_row nil levenshtein_distance_deletions nil levenshtein_distance_i nil levenshtein_distance_insertions nil levenshtein_distance_j nil levenshtein_distance_min_val nil levenshtein_distance_previous_row nil levenshtein_distance_substitutions nil] (try (do (when (< (count levenshtein_distance_first_word) (count levenshtein_distance_second_word)) (throw (ex-info "return" {:v (levenshtein_distance levenshtein_distance_second_word levenshtein_distance_first_word)}))) (when (= (count levenshtein_distance_second_word) 0) (throw (ex-info "return" {:v (count levenshtein_distance_first_word)}))) (set! levenshtein_distance_previous_row (range_list (+ (count levenshtein_distance_second_word) 1))) (set! levenshtein_distance_i 0) (while (< levenshtein_distance_i (count levenshtein_distance_first_word)) (do (set! levenshtein_distance_c1 (subs levenshtein_distance_first_word levenshtein_distance_i (+ levenshtein_distance_i 1))) (set! levenshtein_distance_current_row []) (set! levenshtein_distance_current_row (conj levenshtein_distance_current_row (+ levenshtein_distance_i 1))) (set! levenshtein_distance_j 0) (while (< levenshtein_distance_j (count levenshtein_distance_second_word)) (do (set! levenshtein_distance_c2 (subs levenshtein_distance_second_word levenshtein_distance_j (+ levenshtein_distance_j 1))) (set! levenshtein_distance_insertions (+ (nth levenshtein_distance_previous_row (+ levenshtein_distance_j 1)) 1)) (set! levenshtein_distance_deletions (+ (nth levenshtein_distance_current_row levenshtein_distance_j) 1)) (set! levenshtein_distance_substitutions (+ (nth levenshtein_distance_previous_row levenshtein_distance_j) (if (= levenshtein_distance_c1 levenshtein_distance_c2) 0 1))) (set! levenshtein_distance_min_val (min3 levenshtein_distance_insertions levenshtein_distance_deletions levenshtein_distance_substitutions)) (set! levenshtein_distance_current_row (conj levenshtein_distance_current_row levenshtein_distance_min_val)) (set! levenshtein_distance_j (+ levenshtein_distance_j 1)))) (set! levenshtein_distance_previous_row levenshtein_distance_current_row) (set! levenshtein_distance_i (+ levenshtein_distance_i 1)))) (throw (ex-info "return" {:v (nth levenshtein_distance_previous_row (- (count levenshtein_distance_previous_row) 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn levenshtein_distance_optimized [levenshtein_distance_optimized_first_word levenshtein_distance_optimized_second_word]
  (binding [levenshtein_distance_optimized_c1 nil levenshtein_distance_optimized_c2 nil levenshtein_distance_optimized_current_row nil levenshtein_distance_optimized_deletions nil levenshtein_distance_optimized_i nil levenshtein_distance_optimized_insertions nil levenshtein_distance_optimized_j nil levenshtein_distance_optimized_k nil levenshtein_distance_optimized_min_val nil levenshtein_distance_optimized_previous_row nil levenshtein_distance_optimized_substitutions nil] (try (do (when (< (count levenshtein_distance_optimized_first_word) (count levenshtein_distance_optimized_second_word)) (throw (ex-info "return" {:v (levenshtein_distance_optimized levenshtein_distance_optimized_second_word levenshtein_distance_optimized_first_word)}))) (when (= (count levenshtein_distance_optimized_second_word) 0) (throw (ex-info "return" {:v (count levenshtein_distance_optimized_first_word)}))) (set! levenshtein_distance_optimized_previous_row (range_list (+ (count levenshtein_distance_optimized_second_word) 1))) (set! levenshtein_distance_optimized_i 0) (while (< levenshtein_distance_optimized_i (count levenshtein_distance_optimized_first_word)) (do (set! levenshtein_distance_optimized_c1 (subs levenshtein_distance_optimized_first_word levenshtein_distance_optimized_i (+ levenshtein_distance_optimized_i 1))) (set! levenshtein_distance_optimized_current_row []) (set! levenshtein_distance_optimized_current_row (conj levenshtein_distance_optimized_current_row (+ levenshtein_distance_optimized_i 1))) (set! levenshtein_distance_optimized_k 0) (while (< levenshtein_distance_optimized_k (count levenshtein_distance_optimized_second_word)) (do (set! levenshtein_distance_optimized_current_row (conj levenshtein_distance_optimized_current_row 0)) (set! levenshtein_distance_optimized_k (+ levenshtein_distance_optimized_k 1)))) (set! levenshtein_distance_optimized_j 0) (while (< levenshtein_distance_optimized_j (count levenshtein_distance_optimized_second_word)) (do (set! levenshtein_distance_optimized_c2 (subs levenshtein_distance_optimized_second_word levenshtein_distance_optimized_j (+ levenshtein_distance_optimized_j 1))) (set! levenshtein_distance_optimized_insertions (+ (nth levenshtein_distance_optimized_previous_row (+ levenshtein_distance_optimized_j 1)) 1)) (set! levenshtein_distance_optimized_deletions (+ (nth levenshtein_distance_optimized_current_row levenshtein_distance_optimized_j) 1)) (set! levenshtein_distance_optimized_substitutions (+ (nth levenshtein_distance_optimized_previous_row levenshtein_distance_optimized_j) (if (= levenshtein_distance_optimized_c1 levenshtein_distance_optimized_c2) 0 1))) (set! levenshtein_distance_optimized_min_val (min3 levenshtein_distance_optimized_insertions levenshtein_distance_optimized_deletions levenshtein_distance_optimized_substitutions)) (set! levenshtein_distance_optimized_current_row (assoc levenshtein_distance_optimized_current_row (+ levenshtein_distance_optimized_j 1) levenshtein_distance_optimized_min_val)) (set! levenshtein_distance_optimized_j (+ levenshtein_distance_optimized_j 1)))) (set! levenshtein_distance_optimized_previous_row levenshtein_distance_optimized_current_row) (set! levenshtein_distance_optimized_i (+ levenshtein_distance_optimized_i 1)))) (throw (ex-info "return" {:v (nth levenshtein_distance_optimized_previous_row (- (count levenshtein_distance_optimized_previous_row) 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_a nil main_b nil] (do (set! main_a "kitten") (set! main_b "sitting") (println (str (levenshtein_distance main_a main_b))) (println (str (levenshtein_distance_optimized main_a main_b))))))

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
