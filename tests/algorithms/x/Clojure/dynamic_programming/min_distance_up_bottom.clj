(ns main (:refer-clojure :exclude [min3 helper min_distance_up_bottom]))

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

(declare min3 helper min_distance_up_bottom)

(def ^:dynamic helper_cache nil)

(def ^:dynamic helper_delete_cost nil)

(def ^:dynamic helper_diff nil)

(def ^:dynamic helper_insert_cost nil)

(def ^:dynamic helper_replace_cost nil)

(def ^:dynamic min3_m nil)

(def ^:dynamic min_distance_up_bottom_cache nil)

(def ^:dynamic min_distance_up_bottom_len1 nil)

(def ^:dynamic min_distance_up_bottom_len2 nil)

(def ^:dynamic min_distance_up_bottom_row nil)

(defn min3 [min3_a min3_b min3_c]
  (binding [min3_m nil] (try (do (set! min3_m min3_a) (when (< min3_b min3_m) (set! min3_m min3_b)) (when (< min3_c min3_m) (set! min3_m min3_c)) (throw (ex-info "return" {:v min3_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn helper [helper_word1 helper_word2 helper_cache_p helper_i helper_j helper_len1 helper_len2]
  (binding [helper_cache nil helper_delete_cost nil helper_diff nil helper_insert_cost nil helper_replace_cost nil] (try (do (set! helper_cache helper_cache_p) (when (>= helper_i helper_len1) (throw (ex-info "return" {:v (- helper_len2 helper_j)}))) (when (>= helper_j helper_len2) (throw (ex-info "return" {:v (- helper_len1 helper_i)}))) (when (not= (nth (nth helper_cache helper_i) helper_j) (- 0 1)) (throw (ex-info "return" {:v (nth (nth helper_cache helper_i) helper_j)}))) (set! helper_diff 0) (when (not= (subs helper_word1 helper_i (min (+ helper_i 1) (count helper_word1))) (subs helper_word2 helper_j (min (+ helper_j 1) (count helper_word2)))) (set! helper_diff 1)) (set! helper_delete_cost (+ 1 (helper helper_word1 helper_word2 helper_cache (+ helper_i 1) helper_j helper_len1 helper_len2))) (set! helper_insert_cost (+ 1 (helper helper_word1 helper_word2 helper_cache helper_i (+ helper_j 1) helper_len1 helper_len2))) (set! helper_replace_cost (+ helper_diff (helper helper_word1 helper_word2 helper_cache (+ helper_i 1) (+ helper_j 1) helper_len1 helper_len2))) (set! helper_cache (assoc-in helper_cache [helper_i helper_j] (min3 helper_delete_cost helper_insert_cost helper_replace_cost))) (throw (ex-info "return" {:v (nth (nth helper_cache helper_i) helper_j)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn min_distance_up_bottom [min_distance_up_bottom_word1 min_distance_up_bottom_word2]
  (binding [min_distance_up_bottom_cache nil min_distance_up_bottom_len1 nil min_distance_up_bottom_len2 nil min_distance_up_bottom_row nil] (try (do (set! min_distance_up_bottom_len1 (count min_distance_up_bottom_word1)) (set! min_distance_up_bottom_len2 (count min_distance_up_bottom_word2)) (set! min_distance_up_bottom_cache []) (dotimes [_ min_distance_up_bottom_len1] (do (set! min_distance_up_bottom_row []) (dotimes [_2 min_distance_up_bottom_len2] (set! min_distance_up_bottom_row (conj min_distance_up_bottom_row (- 0 1)))) (set! min_distance_up_bottom_cache (conj min_distance_up_bottom_cache min_distance_up_bottom_row)))) (throw (ex-info "return" {:v (helper min_distance_up_bottom_word1 min_distance_up_bottom_word2 min_distance_up_bottom_cache 0 0 min_distance_up_bottom_len1 min_distance_up_bottom_len2)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (min_distance_up_bottom "intention" "execution")))
      (println (str (min_distance_up_bottom "intention" "")))
      (println (str (min_distance_up_bottom "" "")))
      (println (str (min_distance_up_bottom "zooicoarchaeologist" "zoologist")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
