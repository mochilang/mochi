(ns main (:refer-clojure :exclude [circle_sort_util circle_sort]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare circle_sort_util circle_sort)

(def ^:dynamic circle_sort_is_not_sorted nil)

(def ^:dynamic circle_sort_util_collection nil)

(def ^:dynamic circle_sort_util_left nil)

(def ^:dynamic circle_sort_util_left_swap nil)

(def ^:dynamic circle_sort_util_mid nil)

(def ^:dynamic circle_sort_util_right nil)

(def ^:dynamic circle_sort_util_right_swap nil)

(def ^:dynamic circle_sort_util_swapped nil)

(def ^:dynamic circle_sort_util_tmp nil)

(def ^:dynamic circle_sort_util_tmp2 nil)

(defn circle_sort_util [circle_sort_util_collection_p circle_sort_util_low circle_sort_util_high]
  (binding [circle_sort_util_collection nil circle_sort_util_left nil circle_sort_util_left_swap nil circle_sort_util_mid nil circle_sort_util_right nil circle_sort_util_right_swap nil circle_sort_util_swapped nil circle_sort_util_tmp nil circle_sort_util_tmp2 nil] (try (do (set! circle_sort_util_collection circle_sort_util_collection_p) (set! circle_sort_util_swapped false) (when (= circle_sort_util_low circle_sort_util_high) (throw (ex-info "return" {:v circle_sort_util_swapped}))) (set! circle_sort_util_left circle_sort_util_low) (set! circle_sort_util_right circle_sort_util_high) (while (< circle_sort_util_left circle_sort_util_right) (do (when (> (nth circle_sort_util_collection circle_sort_util_left) (nth circle_sort_util_collection circle_sort_util_right)) (do (set! circle_sort_util_tmp (nth circle_sort_util_collection circle_sort_util_left)) (set! circle_sort_util_collection (assoc circle_sort_util_collection circle_sort_util_left (nth circle_sort_util_collection circle_sort_util_right))) (set! circle_sort_util_collection (assoc circle_sort_util_collection circle_sort_util_right circle_sort_util_tmp)) (set! circle_sort_util_swapped true))) (set! circle_sort_util_left (+ circle_sort_util_left 1)) (set! circle_sort_util_right (- circle_sort_util_right 1)))) (when (and (= circle_sort_util_left circle_sort_util_right) (> (nth circle_sort_util_collection circle_sort_util_left) (nth circle_sort_util_collection (+ circle_sort_util_right 1)))) (do (set! circle_sort_util_tmp2 (nth circle_sort_util_collection circle_sort_util_left)) (set! circle_sort_util_collection (assoc circle_sort_util_collection circle_sort_util_left (nth circle_sort_util_collection (+ circle_sort_util_right 1)))) (set! circle_sort_util_collection (assoc circle_sort_util_collection (+ circle_sort_util_right 1) circle_sort_util_tmp2)) (set! circle_sort_util_swapped true))) (set! circle_sort_util_mid (+ circle_sort_util_low (/ (- circle_sort_util_high circle_sort_util_low) 2))) (set! circle_sort_util_left_swap (circle_sort_util circle_sort_util_collection circle_sort_util_low circle_sort_util_mid)) (set! circle_sort_util_right_swap (circle_sort_util circle_sort_util_collection (+ circle_sort_util_mid 1) circle_sort_util_high)) (if (or (or circle_sort_util_swapped circle_sort_util_left_swap) circle_sort_util_right_swap) (throw (ex-info "return" {:v true})) (throw (ex-info "return" {:v false})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn circle_sort [circle_sort_collection]
  (binding [circle_sort_is_not_sorted nil] (try (do (when (< (count circle_sort_collection) 2) (throw (ex-info "return" {:v circle_sort_collection}))) (set! circle_sort_is_not_sorted true) (while circle_sort_is_not_sorted (set! circle_sort_is_not_sorted (circle_sort_util circle_sort_collection 0 (- (count circle_sort_collection) 1)))) (throw (ex-info "return" {:v circle_sort_collection}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (circle_sort [0 5 3 2 2])))
      (println (str (circle_sort [])))
      (println (str (circle_sort [(- 2) 5 0 (- 45)])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
