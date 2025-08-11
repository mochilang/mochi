(ns main (:refer-clojure :exclude [merge iter_merge_sort list_to_string]))

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

(declare merge iter_merge_sort list_to_string)

(def ^:dynamic iter_merge_sort_arr nil)

(def ^:dynamic iter_merge_sort_high nil)

(def ^:dynamic iter_merge_sort_i nil)

(def ^:dynamic iter_merge_sort_low nil)

(def ^:dynamic iter_merge_sort_mid nil)

(def ^:dynamic iter_merge_sort_mid2 nil)

(def ^:dynamic iter_merge_sort_n nil)

(def ^:dynamic iter_merge_sort_p nil)

(def ^:dynamic list_to_string_i nil)

(def ^:dynamic list_to_string_s nil)

(def ^:dynamic merge_a nil)

(def ^:dynamic merge_i nil)

(def ^:dynamic merge_left nil)

(def ^:dynamic merge_result nil)

(def ^:dynamic merge_right nil)

(defn merge [merge_a_p merge_low merge_mid merge_high]
  (binding [merge_a nil merge_i nil merge_left nil merge_result nil merge_right nil] (try (do (set! merge_a merge_a_p) (set! merge_left (subvec merge_a merge_low merge_mid)) (set! merge_right (subvec merge_a merge_mid (+ merge_high 1))) (set! merge_result []) (while (and (> (count merge_left) 0) (> (count merge_right) 0)) (if (<= (nth merge_left 0) (nth merge_right 0)) (do (set! merge_result (conj merge_result (nth merge_left 0))) (set! merge_left (subvec merge_left 1 (count merge_left)))) (do (set! merge_result (conj merge_result (nth merge_right 0))) (set! merge_right (subvec merge_right 1 (count merge_right)))))) (set! merge_i 0) (while (< merge_i (count merge_left)) (do (set! merge_result (conj merge_result (nth merge_left merge_i))) (set! merge_i (+ merge_i 1)))) (set! merge_i 0) (while (< merge_i (count merge_right)) (do (set! merge_result (conj merge_result (nth merge_right merge_i))) (set! merge_i (+ merge_i 1)))) (set! merge_i 0) (while (< merge_i (count merge_result)) (do (set! merge_a (assoc merge_a (+ merge_low merge_i) (nth merge_result merge_i))) (set! merge_i (+ merge_i 1)))) (throw (ex-info "return" {:v merge_a}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn iter_merge_sort [iter_merge_sort_items]
  (binding [iter_merge_sort_arr nil iter_merge_sort_high nil iter_merge_sort_i nil iter_merge_sort_low nil iter_merge_sort_mid nil iter_merge_sort_mid2 nil iter_merge_sort_n nil iter_merge_sort_p nil] (try (do (set! iter_merge_sort_n (count iter_merge_sort_items)) (when (<= iter_merge_sort_n 1) (throw (ex-info "return" {:v iter_merge_sort_items}))) (set! iter_merge_sort_arr (subvec iter_merge_sort_items 0 (count iter_merge_sort_items))) (set! iter_merge_sort_p 2) (loop [while_flag_1 true] (when (and while_flag_1 (<= iter_merge_sort_p iter_merge_sort_n)) (do (set! iter_merge_sort_i 0) (while (< iter_merge_sort_i iter_merge_sort_n) (do (set! iter_merge_sort_high (- (+ iter_merge_sort_i iter_merge_sort_p) 1)) (when (>= iter_merge_sort_high iter_merge_sort_n) (set! iter_merge_sort_high (- iter_merge_sort_n 1))) (set! iter_merge_sort_low iter_merge_sort_i) (set! iter_merge_sort_mid (/ (+ (+ iter_merge_sort_low iter_merge_sort_high) 1) 2)) (set! iter_merge_sort_arr (merge iter_merge_sort_arr iter_merge_sort_low iter_merge_sort_mid iter_merge_sort_high)) (set! iter_merge_sort_i (+ iter_merge_sort_i iter_merge_sort_p)))) (cond (>= (* iter_merge_sort_p 2) iter_merge_sort_n) (do (set! iter_merge_sort_mid2 (- iter_merge_sort_i iter_merge_sort_p)) (set! iter_merge_sort_arr (merge iter_merge_sort_arr 0 iter_merge_sort_mid2 (- iter_merge_sort_n 1))) (recur false)) :else (do (set! iter_merge_sort_p (* iter_merge_sort_p 2)) (recur while_flag_1)))))) (throw (ex-info "return" {:v iter_merge_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_to_string [list_to_string_arr]
  (binding [list_to_string_i nil list_to_string_s nil] (try (do (set! list_to_string_s "[") (set! list_to_string_i 0) (while (< list_to_string_i (count list_to_string_arr)) (do (set! list_to_string_s (str list_to_string_s (str (nth list_to_string_arr list_to_string_i)))) (when (< list_to_string_i (- (count list_to_string_arr) 1)) (set! list_to_string_s (str list_to_string_s ", "))) (set! list_to_string_i (+ list_to_string_i 1)))) (throw (ex-info "return" {:v (str list_to_string_s "]")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (list_to_string (iter_merge_sort [5 9 8 7 1 2 7])))
      (println (list_to_string (iter_merge_sort [1])))
      (println (list_to_string (iter_merge_sort [2 1])))
      (println (list_to_string (iter_merge_sort [4 3 2 1])))
      (println (list_to_string (iter_merge_sort [5 4 3 2 1])))
      (println (list_to_string (iter_merge_sort [(- 2) (- 9) (- 1) (- 4)])))
      (println (list_to_string (iter_merge_sort [])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
