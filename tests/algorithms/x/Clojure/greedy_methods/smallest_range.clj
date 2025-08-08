(ns main (:refer-clojure :exclude [smallest_range list_to_string main]))

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

(declare smallest_range list_to_string main)

(def ^:dynamic list_to_string_i nil)

(def ^:dynamic list_to_string_s nil)

(def ^:dynamic main_result1 nil)

(def ^:dynamic main_result2 nil)

(def ^:dynamic smallest_range_best nil)

(def ^:dynamic smallest_range_current_max nil)

(def ^:dynamic smallest_range_current_min nil)

(def ^:dynamic smallest_range_first_val nil)

(def ^:dynamic smallest_range_heap nil)

(def ^:dynamic smallest_range_hj nil)

(def ^:dynamic smallest_range_hmin nil)

(def ^:dynamic smallest_range_i nil)

(def ^:dynamic smallest_range_item nil)

(def ^:dynamic smallest_range_j nil)

(def ^:dynamic smallest_range_k nil)

(def ^:dynamic smallest_range_min_idx nil)

(def ^:dynamic smallest_range_new_heap nil)

(def ^:dynamic smallest_range_next_val nil)

(def ^:dynamic main_INF 1000000000)

(defn smallest_range [smallest_range_nums]
  (binding [smallest_range_best nil smallest_range_current_max nil smallest_range_current_min nil smallest_range_first_val nil smallest_range_heap nil smallest_range_hj nil smallest_range_hmin nil smallest_range_i nil smallest_range_item nil smallest_range_j nil smallest_range_k nil smallest_range_min_idx nil smallest_range_new_heap nil smallest_range_next_val nil] (try (do (set! smallest_range_heap []) (set! smallest_range_current_max (- main_INF)) (set! smallest_range_i 0) (while (< smallest_range_i (count smallest_range_nums)) (do (set! smallest_range_first_val (nth (nth smallest_range_nums smallest_range_i) 0)) (set! smallest_range_heap (conj smallest_range_heap {:elem_idx 0 :list_idx smallest_range_i :value smallest_range_first_val})) (when (> smallest_range_first_val smallest_range_current_max) (set! smallest_range_current_max smallest_range_first_val)) (set! smallest_range_i (+ smallest_range_i 1)))) (set! smallest_range_best [(- main_INF) main_INF]) (loop [while_flag_1 true] (when (and while_flag_1 (> (count smallest_range_heap) 0)) (do (set! smallest_range_min_idx 0) (set! smallest_range_j 1) (while (< smallest_range_j (count smallest_range_heap)) (do (set! smallest_range_hj (nth smallest_range_heap smallest_range_j)) (set! smallest_range_hmin (nth smallest_range_heap smallest_range_min_idx)) (when (< (:value smallest_range_hj) (:value smallest_range_hmin)) (set! smallest_range_min_idx smallest_range_j)) (set! smallest_range_j (+ smallest_range_j 1)))) (set! smallest_range_item (nth smallest_range_heap smallest_range_min_idx)) (set! smallest_range_new_heap []) (set! smallest_range_k 0) (while (< smallest_range_k (count smallest_range_heap)) (do (when (not= smallest_range_k smallest_range_min_idx) (set! smallest_range_new_heap (conj smallest_range_new_heap (nth smallest_range_heap smallest_range_k)))) (set! smallest_range_k (+ smallest_range_k 1)))) (set! smallest_range_heap smallest_range_new_heap) (set! smallest_range_current_min (:value smallest_range_item)) (when (< (- smallest_range_current_max smallest_range_current_min) (- (nth smallest_range_best 1) (nth smallest_range_best 0))) (set! smallest_range_best [smallest_range_current_min smallest_range_current_max])) (cond (= (:elem_idx smallest_range_item) (- (count (nth smallest_range_nums (:list_idx smallest_range_item))) 1)) (recur false) :else (do (set! smallest_range_next_val (nth (nth smallest_range_nums (:list_idx smallest_range_item)) (+ (:elem_idx smallest_range_item) 1))) (set! smallest_range_heap (conj smallest_range_heap {:elem_idx (+ (:elem_idx smallest_range_item) 1) :list_idx (:list_idx smallest_range_item) :value smallest_range_next_val})) (when (> smallest_range_next_val smallest_range_current_max) (set! smallest_range_current_max smallest_range_next_val)) (recur while_flag_1)))))) (throw (ex-info "return" {:v smallest_range_best}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_to_string [list_to_string_arr]
  (binding [list_to_string_i nil list_to_string_s nil] (try (do (set! list_to_string_s "[") (set! list_to_string_i 0) (while (< list_to_string_i (count list_to_string_arr)) (do (set! list_to_string_s (str list_to_string_s (str (nth list_to_string_arr list_to_string_i)))) (when (< list_to_string_i (- (count list_to_string_arr) 1)) (set! list_to_string_s (str list_to_string_s ", "))) (set! list_to_string_i (+ list_to_string_i 1)))) (throw (ex-info "return" {:v (str list_to_string_s "]")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_result1 nil main_result2 nil] (do (set! main_result1 (smallest_range [[4 10 15 24 26] [0 9 12 20] [5 18 22 30]])) (println (list_to_string main_result1)) (set! main_result2 (smallest_range [[1 2 3] [1 2 3] [1 2 3]])) (println (list_to_string main_result2)))))

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
