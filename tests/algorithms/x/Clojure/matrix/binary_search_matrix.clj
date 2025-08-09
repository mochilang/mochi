(ns main (:refer-clojure :exclude [binary_search mat_bin_search main]))

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

(declare binary_search mat_bin_search main)

(def ^:dynamic binary_search_r nil)

(def ^:dynamic main_matrix nil)

(def ^:dynamic main_row nil)

(def ^:dynamic mat_bin_search_index nil)

(def ^:dynamic mat_bin_search_r nil)

(defn binary_search [binary_search_arr binary_search_lower_bound binary_search_upper_bound binary_search_value]
  (binding [binary_search_r nil] (try (do (set! binary_search_r (quot (+ binary_search_lower_bound binary_search_upper_bound) 2)) (when (= (nth binary_search_arr binary_search_r) binary_search_value) (throw (ex-info "return" {:v binary_search_r}))) (when (>= binary_search_lower_bound binary_search_upper_bound) (throw (ex-info "return" {:v (- 1)}))) (if (< (nth binary_search_arr binary_search_r) binary_search_value) (binary_search binary_search_arr (+ binary_search_r 1) binary_search_upper_bound binary_search_value) (binary_search binary_search_arr binary_search_lower_bound (- binary_search_r 1) binary_search_value))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mat_bin_search [mat_bin_search_value mat_bin_search_matrix]
  (binding [mat_bin_search_index nil mat_bin_search_r nil] (try (do (set! mat_bin_search_index 0) (when (= (nth (nth mat_bin_search_matrix mat_bin_search_index) 0) mat_bin_search_value) (throw (ex-info "return" {:v [mat_bin_search_index 0]}))) (while (and (< mat_bin_search_index (count mat_bin_search_matrix)) (< (nth (nth mat_bin_search_matrix mat_bin_search_index) 0) mat_bin_search_value)) (do (set! mat_bin_search_r (binary_search (nth mat_bin_search_matrix mat_bin_search_index) 0 (- (count (nth mat_bin_search_matrix mat_bin_search_index)) 1) mat_bin_search_value)) (when (not= mat_bin_search_r (- 1)) (throw (ex-info "return" {:v [mat_bin_search_index mat_bin_search_r]}))) (set! mat_bin_search_index (+ mat_bin_search_index 1)))) (throw (ex-info "return" {:v [(- 1) (- 1)]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_matrix nil main_row nil] (do (set! main_row [1 4 7 11 15]) (println (str (binary_search main_row 0 (- (count main_row) 1) 1))) (println (str (binary_search main_row 0 (- (count main_row) 1) 23))) (set! main_matrix [[1 4 7 11 15] [2 5 8 12 19] [3 6 9 16 22] [10 13 14 17 24] [18 21 23 26 30]]) (println (str (mat_bin_search 1 main_matrix))) (println (str (mat_bin_search 34 main_matrix))))))

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
