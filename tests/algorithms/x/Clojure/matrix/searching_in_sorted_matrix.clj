(ns main (:refer-clojure :exclude [search_in_sorted_matrix main]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare search_in_sorted_matrix main)

(def ^:dynamic main_mat nil)

(def ^:dynamic main_mat2 nil)

(def ^:dynamic search_in_sorted_matrix_i nil)

(def ^:dynamic search_in_sorted_matrix_j nil)

(defn search_in_sorted_matrix [search_in_sorted_matrix_mat search_in_sorted_matrix_m search_in_sorted_matrix_n search_in_sorted_matrix_key]
  (binding [search_in_sorted_matrix_i nil search_in_sorted_matrix_j nil] (try (do (set! search_in_sorted_matrix_i (- search_in_sorted_matrix_m 1)) (set! search_in_sorted_matrix_j 0) (while (and (>= search_in_sorted_matrix_i 0) (< search_in_sorted_matrix_j search_in_sorted_matrix_n)) (do (when (= search_in_sorted_matrix_key (nth (nth search_in_sorted_matrix_mat search_in_sorted_matrix_i) search_in_sorted_matrix_j)) (do (println (str (str (str (str (str "Key " (mochi_str search_in_sorted_matrix_key)) " found at row- ") (mochi_str (+ search_in_sorted_matrix_i 1))) " column- ") (mochi_str (+ search_in_sorted_matrix_j 1)))) (throw (ex-info "return" {:v nil})))) (if (< search_in_sorted_matrix_key (nth (nth search_in_sorted_matrix_mat search_in_sorted_matrix_i) search_in_sorted_matrix_j)) (set! search_in_sorted_matrix_i (- search_in_sorted_matrix_i 1)) (set! search_in_sorted_matrix_j (+ search_in_sorted_matrix_j 1))))) (println (str (str "Key " (mochi_str search_in_sorted_matrix_key)) " not found"))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_mat nil main_mat2 nil] (do (set! main_mat [[2.0 5.0 7.0] [4.0 8.0 13.0] [9.0 11.0 15.0] [12.0 17.0 20.0]]) (search_in_sorted_matrix main_mat (count main_mat) (count (nth main_mat 0)) 5.0) (search_in_sorted_matrix main_mat (count main_mat) (count (nth main_mat 0)) 21.0) (set! main_mat2 [[2.1 5.0 7.0] [4.0 8.0 13.0] [9.0 11.0 15.0] [12.0 17.0 20.0]]) (search_in_sorted_matrix main_mat2 (count main_mat2) (count (nth main_mat2 0)) 2.1) (search_in_sorted_matrix main_mat2 (count main_mat2) (count (nth main_mat2 0)) 2.2))))

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
