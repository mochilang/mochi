(ns main (:refer-clojure :exclude [double_sort]))

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

(declare double_sort)

(def ^:dynamic double_sort_collection nil)

(def ^:dynamic double_sort_i nil)

(def ^:dynamic double_sort_j nil)

(def ^:dynamic double_sort_no_of_elements nil)

(def ^:dynamic double_sort_passes nil)

(def ^:dynamic double_sort_tmp nil)

(def ^:dynamic double_sort_tmp2 nil)

(defn double_sort [double_sort_collection_p]
  (binding [double_sort_collection nil double_sort_i nil double_sort_j nil double_sort_no_of_elements nil double_sort_passes nil double_sort_tmp nil double_sort_tmp2 nil] (try (do (set! double_sort_collection double_sort_collection_p) (set! double_sort_no_of_elements (count double_sort_collection)) (set! double_sort_passes (+ (/ (- double_sort_no_of_elements 1) 2) 1)) (set! double_sort_i 0) (while (< double_sort_i double_sort_passes) (do (set! double_sort_j 0) (while (< double_sort_j (- double_sort_no_of_elements 1)) (do (when (< (nth double_sort_collection (+ double_sort_j 1)) (nth double_sort_collection double_sort_j)) (do (set! double_sort_tmp (nth double_sort_collection double_sort_j)) (set! double_sort_collection (assoc double_sort_collection double_sort_j (nth double_sort_collection (+ double_sort_j 1)))) (set! double_sort_collection (assoc double_sort_collection (+ double_sort_j 1) double_sort_tmp)))) (when (< (nth double_sort_collection (- (- double_sort_no_of_elements 1) double_sort_j)) (nth double_sort_collection (- (- double_sort_no_of_elements 2) double_sort_j))) (do (set! double_sort_tmp2 (nth double_sort_collection (- (- double_sort_no_of_elements 1) double_sort_j))) (set! double_sort_collection (assoc double_sort_collection (- (- double_sort_no_of_elements 1) double_sort_j) (nth double_sort_collection (- (- double_sort_no_of_elements 2) double_sort_j)))) (set! double_sort_collection (assoc double_sort_collection (- (- double_sort_no_of_elements 2) double_sort_j) double_sort_tmp2)))) (set! double_sort_j (+ double_sort_j 1)))) (set! double_sort_i (+ double_sort_i 1)))) (throw (ex-info "return" {:v double_sort_collection}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (double_sort [(- 1) (- 2) (- 3) (- 4) (- 5) (- 6) (- 7)])))
      (println (str (double_sort [])))
      (println (str (double_sort [(- 1) (- 2) (- 3) (- 4) (- 5) (- 6)])))
      (println (str (= (double_sort [(- 3) 10 16 (- 42) 29]) [(- 42) (- 3) 10 16 29])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
