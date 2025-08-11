(ns main (:refer-clojure :exclude [quick_sort]))

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

(declare quick_sort)

(def ^:dynamic quick_sort_greater nil)

(def ^:dynamic quick_sort_i nil)

(def ^:dynamic quick_sort_item nil)

(def ^:dynamic quick_sort_lesser nil)

(def ^:dynamic quick_sort_pivot nil)

(defn quick_sort [quick_sort_items]
  (binding [quick_sort_greater nil quick_sort_i nil quick_sort_item nil quick_sort_lesser nil quick_sort_pivot nil] (try (do (when (< (count quick_sort_items) 2) (throw (ex-info "return" {:v quick_sort_items}))) (set! quick_sort_pivot (nth quick_sort_items 0)) (set! quick_sort_lesser []) (set! quick_sort_greater []) (set! quick_sort_i 1) (while (< quick_sort_i (count quick_sort_items)) (do (set! quick_sort_item (nth quick_sort_items quick_sort_i)) (if (<= quick_sort_item quick_sort_pivot) (set! quick_sort_lesser (conj quick_sort_lesser quick_sort_item)) (set! quick_sort_greater (conj quick_sort_greater quick_sort_item))) (set! quick_sort_i (+ quick_sort_i 1)))) (throw (ex-info "return" {:v (concat (concat (quick_sort quick_sort_lesser) [quick_sort_pivot]) (quick_sort quick_sort_greater))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "sorted1:" (quick_sort [0 5 3 2 2]))
      (println "sorted2:" (quick_sort []))
      (println "sorted3:" (quick_sort [(- 2) 5 0 (- 45)]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
