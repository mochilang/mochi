(ns main (:refer-clojure :exclude [concat quick_sort]))

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

(declare concat quick_sort)

(def ^:dynamic concat_result nil)

(def ^:dynamic quick_sort_e nil)

(def ^:dynamic quick_sort_i nil)

(def ^:dynamic quick_sort_left nil)

(def ^:dynamic quick_sort_left_pivot nil)

(def ^:dynamic quick_sort_pivot nil)

(def ^:dynamic quick_sort_right nil)

(def ^:dynamic quick_sort_sorted_left nil)

(def ^:dynamic quick_sort_sorted_right nil)

(defn concat [concat_a concat_b]
  (binding [concat_result nil] (try (do (set! concat_result []) (doseq [x concat_a] (set! concat_result (conj concat_result x))) (doseq [x concat_b] (set! concat_result (conj concat_result x))) (throw (ex-info "return" {:v concat_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn quick_sort [quick_sort_data]
  (binding [quick_sort_e nil quick_sort_i nil quick_sort_left nil quick_sort_left_pivot nil quick_sort_pivot nil quick_sort_right nil quick_sort_sorted_left nil quick_sort_sorted_right nil] (try (do (when (<= (count quick_sort_data) 1) (throw (ex-info "return" {:v quick_sort_data}))) (set! quick_sort_pivot (nth quick_sort_data 0)) (set! quick_sort_left []) (set! quick_sort_right []) (set! quick_sort_i 1) (while (< quick_sort_i (count quick_sort_data)) (do (set! quick_sort_e (nth quick_sort_data quick_sort_i)) (if (<= quick_sort_e quick_sort_pivot) (set! quick_sort_left (conj quick_sort_left quick_sort_e)) (set! quick_sort_right (conj quick_sort_right quick_sort_e))) (set! quick_sort_i (+ quick_sort_i 1)))) (set! quick_sort_sorted_left (quick_sort quick_sort_left)) (set! quick_sort_sorted_right (quick_sort quick_sort_right)) (set! quick_sort_left_pivot (conj quick_sort_sorted_left quick_sort_pivot)) (throw (ex-info "return" {:v (concat quick_sort_left_pivot quick_sort_sorted_right)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (quick_sort [2 1 0])))
      (println (str (quick_sort [3 5 2 4 1])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
