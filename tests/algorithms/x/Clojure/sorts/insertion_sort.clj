(ns main (:refer-clojure :exclude [insertion_sort]))

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

(declare insertion_sort)

(def ^:dynamic insertion_sort_i nil)

(def ^:dynamic insertion_sort_j nil)

(def ^:dynamic insertion_sort_value nil)

(def ^:dynamic insertion_sort_xs nil)

(defn insertion_sort [insertion_sort_xs_p]
  (binding [insertion_sort_i nil insertion_sort_j nil insertion_sort_value nil insertion_sort_xs nil] (try (do (set! insertion_sort_xs insertion_sort_xs_p) (set! insertion_sort_i 1) (while (< insertion_sort_i (count insertion_sort_xs)) (do (set! insertion_sort_value (nth insertion_sort_xs insertion_sort_i)) (set! insertion_sort_j (- insertion_sort_i 1)) (while (and (>= insertion_sort_j 0) (> (nth insertion_sort_xs insertion_sort_j) insertion_sort_value)) (do (set! insertion_sort_xs (assoc insertion_sort_xs (+ insertion_sort_j 1) (nth insertion_sort_xs insertion_sort_j))) (set! insertion_sort_j (- insertion_sort_j 1)))) (set! insertion_sort_xs (assoc insertion_sort_xs (+ insertion_sort_j 1) insertion_sort_value)) (set! insertion_sort_i (+ insertion_sort_i 1)))) (throw (ex-info "return" {:v insertion_sort_xs}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (insertion_sort [0 5 3 2 2])))
      (println (str (insertion_sort [])))
      (println (str (insertion_sort [(- 2) (- 5) (- 45)])))
      (println (str (insertion_sort [3])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
