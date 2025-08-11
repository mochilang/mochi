(ns main (:refer-clojure :exclude [exchange_sort]))

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

(declare exchange_sort)

(def ^:dynamic exchange_sort_i nil)

(def ^:dynamic exchange_sort_j nil)

(def ^:dynamic exchange_sort_n nil)

(def ^:dynamic exchange_sort_numbers nil)

(def ^:dynamic exchange_sort_temp nil)

(defn exchange_sort [exchange_sort_numbers_p]
  (binding [exchange_sort_i nil exchange_sort_j nil exchange_sort_n nil exchange_sort_numbers nil exchange_sort_temp nil] (try (do (set! exchange_sort_numbers exchange_sort_numbers_p) (set! exchange_sort_n (count exchange_sort_numbers)) (set! exchange_sort_i 0) (while (< exchange_sort_i exchange_sort_n) (do (set! exchange_sort_j (+ exchange_sort_i 1)) (while (< exchange_sort_j exchange_sort_n) (do (when (< (nth exchange_sort_numbers exchange_sort_j) (nth exchange_sort_numbers exchange_sort_i)) (do (set! exchange_sort_temp (nth exchange_sort_numbers exchange_sort_i)) (set! exchange_sort_numbers (assoc exchange_sort_numbers exchange_sort_i (nth exchange_sort_numbers exchange_sort_j))) (set! exchange_sort_numbers (assoc exchange_sort_numbers exchange_sort_j exchange_sort_temp)))) (set! exchange_sort_j (+ exchange_sort_j 1)))) (set! exchange_sort_i (+ exchange_sort_i 1)))) (throw (ex-info "return" {:v exchange_sort_numbers}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (exchange_sort [5 4 3 2 1])))
      (println (str (exchange_sort [(- 1) (- 2) (- 3)])))
      (println (str (exchange_sort [1 2 3 4 5])))
      (println (str (exchange_sort [0 10 (- 2) 5 3])))
      (println (str (exchange_sort [])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
