(ns main (:refer-clojure :exclude [selection_sort]))

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

(declare selection_sort)

(def ^:dynamic selection_sort_arr nil)

(def ^:dynamic selection_sort_i nil)

(def ^:dynamic selection_sort_k nil)

(def ^:dynamic selection_sort_min_index nil)

(def ^:dynamic selection_sort_n nil)

(def ^:dynamic selection_sort_tmp nil)

(defn selection_sort [selection_sort_arr_p]
  (binding [selection_sort_arr nil selection_sort_i nil selection_sort_k nil selection_sort_min_index nil selection_sort_n nil selection_sort_tmp nil] (try (do (set! selection_sort_arr selection_sort_arr_p) (set! selection_sort_n (count selection_sort_arr)) (set! selection_sort_i 0) (while (< selection_sort_i (- selection_sort_n 1)) (do (set! selection_sort_min_index selection_sort_i) (set! selection_sort_k (+ selection_sort_i 1)) (while (< selection_sort_k selection_sort_n) (do (when (< (nth selection_sort_arr selection_sort_k) (nth selection_sort_arr selection_sort_min_index)) (set! selection_sort_min_index selection_sort_k)) (set! selection_sort_k (+ selection_sort_k 1)))) (when (not= selection_sort_min_index selection_sort_i) (do (set! selection_sort_tmp (nth selection_sort_arr selection_sort_i)) (set! selection_sort_arr (assoc selection_sort_arr selection_sort_i (nth selection_sort_arr selection_sort_min_index))) (set! selection_sort_arr (assoc selection_sort_arr selection_sort_min_index selection_sort_tmp)))) (set! selection_sort_i (+ selection_sort_i 1)))) (throw (ex-info "return" {:v selection_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (selection_sort [0 5 3 2 2])))
      (println (str (selection_sort [])))
      (println (str (selection_sort [(- 2) (- 5) (- 45)])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
