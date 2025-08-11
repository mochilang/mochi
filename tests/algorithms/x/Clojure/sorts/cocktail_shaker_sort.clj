(ns main (:refer-clojure :exclude [cocktail_shaker_sort]))

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

(declare cocktail_shaker_sort)

(def ^:dynamic cocktail_shaker_sort_arr nil)

(def ^:dynamic cocktail_shaker_sort_end nil)

(def ^:dynamic cocktail_shaker_sort_i nil)

(def ^:dynamic cocktail_shaker_sort_start nil)

(def ^:dynamic cocktail_shaker_sort_swapped nil)

(def ^:dynamic cocktail_shaker_sort_temp nil)

(def ^:dynamic cocktail_shaker_sort_temp2 nil)

(defn cocktail_shaker_sort [cocktail_shaker_sort_arr_p]
  (binding [cocktail_shaker_sort_arr nil cocktail_shaker_sort_end nil cocktail_shaker_sort_i nil cocktail_shaker_sort_start nil cocktail_shaker_sort_swapped nil cocktail_shaker_sort_temp nil cocktail_shaker_sort_temp2 nil] (try (do (set! cocktail_shaker_sort_arr cocktail_shaker_sort_arr_p) (set! cocktail_shaker_sort_start 0) (set! cocktail_shaker_sort_end (- (count cocktail_shaker_sort_arr) 1)) (loop [while_flag_1 true] (when (and while_flag_1 (< cocktail_shaker_sort_start cocktail_shaker_sort_end)) (do (set! cocktail_shaker_sort_swapped false) (set! cocktail_shaker_sort_i cocktail_shaker_sort_start) (while (< cocktail_shaker_sort_i cocktail_shaker_sort_end) (do (when (> (nth cocktail_shaker_sort_arr cocktail_shaker_sort_i) (nth cocktail_shaker_sort_arr (+ cocktail_shaker_sort_i 1))) (do (set! cocktail_shaker_sort_temp (nth cocktail_shaker_sort_arr cocktail_shaker_sort_i)) (set! cocktail_shaker_sort_arr (assoc cocktail_shaker_sort_arr cocktail_shaker_sort_i (nth cocktail_shaker_sort_arr (+ cocktail_shaker_sort_i 1)))) (set! cocktail_shaker_sort_arr (assoc cocktail_shaker_sort_arr (+ cocktail_shaker_sort_i 1) cocktail_shaker_sort_temp)) (set! cocktail_shaker_sort_swapped true))) (set! cocktail_shaker_sort_i (+ cocktail_shaker_sort_i 1)))) (cond (not cocktail_shaker_sort_swapped) (recur false) (not cocktail_shaker_sort_swapped) (recur false) :else (do (set! cocktail_shaker_sort_end (- cocktail_shaker_sort_end 1)) (set! cocktail_shaker_sort_i cocktail_shaker_sort_end) (while (> cocktail_shaker_sort_i cocktail_shaker_sort_start) (do (when (< (nth cocktail_shaker_sort_arr cocktail_shaker_sort_i) (nth cocktail_shaker_sort_arr (- cocktail_shaker_sort_i 1))) (do (set! cocktail_shaker_sort_temp2 (nth cocktail_shaker_sort_arr cocktail_shaker_sort_i)) (set! cocktail_shaker_sort_arr (assoc cocktail_shaker_sort_arr cocktail_shaker_sort_i (nth cocktail_shaker_sort_arr (- cocktail_shaker_sort_i 1)))) (set! cocktail_shaker_sort_arr (assoc cocktail_shaker_sort_arr (- cocktail_shaker_sort_i 1) cocktail_shaker_sort_temp2)) (set! cocktail_shaker_sort_swapped true))) (set! cocktail_shaker_sort_i (- cocktail_shaker_sort_i 1)))) (set! cocktail_shaker_sort_start (+ cocktail_shaker_sort_start 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v cocktail_shaker_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (cocktail_shaker_sort [4 5 2 1 2])))
      (println (str (cocktail_shaker_sort [(- 4) 5 0 1 2 11])))
      (println (str (cocktail_shaker_sort [1 2 3 4 5])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
