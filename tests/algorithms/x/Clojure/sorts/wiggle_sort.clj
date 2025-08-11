(ns main (:refer-clojure :exclude [swap wiggle_sort]))

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

(declare swap wiggle_sort)

(def ^:dynamic swap_k nil)

(def ^:dynamic swap_res nil)

(def ^:dynamic wiggle_sort_curr nil)

(def ^:dynamic wiggle_sort_i nil)

(def ^:dynamic wiggle_sort_j nil)

(def ^:dynamic wiggle_sort_prev nil)

(def ^:dynamic wiggle_sort_res nil)

(defn swap [swap_xs swap_i swap_j]
  (binding [swap_k nil swap_res nil] (try (do (set! swap_res []) (set! swap_k 0) (while (< swap_k (count swap_xs)) (do (if (= swap_k swap_i) (set! swap_res (conj swap_res (nth swap_xs swap_j))) (if (= swap_k swap_j) (set! swap_res (conj swap_res (nth swap_xs swap_i))) (set! swap_res (conj swap_res (nth swap_xs swap_k))))) (set! swap_k (+ swap_k 1)))) (throw (ex-info "return" {:v swap_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn wiggle_sort [wiggle_sort_nums]
  (binding [wiggle_sort_curr nil wiggle_sort_i nil wiggle_sort_j nil wiggle_sort_prev nil wiggle_sort_res nil] (try (do (set! wiggle_sort_i 0) (set! wiggle_sort_res wiggle_sort_nums) (while (< wiggle_sort_i (count wiggle_sort_res)) (do (set! wiggle_sort_j (if (= wiggle_sort_i 0) (- (count wiggle_sort_res) 1) (- wiggle_sort_i 1))) (set! wiggle_sort_prev (nth wiggle_sort_res wiggle_sort_j)) (set! wiggle_sort_curr (nth wiggle_sort_res wiggle_sort_i)) (when (= (= (mod wiggle_sort_i 2) 1) (> wiggle_sort_prev wiggle_sort_curr)) (set! wiggle_sort_res (swap wiggle_sort_res wiggle_sort_j wiggle_sort_i))) (set! wiggle_sort_i (+ wiggle_sort_i 1)))) (throw (ex-info "return" {:v wiggle_sort_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (wiggle_sort [3.0 5.0 2.0 1.0 6.0 4.0])))
      (println (str (wiggle_sort [0.0 5.0 3.0 2.0 2.0])))
      (println (str (wiggle_sort [])))
      (println (str (wiggle_sort [(- 2.0) (- 5.0) (- 45.0)])))
      (println (str (wiggle_sort [(- 2.1) (- 5.68) (- 45.11)])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
