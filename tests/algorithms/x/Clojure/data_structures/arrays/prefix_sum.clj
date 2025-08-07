(ns main (:refer-clojure :exclude [make_prefix_sum get_sum contains_sum]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare make_prefix_sum get_sum contains_sum)

(def ^:dynamic contains_sum_i nil)

(def ^:dynamic contains_sum_j nil)

(def ^:dynamic contains_sum_prefix nil)

(def ^:dynamic contains_sum_sum_item nil)

(def ^:dynamic contains_sum_sums nil)

(def ^:dynamic get_sum_prefix nil)

(def ^:dynamic make_prefix_sum_i nil)

(def ^:dynamic make_prefix_sum_prefix nil)

(def ^:dynamic make_prefix_sum_running nil)

(defn make_prefix_sum [make_prefix_sum_arr]
  (binding [make_prefix_sum_i nil make_prefix_sum_prefix nil make_prefix_sum_running nil] (try (do (set! make_prefix_sum_prefix []) (set! make_prefix_sum_running 0) (set! make_prefix_sum_i 0) (while (< make_prefix_sum_i (count make_prefix_sum_arr)) (do (set! make_prefix_sum_running (+ make_prefix_sum_running (nth make_prefix_sum_arr make_prefix_sum_i))) (set! make_prefix_sum_prefix (conj make_prefix_sum_prefix make_prefix_sum_running)) (set! make_prefix_sum_i (+ make_prefix_sum_i 1)))) (throw (ex-info "return" {:v {:prefix_sum make_prefix_sum_prefix}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_sum [get_sum_ps get_sum_start get_sum_end]
  (binding [get_sum_prefix nil] (try (do (set! get_sum_prefix (:prefix_sum get_sum_ps)) (when (= (count get_sum_prefix) 0) (throw (Exception. "The array is empty."))) (when (or (or (< get_sum_start 0) (>= get_sum_end (count get_sum_prefix))) (> get_sum_start get_sum_end)) (throw (Exception. "Invalid range specified."))) (if (= get_sum_start 0) (nth get_sum_prefix get_sum_end) (- (nth get_sum_prefix get_sum_end) (nth get_sum_prefix (- get_sum_start 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn contains_sum [contains_sum_ps contains_sum_target_sum]
  (binding [contains_sum_i nil contains_sum_j nil contains_sum_prefix nil contains_sum_sum_item nil contains_sum_sums nil] (try (do (set! contains_sum_prefix (:prefix_sum contains_sum_ps)) (set! contains_sum_sums [0]) (set! contains_sum_i 0) (while (< contains_sum_i (count contains_sum_prefix)) (do (set! contains_sum_sum_item (nth contains_sum_prefix contains_sum_i)) (set! contains_sum_j 0) (while (< contains_sum_j (count contains_sum_sums)) (do (when (= (nth contains_sum_sums contains_sum_j) (- contains_sum_sum_item contains_sum_target_sum)) (throw (ex-info "return" {:v true}))) (set! contains_sum_j (+ contains_sum_j 1)))) (set! contains_sum_sums (conj contains_sum_sums contains_sum_sum_item)) (set! contains_sum_i (+ contains_sum_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_ps (make_prefix_sum [1 2 3]))

(def ^:dynamic main_ps2 (make_prefix_sum [1 (- 2) 3]))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (get_sum main_ps 0 2)))
      (println (str (get_sum main_ps 1 2)))
      (println (str (get_sum main_ps 2 2)))
      (println (str (contains_sum main_ps 6)))
      (println (str (contains_sum main_ps 5)))
      (println (str (contains_sum main_ps 3)))
      (println (str (contains_sum main_ps 4)))
      (println (str (contains_sum main_ps 7)))
      (println (str (contains_sum main_ps2 2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
