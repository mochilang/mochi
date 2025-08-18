(ns main (:refer-clojure :exclude [unique array_equalization]))

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

(declare unique array_equalization)

(def ^:dynamic array_equalization_elems nil)

(def ^:dynamic array_equalization_i nil)

(def ^:dynamic array_equalization_idx nil)

(def ^:dynamic array_equalization_min_updates nil)

(def ^:dynamic array_equalization_target nil)

(def ^:dynamic array_equalization_updates nil)

(def ^:dynamic unique_found nil)

(def ^:dynamic unique_i nil)

(def ^:dynamic unique_j nil)

(def ^:dynamic unique_res nil)

(def ^:dynamic unique_v nil)

(defn unique [unique_nums]
  (binding [unique_found nil unique_i nil unique_j nil unique_res nil unique_v nil] (try (do (set! unique_res []) (set! unique_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< unique_i (count unique_nums))) (do (set! unique_v (nth unique_nums unique_i)) (set! unique_found false) (set! unique_j 0) (loop [while_flag_2 true] (when (and while_flag_2 (< unique_j (count unique_res))) (cond (= (nth unique_res unique_j) unique_v) (do (set! unique_found true) (recur false)) :else (do (set! unique_j (+ unique_j 1)) (recur while_flag_2))))) (when (not unique_found) (set! unique_res (conj unique_res unique_v))) (set! unique_i (+ unique_i 1)) (cond :else (recur while_flag_1))))) (throw (ex-info "return" {:v unique_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn array_equalization [array_equalization_vector array_equalization_step_size]
  (binding [array_equalization_elems nil array_equalization_i nil array_equalization_idx nil array_equalization_min_updates nil array_equalization_target nil array_equalization_updates nil] (try (do (when (<= array_equalization_step_size 0) (throw (Exception. "Step size must be positive and non-zero."))) (set! array_equalization_elems (unique array_equalization_vector)) (set! array_equalization_min_updates (count array_equalization_vector)) (set! array_equalization_i 0) (while (< array_equalization_i (count array_equalization_elems)) (do (set! array_equalization_target (nth array_equalization_elems array_equalization_i)) (set! array_equalization_idx 0) (set! array_equalization_updates 0) (while (< array_equalization_idx (count array_equalization_vector)) (if (not= (nth array_equalization_vector array_equalization_idx) array_equalization_target) (do (set! array_equalization_updates (+ array_equalization_updates 1)) (set! array_equalization_idx (+ array_equalization_idx array_equalization_step_size))) (set! array_equalization_idx (+ array_equalization_idx 1)))) (when (< array_equalization_updates array_equalization_min_updates) (set! array_equalization_min_updates array_equalization_updates)) (set! array_equalization_i (+ array_equalization_i 1)))) (throw (ex-info "return" {:v array_equalization_min_updates}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (mochi_str (array_equalization [1 1 6 2 4 6 5 1 7 2 2 1 7 2 2] 4)))
      (println (mochi_str (array_equalization [22 81 88 71 22 81 632 81 81 22 92] 2)))
      (println (mochi_str (array_equalization [0 0 0 0 0 0 0 0 0 0 0 0] 5)))
      (println (mochi_str (array_equalization [22 22 22 33 33 33] 2)))
      (println (mochi_str (array_equalization [1 2 3] 2147483647)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
