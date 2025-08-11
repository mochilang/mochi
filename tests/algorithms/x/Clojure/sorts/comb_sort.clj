(ns main (:refer-clojure :exclude [comb_sort main]))

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

(declare comb_sort main)

(def ^:dynamic comb_sort_completed nil)

(def ^:dynamic comb_sort_data nil)

(def ^:dynamic comb_sort_gap nil)

(def ^:dynamic comb_sort_index nil)

(def ^:dynamic comb_sort_shrink_factor nil)

(def ^:dynamic comb_sort_tmp nil)

(defn comb_sort [comb_sort_data_p]
  (binding [comb_sort_completed nil comb_sort_data nil comb_sort_gap nil comb_sort_index nil comb_sort_shrink_factor nil comb_sort_tmp nil] (try (do (set! comb_sort_data comb_sort_data_p) (set! comb_sort_shrink_factor 1.3) (set! comb_sort_gap (count comb_sort_data)) (set! comb_sort_completed false) (while (not comb_sort_completed) (do (set! comb_sort_gap (int (/ comb_sort_gap comb_sort_shrink_factor))) (when (<= comb_sort_gap 1) (do (set! comb_sort_gap 1) (set! comb_sort_completed true))) (set! comb_sort_index 0) (while (< (+ comb_sort_index comb_sort_gap) (count comb_sort_data)) (do (when (> (nth comb_sort_data comb_sort_index) (nth comb_sort_data (+ comb_sort_index comb_sort_gap))) (do (set! comb_sort_tmp (nth comb_sort_data comb_sort_index)) (set! comb_sort_data (assoc comb_sort_data comb_sort_index (nth comb_sort_data (+ comb_sort_index comb_sort_gap)))) (set! comb_sort_data (assoc comb_sort_data (+ comb_sort_index comb_sort_gap) comb_sort_tmp)) (set! comb_sort_completed false))) (set! comb_sort_index (+ comb_sort_index 1)))))) (throw (ex-info "return" {:v comb_sort_data}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (comb_sort [0 5 3 2 2])) (println (comb_sort [])) (println (comb_sort [99 45 (- 7) 8 2 0 (- 15) 3]))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
