(ns main (:refer-clojure :exclude [cycle_sort]))

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

(declare cycle_sort)

(def ^:dynamic cycle_sort_arr nil)

(def ^:dynamic cycle_sort_cycle_start nil)

(def ^:dynamic cycle_sort_i nil)

(def ^:dynamic cycle_sort_item nil)

(def ^:dynamic cycle_sort_n nil)

(def ^:dynamic cycle_sort_pos nil)

(def ^:dynamic cycle_sort_temp nil)

(def ^:dynamic cycle_sort_temp2 nil)

(defn cycle_sort [cycle_sort_arr_p]
  (binding [cycle_sort_arr nil cycle_sort_cycle_start nil cycle_sort_i nil cycle_sort_item nil cycle_sort_n nil cycle_sort_pos nil cycle_sort_temp nil cycle_sort_temp2 nil] (try (do (set! cycle_sort_arr cycle_sort_arr_p) (set! cycle_sort_n (count cycle_sort_arr)) (set! cycle_sort_cycle_start 0) (loop [while_flag_1 true] (when (and while_flag_1 (< cycle_sort_cycle_start (- cycle_sort_n 1))) (do (set! cycle_sort_item (nth cycle_sort_arr cycle_sort_cycle_start)) (set! cycle_sort_pos cycle_sort_cycle_start) (set! cycle_sort_i (+ cycle_sort_cycle_start 1)) (while (< cycle_sort_i cycle_sort_n) (do (when (< (nth cycle_sort_arr cycle_sort_i) cycle_sort_item) (set! cycle_sort_pos (+ cycle_sort_pos 1))) (set! cycle_sort_i (+ cycle_sort_i 1)))) (cond (= cycle_sort_pos cycle_sort_cycle_start) (do (set! cycle_sort_cycle_start (+ cycle_sort_cycle_start 1)) (recur true)) :else (do (while (= cycle_sort_item (nth cycle_sort_arr cycle_sort_pos)) (set! cycle_sort_pos (+ cycle_sort_pos 1))) (set! cycle_sort_temp (nth cycle_sort_arr cycle_sort_pos)) (set! cycle_sort_arr (assoc cycle_sort_arr cycle_sort_pos cycle_sort_item)) (set! cycle_sort_item cycle_sort_temp) (while (not= cycle_sort_pos cycle_sort_cycle_start) (do (set! cycle_sort_pos cycle_sort_cycle_start) (set! cycle_sort_i (+ cycle_sort_cycle_start 1)) (while (< cycle_sort_i cycle_sort_n) (do (when (< (nth cycle_sort_arr cycle_sort_i) cycle_sort_item) (set! cycle_sort_pos (+ cycle_sort_pos 1))) (set! cycle_sort_i (+ cycle_sort_i 1)))) (while (= cycle_sort_item (nth cycle_sort_arr cycle_sort_pos)) (set! cycle_sort_pos (+ cycle_sort_pos 1))) (set! cycle_sort_temp2 (nth cycle_sort_arr cycle_sort_pos)) (set! cycle_sort_arr (assoc cycle_sort_arr cycle_sort_pos cycle_sort_item)) (set! cycle_sort_item cycle_sort_temp2))) (set! cycle_sort_cycle_start (+ cycle_sort_cycle_start 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v cycle_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (cycle_sort [4 3 2 1])))
      (println (str (cycle_sort [(- 4) 20 0 (- 50) 100 (- 1)])))
      (println (str (cycle_sort [])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
