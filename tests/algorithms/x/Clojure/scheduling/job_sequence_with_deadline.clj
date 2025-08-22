(ns main (:refer-clojure :exclude [max_tasks main]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare max_tasks main)

(declare _read_file)

(def ^:dynamic main_ex1 nil)

(def ^:dynamic main_ex2 nil)

(def ^:dynamic max_tasks_deadline nil)

(def ^:dynamic max_tasks_i nil)

(def ^:dynamic max_tasks_id nil)

(def ^:dynamic max_tasks_j nil)

(def ^:dynamic max_tasks_n nil)

(def ^:dynamic max_tasks_order nil)

(def ^:dynamic max_tasks_pos nil)

(def ^:dynamic max_tasks_result nil)

(def ^:dynamic max_tasks_tmp nil)

(defn max_tasks [max_tasks_tasks_info]
  (binding [max_tasks_deadline nil max_tasks_i nil max_tasks_id nil max_tasks_j nil max_tasks_n nil max_tasks_order nil max_tasks_pos nil max_tasks_result nil max_tasks_tmp nil] (try (do (set! max_tasks_order []) (set! max_tasks_i 0) (while (< max_tasks_i (count max_tasks_tasks_info)) (do (set! max_tasks_order (conj max_tasks_order max_tasks_i)) (set! max_tasks_i (+ max_tasks_i 1)))) (set! max_tasks_n (count max_tasks_order)) (set! max_tasks_i 0) (while (< max_tasks_i max_tasks_n) (do (set! max_tasks_j (+ max_tasks_i 1)) (while (< max_tasks_j max_tasks_n) (do (when (> (nth (nth max_tasks_tasks_info (nth max_tasks_order max_tasks_j)) 1) (nth (nth max_tasks_tasks_info (nth max_tasks_order max_tasks_i)) 1)) (do (set! max_tasks_tmp (nth max_tasks_order max_tasks_i)) (set! max_tasks_order (assoc max_tasks_order max_tasks_i (nth max_tasks_order max_tasks_j))) (set! max_tasks_order (assoc max_tasks_order max_tasks_j max_tasks_tmp)))) (set! max_tasks_j (+ max_tasks_j 1)))) (set! max_tasks_i (+ max_tasks_i 1)))) (set! max_tasks_result []) (set! max_tasks_pos 1) (set! max_tasks_i 0) (while (< max_tasks_i max_tasks_n) (do (set! max_tasks_id (nth max_tasks_order max_tasks_i)) (set! max_tasks_deadline (nth (nth max_tasks_tasks_info max_tasks_id) 0)) (when (>= max_tasks_deadline max_tasks_pos) (set! max_tasks_result (conj max_tasks_result max_tasks_id))) (set! max_tasks_i (+ max_tasks_i 1)) (set! max_tasks_pos (+ max_tasks_pos 1)))) (throw (ex-info "return" {:v max_tasks_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_ex1 nil main_ex2 nil] (do (set! main_ex1 [[4 20] [1 10] [1 40] [1 30]]) (set! main_ex2 [[1 10] [2 20] [3 30] [2 40]]) (println (mochi_str (max_tasks main_ex1))) (println (mochi_str (max_tasks main_ex2))))))

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
