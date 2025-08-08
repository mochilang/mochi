(ns main (:refer-clojure :exclude [count_assignments count_no_of_ways main]))

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

(declare count_assignments count_no_of_ways main)

(def ^:dynamic count_assignments_i nil)

(def ^:dynamic count_assignments_t nil)

(def ^:dynamic count_assignments_tasks nil)

(def ^:dynamic count_assignments_total nil)

(def ^:dynamic main_task_performed nil)

(defn count_assignments [count_assignments_person count_assignments_task_performed count_assignments_used]
  (binding [count_assignments_i nil count_assignments_t nil count_assignments_tasks nil count_assignments_total nil] (try (do (when (= count_assignments_person (count count_assignments_task_performed)) (throw (ex-info "return" {:v 1}))) (set! count_assignments_total 0) (set! count_assignments_tasks (nth count_assignments_task_performed count_assignments_person)) (set! count_assignments_i 0) (while (< count_assignments_i (count count_assignments_tasks)) (do (set! count_assignments_t (nth count_assignments_tasks count_assignments_i)) (when (not (in count_assignments_t count_assignments_used)) (set! count_assignments_total (+ count_assignments_total (count_assignments (+ count_assignments_person 1) count_assignments_task_performed (conj count_assignments_used count_assignments_t))))) (set! count_assignments_i (+ count_assignments_i 1)))) (throw (ex-info "return" {:v count_assignments_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_no_of_ways [count_no_of_ways_task_performed]
  (try (throw (ex-info "return" {:v (count_assignments 0 count_no_of_ways_task_performed [])})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_task_performed nil] (do (set! main_task_performed [[1 3 4] [1 2 5] [3 4]]) (println (str (count_no_of_ways main_task_performed))))))

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
