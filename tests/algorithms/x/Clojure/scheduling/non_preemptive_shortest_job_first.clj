(ns main (:refer-clojure :exclude [calculate_waitingtime calculate_turnaroundtime average]))

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

(declare calculate_waitingtime calculate_turnaroundtime average)

(declare _read_file)

(def ^:dynamic average_i nil)

(def ^:dynamic average_total nil)

(def ^:dynamic calculate_turnaroundtime_i nil)

(def ^:dynamic calculate_turnaroundtime_turn_around_time nil)

(def ^:dynamic calculate_waitingtime_completed nil)

(def ^:dynamic calculate_waitingtime_i nil)

(def ^:dynamic calculate_waitingtime_idx nil)

(def ^:dynamic calculate_waitingtime_j nil)

(def ^:dynamic calculate_waitingtime_k nil)

(def ^:dynamic calculate_waitingtime_ready_process nil)

(def ^:dynamic calculate_waitingtime_remaining_time nil)

(def ^:dynamic calculate_waitingtime_target_process nil)

(def ^:dynamic calculate_waitingtime_total_time nil)

(def ^:dynamic calculate_waitingtime_waiting_time nil)

(def ^:dynamic main_i nil)

(defn calculate_waitingtime [calculate_waitingtime_arrival_time calculate_waitingtime_burst_time calculate_waitingtime_no_of_processes]
  (binding [calculate_waitingtime_completed nil calculate_waitingtime_i nil calculate_waitingtime_idx nil calculate_waitingtime_j nil calculate_waitingtime_k nil calculate_waitingtime_ready_process nil calculate_waitingtime_remaining_time nil calculate_waitingtime_target_process nil calculate_waitingtime_total_time nil calculate_waitingtime_waiting_time nil] (try (do (set! calculate_waitingtime_waiting_time []) (set! calculate_waitingtime_remaining_time []) (set! calculate_waitingtime_i 0) (while (< calculate_waitingtime_i calculate_waitingtime_no_of_processes) (do (set! calculate_waitingtime_waiting_time (conj calculate_waitingtime_waiting_time 0)) (set! calculate_waitingtime_remaining_time (conj calculate_waitingtime_remaining_time (nth calculate_waitingtime_burst_time calculate_waitingtime_i))) (set! calculate_waitingtime_i (+ calculate_waitingtime_i 1)))) (set! calculate_waitingtime_completed 0) (set! calculate_waitingtime_total_time 0) (while (not= calculate_waitingtime_completed calculate_waitingtime_no_of_processes) (do (set! calculate_waitingtime_ready_process []) (set! calculate_waitingtime_target_process (- 1)) (set! calculate_waitingtime_j 0) (while (< calculate_waitingtime_j calculate_waitingtime_no_of_processes) (do (when (and (<= (nth calculate_waitingtime_arrival_time calculate_waitingtime_j) calculate_waitingtime_total_time) (> (nth calculate_waitingtime_remaining_time calculate_waitingtime_j) 0)) (set! calculate_waitingtime_ready_process (conj calculate_waitingtime_ready_process calculate_waitingtime_j))) (set! calculate_waitingtime_j (+ calculate_waitingtime_j 1)))) (if (> (count calculate_waitingtime_ready_process) 0) (do (set! calculate_waitingtime_target_process (nth calculate_waitingtime_ready_process 0)) (set! calculate_waitingtime_k 0) (while (< calculate_waitingtime_k (count calculate_waitingtime_ready_process)) (do (set! calculate_waitingtime_idx (nth calculate_waitingtime_ready_process calculate_waitingtime_k)) (when (< (nth calculate_waitingtime_remaining_time calculate_waitingtime_idx) (nth calculate_waitingtime_remaining_time calculate_waitingtime_target_process)) (set! calculate_waitingtime_target_process calculate_waitingtime_idx)) (set! calculate_waitingtime_k (+ calculate_waitingtime_k 1)))) (set! calculate_waitingtime_total_time (+ calculate_waitingtime_total_time (nth calculate_waitingtime_burst_time calculate_waitingtime_target_process))) (set! calculate_waitingtime_completed (+ calculate_waitingtime_completed 1)) (set! calculate_waitingtime_remaining_time (assoc calculate_waitingtime_remaining_time calculate_waitingtime_target_process 0)) (set! calculate_waitingtime_waiting_time (assoc calculate_waitingtime_waiting_time calculate_waitingtime_target_process (- (- calculate_waitingtime_total_time (nth calculate_waitingtime_arrival_time calculate_waitingtime_target_process)) (nth calculate_waitingtime_burst_time calculate_waitingtime_target_process))))) (set! calculate_waitingtime_total_time (+ calculate_waitingtime_total_time 1))))) (throw (ex-info "return" {:v calculate_waitingtime_waiting_time}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calculate_turnaroundtime [calculate_turnaroundtime_burst_time calculate_turnaroundtime_no_of_processes calculate_turnaroundtime_waiting_time]
  (binding [calculate_turnaroundtime_i nil calculate_turnaroundtime_turn_around_time nil] (try (do (set! calculate_turnaroundtime_turn_around_time []) (set! calculate_turnaroundtime_i 0) (while (< calculate_turnaroundtime_i calculate_turnaroundtime_no_of_processes) (do (set! calculate_turnaroundtime_turn_around_time (conj calculate_turnaroundtime_turn_around_time (+ (nth calculate_turnaroundtime_burst_time calculate_turnaroundtime_i) (nth calculate_turnaroundtime_waiting_time calculate_turnaroundtime_i)))) (set! calculate_turnaroundtime_i (+ calculate_turnaroundtime_i 1)))) (throw (ex-info "return" {:v calculate_turnaroundtime_turn_around_time}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn average [average_values]
  (binding [average_i nil average_total nil] (try (do (set! average_total 0) (set! average_i 0) (while (< average_i (count average_values)) (do (set! average_total (+ average_total (nth average_values average_i))) (set! average_i (+ average_i 1)))) (throw (ex-info "return" {:v (/ (double average_total) (double (count average_values)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_no_of_processes nil)

(def ^:dynamic main_burst_time nil)

(def ^:dynamic main_arrival_time nil)

(def ^:dynamic main_waiting_time nil)

(def ^:dynamic main_turn_around_time nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_avg_wait nil)

(def ^:dynamic main_avg_turn nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "[TEST CASE 01]")
      (alter-var-root (var main_no_of_processes) (constantly 4))
      (alter-var-root (var main_burst_time) (constantly [2 5 3 7]))
      (alter-var-root (var main_arrival_time) (constantly [0 0 0 0]))
      (alter-var-root (var main_waiting_time) (constantly (calculate_waitingtime main_arrival_time main_burst_time main_no_of_processes)))
      (alter-var-root (var main_turn_around_time) (constantly (calculate_turnaroundtime main_burst_time main_no_of_processes main_waiting_time)))
      (println "PID\tBurst Time\tArrival Time\tWaiting Time\tTurnaround Time")
      (alter-var-root (var main_i) (constantly 0))
      (while (< main_i main_no_of_processes) (do (def ^:dynamic main_pid (+ main_i 1)) (println (str (str (str (str (str (str (str (str (mochi_str main_pid) "\t") (mochi_str (nth main_burst_time main_i))) "\t\t\t") (mochi_str (nth main_arrival_time main_i))) "\t\t\t\t") (mochi_str (nth main_waiting_time main_i))) "\t\t\t\t") (mochi_str (nth main_turn_around_time main_i)))) (alter-var-root (var main_i) (constantly (+ main_i 1)))))
      (alter-var-root (var main_avg_wait) (constantly (average main_waiting_time)))
      (alter-var-root (var main_avg_turn) (constantly (average main_turn_around_time)))
      (println (str "\nAverage waiting time = " (mochi_str main_avg_wait)))
      (println (str "Average turnaround time = " (mochi_str main_avg_turn)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
