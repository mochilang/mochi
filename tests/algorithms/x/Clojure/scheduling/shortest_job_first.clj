(ns main (:refer-clojure :exclude [calculate_waitingtime calculate_turnaroundtime to_float calculate_average_times]))

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

(declare calculate_waitingtime calculate_turnaroundtime to_float calculate_average_times)

(declare _read_file)

(def ^:dynamic calculate_average_times_avg_turn nil)

(def ^:dynamic calculate_average_times_avg_wait nil)

(def ^:dynamic calculate_average_times_i nil)

(def ^:dynamic calculate_average_times_total_turn_around_time nil)

(def ^:dynamic calculate_average_times_total_waiting_time nil)

(def ^:dynamic calculate_turnaroundtime_i nil)

(def ^:dynamic calculate_turnaroundtime_turn_around_time nil)

(def ^:dynamic calculate_waitingtime_check nil)

(def ^:dynamic calculate_waitingtime_complete nil)

(def ^:dynamic calculate_waitingtime_finar nil)

(def ^:dynamic calculate_waitingtime_finish_time nil)

(def ^:dynamic calculate_waitingtime_i nil)

(def ^:dynamic calculate_waitingtime_increment_time nil)

(def ^:dynamic calculate_waitingtime_j nil)

(def ^:dynamic calculate_waitingtime_minm nil)

(def ^:dynamic calculate_waitingtime_remaining_time nil)

(def ^:dynamic calculate_waitingtime_short nil)

(def ^:dynamic calculate_waitingtime_waiting_time nil)

(defn calculate_waitingtime [calculate_waitingtime_arrival_time calculate_waitingtime_burst_time calculate_waitingtime_no_of_processes]
  (binding [calculate_waitingtime_check nil calculate_waitingtime_complete nil calculate_waitingtime_finar nil calculate_waitingtime_finish_time nil calculate_waitingtime_i nil calculate_waitingtime_increment_time nil calculate_waitingtime_j nil calculate_waitingtime_minm nil calculate_waitingtime_remaining_time nil calculate_waitingtime_short nil calculate_waitingtime_waiting_time nil] (try (do (set! calculate_waitingtime_remaining_time []) (set! calculate_waitingtime_i 0) (while (< calculate_waitingtime_i calculate_waitingtime_no_of_processes) (do (set! calculate_waitingtime_remaining_time (conj calculate_waitingtime_remaining_time (nth calculate_waitingtime_burst_time calculate_waitingtime_i))) (set! calculate_waitingtime_i (+ calculate_waitingtime_i 1)))) (set! calculate_waitingtime_waiting_time []) (set! calculate_waitingtime_i 0) (while (< calculate_waitingtime_i calculate_waitingtime_no_of_processes) (do (set! calculate_waitingtime_waiting_time (conj calculate_waitingtime_waiting_time 0)) (set! calculate_waitingtime_i (+ calculate_waitingtime_i 1)))) (set! calculate_waitingtime_complete 0) (set! calculate_waitingtime_increment_time 0) (set! calculate_waitingtime_minm 1000000000) (set! calculate_waitingtime_short 0) (set! calculate_waitingtime_check false) (loop [while_flag_1 true] (when (and while_flag_1 (not= calculate_waitingtime_complete calculate_waitingtime_no_of_processes)) (do (set! calculate_waitingtime_j 0) (while (< calculate_waitingtime_j calculate_waitingtime_no_of_processes) (do (when (and (and (<= (nth calculate_waitingtime_arrival_time calculate_waitingtime_j) calculate_waitingtime_increment_time) (> (nth calculate_waitingtime_remaining_time calculate_waitingtime_j) 0)) (< (nth calculate_waitingtime_remaining_time calculate_waitingtime_j) calculate_waitingtime_minm)) (do (set! calculate_waitingtime_minm (nth calculate_waitingtime_remaining_time calculate_waitingtime_j)) (set! calculate_waitingtime_short calculate_waitingtime_j) (set! calculate_waitingtime_check true))) (set! calculate_waitingtime_j (+ calculate_waitingtime_j 1)))) (cond (not calculate_waitingtime_check) (do (set! calculate_waitingtime_increment_time (+ calculate_waitingtime_increment_time 1)) (recur true)) :else (do (set! calculate_waitingtime_remaining_time (assoc calculate_waitingtime_remaining_time calculate_waitingtime_short (- (nth calculate_waitingtime_remaining_time calculate_waitingtime_short) 1))) (set! calculate_waitingtime_minm (nth calculate_waitingtime_remaining_time calculate_waitingtime_short)) (when (= calculate_waitingtime_minm 0) (set! calculate_waitingtime_minm 1000000000)) (when (= (nth calculate_waitingtime_remaining_time calculate_waitingtime_short) 0) (do (set! calculate_waitingtime_complete (+ calculate_waitingtime_complete 1)) (set! calculate_waitingtime_check false) (set! calculate_waitingtime_finish_time (+ calculate_waitingtime_increment_time 1)) (set! calculate_waitingtime_finar (- calculate_waitingtime_finish_time (nth calculate_waitingtime_arrival_time calculate_waitingtime_short))) (set! calculate_waitingtime_waiting_time (assoc calculate_waitingtime_waiting_time calculate_waitingtime_short (- calculate_waitingtime_finar (nth calculate_waitingtime_burst_time calculate_waitingtime_short)))) (when (< (nth calculate_waitingtime_waiting_time calculate_waitingtime_short) 0) (set! calculate_waitingtime_waiting_time (assoc calculate_waitingtime_waiting_time calculate_waitingtime_short 0))))) (set! calculate_waitingtime_increment_time (+ calculate_waitingtime_increment_time 1)) (recur while_flag_1)))))) (throw (ex-info "return" {:v calculate_waitingtime_waiting_time}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calculate_turnaroundtime [calculate_turnaroundtime_burst_time calculate_turnaroundtime_no_of_processes calculate_turnaroundtime_waiting_time]
  (binding [calculate_turnaroundtime_i nil calculate_turnaroundtime_turn_around_time nil] (try (do (set! calculate_turnaroundtime_turn_around_time []) (set! calculate_turnaroundtime_i 0) (while (< calculate_turnaroundtime_i calculate_turnaroundtime_no_of_processes) (do (set! calculate_turnaroundtime_turn_around_time (conj calculate_turnaroundtime_turn_around_time (+ (nth calculate_turnaroundtime_burst_time calculate_turnaroundtime_i) (nth calculate_turnaroundtime_waiting_time calculate_turnaroundtime_i)))) (set! calculate_turnaroundtime_i (+ calculate_turnaroundtime_i 1)))) (throw (ex-info "return" {:v calculate_turnaroundtime_turn_around_time}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_float [to_float_x]
  (try (throw (ex-info "return" {:v (* to_float_x 1.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn calculate_average_times [calculate_average_times_waiting_time calculate_average_times_turn_around_time calculate_average_times_no_of_processes]
  (binding [calculate_average_times_avg_turn nil calculate_average_times_avg_wait nil calculate_average_times_i nil calculate_average_times_total_turn_around_time nil calculate_average_times_total_waiting_time nil] (do (set! calculate_average_times_total_waiting_time 0) (set! calculate_average_times_total_turn_around_time 0) (set! calculate_average_times_i 0) (while (< calculate_average_times_i calculate_average_times_no_of_processes) (do (set! calculate_average_times_total_waiting_time (+ calculate_average_times_total_waiting_time (nth calculate_average_times_waiting_time calculate_average_times_i))) (set! calculate_average_times_total_turn_around_time (+ calculate_average_times_total_turn_around_time (nth calculate_average_times_turn_around_time calculate_average_times_i))) (set! calculate_average_times_i (+ calculate_average_times_i 1)))) (set! calculate_average_times_avg_wait (/ (to_float calculate_average_times_total_waiting_time) (to_float calculate_average_times_no_of_processes))) (set! calculate_average_times_avg_turn (/ (to_float calculate_average_times_total_turn_around_time) (to_float calculate_average_times_no_of_processes))) (println (str "Average waiting time = " (mochi_str calculate_average_times_avg_wait))) (println (str "Average turn around time = " (mochi_str calculate_average_times_avg_turn))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (calculate_waitingtime [1 2 3 4] [3 3 5 1] 4))
      (println (calculate_waitingtime [1 2 3] [2 5 1] 3))
      (println (calculate_waitingtime [2 3] [5 1] 2))
      (println (calculate_turnaroundtime [3 3 5 1] 4 [0 3 5 0]))
      (println (calculate_turnaroundtime [3 3] 2 [0 3]))
      (println (calculate_turnaroundtime [8 10 1] 3 [1 0 3]))
      (calculate_average_times [0 3 5 0] [3 6 10 1] 4)
      (calculate_average_times [2 3] [3 6] 2)
      (calculate_average_times [10 4 3] [2 7 6] 3)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
