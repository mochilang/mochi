(ns main (:refer-clojure :exclude [make_process make_mlfq calculate_sequence_of_finish_queue calculate_waiting_time calculate_turnaround_time calculate_completion_time calculate_remaining_burst_time_of_processes update_waiting_time first_come_first_served round_robin multi_level_feedback_queue]))

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

(declare make_process make_mlfq calculate_sequence_of_finish_queue calculate_waiting_time calculate_turnaround_time calculate_completion_time calculate_remaining_burst_time_of_processes update_waiting_time first_come_first_served round_robin multi_level_feedback_queue)

(declare _read_file)

(def ^:dynamic calculate_completion_time_i nil)

(def ^:dynamic calculate_completion_time_p nil)

(def ^:dynamic calculate_completion_time_times nil)

(def ^:dynamic calculate_remaining_burst_time_of_processes_i nil)

(def ^:dynamic calculate_remaining_burst_time_of_processes_p nil)

(def ^:dynamic calculate_remaining_burst_time_of_processes_times nil)

(def ^:dynamic calculate_sequence_of_finish_queue_i nil)

(def ^:dynamic calculate_sequence_of_finish_queue_p nil)

(def ^:dynamic calculate_sequence_of_finish_queue_seq nil)

(def ^:dynamic calculate_turnaround_time_i nil)

(def ^:dynamic calculate_turnaround_time_p nil)

(def ^:dynamic calculate_turnaround_time_times nil)

(def ^:dynamic calculate_waiting_time_i nil)

(def ^:dynamic calculate_waiting_time_p nil)

(def ^:dynamic calculate_waiting_time_times nil)

(def ^:dynamic count_v nil)

(def ^:dynamic first_come_first_served_cp nil)

(def ^:dynamic first_come_first_served_finished nil)

(def ^:dynamic first_come_first_served_mlfq nil)

(def ^:dynamic first_come_first_served_rq nil)

(def ^:dynamic multi_level_feedback_queue_i nil)

(def ^:dynamic multi_level_feedback_queue_mlfq nil)

(def ^:dynamic multi_level_feedback_queue_rr nil)

(def ^:dynamic round_robin_cp nil)

(def ^:dynamic round_robin_finished nil)

(def ^:dynamic round_robin_i nil)

(def ^:dynamic round_robin_mlfq nil)

(def ^:dynamic round_robin_rq nil)

(def ^:dynamic update_waiting_time_process nil)

(defn make_process [make_process_name make_process_arrival make_process_burst]
  (try (throw (ex-info "return" {:v {:arrival_time make_process_arrival :burst_time make_process_burst :process_name make_process_name :stop_time make_process_arrival :turnaround_time 0 :waiting_time 0}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn make_mlfq [make_mlfq_nqueues make_mlfq_time_slices make_mlfq_queue make_mlfq_current_time]
  (try (throw (ex-info "return" {:v {:current_time make_mlfq_current_time :finish_queue [] :number_of_queues make_mlfq_nqueues :ready_queue make_mlfq_queue :time_slices make_mlfq_time_slices}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn calculate_sequence_of_finish_queue [calculate_sequence_of_finish_queue_mlfq]
  (binding [calculate_sequence_of_finish_queue_i nil calculate_sequence_of_finish_queue_p nil calculate_sequence_of_finish_queue_seq nil] (try (do (set! calculate_sequence_of_finish_queue_seq []) (set! calculate_sequence_of_finish_queue_i 0) (while (< calculate_sequence_of_finish_queue_i (count (:finish_queue calculate_sequence_of_finish_queue_mlfq))) (do (set! calculate_sequence_of_finish_queue_p (get (:finish_queue calculate_sequence_of_finish_queue_mlfq) calculate_sequence_of_finish_queue_i)) (set! calculate_sequence_of_finish_queue_seq (conj calculate_sequence_of_finish_queue_seq (:process_name calculate_sequence_of_finish_queue_p))) (set! calculate_sequence_of_finish_queue_i (+ calculate_sequence_of_finish_queue_i 1)))) (throw (ex-info "return" {:v calculate_sequence_of_finish_queue_seq}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calculate_waiting_time [calculate_waiting_time_queue]
  (binding [calculate_waiting_time_i nil calculate_waiting_time_p nil calculate_waiting_time_times nil] (try (do (set! calculate_waiting_time_times []) (set! calculate_waiting_time_i 0) (while (< calculate_waiting_time_i (count calculate_waiting_time_queue)) (do (set! calculate_waiting_time_p (nth calculate_waiting_time_queue calculate_waiting_time_i)) (set! calculate_waiting_time_times (conj calculate_waiting_time_times (:waiting_time calculate_waiting_time_p))) (set! calculate_waiting_time_i (+ calculate_waiting_time_i 1)))) (throw (ex-info "return" {:v calculate_waiting_time_times}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calculate_turnaround_time [calculate_turnaround_time_queue]
  (binding [calculate_turnaround_time_i nil calculate_turnaround_time_p nil calculate_turnaround_time_times nil] (try (do (set! calculate_turnaround_time_times []) (set! calculate_turnaround_time_i 0) (while (< calculate_turnaround_time_i (count calculate_turnaround_time_queue)) (do (set! calculate_turnaround_time_p (nth calculate_turnaround_time_queue calculate_turnaround_time_i)) (set! calculate_turnaround_time_times (conj calculate_turnaround_time_times (:turnaround_time calculate_turnaround_time_p))) (set! calculate_turnaround_time_i (+ calculate_turnaround_time_i 1)))) (throw (ex-info "return" {:v calculate_turnaround_time_times}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calculate_completion_time [calculate_completion_time_queue]
  (binding [calculate_completion_time_i nil calculate_completion_time_p nil calculate_completion_time_times nil] (try (do (set! calculate_completion_time_times []) (set! calculate_completion_time_i 0) (while (< calculate_completion_time_i (count calculate_completion_time_queue)) (do (set! calculate_completion_time_p (nth calculate_completion_time_queue calculate_completion_time_i)) (set! calculate_completion_time_times (conj calculate_completion_time_times (:stop_time calculate_completion_time_p))) (set! calculate_completion_time_i (+ calculate_completion_time_i 1)))) (throw (ex-info "return" {:v calculate_completion_time_times}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calculate_remaining_burst_time_of_processes [calculate_remaining_burst_time_of_processes_queue]
  (binding [calculate_remaining_burst_time_of_processes_i nil calculate_remaining_burst_time_of_processes_p nil calculate_remaining_burst_time_of_processes_times nil] (try (do (set! calculate_remaining_burst_time_of_processes_times []) (set! calculate_remaining_burst_time_of_processes_i 0) (while (< calculate_remaining_burst_time_of_processes_i (count calculate_remaining_burst_time_of_processes_queue)) (do (set! calculate_remaining_burst_time_of_processes_p (nth calculate_remaining_burst_time_of_processes_queue calculate_remaining_burst_time_of_processes_i)) (set! calculate_remaining_burst_time_of_processes_times (conj calculate_remaining_burst_time_of_processes_times (:burst_time calculate_remaining_burst_time_of_processes_p))) (set! calculate_remaining_burst_time_of_processes_i (+ calculate_remaining_burst_time_of_processes_i 1)))) (throw (ex-info "return" {:v calculate_remaining_burst_time_of_processes_times}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn update_waiting_time [update_waiting_time_mlfq update_waiting_time_process_p]
  (binding [update_waiting_time_process update_waiting_time_process_p] (try (do (set! update_waiting_time_process (assoc update_waiting_time_process :waiting_time (+ (:waiting_time update_waiting_time_process) (- (:current_time update_waiting_time_mlfq) (:stop_time update_waiting_time_process))))) (throw (ex-info "return" {:v (:waiting_time update_waiting_time_process)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var update_waiting_time_process) (constantly update_waiting_time_process))))))

(defn first_come_first_served [first_come_first_served_mlfq_p first_come_first_served_ready_queue]
  (binding [first_come_first_served_mlfq first_come_first_served_mlfq_p first_come_first_served_cp nil first_come_first_served_finished nil first_come_first_served_rq nil] (try (do (set! first_come_first_served_finished []) (set! first_come_first_served_rq first_come_first_served_ready_queue) (while (not= (count first_come_first_served_rq) 0) (do (set! first_come_first_served_cp (nth first_come_first_served_rq 0)) (set! first_come_first_served_rq (subvec first_come_first_served_rq 1 (min (count first_come_first_served_rq) (count first_come_first_served_rq)))) (when (< (:current_time first_come_first_served_mlfq) (:arrival_time first_come_first_served_cp)) (set! first_come_first_served_mlfq (assoc first_come_first_served_mlfq :current_time (:arrival_time first_come_first_served_cp)))) (let [__res (update_waiting_time first_come_first_served_mlfq first_come_first_served_cp)] (do (set! first_come_first_served_cp update_waiting_time_process) __res)) (set! first_come_first_served_mlfq (assoc first_come_first_served_mlfq :current_time (+ (:current_time first_come_first_served_mlfq) (:burst_time first_come_first_served_cp)))) (set! first_come_first_served_cp (assoc first_come_first_served_cp :burst_time 0)) (set! first_come_first_served_cp (assoc first_come_first_served_cp :turnaround_time (- (:current_time first_come_first_served_mlfq) (:arrival_time first_come_first_served_cp)))) (set! first_come_first_served_cp (assoc first_come_first_served_cp :stop_time (:current_time first_come_first_served_mlfq))) (set! first_come_first_served_finished (conj first_come_first_served_finished first_come_first_served_cp)))) (set! first_come_first_served_mlfq (assoc first_come_first_served_mlfq :finish_queue (concat (:finish_queue first_come_first_served_mlfq) first_come_first_served_finished))) (throw (ex-info "return" {:v first_come_first_served_finished}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var first_come_first_served_mlfq) (constantly first_come_first_served_mlfq))))))

(defn round_robin [round_robin_mlfq_p round_robin_ready_queue round_robin_time_slice]
  (binding [round_robin_mlfq round_robin_mlfq_p count_v nil round_robin_cp nil round_robin_finished nil round_robin_i nil round_robin_rq nil] (try (do (set! round_robin_finished []) (set! round_robin_rq round_robin_ready_queue) (set! count_v (count round_robin_rq)) (set! round_robin_i 0) (while (< round_robin_i count_v) (do (set! round_robin_cp (nth round_robin_rq 0)) (set! round_robin_rq (subvec round_robin_rq 1 (min (count round_robin_rq) (count round_robin_rq)))) (when (< (:current_time round_robin_mlfq) (:arrival_time round_robin_cp)) (set! round_robin_mlfq (assoc round_robin_mlfq :current_time (:arrival_time round_robin_cp)))) (let [__res (update_waiting_time round_robin_mlfq round_robin_cp)] (do (set! round_robin_cp update_waiting_time_process) __res)) (if (> (:burst_time round_robin_cp) round_robin_time_slice) (do (set! round_robin_mlfq (assoc round_robin_mlfq :current_time (+ (:current_time round_robin_mlfq) round_robin_time_slice))) (set! round_robin_cp (assoc round_robin_cp :burst_time (- (:burst_time round_robin_cp) round_robin_time_slice))) (set! round_robin_cp (assoc round_robin_cp :stop_time (:current_time round_robin_mlfq))) (set! round_robin_rq (conj round_robin_rq round_robin_cp))) (do (set! round_robin_mlfq (assoc round_robin_mlfq :current_time (+ (:current_time round_robin_mlfq) (:burst_time round_robin_cp)))) (set! round_robin_cp (assoc round_robin_cp :burst_time 0)) (set! round_robin_cp (assoc round_robin_cp :stop_time (:current_time round_robin_mlfq))) (set! round_robin_cp (assoc round_robin_cp :turnaround_time (- (:current_time round_robin_mlfq) (:arrival_time round_robin_cp)))) (set! round_robin_finished (conj round_robin_finished round_robin_cp)))) (set! round_robin_i (+ round_robin_i 1)))) (set! round_robin_mlfq (assoc round_robin_mlfq :finish_queue (concat (:finish_queue round_robin_mlfq) round_robin_finished))) (throw (ex-info "return" {:v {:finished round_robin_finished :ready round_robin_rq}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var round_robin_mlfq) (constantly round_robin_mlfq))))))

(defn multi_level_feedback_queue [multi_level_feedback_queue_mlfq_p]
  (binding [multi_level_feedback_queue_mlfq multi_level_feedback_queue_mlfq_p multi_level_feedback_queue_i nil multi_level_feedback_queue_rr nil] (try (do (set! multi_level_feedback_queue_i 0) (while (< multi_level_feedback_queue_i (- (:number_of_queues multi_level_feedback_queue_mlfq) 1)) (do (set! multi_level_feedback_queue_rr (let [__res (round_robin multi_level_feedback_queue_mlfq (:ready_queue multi_level_feedback_queue_mlfq) (get (:time_slices multi_level_feedback_queue_mlfq) multi_level_feedback_queue_i))] (do (set! multi_level_feedback_queue_mlfq round_robin_mlfq) __res))) (set! multi_level_feedback_queue_mlfq (assoc multi_level_feedback_queue_mlfq :ready_queue (:ready multi_level_feedback_queue_rr))) (set! multi_level_feedback_queue_i (+ multi_level_feedback_queue_i 1)))) (let [__res (first_come_first_served multi_level_feedback_queue_mlfq (:ready_queue multi_level_feedback_queue_mlfq))] (do (set! multi_level_feedback_queue_mlfq first_come_first_served_mlfq) __res)) (throw (ex-info "return" {:v (:finish_queue multi_level_feedback_queue_mlfq)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var multi_level_feedback_queue_mlfq) (constantly multi_level_feedback_queue_mlfq))))))

(def ^:dynamic main_P1 nil)

(def ^:dynamic main_P2 nil)

(def ^:dynamic main_P3 nil)

(def ^:dynamic main_P4 nil)

(def ^:dynamic main_number_of_queues nil)

(def ^:dynamic main_time_slices nil)

(def ^:dynamic main_queue nil)

(def ^:dynamic main_mlfq nil)

(def ^:dynamic main_finish_queue nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_P1) (constantly (make_process "P1" 0 53)))
      (alter-var-root (var main_P2) (constantly (make_process "P2" 0 17)))
      (alter-var-root (var main_P3) (constantly (make_process "P3" 0 68)))
      (alter-var-root (var main_P4) (constantly (make_process "P4" 0 24)))
      (alter-var-root (var main_number_of_queues) (constantly 3))
      (alter-var-root (var main_time_slices) (constantly [17 25]))
      (alter-var-root (var main_queue) (constantly [main_P1 main_P2 main_P3 main_P4]))
      (alter-var-root (var main_mlfq) (constantly (make_mlfq main_number_of_queues main_time_slices main_queue 0)))
      (alter-var-root (var main_finish_queue) (constantly (let [__res (multi_level_feedback_queue main_mlfq)] (do (alter-var-root (var main_mlfq) (constantly multi_level_feedback_queue_mlfq)) __res))))
      (println (str "waiting time:\t\t\t" (mochi_str (calculate_waiting_time [main_P1 main_P2 main_P3 main_P4]))))
      (println (str "completion time:\t\t" (mochi_str (calculate_completion_time [main_P1 main_P2 main_P3 main_P4]))))
      (println (str "turnaround time:\t\t" (mochi_str (calculate_turnaround_time [main_P1 main_P2 main_P3 main_P4]))))
      (println (str "sequence of finished processes:\t" (mochi_str (calculate_sequence_of_finish_queue main_mlfq))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
