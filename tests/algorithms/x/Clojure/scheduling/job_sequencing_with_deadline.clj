(ns main (:refer-clojure :exclude [sort_jobs_by_profit max_deadline job_sequencing_with_deadlines]))

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

(declare sort_jobs_by_profit max_deadline job_sequencing_with_deadlines)

(declare _read_file)

(def ^:dynamic count_v nil)

(def ^:dynamic job_sequencing_with_deadlines_i nil)

(def ^:dynamic job_sequencing_with_deadlines_j nil)

(def ^:dynamic job_sequencing_with_deadlines_job nil)

(def ^:dynamic job_sequencing_with_deadlines_js nil)

(def ^:dynamic job_sequencing_with_deadlines_max_d nil)

(def ^:dynamic job_sequencing_with_deadlines_max_profit nil)

(def ^:dynamic job_sequencing_with_deadlines_result nil)

(def ^:dynamic job_sequencing_with_deadlines_t nil)

(def ^:dynamic job_sequencing_with_deadlines_time_slots nil)

(def ^:dynamic main_jobs1 nil)

(def ^:dynamic main_jobs2 nil)

(def ^:dynamic max_deadline_d nil)

(def ^:dynamic max_deadline_i nil)

(def ^:dynamic max_deadline_job nil)

(def ^:dynamic max_deadline_max_d nil)

(def ^:dynamic sort_jobs_by_profit_a nil)

(def ^:dynamic sort_jobs_by_profit_b nil)

(def ^:dynamic sort_jobs_by_profit_i nil)

(def ^:dynamic sort_jobs_by_profit_j nil)

(def ^:dynamic sort_jobs_by_profit_js nil)

(defn sort_jobs_by_profit [sort_jobs_by_profit_jobs]
  (binding [sort_jobs_by_profit_a nil sort_jobs_by_profit_b nil sort_jobs_by_profit_i nil sort_jobs_by_profit_j nil sort_jobs_by_profit_js nil] (try (do (set! sort_jobs_by_profit_js sort_jobs_by_profit_jobs) (set! sort_jobs_by_profit_i 0) (while (< sort_jobs_by_profit_i (count sort_jobs_by_profit_js)) (do (set! sort_jobs_by_profit_j 0) (while (< sort_jobs_by_profit_j (- (- (count sort_jobs_by_profit_js) sort_jobs_by_profit_i) 1)) (do (set! sort_jobs_by_profit_a (nth sort_jobs_by_profit_js sort_jobs_by_profit_j)) (set! sort_jobs_by_profit_b (nth sort_jobs_by_profit_js (+ sort_jobs_by_profit_j 1))) (when (< (:profit sort_jobs_by_profit_a) (:profit sort_jobs_by_profit_b)) (do (set! sort_jobs_by_profit_js (assoc sort_jobs_by_profit_js sort_jobs_by_profit_j sort_jobs_by_profit_b)) (set! sort_jobs_by_profit_js (assoc sort_jobs_by_profit_js (+ sort_jobs_by_profit_j 1) sort_jobs_by_profit_a)))) (set! sort_jobs_by_profit_j (+ sort_jobs_by_profit_j 1)))) (set! sort_jobs_by_profit_i (+ sort_jobs_by_profit_i 1)))) (throw (ex-info "return" {:v sort_jobs_by_profit_js}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_deadline [max_deadline_jobs]
  (binding [max_deadline_d nil max_deadline_i nil max_deadline_job nil max_deadline_max_d nil] (try (do (set! max_deadline_max_d 0) (set! max_deadline_i 0) (while (< max_deadline_i (count max_deadline_jobs)) (do (set! max_deadline_job (nth max_deadline_jobs max_deadline_i)) (set! max_deadline_d (:deadline max_deadline_job)) (when (> max_deadline_d max_deadline_max_d) (set! max_deadline_max_d max_deadline_d)) (set! max_deadline_i (+ max_deadline_i 1)))) (throw (ex-info "return" {:v max_deadline_max_d}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn job_sequencing_with_deadlines [job_sequencing_with_deadlines_jobs]
  (binding [count_v nil job_sequencing_with_deadlines_i nil job_sequencing_with_deadlines_j nil job_sequencing_with_deadlines_job nil job_sequencing_with_deadlines_js nil job_sequencing_with_deadlines_max_d nil job_sequencing_with_deadlines_max_profit nil job_sequencing_with_deadlines_result nil job_sequencing_with_deadlines_t nil job_sequencing_with_deadlines_time_slots nil] (try (do (set! job_sequencing_with_deadlines_js (sort_jobs_by_profit job_sequencing_with_deadlines_jobs)) (set! job_sequencing_with_deadlines_max_d (max_deadline job_sequencing_with_deadlines_js)) (set! job_sequencing_with_deadlines_time_slots []) (set! job_sequencing_with_deadlines_t 0) (while (< job_sequencing_with_deadlines_t job_sequencing_with_deadlines_max_d) (do (set! job_sequencing_with_deadlines_time_slots (conj job_sequencing_with_deadlines_time_slots -1)) (set! job_sequencing_with_deadlines_t (+ job_sequencing_with_deadlines_t 1)))) (set! count_v 0) (set! job_sequencing_with_deadlines_max_profit 0) (set! job_sequencing_with_deadlines_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< job_sequencing_with_deadlines_i (count job_sequencing_with_deadlines_js))) (do (set! job_sequencing_with_deadlines_job (nth job_sequencing_with_deadlines_js job_sequencing_with_deadlines_i)) (set! job_sequencing_with_deadlines_j (- (:deadline job_sequencing_with_deadlines_job) 1)) (loop [while_flag_2 true] (when (and while_flag_2 (>= job_sequencing_with_deadlines_j 0)) (cond (= (nth job_sequencing_with_deadlines_time_slots job_sequencing_with_deadlines_j) -1) (do (set! job_sequencing_with_deadlines_time_slots (assoc job_sequencing_with_deadlines_time_slots job_sequencing_with_deadlines_j (:id job_sequencing_with_deadlines_job))) (set! count_v (+ count_v 1)) (set! job_sequencing_with_deadlines_max_profit (+ job_sequencing_with_deadlines_max_profit (:profit job_sequencing_with_deadlines_job))) (recur false)) :else (do (set! job_sequencing_with_deadlines_j (- job_sequencing_with_deadlines_j 1)) (recur while_flag_2))))) (set! job_sequencing_with_deadlines_i (+ job_sequencing_with_deadlines_i 1)) (cond :else (do))))) (set! job_sequencing_with_deadlines_result []) (set! job_sequencing_with_deadlines_result (conj job_sequencing_with_deadlines_result count_v)) (set! job_sequencing_with_deadlines_result (conj job_sequencing_with_deadlines_result job_sequencing_with_deadlines_max_profit)) (throw (ex-info "return" {:v job_sequencing_with_deadlines_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_jobs1 nil)

(def ^:dynamic main_jobs2 nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_jobs1) (constantly []))
      (alter-var-root (var main_jobs1) (constantly (conj main_jobs1 {:deadline 4 :id 1 :profit 20})))
      (alter-var-root (var main_jobs1) (constantly (conj main_jobs1 {:deadline 1 :id 2 :profit 10})))
      (alter-var-root (var main_jobs1) (constantly (conj main_jobs1 {:deadline 1 :id 3 :profit 40})))
      (alter-var-root (var main_jobs1) (constantly (conj main_jobs1 {:deadline 1 :id 4 :profit 30})))
      (println (mochi_str (job_sequencing_with_deadlines main_jobs1)))
      (alter-var-root (var main_jobs2) (constantly []))
      (alter-var-root (var main_jobs2) (constantly (conj main_jobs2 {:deadline 2 :id 1 :profit 100})))
      (alter-var-root (var main_jobs2) (constantly (conj main_jobs2 {:deadline 1 :id 2 :profit 19})))
      (alter-var-root (var main_jobs2) (constantly (conj main_jobs2 {:deadline 2 :id 3 :profit 27})))
      (alter-var-root (var main_jobs2) (constantly (conj main_jobs2 {:deadline 1 :id 4 :profit 25})))
      (alter-var-root (var main_jobs2) (constantly (conj main_jobs2 {:deadline 1 :id 5 :profit 15})))
      (println (mochi_str (job_sequencing_with_deadlines main_jobs2)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
