(ns main (:refer-clojure :exclude [processes_resource_summation available_resources need pretty_print bankers_algorithm]))

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

(declare processes_resource_summation available_resources need pretty_print bankers_algorithm)

(def ^:dynamic available_resources_avail nil)

(def ^:dynamic available_resources_i nil)

(def ^:dynamic bankers_algorithm_alloc_sum nil)

(def ^:dynamic bankers_algorithm_avail nil)

(def ^:dynamic bankers_algorithm_avail_str nil)

(def ^:dynamic bankers_algorithm_exec nil)

(def ^:dynamic bankers_algorithm_finished nil)

(def ^:dynamic bankers_algorithm_i nil)

(def ^:dynamic bankers_algorithm_need_list nil)

(def ^:dynamic bankers_algorithm_p nil)

(def ^:dynamic bankers_algorithm_r nil)

(def ^:dynamic bankers_algorithm_remaining nil)

(def ^:dynamic bankers_algorithm_safe nil)

(def ^:dynamic need_i nil)

(def ^:dynamic need_j nil)

(def ^:dynamic need_needs nil)

(def ^:dynamic need_row nil)

(def ^:dynamic pretty_print_alloc_sum nil)

(def ^:dynamic pretty_print_avail nil)

(def ^:dynamic pretty_print_avail_str nil)

(def ^:dynamic pretty_print_i nil)

(def ^:dynamic pretty_print_j nil)

(def ^:dynamic pretty_print_line nil)

(def ^:dynamic pretty_print_row nil)

(def ^:dynamic pretty_print_usage nil)

(def ^:dynamic processes_resource_summation_i nil)

(def ^:dynamic processes_resource_summation_j nil)

(def ^:dynamic processes_resource_summation_resources nil)

(def ^:dynamic processes_resource_summation_sums nil)

(def ^:dynamic processes_resource_summation_total nil)

(defn processes_resource_summation [processes_resource_summation_alloc]
  (binding [processes_resource_summation_i nil processes_resource_summation_j nil processes_resource_summation_resources nil processes_resource_summation_sums nil processes_resource_summation_total nil] (try (do (set! processes_resource_summation_resources (count (nth processes_resource_summation_alloc 0))) (set! processes_resource_summation_sums []) (set! processes_resource_summation_i 0) (while (< processes_resource_summation_i processes_resource_summation_resources) (do (set! processes_resource_summation_total 0) (set! processes_resource_summation_j 0) (while (< processes_resource_summation_j (count processes_resource_summation_alloc)) (do (set! processes_resource_summation_total (+ processes_resource_summation_total (nth (nth processes_resource_summation_alloc processes_resource_summation_j) processes_resource_summation_i))) (set! processes_resource_summation_j (+ processes_resource_summation_j 1)))) (set! processes_resource_summation_sums (conj processes_resource_summation_sums processes_resource_summation_total)) (set! processes_resource_summation_i (+ processes_resource_summation_i 1)))) (throw (ex-info "return" {:v processes_resource_summation_sums}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn available_resources [available_resources_claim available_resources_alloc_sum]
  (binding [available_resources_avail nil available_resources_i nil] (try (do (set! available_resources_avail []) (set! available_resources_i 0) (while (< available_resources_i (count available_resources_claim)) (do (set! available_resources_avail (conj available_resources_avail (- (nth available_resources_claim available_resources_i) (nth available_resources_alloc_sum available_resources_i)))) (set! available_resources_i (+ available_resources_i 1)))) (throw (ex-info "return" {:v available_resources_avail}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn need [need_max need_alloc]
  (binding [need_i nil need_j nil need_needs nil need_row nil] (try (do (set! need_needs []) (set! need_i 0) (while (< need_i (count need_max)) (do (set! need_row []) (set! need_j 0) (while (< need_j (count (nth need_max 0))) (do (set! need_row (conj need_row (- (nth (nth need_max need_i) need_j) (nth (nth need_alloc need_i) need_j)))) (set! need_j (+ need_j 1)))) (set! need_needs (conj need_needs need_row)) (set! need_i (+ need_i 1)))) (throw (ex-info "return" {:v need_needs}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pretty_print [pretty_print_claim pretty_print_alloc pretty_print_max]
  (binding [pretty_print_alloc_sum nil pretty_print_avail nil pretty_print_avail_str nil pretty_print_i nil pretty_print_j nil pretty_print_line nil pretty_print_row nil pretty_print_usage nil] (do (println "         Allocated Resource Table") (set! pretty_print_i 0) (while (< pretty_print_i (count pretty_print_alloc)) (do (set! pretty_print_row (nth pretty_print_alloc pretty_print_i)) (set! pretty_print_line (str (str "P" (mochi_str (+ pretty_print_i 1))) "       ")) (set! pretty_print_j 0) (while (< pretty_print_j (count pretty_print_row)) (do (set! pretty_print_line (str pretty_print_line (mochi_str (nth pretty_print_row pretty_print_j)))) (when (< pretty_print_j (- (count pretty_print_row) 1)) (set! pretty_print_line (str pretty_print_line "        "))) (set! pretty_print_j (+ pretty_print_j 1)))) (println pretty_print_line) (println "") (set! pretty_print_i (+ pretty_print_i 1)))) (println "         System Resource Table") (set! pretty_print_i 0) (while (< pretty_print_i (count pretty_print_max)) (do (set! pretty_print_row (nth pretty_print_max pretty_print_i)) (set! pretty_print_line (str (str "P" (mochi_str (+ pretty_print_i 1))) "       ")) (set! pretty_print_j 0) (while (< pretty_print_j (count pretty_print_row)) (do (set! pretty_print_line (str pretty_print_line (mochi_str (nth pretty_print_row pretty_print_j)))) (when (< pretty_print_j (- (count pretty_print_row) 1)) (set! pretty_print_line (str pretty_print_line "        "))) (set! pretty_print_j (+ pretty_print_j 1)))) (println pretty_print_line) (println "") (set! pretty_print_i (+ pretty_print_i 1)))) (set! pretty_print_usage "") (set! pretty_print_i 0) (while (< pretty_print_i (count pretty_print_claim)) (do (when (> pretty_print_i 0) (set! pretty_print_usage (str pretty_print_usage " "))) (set! pretty_print_usage (str pretty_print_usage (mochi_str (nth pretty_print_claim pretty_print_i)))) (set! pretty_print_i (+ pretty_print_i 1)))) (set! pretty_print_alloc_sum (processes_resource_summation pretty_print_alloc)) (set! pretty_print_avail (available_resources pretty_print_claim pretty_print_alloc_sum)) (set! pretty_print_avail_str "") (set! pretty_print_i 0) (while (< pretty_print_i (count pretty_print_avail)) (do (when (> pretty_print_i 0) (set! pretty_print_avail_str (str pretty_print_avail_str " "))) (set! pretty_print_avail_str (str pretty_print_avail_str (mochi_str (nth pretty_print_avail pretty_print_i)))) (set! pretty_print_i (+ pretty_print_i 1)))) (println (str "Current Usage by Active Processes: " pretty_print_usage)) (println (str "Initial Available Resources:       " pretty_print_avail_str)))))

(defn bankers_algorithm [bankers_algorithm_claim bankers_algorithm_alloc bankers_algorithm_max]
  (binding [bankers_algorithm_alloc_sum nil bankers_algorithm_avail nil bankers_algorithm_avail_str nil bankers_algorithm_exec nil bankers_algorithm_finished nil bankers_algorithm_i nil bankers_algorithm_need_list nil bankers_algorithm_p nil bankers_algorithm_r nil bankers_algorithm_remaining nil bankers_algorithm_safe nil] (do (set! bankers_algorithm_need_list (need bankers_algorithm_max bankers_algorithm_alloc)) (set! bankers_algorithm_alloc_sum (processes_resource_summation bankers_algorithm_alloc)) (set! bankers_algorithm_avail (available_resources bankers_algorithm_claim bankers_algorithm_alloc_sum)) (println "__________________________________________________") (println "") (set! bankers_algorithm_finished []) (set! bankers_algorithm_i 0) (while (< bankers_algorithm_i (count bankers_algorithm_need_list)) (do (set! bankers_algorithm_finished (conj bankers_algorithm_finished false)) (set! bankers_algorithm_i (+ bankers_algorithm_i 1)))) (set! bankers_algorithm_remaining (count bankers_algorithm_need_list)) (loop [while_flag_1 true] (when (and while_flag_1 (> bankers_algorithm_remaining 0)) (do (set! bankers_algorithm_safe false) (set! bankers_algorithm_p 0) (loop [while_flag_2 true] (when (and while_flag_2 (< bankers_algorithm_p (count bankers_algorithm_need_list))) (do (when (not (nth bankers_algorithm_finished bankers_algorithm_p)) (do (set! bankers_algorithm_exec true) (set! bankers_algorithm_r 0) (loop [while_flag_3 true] (when (and while_flag_3 (< bankers_algorithm_r (count bankers_algorithm_avail))) (cond (> (nth (nth bankers_algorithm_need_list bankers_algorithm_p) bankers_algorithm_r) (nth bankers_algorithm_avail bankers_algorithm_r)) (do (set! bankers_algorithm_exec false) (recur false)) :else (do (set! bankers_algorithm_r (+ bankers_algorithm_r 1)) (recur while_flag_3))))) (when bankers_algorithm_exec (do (set! bankers_algorithm_safe true) (println (str (str "Process " (mochi_str (+ bankers_algorithm_p 1))) " is executing.")) (set! bankers_algorithm_r 0) (while (< bankers_algorithm_r (count bankers_algorithm_avail)) (do (set! bankers_algorithm_avail (assoc bankers_algorithm_avail bankers_algorithm_r (+ (nth bankers_algorithm_avail bankers_algorithm_r) (nth (nth bankers_algorithm_alloc bankers_algorithm_p) bankers_algorithm_r)))) (set! bankers_algorithm_r (+ bankers_algorithm_r 1)))) (set! bankers_algorithm_avail_str "") (set! bankers_algorithm_r 0) (while (< bankers_algorithm_r (count bankers_algorithm_avail)) (do (when (> bankers_algorithm_r 0) (set! bankers_algorithm_avail_str (str bankers_algorithm_avail_str " "))) (set! bankers_algorithm_avail_str (str bankers_algorithm_avail_str (mochi_str (nth bankers_algorithm_avail bankers_algorithm_r)))) (set! bankers_algorithm_r (+ bankers_algorithm_r 1)))) (println (str "Updated available resource stack for processes: " bankers_algorithm_avail_str)) (println "The process is in a safe state.") (println "") (set! bankers_algorithm_finished (assoc bankers_algorithm_finished bankers_algorithm_p true)) (set! bankers_algorithm_remaining (- bankers_algorithm_remaining 1)))))) (set! bankers_algorithm_p (+ bankers_algorithm_p 1)) (cond :else (recur while_flag_2))))) (cond (not bankers_algorithm_safe) (do (println "System in unsafe state. Aborting...") (println "") (recur false)) :else (recur while_flag_1))))))))

(def ^:dynamic main_claim_vector [8 5 9 7])

(def ^:dynamic main_allocated_resources_table [[2 0 1 1] [0 1 2 1] [4 0 0 3] [0 2 1 0] [1 0 3 0]])

(def ^:dynamic main_maximum_claim_table [[3 2 1 4] [0 2 5 2] [5 1 0 5] [1 5 3 0] [3 0 3 3]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (pretty_print main_claim_vector main_allocated_resources_table main_maximum_claim_table)
      (bankers_algorithm main_claim_vector main_allocated_resources_table main_maximum_claim_table)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
