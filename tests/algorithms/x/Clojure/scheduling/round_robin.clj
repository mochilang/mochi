(ns main (:refer-clojure :exclude [calculate_waiting_times calculate_turn_around_times mean format_float_5 main]))

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

(declare calculate_waiting_times calculate_turn_around_times mean format_float_5 main)

(declare _read_file)

(def ^:dynamic calculate_turn_around_times_i nil)

(def ^:dynamic calculate_turn_around_times_result nil)

(def ^:dynamic calculate_waiting_times_done nil)

(def ^:dynamic calculate_waiting_times_i nil)

(def ^:dynamic calculate_waiting_times_j nil)

(def ^:dynamic calculate_waiting_times_quantum nil)

(def ^:dynamic calculate_waiting_times_rem nil)

(def ^:dynamic calculate_waiting_times_t nil)

(def ^:dynamic calculate_waiting_times_waiting nil)

(def ^:dynamic format_float_5_frac_part nil)

(def ^:dynamic format_float_5_frac_str nil)

(def ^:dynamic format_float_5_int_part nil)

(def ^:dynamic format_float_5_scaled nil)

(def ^:dynamic main_burst_times nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_line nil)

(def ^:dynamic main_turn_around_times nil)

(def ^:dynamic main_waiting_times nil)

(def ^:dynamic mean_i nil)

(def ^:dynamic mean_total nil)

(defn calculate_waiting_times [calculate_waiting_times_burst_times]
  (binding [calculate_waiting_times_done nil calculate_waiting_times_i nil calculate_waiting_times_j nil calculate_waiting_times_quantum nil calculate_waiting_times_rem nil calculate_waiting_times_t nil calculate_waiting_times_waiting nil] (try (do (set! calculate_waiting_times_quantum 2) (set! calculate_waiting_times_rem []) (set! calculate_waiting_times_i 0) (while (< calculate_waiting_times_i (count calculate_waiting_times_burst_times)) (do (set! calculate_waiting_times_rem (conj calculate_waiting_times_rem (nth calculate_waiting_times_burst_times calculate_waiting_times_i))) (set! calculate_waiting_times_i (+ calculate_waiting_times_i 1)))) (set! calculate_waiting_times_waiting []) (set! calculate_waiting_times_i 0) (while (< calculate_waiting_times_i (count calculate_waiting_times_burst_times)) (do (set! calculate_waiting_times_waiting (conj calculate_waiting_times_waiting 0)) (set! calculate_waiting_times_i (+ calculate_waiting_times_i 1)))) (set! calculate_waiting_times_t 0) (while true (do (set! calculate_waiting_times_done true) (set! calculate_waiting_times_j 0) (while (< calculate_waiting_times_j (count calculate_waiting_times_burst_times)) (do (when (> (nth calculate_waiting_times_rem calculate_waiting_times_j) 0) (do (set! calculate_waiting_times_done false) (if (> (nth calculate_waiting_times_rem calculate_waiting_times_j) calculate_waiting_times_quantum) (do (set! calculate_waiting_times_t (+ calculate_waiting_times_t calculate_waiting_times_quantum)) (set! calculate_waiting_times_rem (assoc calculate_waiting_times_rem calculate_waiting_times_j (- (nth calculate_waiting_times_rem calculate_waiting_times_j) calculate_waiting_times_quantum)))) (do (set! calculate_waiting_times_t (+ calculate_waiting_times_t (nth calculate_waiting_times_rem calculate_waiting_times_j))) (set! calculate_waiting_times_waiting (assoc calculate_waiting_times_waiting calculate_waiting_times_j (- calculate_waiting_times_t (nth calculate_waiting_times_burst_times calculate_waiting_times_j)))) (set! calculate_waiting_times_rem (assoc calculate_waiting_times_rem calculate_waiting_times_j 0)))))) (set! calculate_waiting_times_j (+ calculate_waiting_times_j 1)))) (when calculate_waiting_times_done (throw (ex-info "return" {:v calculate_waiting_times_waiting}))))) (throw (ex-info "return" {:v calculate_waiting_times_waiting}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calculate_turn_around_times [calculate_turn_around_times_burst_times calculate_turn_around_times_waiting_times]
  (binding [calculate_turn_around_times_i nil calculate_turn_around_times_result nil] (try (do (set! calculate_turn_around_times_result []) (set! calculate_turn_around_times_i 0) (while (< calculate_turn_around_times_i (count calculate_turn_around_times_burst_times)) (do (set! calculate_turn_around_times_result (conj calculate_turn_around_times_result (+ (nth calculate_turn_around_times_burst_times calculate_turn_around_times_i) (nth calculate_turn_around_times_waiting_times calculate_turn_around_times_i)))) (set! calculate_turn_around_times_i (+ calculate_turn_around_times_i 1)))) (throw (ex-info "return" {:v calculate_turn_around_times_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mean [mean_values]
  (binding [mean_i nil mean_total nil] (try (do (set! mean_total 0) (set! mean_i 0) (while (< mean_i (count mean_values)) (do (set! mean_total (+ mean_total (nth mean_values mean_i))) (set! mean_i (+ mean_i 1)))) (throw (ex-info "return" {:v (/ (double mean_total) (double (count mean_values)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn format_float_5 [format_float_5_x]
  (binding [format_float_5_frac_part nil format_float_5_frac_str nil format_float_5_int_part nil format_float_5_scaled nil] (try (do (set! format_float_5_scaled (toi (+ (* format_float_5_x 100000.0) 0.5))) (set! format_float_5_int_part (/ format_float_5_scaled 100000)) (set! format_float_5_frac_part (mod format_float_5_scaled 100000)) (set! format_float_5_frac_str (mochi_str format_float_5_frac_part)) (while (< (count format_float_5_frac_str) 5) (set! format_float_5_frac_str (str "0" format_float_5_frac_str))) (throw (ex-info "return" {:v (str (str (mochi_str format_float_5_int_part) ".") format_float_5_frac_str)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_burst_times nil main_i nil main_line nil main_turn_around_times nil main_waiting_times nil] (do (set! main_burst_times [3 5 7]) (set! main_waiting_times (calculate_waiting_times main_burst_times)) (set! main_turn_around_times (calculate_turn_around_times main_burst_times main_waiting_times)) (println "Process ID \tBurst Time \tWaiting Time \tTurnaround Time") (set! main_i 0) (while (< main_i (count main_burst_times)) (do (set! main_line (str (str (str (str (str (str (str "  " (mochi_str (+ main_i 1))) "\t\t  ") (mochi_str (nth main_burst_times main_i))) "\t\t  ") (mochi_str (nth main_waiting_times main_i))) "\t\t  ") (mochi_str (nth main_turn_around_times main_i)))) (println main_line) (set! main_i (+ main_i 1)))) (println "") (println (str "Average waiting time = " (format_float_5 (mean main_waiting_times)))) (println (str "Average turn around time = " (format_float_5 (mean main_turn_around_times)))))))

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
