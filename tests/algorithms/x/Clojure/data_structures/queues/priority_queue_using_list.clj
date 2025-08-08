(ns main (:refer-clojure :exclude [panic fpq_new fpq_enqueue fpq_dequeue fpq_to_string epq_new epq_enqueue epq_dequeue epq_to_string fixed_priority_queue element_priority_queue main]))

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

(declare panic fpq_new fpq_enqueue fpq_dequeue fpq_to_string epq_new epq_enqueue epq_dequeue epq_to_string fixed_priority_queue element_priority_queue main)

(def ^:dynamic element_priority_queue_epq nil)

(def ^:dynamic element_priority_queue_res nil)

(def ^:dynamic epq_dequeue_epq nil)

(def ^:dynamic epq_dequeue_i nil)

(def ^:dynamic epq_dequeue_idx nil)

(def ^:dynamic epq_dequeue_min_val nil)

(def ^:dynamic epq_dequeue_new_q nil)

(def ^:dynamic epq_dequeue_v nil)

(def ^:dynamic epq_enqueue_epq nil)

(def ^:dynamic fixed_priority_queue_fpq nil)

(def ^:dynamic fixed_priority_queue_res nil)

(def ^:dynamic fpq_dequeue_fpq nil)

(def ^:dynamic fpq_dequeue_i nil)

(def ^:dynamic fpq_dequeue_j nil)

(def ^:dynamic fpq_dequeue_new_q nil)

(def ^:dynamic fpq_dequeue_q nil)

(def ^:dynamic fpq_dequeue_qs nil)

(def ^:dynamic fpq_dequeue_val nil)

(def ^:dynamic fpq_enqueue_fpq nil)

(def ^:dynamic fpq_enqueue_qs nil)

(def ^:dynamic fpq_to_string_i nil)

(def ^:dynamic fpq_to_string_j nil)

(def ^:dynamic fpq_to_string_lines nil)

(def ^:dynamic fpq_to_string_q nil)

(def ^:dynamic fpq_to_string_q_str nil)

(def ^:dynamic fpq_to_string_res nil)

(defn panic [panic_msg]
  (println panic_msg))

(defn fpq_new []
  (try (throw (ex-info "return" {:v {:queues [[] [] []]}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fpq_enqueue [fpq_enqueue_fpq_p fpq_enqueue_priority fpq_enqueue_data]
  (binding [fpq_enqueue_fpq nil fpq_enqueue_qs nil] (try (do (set! fpq_enqueue_fpq fpq_enqueue_fpq_p) (when (or (< fpq_enqueue_priority 0) (>= fpq_enqueue_priority (count (:queues fpq_enqueue_fpq)))) (do (panic "Valid priorities are 0, 1, and 2") (throw (ex-info "return" {:v fpq_enqueue_fpq})))) (when (>= (count (get (:queues fpq_enqueue_fpq) fpq_enqueue_priority)) 100) (do (panic "Maximum queue size is 100") (throw (ex-info "return" {:v fpq_enqueue_fpq})))) (set! fpq_enqueue_qs (:queues fpq_enqueue_fpq)) (set! fpq_enqueue_qs (assoc fpq_enqueue_qs fpq_enqueue_priority (conj (nth fpq_enqueue_qs fpq_enqueue_priority) fpq_enqueue_data))) (set! fpq_enqueue_fpq (assoc fpq_enqueue_fpq :queues fpq_enqueue_qs)) (throw (ex-info "return" {:v fpq_enqueue_fpq}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fpq_dequeue [fpq_dequeue_fpq_p]
  (binding [fpq_dequeue_fpq nil fpq_dequeue_i nil fpq_dequeue_j nil fpq_dequeue_new_q nil fpq_dequeue_q nil fpq_dequeue_qs nil fpq_dequeue_val nil] (try (do (set! fpq_dequeue_fpq fpq_dequeue_fpq_p) (set! fpq_dequeue_qs (:queues fpq_dequeue_fpq)) (set! fpq_dequeue_i 0) (while (< fpq_dequeue_i (count fpq_dequeue_qs)) (do (set! fpq_dequeue_q (nth fpq_dequeue_qs fpq_dequeue_i)) (when (> (count fpq_dequeue_q) 0) (do (set! fpq_dequeue_val (nth fpq_dequeue_q 0)) (set! fpq_dequeue_new_q []) (set! fpq_dequeue_j 1) (while (< fpq_dequeue_j (count fpq_dequeue_q)) (do (set! fpq_dequeue_new_q (conj fpq_dequeue_new_q (nth fpq_dequeue_q fpq_dequeue_j))) (set! fpq_dequeue_j (+ fpq_dequeue_j 1)))) (set! fpq_dequeue_qs (assoc fpq_dequeue_qs fpq_dequeue_i fpq_dequeue_new_q)) (set! fpq_dequeue_fpq (assoc fpq_dequeue_fpq :queues fpq_dequeue_qs)) (throw (ex-info "return" {:v {:queue fpq_dequeue_fpq :value fpq_dequeue_val}})))) (set! fpq_dequeue_i (+ fpq_dequeue_i 1)))) (panic "All queues are empty") (throw (ex-info "return" {:v {:queue fpq_dequeue_fpq :value 0}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fpq_to_string [fpq_to_string_fpq]
  (binding [fpq_to_string_i nil fpq_to_string_j nil fpq_to_string_lines nil fpq_to_string_q nil fpq_to_string_q_str nil fpq_to_string_res nil] (try (do (set! fpq_to_string_lines []) (set! fpq_to_string_i 0) (while (< fpq_to_string_i (count (:queues fpq_to_string_fpq))) (do (set! fpq_to_string_q_str "[") (set! fpq_to_string_q (get (:queues fpq_to_string_fpq) fpq_to_string_i)) (set! fpq_to_string_j 0) (while (< fpq_to_string_j (count fpq_to_string_q)) (do (when (> fpq_to_string_j 0) (set! fpq_to_string_q_str (str fpq_to_string_q_str ", "))) (set! fpq_to_string_q_str (str fpq_to_string_q_str (str (nth fpq_to_string_q fpq_to_string_j)))) (set! fpq_to_string_j (+ fpq_to_string_j 1)))) (set! fpq_to_string_q_str (str fpq_to_string_q_str "]")) (set! fpq_to_string_lines (conj fpq_to_string_lines (str (str (str "Priority " (str fpq_to_string_i)) ": ") fpq_to_string_q_str))) (set! fpq_to_string_i (+ fpq_to_string_i 1)))) (set! fpq_to_string_res "") (set! fpq_to_string_i 0) (while (< fpq_to_string_i (count fpq_to_string_lines)) (do (when (> fpq_to_string_i 0) (set! fpq_to_string_res (str fpq_to_string_res "\n"))) (set! fpq_to_string_res (str fpq_to_string_res (nth fpq_to_string_lines fpq_to_string_i))) (set! fpq_to_string_i (+ fpq_to_string_i 1)))) (throw (ex-info "return" {:v fpq_to_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn epq_new []
  (try (throw (ex-info "return" {:v {:queue []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn epq_enqueue [epq_enqueue_epq_p epq_enqueue_data]
  (binding [epq_enqueue_epq nil] (try (do (set! epq_enqueue_epq epq_enqueue_epq_p) (when (>= (count (:queue epq_enqueue_epq)) 100) (do (panic "Maximum queue size is 100") (throw (ex-info "return" {:v epq_enqueue_epq})))) (set! epq_enqueue_epq (assoc epq_enqueue_epq :queue (conj (:queue epq_enqueue_epq) epq_enqueue_data))) (throw (ex-info "return" {:v epq_enqueue_epq}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn epq_dequeue [epq_dequeue_epq_p]
  (binding [epq_dequeue_epq nil epq_dequeue_i nil epq_dequeue_idx nil epq_dequeue_min_val nil epq_dequeue_new_q nil epq_dequeue_v nil] (try (do (set! epq_dequeue_epq epq_dequeue_epq_p) (when (= (count (:queue epq_dequeue_epq)) 0) (do (panic "The queue is empty") (throw (ex-info "return" {:v {:queue epq_dequeue_epq :value 0}})))) (set! epq_dequeue_min_val (get (:queue epq_dequeue_epq) 0)) (set! epq_dequeue_idx 0) (set! epq_dequeue_i 1) (while (< epq_dequeue_i (count (:queue epq_dequeue_epq))) (do (set! epq_dequeue_v (get (:queue epq_dequeue_epq) epq_dequeue_i)) (when (< epq_dequeue_v epq_dequeue_min_val) (do (set! epq_dequeue_min_val epq_dequeue_v) (set! epq_dequeue_idx epq_dequeue_i))) (set! epq_dequeue_i (+ epq_dequeue_i 1)))) (set! epq_dequeue_new_q []) (set! epq_dequeue_i 0) (while (< epq_dequeue_i (count (:queue epq_dequeue_epq))) (do (when (not= epq_dequeue_i epq_dequeue_idx) (set! epq_dequeue_new_q (conj epq_dequeue_new_q (get (:queue epq_dequeue_epq) epq_dequeue_i)))) (set! epq_dequeue_i (+ epq_dequeue_i 1)))) (set! epq_dequeue_epq (assoc epq_dequeue_epq :queue epq_dequeue_new_q)) (throw (ex-info "return" {:v {:queue epq_dequeue_epq :value epq_dequeue_min_val}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn epq_to_string [epq_to_string_epq]
  (try (throw (ex-info "return" {:v (str (:queue epq_to_string_epq))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fixed_priority_queue []
  (binding [fixed_priority_queue_fpq nil fixed_priority_queue_res nil] (do (set! fixed_priority_queue_fpq (fpq_new)) (set! fixed_priority_queue_fpq (fpq_enqueue fixed_priority_queue_fpq 0 10)) (set! fixed_priority_queue_fpq (fpq_enqueue fixed_priority_queue_fpq 1 70)) (set! fixed_priority_queue_fpq (fpq_enqueue fixed_priority_queue_fpq 0 100)) (set! fixed_priority_queue_fpq (fpq_enqueue fixed_priority_queue_fpq 2 1)) (set! fixed_priority_queue_fpq (fpq_enqueue fixed_priority_queue_fpq 2 5)) (set! fixed_priority_queue_fpq (fpq_enqueue fixed_priority_queue_fpq 1 7)) (set! fixed_priority_queue_fpq (fpq_enqueue fixed_priority_queue_fpq 2 4)) (set! fixed_priority_queue_fpq (fpq_enqueue fixed_priority_queue_fpq 1 64)) (set! fixed_priority_queue_fpq (fpq_enqueue fixed_priority_queue_fpq 0 128)) (println (fpq_to_string fixed_priority_queue_fpq)) (set! fixed_priority_queue_res (fpq_dequeue fixed_priority_queue_fpq)) (set! fixed_priority_queue_fpq (:queue fixed_priority_queue_res)) (println (:value fixed_priority_queue_res)) (set! fixed_priority_queue_res (fpq_dequeue fixed_priority_queue_fpq)) (set! fixed_priority_queue_fpq (:queue fixed_priority_queue_res)) (println (:value fixed_priority_queue_res)) (set! fixed_priority_queue_res (fpq_dequeue fixed_priority_queue_fpq)) (set! fixed_priority_queue_fpq (:queue fixed_priority_queue_res)) (println (:value fixed_priority_queue_res)) (set! fixed_priority_queue_res (fpq_dequeue fixed_priority_queue_fpq)) (set! fixed_priority_queue_fpq (:queue fixed_priority_queue_res)) (println (:value fixed_priority_queue_res)) (set! fixed_priority_queue_res (fpq_dequeue fixed_priority_queue_fpq)) (set! fixed_priority_queue_fpq (:queue fixed_priority_queue_res)) (println (:value fixed_priority_queue_res)) (println (fpq_to_string fixed_priority_queue_fpq)) (set! fixed_priority_queue_res (fpq_dequeue fixed_priority_queue_fpq)) (set! fixed_priority_queue_fpq (:queue fixed_priority_queue_res)) (println (:value fixed_priority_queue_res)) (set! fixed_priority_queue_res (fpq_dequeue fixed_priority_queue_fpq)) (set! fixed_priority_queue_fpq (:queue fixed_priority_queue_res)) (println (:value fixed_priority_queue_res)) (set! fixed_priority_queue_res (fpq_dequeue fixed_priority_queue_fpq)) (set! fixed_priority_queue_fpq (:queue fixed_priority_queue_res)) (println (:value fixed_priority_queue_res)) (set! fixed_priority_queue_res (fpq_dequeue fixed_priority_queue_fpq)) (set! fixed_priority_queue_fpq (:queue fixed_priority_queue_res)) (println (:value fixed_priority_queue_res)) (set! fixed_priority_queue_res (fpq_dequeue fixed_priority_queue_fpq)) (set! fixed_priority_queue_fpq (:queue fixed_priority_queue_res)) (println (:value fixed_priority_queue_res)))))

(defn element_priority_queue []
  (binding [element_priority_queue_epq nil element_priority_queue_res nil] (do (set! element_priority_queue_epq (epq_new)) (set! element_priority_queue_epq (epq_enqueue element_priority_queue_epq 10)) (set! element_priority_queue_epq (epq_enqueue element_priority_queue_epq 70)) (set! element_priority_queue_epq (epq_enqueue element_priority_queue_epq 100)) (set! element_priority_queue_epq (epq_enqueue element_priority_queue_epq 1)) (set! element_priority_queue_epq (epq_enqueue element_priority_queue_epq 5)) (set! element_priority_queue_epq (epq_enqueue element_priority_queue_epq 7)) (set! element_priority_queue_epq (epq_enqueue element_priority_queue_epq 4)) (set! element_priority_queue_epq (epq_enqueue element_priority_queue_epq 64)) (set! element_priority_queue_epq (epq_enqueue element_priority_queue_epq 128)) (println (epq_to_string element_priority_queue_epq)) (set! element_priority_queue_res (epq_dequeue element_priority_queue_epq)) (set! element_priority_queue_epq (:queue element_priority_queue_res)) (println (:value element_priority_queue_res)) (set! element_priority_queue_res (epq_dequeue element_priority_queue_epq)) (set! element_priority_queue_epq (:queue element_priority_queue_res)) (println (:value element_priority_queue_res)) (set! element_priority_queue_res (epq_dequeue element_priority_queue_epq)) (set! element_priority_queue_epq (:queue element_priority_queue_res)) (println (:value element_priority_queue_res)) (set! element_priority_queue_res (epq_dequeue element_priority_queue_epq)) (set! element_priority_queue_epq (:queue element_priority_queue_res)) (println (:value element_priority_queue_res)) (set! element_priority_queue_res (epq_dequeue element_priority_queue_epq)) (set! element_priority_queue_epq (:queue element_priority_queue_res)) (println (:value element_priority_queue_res)) (println (epq_to_string element_priority_queue_epq)) (set! element_priority_queue_res (epq_dequeue element_priority_queue_epq)) (set! element_priority_queue_epq (:queue element_priority_queue_res)) (println (:value element_priority_queue_res)) (set! element_priority_queue_res (epq_dequeue element_priority_queue_epq)) (set! element_priority_queue_epq (:queue element_priority_queue_res)) (println (:value element_priority_queue_res)) (set! element_priority_queue_res (epq_dequeue element_priority_queue_epq)) (set! element_priority_queue_epq (:queue element_priority_queue_res)) (println (:value element_priority_queue_res)) (set! element_priority_queue_res (epq_dequeue element_priority_queue_epq)) (set! element_priority_queue_epq (:queue element_priority_queue_res)) (println (:value element_priority_queue_res)) (set! element_priority_queue_res (epq_dequeue element_priority_queue_epq)) (set! element_priority_queue_epq (:queue element_priority_queue_res)) (println (:value element_priority_queue_res)))))

(defn main []
  (do (fixed_priority_queue) (element_priority_queue)))

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
