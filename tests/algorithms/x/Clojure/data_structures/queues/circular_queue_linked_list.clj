(ns main (:refer-clojure :exclude [create_queue is_empty check_can_perform check_is_full peek enqueue dequeue main]))

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

(declare create_queue is_empty check_can_perform check_is_full peek enqueue dequeue main)

(def ^:dynamic create_queue_data nil)

(def ^:dynamic create_queue_i nil)

(def ^:dynamic create_queue_prev nil)

(def ^:dynamic dequeue_data nil)

(def ^:dynamic dequeue_q nil)

(def ^:dynamic dequeue_val nil)

(def ^:dynamic enqueue_data nil)

(def ^:dynamic enqueue_q nil)

(def ^:dynamic main_q nil)

(def ^:dynamic main_res nil)

(def ^:dynamic next_v nil)

(defn create_queue [create_queue_capacity]
  (binding [create_queue_data nil create_queue_i nil create_queue_prev nil next_v nil] (try (do (set! create_queue_data []) (set! next_v []) (set! create_queue_prev []) (set! create_queue_i 0) (while (< create_queue_i create_queue_capacity) (do (set! create_queue_data (conj create_queue_data "")) (set! next_v (conj next_v (mod (+ create_queue_i 1) create_queue_capacity))) (set! create_queue_prev (conj create_queue_prev (mod (+ (- create_queue_i 1) create_queue_capacity) create_queue_capacity))) (set! create_queue_i (+ create_queue_i 1)))) (throw (ex-info "return" {:v {:data create_queue_data :front 0 :next next_v :prev create_queue_prev :rear 0}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_empty [is_empty_q]
  (try (throw (ex-info "return" {:v (and (= (:front is_empty_q) (:rear is_empty_q)) (= (get (:data is_empty_q) (:front is_empty_q)) ""))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn check_can_perform [check_can_perform_q]
  (when (is_empty check_can_perform_q) (throw (Exception. "Empty Queue"))))

(defn check_is_full [check_is_full_q]
  (when (= (get (:next check_is_full_q) (:rear check_is_full_q)) (:front check_is_full_q)) (throw (Exception. "Full Queue"))))

(defn peek [peek_q]
  (try (do (check_can_perform peek_q) (throw (ex-info "return" {:v (get (:data peek_q) (:front peek_q))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn enqueue [enqueue_q_p enqueue_value]
  (binding [enqueue_data nil enqueue_q nil] (try (do (set! enqueue_q enqueue_q_p) (check_is_full enqueue_q) (when (not (is_empty enqueue_q)) (set! enqueue_q (assoc enqueue_q :rear (get (:next enqueue_q) (:rear enqueue_q))))) (set! enqueue_data (:data enqueue_q)) (set! enqueue_data (assoc enqueue_data (:rear enqueue_q) enqueue_value)) (set! enqueue_q (assoc enqueue_q :data enqueue_data)) (throw (ex-info "return" {:v enqueue_q}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dequeue [dequeue_q_p]
  (binding [dequeue_data nil dequeue_q nil dequeue_val nil] (try (do (set! dequeue_q dequeue_q_p) (check_can_perform dequeue_q) (set! dequeue_data (:data dequeue_q)) (set! dequeue_val (nth dequeue_data (:front dequeue_q))) (set! dequeue_data (assoc dequeue_data (:front dequeue_q) "")) (set! dequeue_q (assoc dequeue_q :data dequeue_data)) (when (not= (:front dequeue_q) (:rear dequeue_q)) (set! dequeue_q (assoc dequeue_q :front (get (:next dequeue_q) (:front dequeue_q))))) (throw (ex-info "return" {:v {:queue dequeue_q :value dequeue_val}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_q nil main_res nil] (do (set! main_q (create_queue 3)) (println (str (is_empty main_q))) (set! main_q (enqueue main_q "a")) (set! main_q (enqueue main_q "b")) (println (peek main_q)) (set! main_res (dequeue main_q)) (set! main_q (:queue main_res)) (println (:value main_res)) (set! main_res (dequeue main_q)) (set! main_q (:queue main_res)) (println (:value main_res)) (println (str (is_empty main_q))))))

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
