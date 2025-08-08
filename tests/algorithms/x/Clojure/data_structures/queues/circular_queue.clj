(ns main (:refer-clojure :exclude [create_queue length is_empty front enqueue dequeue main]))

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

(declare create_queue length is_empty front enqueue dequeue main)

(def ^:dynamic create_queue_arr nil)

(def ^:dynamic create_queue_i nil)

(def ^:dynamic dequeue_arr2 nil)

(def ^:dynamic dequeue_q nil)

(def ^:dynamic dequeue_value nil)

(def ^:dynamic enqueue_arr nil)

(def ^:dynamic enqueue_q nil)

(def ^:dynamic main_q nil)

(def ^:dynamic main_r nil)

(defn create_queue [create_queue_capacity]
  (binding [create_queue_arr nil create_queue_i nil] (try (do (set! create_queue_arr []) (set! create_queue_i 0) (while (< create_queue_i create_queue_capacity) (do (set! create_queue_arr (conj create_queue_arr 0)) (set! create_queue_i (+ create_queue_i 1)))) (throw (ex-info "return" {:v {:capacity create_queue_capacity :data create_queue_arr :front 0 :rear 0 :size 0}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn length [length_q]
  (try (throw (ex-info "return" {:v (:size length_q)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_empty [is_empty_q]
  (try (throw (ex-info "return" {:v (= (:size is_empty_q) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn front [front_q]
  (try (if (is_empty front_q) 0 (get (:data front_q) (:front front_q))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn enqueue [enqueue_q_p enqueue_value]
  (binding [enqueue_arr nil enqueue_q nil] (try (do (set! enqueue_q enqueue_q_p) (when (>= (:size enqueue_q) (:capacity enqueue_q)) (throw (Exception. "QUEUE IS FULL"))) (set! enqueue_arr (:data enqueue_q)) (set! enqueue_arr (assoc enqueue_arr (:rear enqueue_q) enqueue_value)) (set! enqueue_q (assoc enqueue_q :data enqueue_arr)) (set! enqueue_q (assoc enqueue_q :rear (mod (+ (:rear enqueue_q) 1) (:capacity enqueue_q)))) (set! enqueue_q (assoc enqueue_q :size (+ (:size enqueue_q) 1))) (throw (ex-info "return" {:v enqueue_q}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dequeue [dequeue_q_p]
  (binding [dequeue_arr2 nil dequeue_q nil dequeue_value nil] (try (do (set! dequeue_q dequeue_q_p) (when (= (:size dequeue_q) 0) (throw (Exception. "UNDERFLOW"))) (set! dequeue_value (get (:data dequeue_q) (:front dequeue_q))) (set! dequeue_arr2 (:data dequeue_q)) (set! dequeue_arr2 (assoc dequeue_arr2 (:front dequeue_q) 0)) (set! dequeue_q (assoc dequeue_q :data dequeue_arr2)) (set! dequeue_q (assoc dequeue_q :front (mod (+ (:front dequeue_q) 1) (:capacity dequeue_q)))) (set! dequeue_q (assoc dequeue_q :size (- (:size dequeue_q) 1))) (throw (ex-info "return" {:v {:queue dequeue_q :value dequeue_value}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_q nil main_r nil] (do (set! main_q (create_queue 5)) (println (is_empty main_q)) (set! main_q (enqueue main_q 10)) (println (is_empty main_q)) (set! main_q (enqueue main_q 20)) (set! main_q (enqueue main_q 30)) (println (front main_q)) (set! main_r (dequeue main_q)) (set! main_q (:queue main_r)) (println (:value main_r)) (println (front main_q)) (println (length main_q)))))

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
