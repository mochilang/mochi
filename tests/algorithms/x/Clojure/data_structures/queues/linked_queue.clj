(ns main (:refer-clojure :exclude [new_queue is_empty put get length to_string clear]))

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

(declare new_queue is_empty put get length to_string clear)

(def ^:dynamic clear_q nil)

(def ^:dynamic count_v nil)

(def ^:dynamic first_v nil)

(def ^:dynamic get_idx nil)

(def ^:dynamic get_node nil)

(def ^:dynamic get_q nil)

(def ^:dynamic length_idx nil)

(def ^:dynamic put_idx nil)

(def ^:dynamic put_node nil)

(def ^:dynamic put_nodes nil)

(def ^:dynamic put_q nil)

(def ^:dynamic to_string_idx nil)

(def ^:dynamic to_string_node nil)

(def ^:dynamic to_string_res nil)

(defn new_queue []
  (try (throw (ex-info "return" {:v {:front (- 0 1) :nodes [] :rear (- 0 1)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_empty [is_empty_q]
  (try (throw (ex-info "return" {:v (= (:front is_empty_q) (- 0 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn put [put_q_p put_item]
  (binding [put_idx nil put_node nil put_nodes nil put_q nil] (do (set! put_q put_q_p) (set! put_node {:data put_item :next (- 0 1)}) (set! put_q (assoc put_q :nodes (conj (:nodes put_q) put_node))) (set! put_idx (- (count (:nodes put_q)) 1)) (if (= (:front put_q) (- 0 1)) (do (set! put_q (assoc put_q :front put_idx)) (set! put_q (assoc put_q :rear put_idx))) (do (set! put_nodes (:nodes put_q)) (set! put_nodes (assoc-in put_nodes [(:rear put_q) :next] put_idx)) (set! put_q (assoc put_q :nodes put_nodes)) (set! put_q (assoc put_q :rear put_idx)))))))

(defn get [get_q_p]
  (binding [get_idx nil get_node nil get_q nil] (try (do (set! get_q get_q_p) (when (is_empty get_q) (throw (Exception. "dequeue from empty queue"))) (set! get_idx (:front get_q)) (set! get_node (get (:nodes get_q) get_idx)) (set! get_q (assoc get_q :front (:next get_node))) (when (= (:front get_q) (- 0 1)) (set! get_q (assoc get_q :rear (- 0 1)))) (throw (ex-info "return" {:v (:data get_node)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn length [length_q]
  (binding [count_v nil length_idx nil] (try (do (set! count_v 0) (set! length_idx (:front length_q)) (while (not= length_idx (- 0 1)) (do (set! count_v (+ count_v 1)) (set! length_idx (:next (get (:nodes length_q) length_idx))))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn to_string [to_string_q]
  (binding [first_v nil to_string_idx nil to_string_node nil to_string_res nil] (try (do (set! to_string_res "") (set! to_string_idx (:front to_string_q)) (set! first_v true) (while (not= to_string_idx (- 0 1)) (do (set! to_string_node (get (:nodes to_string_q) to_string_idx)) (if first_v (do (set! to_string_res (:data to_string_node)) (set! first_v false)) (set! to_string_res (str (str to_string_res " <- ") (:data to_string_node)))) (set! to_string_idx (:next to_string_node)))) (throw (ex-info "return" {:v to_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn clear [clear_q_p]
  (binding [clear_q nil] (do (set! clear_q clear_q_p) (set! clear_q (assoc clear_q :nodes [])) (set! clear_q (assoc clear_q :front (- 0 1))) (set! clear_q (assoc clear_q :rear (- 0 1))))))

(def ^:dynamic main_queue (new_queue))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_empty main_queue)))
      (put main_queue "5")
      (put main_queue "9")
      (put main_queue "python")
      (println (str (is_empty main_queue)))
      (println (get main_queue))
      (put main_queue "algorithms")
      (println (get main_queue))
      (println (get main_queue))
      (println (get main_queue))
      (println (str (is_empty main_queue)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
