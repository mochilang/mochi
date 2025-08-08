(ns main (:refer-clojure :exclude [new_queue len_queue str_queue put get rotate get_front]))

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

(declare new_queue len_queue str_queue put get rotate get_front)

(def ^:dynamic first_v nil)

(def ^:dynamic get_i nil)

(def ^:dynamic get_new_entries nil)

(def ^:dynamic get_value nil)

(def ^:dynamic main_q nil)

(def ^:dynamic put_e nil)

(def ^:dynamic rest_v nil)

(def ^:dynamic rotate_e nil)

(def ^:dynamic rotate_i nil)

(def ^:dynamic rotate_r nil)

(def ^:dynamic str_queue_i nil)

(def ^:dynamic str_queue_s nil)

(defn new_queue [new_queue_items]
  (try (throw (ex-info "return" {:v {:entries new_queue_items}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn len_queue [len_queue_q]
  (try (throw (ex-info "return" {:v (count (:entries len_queue_q))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn str_queue [str_queue_q]
  (binding [str_queue_i nil str_queue_s nil] (try (do (set! str_queue_s "Queue((") (set! str_queue_i 0) (while (< str_queue_i (count (:entries str_queue_q))) (do (set! str_queue_s (str str_queue_s (str (get (:entries str_queue_q) str_queue_i)))) (when (< str_queue_i (- (count (:entries str_queue_q)) 1)) (set! str_queue_s (str str_queue_s ", "))) (set! str_queue_i (+ str_queue_i 1)))) (set! str_queue_s (str str_queue_s "))")) (throw (ex-info "return" {:v str_queue_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn put [put_q put_item]
  (binding [put_e nil] (try (do (set! put_e (:entries put_q)) (set! put_e (conj put_e put_item)) (throw (ex-info "return" {:v {:entries put_e}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get [get_q]
  (binding [get_i nil get_new_entries nil get_value nil] (try (do (when (= (count (:entries get_q)) 0) (throw (Exception. "Queue is empty"))) (set! get_value (get (:entries get_q) 0)) (set! get_new_entries []) (set! get_i 1) (while (< get_i (count (:entries get_q))) (do (set! get_new_entries (conj get_new_entries (get (:entries get_q) get_i))) (set! get_i (+ get_i 1)))) (throw (ex-info "return" {:v {:queue {:entries get_new_entries} :value get_value}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rotate [rotate_q rotate_rotation]
  (binding [first_v nil rest_v nil rotate_e nil rotate_i nil rotate_r nil] (try (do (set! rotate_e (:entries rotate_q)) (set! rotate_r 0) (while (< rotate_r rotate_rotation) (do (when (> (count rotate_e) 0) (do (set! first_v (nth rotate_e 0)) (set! rest_v []) (set! rotate_i 1) (while (< rotate_i (count rotate_e)) (do (set! rest_v (conj rest_v (nth rotate_e rotate_i))) (set! rotate_i (+ rotate_i 1)))) (set! rest_v (conj rest_v first_v)) (set! rotate_e rest_v))) (set! rotate_r (+ rotate_r 1)))) (throw (ex-info "return" {:v {:entries rotate_e}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_front [get_front_q]
  (try (throw (ex-info "return" {:v (get (:entries get_front_q) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_q (new_queue []))

(def ^:dynamic main_res (get main_q))

(def ^:dynamic main_front (get_front main_q))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (len_queue main_q))
      (def main_q (put main_q 10))
      (def main_q (put main_q 20))
      (def main_q (put main_q 30))
      (def main_q (put main_q 40))
      (println (str_queue main_q))
      (def main_q (:queue main_res))
      (println (:value main_res))
      (println (str_queue main_q))
      (def main_q (rotate main_q 2))
      (println (str_queue main_q))
      (println main_front)
      (println (str_queue main_q))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
