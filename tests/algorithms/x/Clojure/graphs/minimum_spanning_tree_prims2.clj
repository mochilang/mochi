(ns main (:refer-clojure :exclude [prims_algo iabs]))

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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare prims_algo iabs)

(def ^:dynamic main_graph nil)

(def ^:dynamic prims_algo_INF nil)

(def ^:dynamic prims_algo_best_idx nil)

(def ^:dynamic prims_algo_dist nil)

(def ^:dynamic prims_algo_i nil)

(def ^:dynamic prims_algo_j nil)

(def ^:dynamic prims_algo_k nil)

(def ^:dynamic prims_algo_min_idx nil)

(def ^:dynamic prims_algo_new_q nil)

(def ^:dynamic prims_algo_node nil)

(def ^:dynamic prims_algo_node_entry nil)

(def ^:dynamic prims_algo_p nil)

(def ^:dynamic prims_algo_parent nil)

(def ^:dynamic prims_algo_q nil)

(def ^:dynamic prims_algo_queue nil)

(def ^:dynamic prims_algo_r nil)

(def ^:dynamic prims_algo_start nil)

(def ^:dynamic prims_algo_start_node nil)

(def ^:dynamic prims_algo_tmp nil)

(def ^:dynamic prims_algo_w nil)

(defn prims_algo [prims_algo_graph]
  (binding [prims_algo_INF nil prims_algo_best_idx nil prims_algo_dist nil prims_algo_i nil prims_algo_j nil prims_algo_k nil prims_algo_min_idx nil prims_algo_new_q nil prims_algo_node nil prims_algo_node_entry nil prims_algo_p nil prims_algo_parent nil prims_algo_q nil prims_algo_queue nil prims_algo_r nil prims_algo_start nil prims_algo_start_node nil prims_algo_tmp nil prims_algo_w nil] (try (do (set! prims_algo_INF 2147483647) (set! prims_algo_dist {}) (set! prims_algo_parent {}) (set! prims_algo_queue []) (doseq [node (keys prims_algo_graph)] (do (set! prims_algo_dist (assoc prims_algo_dist node prims_algo_INF)) (set! prims_algo_parent (assoc prims_algo_parent node "")) (set! prims_algo_queue (conj prims_algo_queue {:node node :weight prims_algo_INF})))) (when (= (count prims_algo_queue) 0) (throw (ex-info "return" {:v {:dist prims_algo_dist :parent prims_algo_parent}}))) (set! prims_algo_min_idx 0) (set! prims_algo_i 1) (while (< prims_algo_i (count prims_algo_queue)) (do (when (< (:weight (nth prims_algo_queue prims_algo_i)) (:weight (nth prims_algo_queue prims_algo_min_idx))) (set! prims_algo_min_idx prims_algo_i)) (set! prims_algo_i (+ prims_algo_i 1)))) (set! prims_algo_start_node (nth prims_algo_queue prims_algo_min_idx)) (set! prims_algo_start (:node prims_algo_start_node)) (set! prims_algo_new_q []) (set! prims_algo_j 0) (while (< prims_algo_j (count prims_algo_queue)) (do (when (not= prims_algo_j prims_algo_min_idx) (set! prims_algo_new_q (conj prims_algo_new_q (nth prims_algo_queue prims_algo_j)))) (set! prims_algo_j (+ prims_algo_j 1)))) (set! prims_algo_queue prims_algo_new_q) (set! prims_algo_dist (assoc prims_algo_dist prims_algo_start 0)) (doseq [neighbour (get prims_algo_graph prims_algo_start)] (do (set! prims_algo_w (nth (get prims_algo_graph prims_algo_start) neighbour)) (when (> (get prims_algo_dist neighbour) (+ (get prims_algo_dist prims_algo_start) prims_algo_w)) (do (set! prims_algo_dist (assoc prims_algo_dist neighbour (+ (get prims_algo_dist prims_algo_start) prims_algo_w))) (set! prims_algo_parent (assoc prims_algo_parent neighbour prims_algo_start)) (set! prims_algo_k 0) (loop [while_flag_1 true] (when (and while_flag_1 (< prims_algo_k (count prims_algo_queue))) (cond (= (:node (nth prims_algo_queue prims_algo_k)) neighbour) (do (set! prims_algo_queue (assoc-in prims_algo_queue [prims_algo_k :weight] (get prims_algo_dist neighbour))) (recur false)) :else (do (set! prims_algo_k (+ prims_algo_k 1)) (recur while_flag_1))))))))) (while (> (count prims_algo_queue) 0) (do (set! prims_algo_best_idx 0) (set! prims_algo_p 1) (while (< prims_algo_p (count prims_algo_queue)) (do (when (< (:weight (nth prims_algo_queue prims_algo_p)) (:weight (nth prims_algo_queue prims_algo_best_idx))) (set! prims_algo_best_idx prims_algo_p)) (set! prims_algo_p (+ prims_algo_p 1)))) (set! prims_algo_node_entry (nth prims_algo_queue prims_algo_best_idx)) (set! prims_algo_node (:node prims_algo_node_entry)) (set! prims_algo_tmp []) (set! prims_algo_q 0) (while (< prims_algo_q (count prims_algo_queue)) (do (when (not= prims_algo_q prims_algo_best_idx) (set! prims_algo_tmp (conj prims_algo_tmp (nth prims_algo_queue prims_algo_q)))) (set! prims_algo_q (+ prims_algo_q 1)))) (set! prims_algo_queue prims_algo_tmp) (doseq [neighbour (get prims_algo_graph prims_algo_node)] (do (set! prims_algo_w (nth (get prims_algo_graph prims_algo_node) neighbour)) (when (> (get prims_algo_dist neighbour) (+ (get prims_algo_dist prims_algo_node) prims_algo_w)) (do (set! prims_algo_dist (assoc prims_algo_dist neighbour (+ (get prims_algo_dist prims_algo_node) prims_algo_w))) (set! prims_algo_parent (assoc prims_algo_parent neighbour prims_algo_node)) (set! prims_algo_r 0) (loop [while_flag_2 true] (when (and while_flag_2 (< prims_algo_r (count prims_algo_queue))) (cond (= (:node (nth prims_algo_queue prims_algo_r)) neighbour) (do (set! prims_algo_queue (assoc-in prims_algo_queue [prims_algo_r :weight] (get prims_algo_dist neighbour))) (recur false)) :else (do (set! prims_algo_r (+ prims_algo_r 1)) (recur while_flag_2))))))))))) (throw (ex-info "return" {:v {:dist prims_algo_dist :parent prims_algo_parent}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn iabs [iabs_x]
  (try (if (< iabs_x 0) (- iabs_x) iabs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_graph {})

(def ^:dynamic main_res (prims_algo main_graph))

(def ^:dynamic main_dist (:dist main_res))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def main_graph (assoc main_graph "a" {"b" 3 "c" 15}))
      (def main_graph (assoc main_graph "b" {"a" 3 "c" 10 "d" 100}))
      (def main_graph (assoc main_graph "c" {"a" 15 "b" 10 "d" 5}))
      (def main_graph (assoc main_graph "d" {"b" 100 "c" 5}))
      (println (str (iabs (- (get main_dist "a") (get main_dist "b")))))
      (println (str (iabs (- (get main_dist "d") (get main_dist "b")))))
      (println (str (iabs (- (get main_dist "a") (get main_dist "c")))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
