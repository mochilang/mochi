(ns main (:refer-clojure :exclude [new_deque is_empty front back insert delete add_first add_last remove_first remove_last main]))

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

(declare new_deque is_empty front back insert delete add_first add_last remove_first remove_last main)

(def ^:dynamic add_first_head nil)

(def ^:dynamic add_first_succ nil)

(def ^:dynamic add_last_pred nil)

(def ^:dynamic add_last_tail nil)

(def ^:dynamic back_idx nil)

(def ^:dynamic back_node nil)

(def ^:dynamic back_tail nil)

(def ^:dynamic delete_d nil)

(def ^:dynamic delete_node nil)

(def ^:dynamic delete_nodes nil)

(def ^:dynamic delete_pred nil)

(def ^:dynamic delete_pred_node nil)

(def ^:dynamic delete_succ nil)

(def ^:dynamic delete_succ_node nil)

(def ^:dynamic delete_val nil)

(def ^:dynamic front_head nil)

(def ^:dynamic front_idx nil)

(def ^:dynamic front_node nil)

(def ^:dynamic insert_d nil)

(def ^:dynamic insert_new_idx nil)

(def ^:dynamic insert_nodes nil)

(def ^:dynamic insert_pred_node nil)

(def ^:dynamic insert_succ_node nil)

(def ^:dynamic main_d nil)

(def ^:dynamic main_r nil)

(def ^:dynamic new_deque_nodes nil)

(def ^:dynamic remove_first_head nil)

(def ^:dynamic remove_first_idx nil)

(def ^:dynamic remove_last_idx nil)

(def ^:dynamic remove_last_tail nil)

(defn new_deque []
  (binding [new_deque_nodes nil] (try (do (set! new_deque_nodes []) (set! new_deque_nodes (conj new_deque_nodes {:data "" :next 1 :prev (- 1)})) (set! new_deque_nodes (conj new_deque_nodes {:data "" :next (- 1) :prev 0})) (throw (ex-info "return" {:v {:header 0 :nodes new_deque_nodes :size 0 :trailer 1}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_empty [is_empty_d]
  (try (throw (ex-info "return" {:v (= (:size is_empty_d) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn front [front_d]
  (binding [front_head nil front_idx nil front_node nil] (try (do (when (is_empty front_d) (throw (Exception. "List is empty"))) (set! front_head (get (:nodes front_d) (:header front_d))) (set! front_idx (:next front_head)) (set! front_node (get (:nodes front_d) front_idx)) (throw (ex-info "return" {:v (:data front_node)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn back [back_d]
  (binding [back_idx nil back_node nil back_tail nil] (try (do (when (is_empty back_d) (throw (Exception. "List is empty"))) (set! back_tail (get (:nodes back_d) (:trailer back_d))) (set! back_idx (:prev back_tail)) (set! back_node (get (:nodes back_d) back_idx)) (throw (ex-info "return" {:v (:data back_node)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert [insert_d_p insert_pred insert_value insert_succ]
  (binding [insert_d nil insert_new_idx nil insert_nodes nil insert_pred_node nil insert_succ_node nil] (try (do (set! insert_d insert_d_p) (set! insert_nodes (:nodes insert_d)) (set! insert_new_idx (count insert_nodes)) (set! insert_nodes (conj insert_nodes {:data insert_value :next insert_succ :prev insert_pred})) (set! insert_pred_node (nth insert_nodes insert_pred)) (set! insert_pred_node (assoc insert_pred_node :next insert_new_idx)) (set! insert_nodes (assoc insert_nodes insert_pred insert_pred_node)) (set! insert_succ_node (nth insert_nodes insert_succ)) (set! insert_succ_node (assoc insert_succ_node :prev insert_new_idx)) (set! insert_nodes (assoc insert_nodes insert_succ insert_succ_node)) (set! insert_d (assoc insert_d :nodes insert_nodes)) (set! insert_d (assoc insert_d :size (+ (:size insert_d) 1))) (throw (ex-info "return" {:v insert_d}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn delete [delete_d_p delete_idx]
  (binding [delete_d nil delete_node nil delete_nodes nil delete_pred nil delete_pred_node nil delete_succ nil delete_succ_node nil delete_val nil] (try (do (set! delete_d delete_d_p) (set! delete_nodes (:nodes delete_d)) (set! delete_node (nth delete_nodes delete_idx)) (set! delete_pred (:prev delete_node)) (set! delete_succ (:next delete_node)) (set! delete_pred_node (nth delete_nodes delete_pred)) (set! delete_pred_node (assoc delete_pred_node :next delete_succ)) (set! delete_nodes (assoc delete_nodes delete_pred delete_pred_node)) (set! delete_succ_node (nth delete_nodes delete_succ)) (set! delete_succ_node (assoc delete_succ_node :prev delete_pred)) (set! delete_nodes (assoc delete_nodes delete_succ delete_succ_node)) (set! delete_val (:data delete_node)) (set! delete_d (assoc delete_d :nodes delete_nodes)) (set! delete_d (assoc delete_d :size (- (:size delete_d) 1))) (throw (ex-info "return" {:v {:deque delete_d :value delete_val}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn add_first [add_first_d add_first_value]
  (binding [add_first_head nil add_first_succ nil] (try (do (set! add_first_head (get (:nodes add_first_d) (:header add_first_d))) (set! add_first_succ (:next add_first_head)) (throw (ex-info "return" {:v (insert add_first_d (:header add_first_d) add_first_value add_first_succ)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn add_last [add_last_d add_last_value]
  (binding [add_last_pred nil add_last_tail nil] (try (do (set! add_last_tail (get (:nodes add_last_d) (:trailer add_last_d))) (set! add_last_pred (:prev add_last_tail)) (throw (ex-info "return" {:v (insert add_last_d add_last_pred add_last_value (:trailer add_last_d))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_first [remove_first_d]
  (binding [remove_first_head nil remove_first_idx nil] (try (do (when (is_empty remove_first_d) (throw (Exception. "remove_first from empty list"))) (set! remove_first_head (get (:nodes remove_first_d) (:header remove_first_d))) (set! remove_first_idx (:next remove_first_head)) (throw (ex-info "return" {:v (delete remove_first_d remove_first_idx)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_last [remove_last_d]
  (binding [remove_last_idx nil remove_last_tail nil] (try (do (when (is_empty remove_last_d) (throw (Exception. "remove_first from empty list"))) (set! remove_last_tail (get (:nodes remove_last_d) (:trailer remove_last_d))) (set! remove_last_idx (:prev remove_last_tail)) (throw (ex-info "return" {:v (delete remove_last_d remove_last_idx)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_d nil main_r nil] (do (set! main_d (new_deque)) (set! main_d (add_first main_d "A")) (println (front main_d)) (set! main_d (add_last main_d "B")) (println (back main_d)) (set! main_r (remove_first main_d)) (set! main_d (:deque main_r)) (println (:value main_r)) (set! main_r (remove_last main_d)) (set! main_d (:deque main_r)) (println (:value main_r)) (println (str (is_empty main_d))))))

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
