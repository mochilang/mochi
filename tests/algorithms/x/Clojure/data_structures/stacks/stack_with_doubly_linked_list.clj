(ns main (:refer-clojure :exclude [empty_stack push pop top size is_empty print_stack main]))

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

(declare empty_stack push pop top size is_empty print_stack main)

(def ^:dynamic count_v nil)

(def ^:dynamic main_p nil)

(def ^:dynamic main_stack nil)

(def ^:dynamic main_t nil)

(def ^:dynamic pop_head_node nil)

(def ^:dynamic pop_new_stack nil)

(def ^:dynamic pop_next_idx nil)

(def ^:dynamic pop_next_node nil)

(def ^:dynamic pop_nodes nil)

(def ^:dynamic pop_value nil)

(def ^:dynamic print_stack_idx nil)

(def ^:dynamic print_stack_node nil)

(def ^:dynamic print_stack_s nil)

(def ^:dynamic push_head_node nil)

(def ^:dynamic push_idx nil)

(def ^:dynamic push_new_node nil)

(def ^:dynamic push_nodes nil)

(def ^:dynamic size_idx nil)

(def ^:dynamic size_node nil)

(def ^:dynamic top_node nil)

(defn empty_stack []
  (try (throw (ex-info "return" {:v {:head (- 0 1) :nodes []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn push [push_stack push_value]
  (binding [push_head_node nil push_idx nil push_new_node nil push_nodes nil] (try (do (set! push_nodes (:nodes push_stack)) (set! push_idx (count push_nodes)) (set! push_new_node {:data push_value :next (:head push_stack) :prev (- 0 1)}) (set! push_nodes (conj push_nodes push_new_node)) (when (not= (:head push_stack) (- 0 1)) (do (set! push_head_node (nth push_nodes (:head push_stack))) (set! push_head_node (assoc push_head_node :prev push_idx)) (set! push_nodes (assoc push_nodes (:head push_stack) push_head_node)))) (throw (ex-info "return" {:v {:head push_idx :nodes push_nodes}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pop [pop_stack]
  (binding [pop_head_node nil pop_new_stack nil pop_next_idx nil pop_next_node nil pop_nodes nil pop_value nil] (try (do (when (= (:head pop_stack) (- 0 1)) (throw (ex-info "return" {:v {:ok false :stack pop_stack :value 0}}))) (set! pop_nodes (:nodes pop_stack)) (set! pop_head_node (nth pop_nodes (:head pop_stack))) (set! pop_value (:data pop_head_node)) (set! pop_next_idx (:next pop_head_node)) (when (not= pop_next_idx (- 0 1)) (do (set! pop_next_node (nth pop_nodes pop_next_idx)) (set! pop_next_node (assoc pop_next_node :prev (- 0 1))) (set! pop_nodes (assoc pop_nodes pop_next_idx pop_next_node)))) (set! pop_new_stack {:head pop_next_idx :nodes pop_nodes}) (throw (ex-info "return" {:v {:ok true :stack pop_new_stack :value pop_value}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn top [top_stack]
  (binding [top_node nil] (try (do (when (= (:head top_stack) (- 0 1)) (throw (ex-info "return" {:v {:ok false :value 0}}))) (set! top_node (get (:nodes top_stack) (:head top_stack))) (throw (ex-info "return" {:v {:ok true :value (:data top_node)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn size [size_stack]
  (binding [count_v nil size_idx nil size_node nil] (try (do (set! count_v 0) (set! size_idx (:head size_stack)) (while (not= size_idx (- 0 1)) (do (set! count_v (+ count_v 1)) (set! size_node (get (:nodes size_stack) size_idx)) (set! size_idx (:next size_node)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_empty [is_empty_stack]
  (try (throw (ex-info "return" {:v (= (:head is_empty_stack) (- 0 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn print_stack [print_stack_stack]
  (binding [print_stack_idx nil print_stack_node nil print_stack_s nil] (do (println "stack elements are:") (set! print_stack_idx (:head print_stack_stack)) (set! print_stack_s "") (while (not= print_stack_idx (- 0 1)) (do (set! print_stack_node (get (:nodes print_stack_stack) print_stack_idx)) (set! print_stack_s (str (str print_stack_s (str (:data print_stack_node))) "->")) (set! print_stack_idx (:next print_stack_node)))) (when (> (count print_stack_s) 0) (println print_stack_s)))))

(defn main []
  (binding [main_p nil main_stack nil main_t nil] (do (set! main_stack (empty_stack)) (println "Stack operations using Doubly LinkedList") (set! main_stack (push main_stack 4)) (set! main_stack (push main_stack 5)) (set! main_stack (push main_stack 6)) (set! main_stack (push main_stack 7)) (print_stack main_stack) (set! main_t (top main_stack)) (if (:ok main_t) (println (str "Top element is " (str (:value main_t)))) (println "Top element is None")) (println (str "Size of the stack is " (str (size main_stack)))) (set! main_p (pop main_stack)) (set! main_stack (:stack main_p)) (set! main_p (pop main_stack)) (set! main_stack (:stack main_p)) (print_stack main_stack) (println (str "stack is empty: " (str (is_empty main_stack)))))))

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
