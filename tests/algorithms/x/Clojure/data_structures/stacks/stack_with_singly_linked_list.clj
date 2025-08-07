(ns main (:refer-clojure :exclude [empty_stack is_empty push pop peek clear main]))

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

(declare empty_stack is_empty push pop peek clear main)

(def ^:dynamic main_res nil)

(def ^:dynamic main_stack nil)

(def ^:dynamic peek_node nil)

(def ^:dynamic pop_new_stack nil)

(def ^:dynamic pop_new_top nil)

(def ^:dynamic pop_node nil)

(def ^:dynamic push_new_node nil)

(def ^:dynamic push_new_nodes nil)

(def ^:dynamic push_new_top nil)

(defn empty_stack []
  (try (throw (ex-info "return" {:v {:nodes [] :top (- 1)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_empty [is_empty_stack]
  (try (throw (ex-info "return" {:v (= (:top is_empty_stack) (- 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn push [push_stack push_item]
  (binding [push_new_node nil push_new_nodes nil push_new_top nil] (try (do (set! push_new_node {:next (:top push_stack) :value push_item}) (set! push_new_nodes (:nodes push_stack)) (set! push_new_nodes (conj push_new_nodes push_new_node)) (set! push_new_top (- (count push_new_nodes) 1)) (throw (ex-info "return" {:v {:nodes push_new_nodes :top push_new_top}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pop [pop_stack]
  (binding [pop_new_stack nil pop_new_top nil pop_node nil] (try (do (when (= (:top pop_stack) (- 1)) (throw (Exception. "pop from empty stack"))) (set! pop_node (get (:nodes pop_stack) (:top pop_stack))) (set! pop_new_top (:next pop_node)) (set! pop_new_stack {:nodes (:nodes pop_stack) :top pop_new_top}) (throw (ex-info "return" {:v {:stack pop_new_stack :value (:value pop_node)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn peek [peek_stack]
  (binding [peek_node nil] (try (do (when (= (:top peek_stack) (- 1)) (throw (Exception. "peek from empty stack"))) (set! peek_node (get (:nodes peek_stack) (:top peek_stack))) (throw (ex-info "return" {:v (:value peek_node)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn clear [clear_stack]
  (try (throw (ex-info "return" {:v {:nodes [] :top (- 1)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_res nil main_stack nil] (do (set! main_stack (empty_stack)) (println (is_empty main_stack)) (set! main_stack (push main_stack "5")) (set! main_stack (push main_stack "9")) (set! main_stack (push main_stack "python")) (println (is_empty main_stack)) (set! main_res (pop main_stack)) (set! main_stack (:stack main_res)) (println (:value main_res)) (set! main_stack (push main_stack "algorithms")) (set! main_res (pop main_stack)) (set! main_stack (:stack main_res)) (println (:value main_res)) (set! main_res (pop main_stack)) (set! main_stack (:stack main_res)) (println (:value main_res)) (set! main_res (pop main_stack)) (set! main_stack (:stack main_res)) (println (:value main_res)) (println (is_empty main_stack)))))

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
