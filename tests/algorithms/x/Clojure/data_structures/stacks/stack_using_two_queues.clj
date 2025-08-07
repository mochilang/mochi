(ns main (:refer-clojure :exclude [make_stack push pop peek is_empty]))

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

(declare make_stack push pop peek is_empty)

(def ^:dynamic pop_item nil)

(def ^:dynamic pop_s nil)

(def ^:dynamic push_new_main nil)

(def ^:dynamic push_s nil)

(defn make_stack []
  (try (throw (ex-info "return" {:v {:main_queue [] :temp_queue []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn push [push_s_p push_item]
  (binding [push_new_main nil push_s nil] (do (set! push_s push_s_p) (set! push_s (assoc push_s :temp_queue (conj (:temp_queue push_s) push_item))) (while (> (count (:main_queue push_s)) 0) (do (set! push_s (assoc push_s :temp_queue (conj (:temp_queue push_s) (get (:main_queue push_s) 0)))) (set! push_s (assoc push_s :main_queue (subvec (:main_queue push_s) 1 (count (:main_queue push_s))))))) (set! push_new_main (:temp_queue push_s)) (set! push_s (assoc push_s :temp_queue (:main_queue push_s))) (set! push_s (assoc push_s :main_queue push_new_main)))))

(defn pop [pop_s_p]
  (binding [pop_item nil pop_s nil] (try (do (set! pop_s pop_s_p) (when (= (count (:main_queue pop_s)) 0) (throw (Exception. "pop from empty stack"))) (set! pop_item (get (:main_queue pop_s) 0)) (set! pop_s (assoc pop_s :main_queue (subvec (:main_queue pop_s) 1 (count (:main_queue pop_s))))) (throw (ex-info "return" {:v pop_item}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn peek [peek_s]
  (try (do (when (= (count (:main_queue peek_s)) 0) (throw (Exception. "peek from empty stack"))) (throw (ex-info "return" {:v (get (:main_queue peek_s) 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_empty [is_empty_s]
  (try (throw (ex-info "return" {:v (= (count (:main_queue is_empty_s)) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_stack (make_stack))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (push main_stack 1)
      (push main_stack 2)
      (push main_stack 3)
      (println (str (peek main_stack)))
      (println (str (pop main_stack)))
      (println (str (peek main_stack)))
      (println (str (pop main_stack)))
      (println (str (pop main_stack)))
      (println (str (is_empty main_stack)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
