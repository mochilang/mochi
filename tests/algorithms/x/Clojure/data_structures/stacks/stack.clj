(ns main (:refer-clojure :exclude [make_stack is_empty size is_full push pop peek contains stack_repr main]))

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

(declare make_stack is_empty size is_full push pop peek contains stack_repr main)

(def ^:dynamic contains_i nil)

(def ^:dynamic main_s nil)

(def ^:dynamic pop_n nil)

(def ^:dynamic pop_s nil)

(def ^:dynamic pop_val nil)

(def ^:dynamic push_s nil)

(defn make_stack [make_stack_limit]
  (try (throw (ex-info "return" {:v {:items [] :limit make_stack_limit}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_empty [is_empty_s]
  (try (throw (ex-info "return" {:v (= (count (:items is_empty_s)) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn size [size_s]
  (try (throw (ex-info "return" {:v (count (:items size_s))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_full [is_full_s]
  (try (throw (ex-info "return" {:v (>= (count (:items is_full_s)) (:limit is_full_s))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn push [push_s_p push_item]
  (binding [push_s nil] (do (set! push_s push_s_p) (when (is_full push_s) (throw (Exception. "stack overflow"))) (set! push_s (assoc push_s :items (conj (:items push_s) push_item))))))

(defn pop [pop_s_p]
  (binding [pop_n nil pop_s nil pop_val nil] (try (do (set! pop_s pop_s_p) (when (is_empty pop_s) (throw (Exception. "stack underflow"))) (set! pop_n (count (:items pop_s))) (set! pop_val (get (:items pop_s) (- pop_n 1))) (set! pop_s (assoc pop_s :items (subvec (:items pop_s) 0 (- pop_n 1)))) (throw (ex-info "return" {:v pop_val}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn peek [peek_s]
  (try (do (when (is_empty peek_s) (throw (Exception. "peek from empty stack"))) (throw (ex-info "return" {:v (get (:items peek_s) (- (count (:items peek_s)) 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn contains [contains_s contains_item]
  (binding [contains_i nil] (try (do (set! contains_i 0) (while (< contains_i (count (:items contains_s))) (do (when (= (get (:items contains_s) contains_i) contains_item) (throw (ex-info "return" {:v true}))) (set! contains_i (+ contains_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn stack_repr [stack_repr_s]
  (try (throw (ex-info "return" {:v (str (:items stack_repr_s))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_s nil] (do (set! main_s (make_stack 5)) (println (str (is_empty main_s))) (push main_s 0) (push main_s 1) (push main_s 2) (println (str (peek main_s))) (println (str (size main_s))) (println (str (is_full main_s))) (push main_s 3) (push main_s 4) (println (str (is_full main_s))) (println (stack_repr main_s)) (println (str (pop main_s))) (println (str (peek main_s))) (println (str (contains main_s 1))) (println (str (contains main_s 9))))))

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
