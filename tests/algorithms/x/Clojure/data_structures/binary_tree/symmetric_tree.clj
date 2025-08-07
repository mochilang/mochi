(ns main (:refer-clojure :exclude [make_symmetric_tree make_asymmetric_tree is_symmetric_tree]))

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

(declare make_symmetric_tree make_asymmetric_tree is_symmetric_tree)

(def ^:dynamic is_symmetric_tree_left nil)

(def ^:dynamic is_symmetric_tree_lnode nil)

(def ^:dynamic is_symmetric_tree_right nil)

(def ^:dynamic is_symmetric_tree_rnode nil)

(def ^:dynamic is_symmetric_tree_stack nil)

(defn make_symmetric_tree []
  (try (throw (ex-info "return" {:v [[1 1 2] [2 3 4] [2 5 6] [3 (- 1) (- 1)] [4 (- 1) (- 1)] [4 (- 1) (- 1)] [3 (- 1) (- 1)]]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn make_asymmetric_tree []
  (try (throw (ex-info "return" {:v [[1 1 2] [2 3 4] [2 5 6] [3 (- 1) (- 1)] [4 (- 1) (- 1)] [3 (- 1) (- 1)] [4 (- 1) (- 1)]]})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_symmetric_tree [is_symmetric_tree_tree]
  (binding [is_symmetric_tree_left nil is_symmetric_tree_lnode nil is_symmetric_tree_right nil is_symmetric_tree_rnode nil is_symmetric_tree_stack nil] (try (do (set! is_symmetric_tree_stack [(nth (nth is_symmetric_tree_tree 0) 1) (nth (nth is_symmetric_tree_tree 0) 2)]) (loop [while_flag_1 true] (when (and while_flag_1 (>= (count is_symmetric_tree_stack) 2)) (do (set! is_symmetric_tree_left (nth is_symmetric_tree_stack (- (count is_symmetric_tree_stack) 2))) (set! is_symmetric_tree_right (nth is_symmetric_tree_stack (- (count is_symmetric_tree_stack) 1))) (set! is_symmetric_tree_stack (subvec is_symmetric_tree_stack 0 (- (count is_symmetric_tree_stack) 2))) (cond (and (= is_symmetric_tree_left (- 1)) (= is_symmetric_tree_right (- 1))) (recur true) :else (do (when (or (= is_symmetric_tree_left (- 1)) (= is_symmetric_tree_right (- 1))) (throw (ex-info "return" {:v false}))) (set! is_symmetric_tree_lnode (nth is_symmetric_tree_tree is_symmetric_tree_left)) (set! is_symmetric_tree_rnode (nth is_symmetric_tree_tree is_symmetric_tree_right)) (when (not= (nth is_symmetric_tree_lnode 0) (nth is_symmetric_tree_rnode 0)) (throw (ex-info "return" {:v false}))) (set! is_symmetric_tree_stack (conj is_symmetric_tree_stack (nth is_symmetric_tree_lnode 1))) (set! is_symmetric_tree_stack (conj is_symmetric_tree_stack (nth is_symmetric_tree_rnode 2))) (set! is_symmetric_tree_stack (conj is_symmetric_tree_stack (nth is_symmetric_tree_lnode 2))) (set! is_symmetric_tree_stack (conj is_symmetric_tree_stack (nth is_symmetric_tree_rnode 1))) (recur while_flag_1)))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_symmetric_tree (make_symmetric_tree))

(def ^:dynamic main_asymmetric_tree (make_asymmetric_tree))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (is_symmetric_tree main_symmetric_tree)))
      (println (str (is_symmetric_tree main_asymmetric_tree)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
