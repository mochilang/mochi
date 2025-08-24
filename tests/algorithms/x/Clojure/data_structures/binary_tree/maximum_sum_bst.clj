(ns main (:refer-clojure :exclude [min_int max_int solver max_sum_bst main]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare min_int max_int solver max_sum_bst main)

(declare _read_file)

(def ^:dynamic main_t1_nodes nil)

(def ^:dynamic main_t2_nodes nil)

(def ^:dynamic main_t3_nodes nil)

(def ^:dynamic max_sum_bst_info nil)

(def ^:dynamic solver_current_best nil)

(def ^:dynamic solver_left_info nil)

(def ^:dynamic solver_node nil)

(def ^:dynamic solver_right_info nil)

(def ^:dynamic solver_sum_val nil)

(defn min_int [min_int_a min_int_b]
  (try (if (< min_int_a min_int_b) min_int_a min_int_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn max_int [max_int_a max_int_b]
  (try (if (> max_int_a max_int_b) max_int_a max_int_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn solver [solver_nodes solver_idx]
  (binding [solver_current_best nil solver_left_info nil solver_node nil solver_right_info nil solver_sum_val nil] (try (do (when (= solver_idx -1) (throw (ex-info "return" {:v {:best 0 :is_bst true :max_val (- 2147483648) :min_val 2147483647 :total 0}}))) (set! solver_node (nth solver_nodes solver_idx)) (set! solver_left_info (solver solver_nodes (:left solver_node))) (set! solver_right_info (solver solver_nodes (:right solver_node))) (set! solver_current_best (max_int (:best solver_left_info) (:best solver_right_info))) (when (and (and (and (:is_bst solver_left_info) (:is_bst solver_right_info)) (< (:max_val solver_left_info) (:val solver_node))) (< (:val solver_node) (:min_val solver_right_info))) (do (set! solver_sum_val (+ (+ (:total solver_left_info) (:total solver_right_info)) (:val solver_node))) (set! solver_current_best (max_int solver_current_best solver_sum_val)) (throw (ex-info "return" {:v {:best solver_current_best :is_bst true :max_val (max_int (:max_val solver_right_info) (:val solver_node)) :min_val (min_int (:min_val solver_left_info) (:val solver_node)) :total solver_sum_val}})))) (throw (ex-info "return" {:v {:best solver_current_best :is_bst false :max_val 0 :min_val 0 :total 0}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_sum_bst [max_sum_bst_nodes max_sum_bst_root]
  (binding [max_sum_bst_info nil] (try (do (set! max_sum_bst_info (solver max_sum_bst_nodes max_sum_bst_root)) (throw (ex-info "return" {:v (:best max_sum_bst_info)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_t1_nodes nil main_t2_nodes nil main_t3_nodes nil] (do (set! main_t1_nodes [{:left 1 :right -1 :val 4} {:left 2 :right 3 :val 3} {:left -1 :right -1 :val 1} {:left -1 :right -1 :val 2}]) (println (max_sum_bst main_t1_nodes 0)) (set! main_t2_nodes [{:left 1 :right 2 :val (- 4)} {:left -1 :right -1 :val (- 2)} {:left -1 :right -1 :val (- 5)}]) (println (max_sum_bst main_t2_nodes 0)) (set! main_t3_nodes [{:left 1 :right 2 :val 1} {:left 3 :right 4 :val 4} {:left 5 :right 6 :val 3} {:left -1 :right -1 :val 2} {:left -1 :right -1 :val 4} {:left -1 :right -1 :val 2} {:left 7 :right 8 :val 5} {:left -1 :right -1 :val 4} {:left -1 :right -1 :val 6}]) (println (max_sum_bst main_t3_nodes 0)))))

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
