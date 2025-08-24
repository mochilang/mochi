(ns main (:refer-clojure :exclude [max_list disjoint_set_new get_parent merge]))

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

(declare max_list disjoint_set_new get_parent merge)

(declare _read_file)

(def ^:dynamic disjoint_set_new_i nil)

(def ^:dynamic disjoint_set_new_max_set nil)

(def ^:dynamic disjoint_set_new_num_sets nil)

(def ^:dynamic disjoint_set_new_parents nil)

(def ^:dynamic disjoint_set_new_ranks nil)

(def ^:dynamic get_parent_ds nil)

(def ^:dynamic get_parent_parents nil)

(def ^:dynamic max_list_i nil)

(def ^:dynamic max_list_m nil)

(def ^:dynamic merge_counts nil)

(def ^:dynamic merge_ds nil)

(def ^:dynamic merge_dst_parent nil)

(def ^:dynamic merge_joined nil)

(def ^:dynamic merge_parents nil)

(def ^:dynamic merge_ranks nil)

(def ^:dynamic merge_src_parent nil)

(defn max_list [max_list_xs]
  (binding [max_list_i nil max_list_m nil] (try (do (set! max_list_m (nth max_list_xs 0)) (set! max_list_i 1) (while (< max_list_i (count max_list_xs)) (do (when (> (nth max_list_xs max_list_i) max_list_m) (set! max_list_m (nth max_list_xs max_list_i))) (set! max_list_i (+ max_list_i 1)))) (throw (ex-info "return" {:v max_list_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn disjoint_set_new [disjoint_set_new_set_counts]
  (binding [disjoint_set_new_i nil disjoint_set_new_max_set nil disjoint_set_new_num_sets nil disjoint_set_new_parents nil disjoint_set_new_ranks nil] (try (do (set! disjoint_set_new_max_set (max_list disjoint_set_new_set_counts)) (set! disjoint_set_new_num_sets (count disjoint_set_new_set_counts)) (set! disjoint_set_new_ranks []) (set! disjoint_set_new_parents []) (set! disjoint_set_new_i 0) (while (< disjoint_set_new_i disjoint_set_new_num_sets) (do (set! disjoint_set_new_ranks (conj disjoint_set_new_ranks 1)) (set! disjoint_set_new_parents (conj disjoint_set_new_parents disjoint_set_new_i)) (set! disjoint_set_new_i (+ disjoint_set_new_i 1)))) (throw (ex-info "return" {:v {:max_set disjoint_set_new_max_set :parents disjoint_set_new_parents :ranks disjoint_set_new_ranks :set_counts disjoint_set_new_set_counts}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_parent [get_parent_ds_p get_parent_idx]
  (binding [get_parent_ds get_parent_ds_p get_parent_parents nil] (try (do (when (= (get (:parents get_parent_ds) get_parent_idx) get_parent_idx) (throw (ex-info "return" {:v get_parent_idx}))) (set! get_parent_parents (:parents get_parent_ds)) (set! get_parent_parents (assoc get_parent_parents get_parent_idx (let [__res (get_parent get_parent_ds (get get_parent_parents get_parent_idx))] (do (set! get_parent_ds get_parent_ds) __res)))) (set! get_parent_ds (assoc get_parent_ds :parents get_parent_parents)) (throw (ex-info "return" {:v (get (:parents get_parent_ds) get_parent_idx)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var get_parent_ds) (constantly get_parent_ds))))))

(defn merge [merge_ds_p merge_src merge_dst]
  (binding [merge_ds merge_ds_p merge_counts nil merge_dst_parent nil merge_joined nil merge_parents nil merge_ranks nil merge_src_parent nil] (try (do (set! merge_src_parent (let [__res (get_parent merge_ds merge_src)] (do (set! merge_ds get_parent_ds) __res))) (set! merge_dst_parent (let [__res (get_parent merge_ds merge_dst)] (do (set! merge_ds get_parent_ds) __res))) (when (= merge_src_parent merge_dst_parent) (throw (ex-info "return" {:v false}))) (if (>= (get (:ranks merge_ds) merge_dst_parent) (get (:ranks merge_ds) merge_src_parent)) (do (set! merge_counts (:set_counts merge_ds)) (set! merge_counts (assoc merge_counts merge_dst_parent (+ (get merge_counts merge_dst_parent) (get merge_counts merge_src_parent)))) (set! merge_counts (assoc merge_counts merge_src_parent 0)) (set! merge_ds (assoc merge_ds :set_counts merge_counts)) (set! merge_parents (:parents merge_ds)) (set! merge_parents (assoc merge_parents merge_src_parent merge_dst_parent)) (set! merge_ds (assoc merge_ds :parents merge_parents)) (when (= (get (:ranks merge_ds) merge_dst_parent) (get (:ranks merge_ds) merge_src_parent)) (do (set! merge_ranks (:ranks merge_ds)) (set! merge_ranks (assoc merge_ranks merge_dst_parent (+ (get merge_ranks merge_dst_parent) 1))) (set! merge_ds (assoc merge_ds :ranks merge_ranks)))) (set! merge_joined (get (:set_counts merge_ds) merge_dst_parent)) (when (> merge_joined (:max_set merge_ds)) (set! merge_ds (assoc merge_ds :max_set merge_joined)))) (do (set! merge_counts (:set_counts merge_ds)) (set! merge_counts (assoc merge_counts merge_src_parent (+ (get merge_counts merge_src_parent) (get merge_counts merge_dst_parent)))) (set! merge_counts (assoc merge_counts merge_dst_parent 0)) (set! merge_ds (assoc merge_ds :set_counts merge_counts)) (set! merge_parents (:parents merge_ds)) (set! merge_parents (assoc merge_parents merge_dst_parent merge_src_parent)) (set! merge_ds (assoc merge_ds :parents merge_parents)) (set! merge_joined (get (:set_counts merge_ds) merge_src_parent)) (when (> merge_joined (:max_set merge_ds)) (set! merge_ds (assoc merge_ds :max_set merge_joined))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var merge_ds) (constantly merge_ds))))))

(def ^:dynamic main_ds nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_ds) (constantly (disjoint_set_new [1 1 1])))
      (println (let [__res (merge main_ds 1 2)] (do (alter-var-root (var main_ds) (constantly merge_ds)) __res)))
      (println (let [__res (merge main_ds 0 2)] (do (alter-var-root (var main_ds) (constantly merge_ds)) __res)))
      (println (let [__res (merge main_ds 0 1)] (do (alter-var-root (var main_ds) (constantly merge_ds)) __res)))
      (println (let [__res (get_parent main_ds 0)] (do (alter-var-root (var main_ds) (constantly get_parent_ds)) __res)))
      (println (let [__res (get_parent main_ds 1)] (do (alter-var-root (var main_ds) (constantly get_parent_ds)) __res)))
      (println (:max_set main_ds))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
