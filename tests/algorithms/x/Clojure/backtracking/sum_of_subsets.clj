(ns main (:refer-clojure :exclude [sum_list create_state_space_tree generate_sum_of_subsets_solutions main]))

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

(declare sum_list create_state_space_tree generate_sum_of_subsets_solutions main)

(def ^:dynamic create_state_space_tree_index nil)

(def ^:dynamic create_state_space_tree_j nil)

(def ^:dynamic create_state_space_tree_result nil)

(def ^:dynamic create_state_space_tree_subres nil)

(def ^:dynamic create_state_space_tree_value nil)

(def ^:dynamic generate_sum_of_subsets_solutions_total nil)

(def ^:dynamic sum_list_s nil)

(defn sum_list [sum_list_nums]
  (binding [sum_list_s nil] (try (do (set! sum_list_s 0) (doseq [n sum_list_nums] (set! sum_list_s (+ sum_list_s n))) (throw (ex-info "return" {:v sum_list_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn create_state_space_tree [create_state_space_tree_nums create_state_space_tree_max_sum create_state_space_tree_num_index create_state_space_tree_path create_state_space_tree_curr_sum create_state_space_tree_remaining_sum]
  (binding [create_state_space_tree_index nil create_state_space_tree_j nil create_state_space_tree_result nil create_state_space_tree_subres nil create_state_space_tree_value nil] (try (do (set! create_state_space_tree_result []) (when (or (> create_state_space_tree_curr_sum create_state_space_tree_max_sum) (< (+ create_state_space_tree_curr_sum create_state_space_tree_remaining_sum) create_state_space_tree_max_sum)) (throw (ex-info "return" {:v create_state_space_tree_result}))) (when (= create_state_space_tree_curr_sum create_state_space_tree_max_sum) (do (set! create_state_space_tree_result (conj create_state_space_tree_result create_state_space_tree_path)) (throw (ex-info "return" {:v create_state_space_tree_result})))) (set! create_state_space_tree_index create_state_space_tree_num_index) (while (< create_state_space_tree_index (count create_state_space_tree_nums)) (do (set! create_state_space_tree_value (nth create_state_space_tree_nums create_state_space_tree_index)) (set! create_state_space_tree_subres (create_state_space_tree create_state_space_tree_nums create_state_space_tree_max_sum (+ create_state_space_tree_index 1) (conj create_state_space_tree_path create_state_space_tree_value) (+ create_state_space_tree_curr_sum create_state_space_tree_value) (- create_state_space_tree_remaining_sum create_state_space_tree_value))) (set! create_state_space_tree_j 0) (while (< create_state_space_tree_j (count create_state_space_tree_subres)) (do (set! create_state_space_tree_result (conj create_state_space_tree_result (nth create_state_space_tree_subres create_state_space_tree_j))) (set! create_state_space_tree_j (+ create_state_space_tree_j 1)))) (set! create_state_space_tree_index (+ create_state_space_tree_index 1)))) (throw (ex-info "return" {:v create_state_space_tree_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_sum_of_subsets_solutions [generate_sum_of_subsets_solutions_nums generate_sum_of_subsets_solutions_max_sum]
  (binding [generate_sum_of_subsets_solutions_total nil] (try (do (set! generate_sum_of_subsets_solutions_total (sum_list generate_sum_of_subsets_solutions_nums)) (throw (ex-info "return" {:v (create_state_space_tree generate_sum_of_subsets_solutions_nums generate_sum_of_subsets_solutions_max_sum 0 [] 0 generate_sum_of_subsets_solutions_total)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) (generate_sum_of_subsets_solutions [3 34 4 12 5 2] 9))))

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
