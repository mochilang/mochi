(ns main (:refer-clojure :exclude [repeat_bool set_bool create_state_space_tree generate_all_permutations]))

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

(declare repeat_bool set_bool create_state_space_tree generate_all_permutations)

(def ^:dynamic create_state_space_tree_i nil)

(def ^:dynamic create_state_space_tree_next_current nil)

(def ^:dynamic create_state_space_tree_next_used nil)

(def ^:dynamic generate_all_permutations_used nil)

(def ^:dynamic repeat_bool_i nil)

(def ^:dynamic repeat_bool_res nil)

(def ^:dynamic set_bool_i nil)

(def ^:dynamic set_bool_res nil)

(defn repeat_bool [repeat_bool_times]
  (binding [repeat_bool_i nil repeat_bool_res nil] (try (do (set! repeat_bool_res []) (set! repeat_bool_i 0) (while (< repeat_bool_i repeat_bool_times) (do (set! repeat_bool_res (conj repeat_bool_res false)) (set! repeat_bool_i (+ repeat_bool_i 1)))) (throw (ex-info "return" {:v repeat_bool_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn set_bool [set_bool_xs set_bool_idx set_bool_value]
  (binding [set_bool_i nil set_bool_res nil] (try (do (set! set_bool_res []) (set! set_bool_i 0) (while (< set_bool_i (count set_bool_xs)) (do (if (= set_bool_i set_bool_idx) (set! set_bool_res (conj set_bool_res set_bool_value)) (set! set_bool_res (conj set_bool_res (nth set_bool_xs set_bool_i)))) (set! set_bool_i (+ set_bool_i 1)))) (throw (ex-info "return" {:v set_bool_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn create_state_space_tree [create_state_space_tree_sequence create_state_space_tree_current create_state_space_tree_index create_state_space_tree_used]
  (binding [create_state_space_tree_i nil create_state_space_tree_next_current nil create_state_space_tree_next_used nil] (try (do (when (= create_state_space_tree_index (count create_state_space_tree_sequence)) (do (println (str create_state_space_tree_current)) (throw (ex-info "return" {:v nil})))) (set! create_state_space_tree_i 0) (while (< create_state_space_tree_i (count create_state_space_tree_sequence)) (do (when (not (nth create_state_space_tree_used create_state_space_tree_i)) (do (set! create_state_space_tree_next_current (conj create_state_space_tree_current (nth create_state_space_tree_sequence create_state_space_tree_i))) (set! create_state_space_tree_next_used (set_bool create_state_space_tree_used create_state_space_tree_i true)) (create_state_space_tree create_state_space_tree_sequence create_state_space_tree_next_current (+ create_state_space_tree_index 1) create_state_space_tree_next_used))) (set! create_state_space_tree_i (+ create_state_space_tree_i 1))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_all_permutations [generate_all_permutations_sequence]
  (binding [generate_all_permutations_used nil] (do (set! generate_all_permutations_used (repeat_bool (count generate_all_permutations_sequence))) (create_state_space_tree generate_all_permutations_sequence [] 0 generate_all_permutations_used))))

(def ^:dynamic main_sequence [3 1 2 4])

(def ^:dynamic main_sequence_2 ["A" "B" "C"])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (generate_all_permutations main_sequence)
      (generate_all_permutations main_sequence_2)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
