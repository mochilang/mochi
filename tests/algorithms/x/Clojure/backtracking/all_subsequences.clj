(ns main (:refer-clojure :exclude [create_state_space_tree generate_all_subsequences]))

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

(declare create_state_space_tree generate_all_subsequences)

(def ^:dynamic create_state_space_tree_with_elem nil)

(defn create_state_space_tree [create_state_space_tree_sequence create_state_space_tree_current create_state_space_tree_index]
  (binding [create_state_space_tree_with_elem nil] (try (do (when (= create_state_space_tree_index (count create_state_space_tree_sequence)) (do (println create_state_space_tree_current) (throw (ex-info "return" {:v nil})))) (create_state_space_tree create_state_space_tree_sequence create_state_space_tree_current (+ create_state_space_tree_index 1)) (set! create_state_space_tree_with_elem (conj create_state_space_tree_current (nth create_state_space_tree_sequence create_state_space_tree_index))) (create_state_space_tree create_state_space_tree_sequence create_state_space_tree_with_elem (+ create_state_space_tree_index 1))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn generate_all_subsequences [generate_all_subsequences_sequence]
  (create_state_space_tree generate_all_subsequences_sequence [] 0))

(def ^:dynamic main_seq [1 2 3])

(def ^:dynamic main_seq2 ["A" "B" "C"])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (generate_all_subsequences main_seq)
      (generate_all_subsequences main_seq2)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
