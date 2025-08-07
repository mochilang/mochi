(ns main (:refer-clojure :exclude [inorder is_sorted]))

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

(declare inorder is_sorted)

(def ^:dynamic inorder_left_idx nil)

(def ^:dynamic inorder_res nil)

(def ^:dynamic inorder_right_idx nil)

(def ^:dynamic is_sorted_left_idx nil)

(def ^:dynamic is_sorted_right_idx nil)

(def ^:dynamic main_NONE (- 0 1))

(defn inorder [inorder_tree inorder_index]
  (binding [inorder_left_idx nil inorder_res nil inorder_right_idx nil] (try (do (set! inorder_res []) (when (= inorder_index main_NONE) (throw (ex-info "return" {:v inorder_res}))) (set! inorder_left_idx (get (:left inorder_tree) inorder_index)) (when (not= inorder_left_idx main_NONE) (set! inorder_res (concat inorder_res (inorder inorder_tree inorder_left_idx)))) (set! inorder_res (conj inorder_res (get (:data inorder_tree) inorder_index))) (set! inorder_right_idx (get (:right inorder_tree) inorder_index)) (when (not= inorder_right_idx main_NONE) (set! inorder_res (concat inorder_res (inorder inorder_tree inorder_right_idx)))) (throw (ex-info "return" {:v inorder_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_sorted [is_sorted_tree is_sorted_index]
  (binding [is_sorted_left_idx nil is_sorted_right_idx nil] (try (do (when (= is_sorted_index main_NONE) (throw (ex-info "return" {:v true}))) (set! is_sorted_left_idx (get (:left is_sorted_tree) is_sorted_index)) (when (not= is_sorted_left_idx main_NONE) (do (when (< (get (:data is_sorted_tree) is_sorted_index) (get (:data is_sorted_tree) is_sorted_left_idx)) (throw (ex-info "return" {:v false}))) (when (not (is_sorted is_sorted_tree is_sorted_left_idx)) (throw (ex-info "return" {:v false}))))) (set! is_sorted_right_idx (get (:right is_sorted_tree) is_sorted_index)) (when (not= is_sorted_right_idx main_NONE) (do (when (> (get (:data is_sorted_tree) is_sorted_index) (get (:data is_sorted_tree) is_sorted_right_idx)) (throw (ex-info "return" {:v false}))) (when (not (is_sorted is_sorted_tree is_sorted_right_idx)) (throw (ex-info "return" {:v false}))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_tree1 {:data [2.1 2.0 2.2] :left [1 main_NONE main_NONE] :right [2 main_NONE main_NONE]})

(def ^:dynamic main_tree2 {:data [2.1 2.0 2.0] :left [1 main_NONE main_NONE] :right [2 main_NONE main_NONE]})

(def ^:dynamic main_tree3 {:data [2.1 2.0 2.1] :left [1 main_NONE main_NONE] :right [2 main_NONE main_NONE]})

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (str (str "Tree " (str (inorder main_tree1 0))) " is sorted: ") (str (is_sorted main_tree1 0))))
      (println (str (str (str "Tree " (str (inorder main_tree2 0))) " is sorted: ") (str (is_sorted main_tree2 0))))
      (println (str (str (str "Tree " (str (inorder main_tree3 0))) " is sorted: ") (str (is_sorted main_tree3 0))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
