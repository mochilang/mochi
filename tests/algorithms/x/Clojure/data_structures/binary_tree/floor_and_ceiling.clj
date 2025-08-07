(ns main (:refer-clojure :exclude [inorder floor_ceiling]))

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

(declare inorder floor_ceiling)

(def ^:dynamic floor_ceiling_ceiling_val nil)

(def ^:dynamic floor_ceiling_current nil)

(def ^:dynamic floor_ceiling_floor_val nil)

(def ^:dynamic floor_ceiling_node nil)

(def ^:dynamic inorder_node nil)

(def ^:dynamic inorder_result nil)

(defn inorder [inorder_nodes inorder_idx]
  (binding [inorder_node nil inorder_result nil] (try (do (when (= inorder_idx (- 1)) (throw (ex-info "return" {:v []}))) (set! inorder_node (nth inorder_nodes inorder_idx)) (set! inorder_result (inorder inorder_nodes (:left inorder_node))) (set! inorder_result (conj inorder_result (:key inorder_node))) (set! inorder_result (concat inorder_result (inorder inorder_nodes (:right inorder_node)))) (throw (ex-info "return" {:v inorder_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn floor_ceiling [floor_ceiling_nodes floor_ceiling_idx floor_ceiling_key]
  (binding [floor_ceiling_ceiling_val nil floor_ceiling_current nil floor_ceiling_floor_val nil floor_ceiling_node nil] (try (do (set! floor_ceiling_floor_val nil) (set! floor_ceiling_ceiling_val nil) (set! floor_ceiling_current floor_ceiling_idx) (loop [while_flag_1 true] (when (and while_flag_1 (not= floor_ceiling_current (- 1))) (do (set! floor_ceiling_node (nth floor_ceiling_nodes floor_ceiling_current)) (cond (= (:key floor_ceiling_node) floor_ceiling_key) (do (set! floor_ceiling_floor_val (:key floor_ceiling_node)) (set! floor_ceiling_ceiling_val (:key floor_ceiling_node)) (recur false)) :else (do (if (< floor_ceiling_key (:key floor_ceiling_node)) (do (set! floor_ceiling_ceiling_val (:key floor_ceiling_node)) (set! floor_ceiling_current (:left floor_ceiling_node))) (do (set! floor_ceiling_floor_val (:key floor_ceiling_node)) (set! floor_ceiling_current (:right floor_ceiling_node)))) (recur while_flag_1)))))) (throw (ex-info "return" {:v [floor_ceiling_floor_val floor_ceiling_ceiling_val]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_tree [{:key 10 :left 1 :right 2} {:key 5 :left 3 :right 4} {:key 20 :left 5 :right 6} {:key 3 :left (- 1) :right (- 1)} {:key 7 :left (- 1) :right (- 1)} {:key 15 :left (- 1) :right (- 1)} {:key 25 :left (- 1) :right (- 1)}])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (inorder main_tree 0)))
      (println (str (floor_ceiling main_tree 0 8)))
      (println (str (floor_ceiling main_tree 0 14)))
      (println (str (floor_ceiling main_tree 0 (- 1))))
      (println (str (floor_ceiling main_tree 0 30)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
