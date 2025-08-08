(ns main (:refer-clojure :exclude [make_kd_node]))

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

(declare make_kd_node)

(def ^:dynamic main_nodes nil)

(defn make_kd_node [make_kd_node_point make_kd_node_left make_kd_node_right]
  (try (throw (ex-info "return" {:v {:left make_kd_node_left :point make_kd_node_point :right make_kd_node_right}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_nodes [])

(def ^:dynamic main_root (nth main_nodes 0))

(def ^:dynamic main_left_child (nth main_nodes 1))

(def ^:dynamic main_right_child (nth main_nodes 2))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (def main_nodes (conj main_nodes (make_kd_node [2.0 3.0] 1 2)))
      (def main_nodes (conj main_nodes (make_kd_node [1.0 5.0] (- 1) (- 1))))
      (def main_nodes (conj main_nodes (make_kd_node [4.0 2.0] (- 1) (- 1))))
      (println (str (:point main_root)))
      (println (str (:left main_root)))
      (println (str (:right main_root)))
      (println (str (:point main_left_child)))
      (println (str (:point main_right_child)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
