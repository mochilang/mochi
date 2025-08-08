(ns main)

(require 'clojure.set)

(defrecord G3 [B C D E F G])

(defrecord G2 [B C D E F])

(defrecord G [A B C D E F])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def ^:dynamic main_best2_idx nil)

(def ^:dynamic main_best3_idx nil)

(def ^:dynamic main_best_idx nil)

(def ^:dynamic main_heap nil)

(def ^:dynamic main_heap2 nil)

(def ^:dynamic main_heap3 nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_i2 nil)

(def ^:dynamic main_i3 nil)

(def ^:dynamic main_j nil)

(def ^:dynamic main_j2 nil)

(def ^:dynamic main_j3 nil)

(def ^:dynamic main_new_heap nil)

(def ^:dynamic main_new_heap2 nil)

(def ^:dynamic main_new_heap3 nil)

(def ^:dynamic main_result nil)

(def ^:dynamic main_result2 nil)

(def ^:dynamic main_result3 nil)

(def ^:dynamic main_visited nil)

(def ^:dynamic main_visited2 nil)

(def ^:dynamic main_visited3 nil)

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(def ^:dynamic main_G {"A" {"B" 2 "C" 5} "B" {"A" 2 "D" 3 "E" 1 "F" 1} "C" {"A" 5 "F" 3} "D" {"B" 3} "E" {"B" 4 "F" 3} "F" {"C" 3 "E" 3}})

(def ^:dynamic main_heap [{:cost 0 :node "E"}])

(def ^:dynamic main_visited {})

(def ^:dynamic main_result (- 1))

(def ^:dynamic main_G2 {"B" {"C" 1} "C" {"D" 1} "D" {"F" 1} "E" {"B" 1 "F" 3} "F" {}})

(def ^:dynamic main_heap2 [{:cost 0 :node "E"}])

(def ^:dynamic main_visited2 {})

(def ^:dynamic main_result2 (- 1))

(def ^:dynamic main_G3 {"B" {"C" 1} "C" {"D" 1} "D" {"F" 1} "E" {"B" 1 "G" 2} "F" {} "G" {"F" 1}})

(def ^:dynamic main_heap3 [{:cost 0 :node "E"}])

(def ^:dynamic main_visited3 {})

(def ^:dynamic main_result3 (- 1))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (loop [while_flag_1 true] (when (and while_flag_1 (> (count main_heap) 0)) (do (def ^:dynamic main_best_idx 0) (def ^:dynamic main_i 1) (while (< main_i (count main_heap)) (do (when (< (:cost (nth main_heap main_i)) (:cost (nth main_heap main_best_idx))) (def main_best_idx main_i)) (def main_i (+ main_i 1)))) (def ^:dynamic main_best (nth main_heap main_best_idx)) (def ^:dynamic main_new_heap []) (def ^:dynamic main_j 0) (while (< main_j (count main_heap)) (do (when (not= main_j main_best_idx) (def main_new_heap (conj main_new_heap (nth main_heap main_j)))) (def main_j (+ main_j 1)))) (def main_heap main_new_heap) (def ^:dynamic main_u (:node main_best)) (def ^:dynamic main_cost (:cost main_best)) (cond (in main_u main_visited) (recur true) (= main_u "C") (do (def main_result main_cost) (recur false)) :else (do (def main_visited (assoc main_visited main_u true)) (loop [v_seq (get main_G main_u)] (when (seq v_seq) (let [v (first v_seq)] (cond (in v main_visited) (recur (rest v_seq)) :else (do (def ^:dynamic main_next_cost (+ main_cost (nth (get main_G main_u) v))) (def main_heap (conj main_heap {:cost main_next_cost :node v})) (recur (rest v_seq))))))) (recur while_flag_1))))))
      (println main_result)
      (loop [while_flag_2 true] (when (and while_flag_2 (> (count main_heap2) 0)) (do (def ^:dynamic main_best2_idx 0) (def ^:dynamic main_i2 1) (while (< main_i2 (count main_heap2)) (do (when (< (:cost (nth main_heap2 main_i2)) (:cost (nth main_heap2 main_best2_idx))) (def main_best2_idx main_i2)) (def main_i2 (+ main_i2 1)))) (def ^:dynamic main_best2 (nth main_heap2 main_best2_idx)) (def ^:dynamic main_new_heap2 []) (def ^:dynamic main_j2 0) (while (< main_j2 (count main_heap2)) (do (when (not= main_j2 main_best2_idx) (def main_new_heap2 (conj main_new_heap2 (nth main_heap2 main_j2)))) (def main_j2 (+ main_j2 1)))) (def main_heap2 main_new_heap2) (def ^:dynamic main_u2 (:node main_best2)) (def ^:dynamic main_cost2 (:cost main_best2)) (cond (in main_u2 main_visited2) (recur true) (= main_u2 "F") (do (def main_result2 main_cost2) (recur false)) :else (do (def main_visited2 (assoc main_visited2 main_u2 true)) (loop [v2_seq (get main_G2 main_u2)] (when (seq v2_seq) (let [v2 (first v2_seq)] (cond (in v2 main_visited2) (recur (rest v2_seq)) :else (do (def ^:dynamic main_next_cost2 (+ main_cost2 (nth (get main_G2 main_u2) v2))) (def main_heap2 (conj main_heap2 {:cost main_next_cost2 :node v2})) (recur (rest v2_seq))))))) (recur while_flag_2))))))
      (println main_result2)
      (loop [while_flag_3 true] (when (and while_flag_3 (> (count main_heap3) 0)) (do (def ^:dynamic main_best3_idx 0) (def ^:dynamic main_i3 1) (while (< main_i3 (count main_heap3)) (do (when (< (:cost (nth main_heap3 main_i3)) (:cost (nth main_heap3 main_best3_idx))) (def main_best3_idx main_i3)) (def main_i3 (+ main_i3 1)))) (def ^:dynamic main_best3 (nth main_heap3 main_best3_idx)) (def ^:dynamic main_new_heap3 []) (def ^:dynamic main_j3 0) (while (< main_j3 (count main_heap3)) (do (when (not= main_j3 main_best3_idx) (def main_new_heap3 (conj main_new_heap3 (nth main_heap3 main_j3)))) (def main_j3 (+ main_j3 1)))) (def main_heap3 main_new_heap3) (def ^:dynamic main_u3 (:node main_best3)) (def ^:dynamic main_cost3 (:cost main_best3)) (cond (in main_u3 main_visited3) (recur true) (= main_u3 "F") (do (def main_result3 main_cost3) (recur false)) :else (do (def main_visited3 (assoc main_visited3 main_u3 true)) (loop [v3_seq (get main_G3 main_u3)] (when (seq v3_seq) (let [v3 (first v3_seq)] (cond (in v3 main_visited3) (recur (rest v3_seq)) :else (do (def ^:dynamic main_next_cost3 (+ main_cost3 (nth (get main_G3 main_u3) v3))) (def main_heap3 (conj main_heap3 {:cost main_next_cost3 :node v3})) (recur (rest v3_seq))))))) (recur while_flag_3))))))
      (println main_result3)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
