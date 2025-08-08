(ns main (:refer-clojure :exclude [get_min_index remove_at pass_and_relaxation bidirectional_dij]))

(require 'clojure.set)

(defrecord GraphBwd [B C D F E G])

(defrecord GraphFwd [B C D E F G])

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def ^:dynamic bidirectional_dij_cst_bwd nil)

(def ^:dynamic bidirectional_dij_cst_fwd nil)

(def ^:dynamic bidirectional_dij_idx_b nil)

(def ^:dynamic bidirectional_dij_idx_f nil)

(def ^:dynamic bidirectional_dij_item_b nil)

(def ^:dynamic bidirectional_dij_item_f nil)

(def ^:dynamic bidirectional_dij_parent_backward nil)

(def ^:dynamic bidirectional_dij_parent_forward nil)

(def ^:dynamic bidirectional_dij_queue_backward nil)

(def ^:dynamic bidirectional_dij_queue_forward nil)

(def ^:dynamic bidirectional_dij_res_b nil)

(def ^:dynamic bidirectional_dij_res_f nil)

(def ^:dynamic bidirectional_dij_shortest_distance nil)

(def ^:dynamic bidirectional_dij_shortest_path_distance nil)

(def ^:dynamic bidirectional_dij_v_bwd nil)

(def ^:dynamic bidirectional_dij_v_fwd nil)

(def ^:dynamic bidirectional_dij_visited_backward nil)

(def ^:dynamic bidirectional_dij_visited_forward nil)

(def ^:dynamic get_min_index_i nil)

(def ^:dynamic get_min_index_idx nil)

(def ^:dynamic pass_and_relaxation_alt nil)

(def ^:dynamic pass_and_relaxation_cst_fwd nil)

(def ^:dynamic pass_and_relaxation_d nil)

(def ^:dynamic pass_and_relaxation_new_cost nil)

(def ^:dynamic pass_and_relaxation_nxt nil)

(def ^:dynamic pass_and_relaxation_old_cost nil)

(def ^:dynamic pass_and_relaxation_parent nil)

(def ^:dynamic pass_and_relaxation_q nil)

(def ^:dynamic pass_and_relaxation_sd nil)

(def ^:dynamic remove_at_i nil)

(def ^:dynamic remove_at_res nil)

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare get_min_index remove_at pass_and_relaxation bidirectional_dij)

(defn get_min_index [get_min_index_q]
  (binding [get_min_index_i nil get_min_index_idx nil] (try (do (set! get_min_index_idx 0) (set! get_min_index_i 1) (while (< get_min_index_i (count get_min_index_q)) (do (when (< (:cost (nth get_min_index_q get_min_index_i)) (:cost (nth get_min_index_q get_min_index_idx))) (set! get_min_index_idx get_min_index_i)) (set! get_min_index_i (+ get_min_index_i 1)))) (throw (ex-info "return" {:v get_min_index_idx}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_at [remove_at_q remove_at_idx]
  (binding [remove_at_i nil remove_at_res nil] (try (do (set! remove_at_res []) (set! remove_at_i 0) (while (< remove_at_i (count remove_at_q)) (do (when (not= remove_at_i remove_at_idx) (set! remove_at_res (conj remove_at_res (nth remove_at_q remove_at_i)))) (set! remove_at_i (+ remove_at_i 1)))) (throw (ex-info "return" {:v remove_at_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pass_and_relaxation [pass_and_relaxation_graph pass_and_relaxation_v pass_and_relaxation_visited_forward pass_and_relaxation_visited_backward pass_and_relaxation_cst_fwd_p pass_and_relaxation_cst_bwd pass_and_relaxation_queue pass_and_relaxation_parent_p pass_and_relaxation_shortest_distance]
  (binding [pass_and_relaxation_alt nil pass_and_relaxation_cst_fwd nil pass_and_relaxation_d nil pass_and_relaxation_new_cost nil pass_and_relaxation_nxt nil pass_and_relaxation_old_cost nil pass_and_relaxation_parent nil pass_and_relaxation_q nil pass_and_relaxation_sd nil] (try (do (set! pass_and_relaxation_cst_fwd pass_and_relaxation_cst_fwd_p) (set! pass_and_relaxation_parent pass_and_relaxation_parent_p) (set! pass_and_relaxation_q pass_and_relaxation_queue) (set! pass_and_relaxation_sd pass_and_relaxation_shortest_distance) (loop [e_seq (get pass_and_relaxation_graph pass_and_relaxation_v)] (when (seq e_seq) (let [e (first e_seq)] (do (set! pass_and_relaxation_nxt (:to e)) (set! pass_and_relaxation_d (:cost e)) (cond (in pass_and_relaxation_nxt pass_and_relaxation_visited_forward) (recur (rest e_seq)) :else (do (set! pass_and_relaxation_old_cost (if (in pass_and_relaxation_nxt pass_and_relaxation_cst_fwd) (nth pass_and_relaxation_cst_fwd pass_and_relaxation_nxt) 2147483647)) (set! pass_and_relaxation_new_cost (+ (get pass_and_relaxation_cst_fwd pass_and_relaxation_v) pass_and_relaxation_d)) (when (< pass_and_relaxation_new_cost pass_and_relaxation_old_cost) (do (set! pass_and_relaxation_q (conj pass_and_relaxation_q {:cost pass_and_relaxation_new_cost :node pass_and_relaxation_nxt})) (set! pass_and_relaxation_cst_fwd (assoc pass_and_relaxation_cst_fwd pass_and_relaxation_nxt pass_and_relaxation_new_cost)) (set! pass_and_relaxation_parent (assoc pass_and_relaxation_parent pass_and_relaxation_nxt pass_and_relaxation_v)))) (when (in pass_and_relaxation_nxt pass_and_relaxation_visited_backward) (do (set! pass_and_relaxation_alt (+ (+ (get pass_and_relaxation_cst_fwd pass_and_relaxation_v) pass_and_relaxation_d) (nth pass_and_relaxation_cst_bwd pass_and_relaxation_nxt))) (when (< pass_and_relaxation_alt pass_and_relaxation_sd) (set! pass_and_relaxation_sd pass_and_relaxation_alt)))) (recur (rest e_seq)))))))) (throw (ex-info "return" {:v {:dist pass_and_relaxation_sd :queue pass_and_relaxation_q}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bidirectional_dij [bidirectional_dij_source bidirectional_dij_destination bidirectional_dij_graph_forward bidirectional_dij_graph_backward]
  (binding [bidirectional_dij_cst_bwd nil bidirectional_dij_cst_fwd nil bidirectional_dij_idx_b nil bidirectional_dij_idx_f nil bidirectional_dij_item_b nil bidirectional_dij_item_f nil bidirectional_dij_parent_backward nil bidirectional_dij_parent_forward nil bidirectional_dij_queue_backward nil bidirectional_dij_queue_forward nil bidirectional_dij_res_b nil bidirectional_dij_res_f nil bidirectional_dij_shortest_distance nil bidirectional_dij_shortest_path_distance nil bidirectional_dij_v_bwd nil bidirectional_dij_v_fwd nil bidirectional_dij_visited_backward nil bidirectional_dij_visited_forward nil] (try (do (set! bidirectional_dij_shortest_path_distance (- 1)) (set! bidirectional_dij_visited_forward {}) (set! bidirectional_dij_visited_backward {}) (set! bidirectional_dij_cst_fwd {}) (set! bidirectional_dij_cst_fwd (assoc bidirectional_dij_cst_fwd bidirectional_dij_source 0)) (set! bidirectional_dij_cst_bwd {}) (set! bidirectional_dij_cst_bwd (assoc bidirectional_dij_cst_bwd bidirectional_dij_destination 0)) (set! bidirectional_dij_parent_forward {}) (set! bidirectional_dij_parent_forward (assoc bidirectional_dij_parent_forward bidirectional_dij_source "")) (set! bidirectional_dij_parent_backward {}) (set! bidirectional_dij_parent_backward (assoc bidirectional_dij_parent_backward bidirectional_dij_destination "")) (set! bidirectional_dij_queue_forward []) (set! bidirectional_dij_queue_forward (conj bidirectional_dij_queue_forward {:cost 0 :node bidirectional_dij_source})) (set! bidirectional_dij_queue_backward []) (set! bidirectional_dij_queue_backward (conj bidirectional_dij_queue_backward {:cost 0 :node bidirectional_dij_destination})) (set! bidirectional_dij_shortest_distance 2147483647) (when (= bidirectional_dij_source bidirectional_dij_destination) (throw (ex-info "return" {:v 0}))) (loop [while_flag_1 true] (when (and while_flag_1 (and (> (count bidirectional_dij_queue_forward) 0) (> (count bidirectional_dij_queue_backward) 0))) (do (set! bidirectional_dij_idx_f (get_min_index bidirectional_dij_queue_forward)) (set! bidirectional_dij_item_f (nth bidirectional_dij_queue_forward bidirectional_dij_idx_f)) (set! bidirectional_dij_queue_forward (remove_at bidirectional_dij_queue_forward bidirectional_dij_idx_f)) (set! bidirectional_dij_v_fwd (:node bidirectional_dij_item_f)) (set! bidirectional_dij_visited_forward (assoc bidirectional_dij_visited_forward bidirectional_dij_v_fwd true)) (set! bidirectional_dij_idx_b (get_min_index bidirectional_dij_queue_backward)) (set! bidirectional_dij_item_b (nth bidirectional_dij_queue_backward bidirectional_dij_idx_b)) (set! bidirectional_dij_queue_backward (remove_at bidirectional_dij_queue_backward bidirectional_dij_idx_b)) (set! bidirectional_dij_v_bwd (:node bidirectional_dij_item_b)) (set! bidirectional_dij_visited_backward (assoc bidirectional_dij_visited_backward bidirectional_dij_v_bwd true)) (set! bidirectional_dij_res_f (pass_and_relaxation bidirectional_dij_graph_forward bidirectional_dij_v_fwd bidirectional_dij_visited_forward bidirectional_dij_visited_backward bidirectional_dij_cst_fwd bidirectional_dij_cst_bwd bidirectional_dij_queue_forward bidirectional_dij_parent_forward bidirectional_dij_shortest_distance)) (set! bidirectional_dij_queue_forward (:queue bidirectional_dij_res_f)) (set! bidirectional_dij_shortest_distance (:dist bidirectional_dij_res_f)) (set! bidirectional_dij_res_b (pass_and_relaxation bidirectional_dij_graph_backward bidirectional_dij_v_bwd bidirectional_dij_visited_backward bidirectional_dij_visited_forward bidirectional_dij_cst_bwd bidirectional_dij_cst_fwd bidirectional_dij_queue_backward bidirectional_dij_parent_backward bidirectional_dij_shortest_distance)) (set! bidirectional_dij_queue_backward (:queue bidirectional_dij_res_b)) (set! bidirectional_dij_shortest_distance (:dist bidirectional_dij_res_b)) (cond (>= (+ (nth bidirectional_dij_cst_fwd bidirectional_dij_v_fwd) (nth bidirectional_dij_cst_bwd bidirectional_dij_v_bwd)) bidirectional_dij_shortest_distance) (recur false) :else (recur while_flag_1))))) (when (not= bidirectional_dij_shortest_distance 2147483647) (set! bidirectional_dij_shortest_path_distance bidirectional_dij_shortest_distance)) (throw (ex-info "return" {:v bidirectional_dij_shortest_path_distance}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_graph_fwd {"B" [{:cost 1 :to "C"}] "C" [{:cost 1 :to "D"}] "D" [{:cost 1 :to "F"}] "E" [{:cost 1 :to "B"} {:cost 2 :to "G"}] "F" [] "G" [{:cost 1 :to "F"}]})

(def ^:dynamic main_graph_bwd {"B" [{:cost 1 :to "E"}] "C" [{:cost 1 :to "B"}] "D" [{:cost 1 :to "C"}] "E" [] "F" [{:cost 1 :to "D"} {:cost 1 :to "G"}] "G" [{:cost 2 :to "E"}]})

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (bidirectional_dij "E" "F" main_graph_fwd main_graph_bwd)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
