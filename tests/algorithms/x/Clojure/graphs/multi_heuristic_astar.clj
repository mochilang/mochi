(ns main (:refer-clojure :exclude [pos_equal pos_key sqrtApprox consistent_heuristic iabs heuristic_1 heuristic_2 heuristic key_fn valid in_blocks pq_put pq_minkey pq_pop_min pq_remove reconstruct neighbours multi_a_star]))

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
  (Integer/parseInt (str s)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare pos_equal pos_key sqrtApprox consistent_heuristic iabs heuristic_1 heuristic_2 heuristic key_fn valid in_blocks pq_put pq_minkey pq_pop_min pq_remove reconstruct neighbours multi_a_star)

(def ^:dynamic consistent_heuristic_dx nil)

(def ^:dynamic consistent_heuristic_dy nil)

(def ^:dynamic first_v nil)

(def ^:dynamic heuristic_2_h nil)

(def ^:dynamic in_blocks_i nil)

(def ^:dynamic key_fn_g nil)

(def ^:dynamic multi_a_star_back_pointer nil)

(def ^:dynamic multi_a_star_chosen nil)

(def ^:dynamic multi_a_star_ckey nil)

(def ^:dynamic multi_a_star_current nil)

(def ^:dynamic multi_a_star_g_function nil)

(def ^:dynamic multi_a_star_i nil)

(def ^:dynamic multi_a_star_j nil)

(def ^:dynamic multi_a_star_k nil)

(def ^:dynamic multi_a_star_nb nil)

(def ^:dynamic multi_a_star_neighs nil)

(def ^:dynamic multi_a_star_nkey nil)

(def ^:dynamic multi_a_star_open_list nil)

(def ^:dynamic multi_a_star_p nil)

(def ^:dynamic multi_a_star_pair nil)

(def ^:dynamic multi_a_star_path nil)

(def ^:dynamic multi_a_star_pri nil)

(def ^:dynamic multi_a_star_pri2 nil)

(def ^:dynamic multi_a_star_tentative nil)

(def ^:dynamic multi_a_star_visited nil)

(def ^:dynamic neighbours_down nil)

(def ^:dynamic neighbours_left nil)

(def ^:dynamic neighbours_right nil)

(def ^:dynamic neighbours_up nil)

(def ^:dynamic pq_minkey_i nil)

(def ^:dynamic pq_minkey_item nil)

(def ^:dynamic pq_minkey_m nil)

(def ^:dynamic pq_pop_min_best nil)

(def ^:dynamic pq_pop_min_i nil)

(def ^:dynamic pq_pop_min_idx nil)

(def ^:dynamic pq_pop_min_new_pq nil)

(def ^:dynamic pq_put_i nil)

(def ^:dynamic pq_put_pq nil)

(def ^:dynamic pq_put_updated nil)

(def ^:dynamic pq_remove_i nil)

(def ^:dynamic pq_remove_new_pq nil)

(def ^:dynamic reconstruct_current nil)

(def ^:dynamic reconstruct_i nil)

(def ^:dynamic reconstruct_key nil)

(def ^:dynamic reconstruct_path nil)

(def ^:dynamic reconstruct_rev nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic main_W1 1.0)

(def ^:dynamic main_W2 1.0)

(def ^:dynamic main_n 20)

(def ^:dynamic main_n_heuristic 3)

(def ^:dynamic main_INF 1000000000.0)

(def ^:dynamic main_t 1)

(defn pos_equal [pos_equal_a pos_equal_b]
  (try (throw (ex-info "return" {:v (and (= (:x pos_equal_a) (:x pos_equal_b)) (= (:y pos_equal_a) (:y pos_equal_b)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pos_key [pos_key_p]
  (try (throw (ex-info "return" {:v (str (str (str (:x pos_key_p)) ",") (str (:y pos_key_p)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 10) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn consistent_heuristic [consistent_heuristic_p consistent_heuristic_goal]
  (binding [consistent_heuristic_dx nil consistent_heuristic_dy nil] (try (do (set! consistent_heuristic_dx (double (- (:x consistent_heuristic_p) (:x consistent_heuristic_goal)))) (set! consistent_heuristic_dy (double (- (:y consistent_heuristic_p) (:y consistent_heuristic_goal)))) (throw (ex-info "return" {:v (sqrtApprox (+ (* consistent_heuristic_dx consistent_heuristic_dx) (* consistent_heuristic_dy consistent_heuristic_dy)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn iabs [iabs_x]
  (try (if (< iabs_x 0) (- iabs_x) iabs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn heuristic_1 [heuristic_1_p heuristic_1_goal]
  (try (throw (ex-info "return" {:v (double (+ (iabs (- (:x heuristic_1_p) (:x heuristic_1_goal))) (iabs (- (:y heuristic_1_p) (:y heuristic_1_goal)))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn heuristic_2 [heuristic_2_p heuristic_2_goal]
  (binding [heuristic_2_h nil] (try (do (set! heuristic_2_h (consistent_heuristic heuristic_2_p heuristic_2_goal)) (throw (ex-info "return" {:v (quot heuristic_2_h (double main_t))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn heuristic [heuristic_i heuristic_p heuristic_goal]
  (try (do (when (= heuristic_i 0) (throw (ex-info "return" {:v (consistent_heuristic heuristic_p heuristic_goal)}))) (if (= heuristic_i 1) (heuristic_1 heuristic_p heuristic_goal) (heuristic_2 heuristic_p heuristic_goal))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn key_fn [key_fn_start key_fn_i key_fn_goal key_fn_g_func]
  (binding [key_fn_g nil] (try (do (set! key_fn_g (get key_fn_g_func (pos_key key_fn_start))) (throw (ex-info "return" {:v (+ key_fn_g (* main_W1 (heuristic key_fn_i key_fn_start key_fn_goal)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn valid [valid_p]
  (try (do (when (or (< (:x valid_p) 0) (> (:x valid_p) (- main_n 1))) (throw (ex-info "return" {:v false}))) (if (or (< (:y valid_p) 0) (> (:y valid_p) (- main_n 1))) false true)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_blocks [{:x 0 :y 1} {:x 1 :y 1} {:x 2 :y 1} {:x 3 :y 1} {:x 4 :y 1} {:x 5 :y 1} {:x 6 :y 1} {:x 7 :y 1} {:x 8 :y 1} {:x 9 :y 1} {:x 10 :y 1} {:x 11 :y 1} {:x 12 :y 1} {:x 13 :y 1} {:x 14 :y 1} {:x 15 :y 1} {:x 16 :y 1} {:x 17 :y 1} {:x 18 :y 1} {:x 19 :y 1}])

(defn in_blocks [in_blocks_p]
  (binding [in_blocks_i nil] (try (do (set! in_blocks_i 0) (while (< in_blocks_i (count main_blocks)) (do (when (pos_equal (nth main_blocks in_blocks_i) in_blocks_p) (throw (ex-info "return" {:v true}))) (set! in_blocks_i (+ in_blocks_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pq_put [pq_put_pq_p pq_put_node pq_put_pri]
  (binding [pq_put_i nil pq_put_pq nil pq_put_updated nil] (try (do (set! pq_put_pq pq_put_pq_p) (set! pq_put_updated false) (set! pq_put_i 0) (while (< pq_put_i (count pq_put_pq)) (do (when (pos_equal (:pos (nth pq_put_pq pq_put_i)) pq_put_node) (do (when (< pq_put_pri (:pri (nth pq_put_pq pq_put_i))) (set! pq_put_pq (assoc pq_put_pq pq_put_i {:pos pq_put_node :pri pq_put_pri}))) (set! pq_put_updated true))) (set! pq_put_i (+ pq_put_i 1)))) (when (not pq_put_updated) (set! pq_put_pq (conj pq_put_pq {:pos pq_put_node :pri pq_put_pri}))) (throw (ex-info "return" {:v pq_put_pq}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pq_minkey [pq_minkey_pq]
  (binding [first_v nil pq_minkey_i nil pq_minkey_item nil pq_minkey_m nil] (try (do (when (= (count pq_minkey_pq) 0) (throw (ex-info "return" {:v main_INF}))) (set! first_v (nth pq_minkey_pq 0)) (set! pq_minkey_m (:pri first_v)) (set! pq_minkey_i 1) (while (< pq_minkey_i (count pq_minkey_pq)) (do (set! pq_minkey_item (nth pq_minkey_pq pq_minkey_i)) (when (< (:pri pq_minkey_item) pq_minkey_m) (set! pq_minkey_m (:pri pq_minkey_item))) (set! pq_minkey_i (+ pq_minkey_i 1)))) (throw (ex-info "return" {:v pq_minkey_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pq_pop_min [pq_pop_min_pq]
  (binding [pq_pop_min_best nil pq_pop_min_i nil pq_pop_min_idx nil pq_pop_min_new_pq nil] (try (do (set! pq_pop_min_best (nth pq_pop_min_pq 0)) (set! pq_pop_min_idx 0) (set! pq_pop_min_i 1) (while (< pq_pop_min_i (count pq_pop_min_pq)) (do (when (< (:pri (nth pq_pop_min_pq pq_pop_min_i)) (:pri pq_pop_min_best)) (do (set! pq_pop_min_best (nth pq_pop_min_pq pq_pop_min_i)) (set! pq_pop_min_idx pq_pop_min_i))) (set! pq_pop_min_i (+ pq_pop_min_i 1)))) (set! pq_pop_min_new_pq []) (set! pq_pop_min_i 0) (while (< pq_pop_min_i (count pq_pop_min_pq)) (do (when (not= pq_pop_min_i pq_pop_min_idx) (set! pq_pop_min_new_pq (conj pq_pop_min_new_pq (nth pq_pop_min_pq pq_pop_min_i)))) (set! pq_pop_min_i (+ pq_pop_min_i 1)))) (throw (ex-info "return" {:v {:node pq_pop_min_best :pq pq_pop_min_new_pq}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pq_remove [pq_remove_pq pq_remove_node]
  (binding [pq_remove_i nil pq_remove_new_pq nil] (try (do (set! pq_remove_new_pq []) (set! pq_remove_i 0) (while (< pq_remove_i (count pq_remove_pq)) (do (when (not (pos_equal (:pos (nth pq_remove_pq pq_remove_i)) pq_remove_node)) (set! pq_remove_new_pq (conj pq_remove_new_pq (nth pq_remove_pq pq_remove_i)))) (set! pq_remove_i (+ pq_remove_i 1)))) (throw (ex-info "return" {:v pq_remove_new_pq}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reconstruct [reconstruct_back_pointer reconstruct_goal reconstruct_start]
  (binding [reconstruct_current nil reconstruct_i nil reconstruct_key nil reconstruct_path nil reconstruct_rev nil] (try (do (set! reconstruct_path []) (set! reconstruct_current reconstruct_goal) (set! reconstruct_key (pos_key reconstruct_current)) (set! reconstruct_path (conj reconstruct_path reconstruct_current)) (while (not (pos_equal reconstruct_current reconstruct_start)) (do (set! reconstruct_current (get reconstruct_back_pointer reconstruct_key)) (set! reconstruct_key (pos_key reconstruct_current)) (set! reconstruct_path (conj reconstruct_path reconstruct_current)))) (set! reconstruct_rev []) (set! reconstruct_i (- (count reconstruct_path) 1)) (while (>= reconstruct_i 0) (do (set! reconstruct_rev (conj reconstruct_rev (nth reconstruct_path reconstruct_i))) (set! reconstruct_i (- reconstruct_i 1)))) (throw (ex-info "return" {:v reconstruct_rev}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn neighbours [neighbours_p]
  (binding [neighbours_down nil neighbours_left nil neighbours_right nil neighbours_up nil] (try (do (set! neighbours_left {:x (- (:x neighbours_p) 1) :y (:y neighbours_p)}) (set! neighbours_right {:x (+ (:x neighbours_p) 1) :y (:y neighbours_p)}) (set! neighbours_up {:x (:x neighbours_p) :y (+ (:y neighbours_p) 1)}) (set! neighbours_down {:x (:x neighbours_p) :y (- (:y neighbours_p) 1)}) (throw (ex-info "return" {:v [neighbours_left neighbours_right neighbours_up neighbours_down]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn multi_a_star [multi_a_star_start multi_a_star_goal main_n_heuristic]
  (binding [multi_a_star_back_pointer nil multi_a_star_chosen nil multi_a_star_ckey nil multi_a_star_current nil multi_a_star_g_function nil multi_a_star_i nil multi_a_star_j nil multi_a_star_k nil multi_a_star_nb nil multi_a_star_neighs nil multi_a_star_nkey nil multi_a_star_open_list nil multi_a_star_p nil multi_a_star_pair nil multi_a_star_path nil multi_a_star_pri nil multi_a_star_pri2 nil multi_a_star_tentative nil multi_a_star_visited nil] (try (do (set! multi_a_star_g_function {}) (set! multi_a_star_back_pointer {}) (set! multi_a_star_visited {}) (set! multi_a_star_open_list []) (set! multi_a_star_g_function (assoc multi_a_star_g_function (pos_key multi_a_star_start) 0.0)) (set! multi_a_star_g_function (assoc multi_a_star_g_function (pos_key multi_a_star_goal) main_INF)) (set! multi_a_star_back_pointer (assoc multi_a_star_back_pointer (pos_key multi_a_star_start) {:x (- 1) :y (- 1)})) (set! multi_a_star_back_pointer (assoc multi_a_star_back_pointer (pos_key multi_a_star_goal) {:x (- 1) :y (- 1)})) (set! multi_a_star_visited (assoc multi_a_star_visited (pos_key multi_a_star_start) true)) (set! multi_a_star_i 0) (while (< multi_a_star_i main_n_heuristic) (do (set! multi_a_star_open_list (conj multi_a_star_open_list [])) (set! multi_a_star_pri (key_fn multi_a_star_start multi_a_star_i multi_a_star_goal multi_a_star_g_function)) (set! multi_a_star_open_list (assoc multi_a_star_open_list multi_a_star_i (pq_put (nth multi_a_star_open_list multi_a_star_i) multi_a_star_start multi_a_star_pri))) (set! multi_a_star_i (+ multi_a_star_i 1)))) (loop [while_flag_1 true] (when (and while_flag_1 (< (pq_minkey (nth multi_a_star_open_list 0)) main_INF)) (do (set! multi_a_star_chosen 0) (set! multi_a_star_i 1) (loop [while_flag_2 true] (when (and while_flag_2 (< multi_a_star_i main_n_heuristic)) (cond (<= (pq_minkey (nth multi_a_star_open_list multi_a_star_i)) (* main_W2 (pq_minkey (nth multi_a_star_open_list 0)))) (do (set! multi_a_star_chosen multi_a_star_i) (recur false)) :else (do (set! multi_a_star_i (+ multi_a_star_i 1)) (recur while_flag_2))))) (when (not= multi_a_star_chosen 0) (alter-var-root (var main_t) (fn [_] (+ main_t 1)))) (set! multi_a_star_pair (pq_pop_min (nth multi_a_star_open_list multi_a_star_chosen))) (set! multi_a_star_open_list (assoc multi_a_star_open_list multi_a_star_chosen (:pq multi_a_star_pair))) (set! multi_a_star_current (:node multi_a_star_pair)) (set! multi_a_star_i 0) (while (< multi_a_star_i main_n_heuristic) (do (when (not= multi_a_star_i multi_a_star_chosen) (set! multi_a_star_open_list (assoc multi_a_star_open_list multi_a_star_i (pq_remove (get multi_a_star_open_list multi_a_star_i) (:pos multi_a_star_current))))) (set! multi_a_star_i (+ multi_a_star_i 1)))) (set! multi_a_star_ckey (pos_key (:pos multi_a_star_current))) (cond (in multi_a_star_ckey multi_a_star_visited) (recur true) :else (do (set! multi_a_star_visited (assoc multi_a_star_visited multi_a_star_ckey true)) (when (pos_equal (:pos multi_a_star_current) multi_a_star_goal) (do (set! multi_a_star_path (reconstruct multi_a_star_back_pointer multi_a_star_goal multi_a_star_start)) (set! multi_a_star_j 0) (while (< multi_a_star_j (count multi_a_star_path)) (do (set! multi_a_star_p (nth multi_a_star_path multi_a_star_j)) (println (str (str (str (str "(" (str (:x multi_a_star_p))) ",") (str (:y multi_a_star_p))) ")")) (set! multi_a_star_j (+ multi_a_star_j 1)))) (throw (ex-info "return" {:v nil})))) (set! multi_a_star_neighs (neighbours (:pos multi_a_star_current))) (set! multi_a_star_k 0) (while (< multi_a_star_k (count multi_a_star_neighs)) (do (set! multi_a_star_nb (nth multi_a_star_neighs multi_a_star_k)) (when (and (valid multi_a_star_nb) (= (in_blocks multi_a_star_nb) false)) (do (set! multi_a_star_nkey (pos_key multi_a_star_nb)) (set! multi_a_star_tentative (+ (get multi_a_star_g_function multi_a_star_ckey) 1.0)) (when (or (not (in multi_a_star_nkey multi_a_star_g_function)) (< multi_a_star_tentative (get multi_a_star_g_function multi_a_star_nkey))) (do (set! multi_a_star_g_function (assoc multi_a_star_g_function multi_a_star_nkey multi_a_star_tentative)) (set! multi_a_star_back_pointer (assoc multi_a_star_back_pointer multi_a_star_nkey (:pos multi_a_star_current))) (set! multi_a_star_i 0) (while (< multi_a_star_i main_n_heuristic) (do (set! multi_a_star_pri2 (+ multi_a_star_tentative (* main_W1 (heuristic multi_a_star_i multi_a_star_nb multi_a_star_goal)))) (set! multi_a_star_open_list (assoc multi_a_star_open_list multi_a_star_i (pq_put (get multi_a_star_open_list multi_a_star_i) multi_a_star_nb multi_a_star_pri2))) (set! multi_a_star_i (+ multi_a_star_i 1)))))))) (set! multi_a_star_k (+ multi_a_star_k 1)))) (recur while_flag_1)))))) (println "No path found to goal")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_start {:x 0 :y 0})

(def ^:dynamic main_goal {:x (- main_n 1) :y (- main_n 1)})

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_start) (constantly (multi_a_star main_start main_goal main_n_heuristic)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
