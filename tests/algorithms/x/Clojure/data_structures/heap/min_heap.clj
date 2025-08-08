(ns main (:refer-clojure :exclude [get_parent_idx get_left_child_idx get_right_child_idx remove_key slice_without_last sift_down sift_up new_min_heap peek remove_min insert is_empty get_value decrease_key node_to_string]))

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

(declare get_parent_idx get_left_child_idx get_right_child_idx remove_key slice_without_last sift_down sift_up new_min_heap peek remove_min insert is_empty get_value decrease_key node_to_string)

(def ^:dynamic decrease_key_heap nil)

(def ^:dynamic decrease_key_idx nil)

(def ^:dynamic decrease_key_idx_map nil)

(def ^:dynamic decrease_key_mh nil)

(def ^:dynamic decrease_key_node nil)

(def ^:dynamic decrease_key_val_map nil)

(def ^:dynamic insert_heap nil)

(def ^:dynamic insert_idx nil)

(def ^:dynamic insert_idx_map nil)

(def ^:dynamic insert_mh nil)

(def ^:dynamic insert_val_map nil)

(def ^:dynamic new_min_heap_heap nil)

(def ^:dynamic new_min_heap_i nil)

(def ^:dynamic new_min_heap_idx_map nil)

(def ^:dynamic new_min_heap_mh nil)

(def ^:dynamic new_min_heap_n nil)

(def ^:dynamic new_min_heap_start nil)

(def ^:dynamic new_min_heap_val_map nil)

(def ^:dynamic remove_key_out nil)

(def ^:dynamic remove_min_heap nil)

(def ^:dynamic remove_min_idx_map nil)

(def ^:dynamic remove_min_last nil)

(def ^:dynamic remove_min_last_idx nil)

(def ^:dynamic remove_min_mh nil)

(def ^:dynamic remove_min_top nil)

(def ^:dynamic remove_min_val_map nil)

(def ^:dynamic sift_down_heap nil)

(def ^:dynamic sift_down_i nil)

(def ^:dynamic sift_down_idx_map nil)

(def ^:dynamic sift_down_left nil)

(def ^:dynamic sift_down_mh nil)

(def ^:dynamic sift_down_right nil)

(def ^:dynamic sift_down_smallest nil)

(def ^:dynamic sift_down_tmp nil)

(def ^:dynamic sift_up_heap nil)

(def ^:dynamic sift_up_i nil)

(def ^:dynamic sift_up_idx_map nil)

(def ^:dynamic sift_up_mh nil)

(def ^:dynamic sift_up_p nil)

(def ^:dynamic sift_up_tmp nil)

(def ^:dynamic slice_without_last_i nil)

(def ^:dynamic slice_without_last_res nil)

(defn get_parent_idx [get_parent_idx_idx]
  (try (throw (ex-info "return" {:v (quot (- get_parent_idx_idx 1) 2)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_left_child_idx [get_left_child_idx_idx]
  (try (throw (ex-info "return" {:v (+ (* get_left_child_idx_idx 2) 1)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_right_child_idx [get_right_child_idx_idx]
  (try (throw (ex-info "return" {:v (+ (* get_right_child_idx_idx 2) 2)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn remove_key [remove_key_m remove_key_k]
  (binding [remove_key_out nil] (try (do (set! remove_key_out {}) (doseq [key remove_key_m] (when (not= key remove_key_k) (set! remove_key_out (assoc remove_key_out key (nth remove_key_m key))))) (throw (ex-info "return" {:v remove_key_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn slice_without_last [slice_without_last_xs]
  (binding [slice_without_last_i nil slice_without_last_res nil] (try (do (set! slice_without_last_res []) (set! slice_without_last_i 0) (while (< slice_without_last_i (- (count slice_without_last_xs) 1)) (do (set! slice_without_last_res (conj slice_without_last_res (nth slice_without_last_xs slice_without_last_i))) (set! slice_without_last_i (+ slice_without_last_i 1)))) (throw (ex-info "return" {:v slice_without_last_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sift_down [sift_down_mh_p sift_down_idx]
  (binding [sift_down_heap nil sift_down_i nil sift_down_idx_map nil sift_down_left nil sift_down_mh nil sift_down_right nil sift_down_smallest nil sift_down_tmp nil] (do (set! sift_down_mh sift_down_mh_p) (set! sift_down_heap (:heap sift_down_mh)) (set! sift_down_idx_map (:idx_of_element sift_down_mh)) (set! sift_down_i sift_down_idx) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! sift_down_left (get_left_child_idx sift_down_i)) (set! sift_down_right (get_right_child_idx sift_down_i)) (set! sift_down_smallest sift_down_i) (when (and (< sift_down_left (count sift_down_heap)) (< (:val (nth sift_down_heap sift_down_left)) (:val (nth sift_down_heap sift_down_smallest)))) (set! sift_down_smallest sift_down_left)) (when (and (< sift_down_right (count sift_down_heap)) (< (:val (nth sift_down_heap sift_down_right)) (:val (nth sift_down_heap sift_down_smallest)))) (set! sift_down_smallest sift_down_right)) (if (not= sift_down_smallest sift_down_i) (do (set! sift_down_tmp (nth sift_down_heap sift_down_i)) (set! sift_down_heap (assoc sift_down_heap sift_down_i (nth sift_down_heap sift_down_smallest))) (set! sift_down_heap (assoc sift_down_heap sift_down_smallest sift_down_tmp)) (set! sift_down_idx_map (assoc sift_down_idx_map (:name (nth sift_down_heap sift_down_i)) sift_down_i)) (set! sift_down_idx_map (assoc sift_down_idx_map (:name (nth sift_down_heap sift_down_smallest)) sift_down_smallest)) (set! sift_down_i sift_down_smallest) (recur while_flag_1)) (recur false))))) (set! sift_down_mh (assoc sift_down_mh :heap sift_down_heap)) (set! sift_down_mh (assoc sift_down_mh :idx_of_element sift_down_idx_map)))))

(defn sift_up [sift_up_mh_p sift_up_idx]
  (binding [sift_up_heap nil sift_up_i nil sift_up_idx_map nil sift_up_mh nil sift_up_p nil sift_up_tmp nil] (do (set! sift_up_mh sift_up_mh_p) (set! sift_up_heap (:heap sift_up_mh)) (set! sift_up_idx_map (:idx_of_element sift_up_mh)) (set! sift_up_i sift_up_idx) (set! sift_up_p (get_parent_idx sift_up_i)) (while (and (>= sift_up_p 0) (> (:val (nth sift_up_heap sift_up_p)) (:val (nth sift_up_heap sift_up_i)))) (do (set! sift_up_tmp (nth sift_up_heap sift_up_p)) (set! sift_up_heap (assoc sift_up_heap sift_up_p (nth sift_up_heap sift_up_i))) (set! sift_up_heap (assoc sift_up_heap sift_up_i sift_up_tmp)) (set! sift_up_idx_map (assoc sift_up_idx_map (:name (nth sift_up_heap sift_up_p)) sift_up_p)) (set! sift_up_idx_map (assoc sift_up_idx_map (:name (nth sift_up_heap sift_up_i)) sift_up_i)) (set! sift_up_i sift_up_p) (set! sift_up_p (get_parent_idx sift_up_i)))) (set! sift_up_mh (assoc sift_up_mh :heap sift_up_heap)) (set! sift_up_mh (assoc sift_up_mh :idx_of_element sift_up_idx_map)))))

(defn new_min_heap [new_min_heap_array]
  (binding [new_min_heap_heap nil new_min_heap_i nil new_min_heap_idx_map nil new_min_heap_mh nil new_min_heap_n nil new_min_heap_start nil new_min_heap_val_map nil] (try (do (set! new_min_heap_idx_map {}) (set! new_min_heap_val_map {}) (set! new_min_heap_heap new_min_heap_array) (set! new_min_heap_i 0) (while (< new_min_heap_i (count new_min_heap_array)) (do (set! new_min_heap_n (nth new_min_heap_array new_min_heap_i)) (set! new_min_heap_idx_map (assoc new_min_heap_idx_map (:name new_min_heap_n) new_min_heap_i)) (set! new_min_heap_val_map (assoc new_min_heap_val_map (:name new_min_heap_n) (:val new_min_heap_n))) (set! new_min_heap_i (+ new_min_heap_i 1)))) (set! new_min_heap_mh {:heap new_min_heap_heap :heap_dict new_min_heap_val_map :idx_of_element new_min_heap_idx_map}) (set! new_min_heap_start (get_parent_idx (- (count new_min_heap_array) 1))) (while (>= new_min_heap_start 0) (do (sift_down new_min_heap_mh new_min_heap_start) (set! new_min_heap_start (- new_min_heap_start 1)))) (throw (ex-info "return" {:v new_min_heap_mh}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn peek [peek_mh]
  (try (throw (ex-info "return" {:v (get (:heap peek_mh) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn remove_min [remove_min_mh_p]
  (binding [remove_min_heap nil remove_min_idx_map nil remove_min_last nil remove_min_last_idx nil remove_min_mh nil remove_min_top nil remove_min_val_map nil] (try (do (set! remove_min_mh remove_min_mh_p) (set! remove_min_heap (:heap remove_min_mh)) (set! remove_min_idx_map (:idx_of_element remove_min_mh)) (set! remove_min_val_map (:heap_dict remove_min_mh)) (set! remove_min_last_idx (- (count remove_min_heap) 1)) (set! remove_min_top (nth remove_min_heap 0)) (set! remove_min_last (nth remove_min_heap remove_min_last_idx)) (set! remove_min_heap (assoc remove_min_heap 0 remove_min_last)) (set! remove_min_idx_map (assoc remove_min_idx_map (:name remove_min_last) 0)) (set! remove_min_heap (slice_without_last remove_min_heap)) (set! remove_min_idx_map (remove_key remove_min_idx_map (:name remove_min_top))) (set! remove_min_val_map (remove_key remove_min_val_map (:name remove_min_top))) (set! remove_min_mh (assoc remove_min_mh :heap remove_min_heap)) (set! remove_min_mh (assoc remove_min_mh :idx_of_element remove_min_idx_map)) (set! remove_min_mh (assoc remove_min_mh :heap_dict remove_min_val_map)) (when (> (count remove_min_heap) 0) (sift_down remove_min_mh 0)) (throw (ex-info "return" {:v remove_min_top}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert [insert_mh_p insert_node]
  (binding [insert_heap nil insert_idx nil insert_idx_map nil insert_mh nil insert_val_map nil] (do (set! insert_mh insert_mh_p) (set! insert_heap (:heap insert_mh)) (set! insert_idx_map (:idx_of_element insert_mh)) (set! insert_val_map (:heap_dict insert_mh)) (set! insert_heap (conj insert_heap insert_node)) (set! insert_idx (- (count insert_heap) 1)) (set! insert_idx_map (assoc insert_idx_map (:name insert_node) insert_idx)) (set! insert_val_map (assoc insert_val_map (:name insert_node) (:val insert_node))) (set! insert_mh (assoc insert_mh :heap insert_heap)) (set! insert_mh (assoc insert_mh :idx_of_element insert_idx_map)) (set! insert_mh (assoc insert_mh :heap_dict insert_val_map)) (sift_up insert_mh insert_idx))))

(defn is_empty [is_empty_mh]
  (try (throw (ex-info "return" {:v (= (count (:heap is_empty_mh)) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_value [get_value_mh get_value_key]
  (try (throw (ex-info "return" {:v (get (:heap_dict get_value_mh) get_value_key)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn decrease_key [decrease_key_mh_p decrease_key_node_p decrease_key_new_value]
  (binding [decrease_key_heap nil decrease_key_idx nil decrease_key_idx_map nil decrease_key_mh nil decrease_key_node nil decrease_key_val_map nil] (do (set! decrease_key_mh decrease_key_mh_p) (set! decrease_key_node decrease_key_node_p) (set! decrease_key_heap (:heap decrease_key_mh)) (set! decrease_key_val_map (:heap_dict decrease_key_mh)) (set! decrease_key_idx_map (:idx_of_element decrease_key_mh)) (set! decrease_key_idx (nth decrease_key_idx_map (:name decrease_key_node))) (when (not (> (:val (nth decrease_key_heap decrease_key_idx)) decrease_key_new_value)) (throw (Exception. "newValue must be less than current value"))) (set! decrease_key_node (assoc decrease_key_node :val decrease_key_new_value)) (set! decrease_key_heap (assoc-in decrease_key_heap [decrease_key_idx :val] decrease_key_new_value)) (set! decrease_key_val_map (assoc decrease_key_val_map (:name decrease_key_node) decrease_key_new_value)) (set! decrease_key_mh (assoc decrease_key_mh :heap decrease_key_heap)) (set! decrease_key_mh (assoc decrease_key_mh :heap_dict decrease_key_val_map)) (sift_up decrease_key_mh decrease_key_idx))))

(defn node_to_string [node_to_string_n]
  (try (throw (ex-info "return" {:v (str (str (str (str "Node(" (:name node_to_string_n)) ", ") (str (:val node_to_string_n))) ")")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_r {:name "R" :val (- 1)})

(def ^:dynamic main_b {:name "B" :val 6})

(def ^:dynamic main_a {:name "A" :val 3})

(def ^:dynamic main_x {:name "X" :val 1})

(def ^:dynamic main_e {:name "E" :val 4})

(def ^:dynamic main_my_min_heap (new_min_heap [main_r main_b main_a main_x main_e]))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "Min Heap - before decrease key")
      (doseq [n (keys (:heap main_my_min_heap))] (println (node_to_string n)))
      (println "Min Heap - After decrease key of node [B -> -17]")
      (decrease_key main_my_min_heap main_b (- 17))
      (doseq [n (keys (:heap main_my_min_heap))] (println (node_to_string n)))
      (println (str (get_value main_my_min_heap "B")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
