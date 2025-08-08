(ns main (:refer-clojure :exclude [new_heap parent left right swap cmp get_valid_parent heapify_up heapify_down update_item delete_item insert_item get_top extract_top identity negate]))

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

(declare new_heap parent left right swap cmp get_valid_parent heapify_up heapify_down update_item delete_item insert_item get_top extract_top identity negate)

(def ^:dynamic cmp_arr nil)

(def ^:dynamic delete_item_arr nil)

(def ^:dynamic delete_item_h nil)

(def ^:dynamic delete_item_index nil)

(def ^:dynamic delete_item_last_index nil)

(def ^:dynamic delete_item_moved nil)

(def ^:dynamic delete_item_pm nil)

(def ^:dynamic extract_top_top nil)

(def ^:dynamic get_top_arr nil)

(def ^:dynamic get_valid_parent_l nil)

(def ^:dynamic get_valid_parent_r nil)

(def ^:dynamic get_valid_parent_vp nil)

(def ^:dynamic heapify_down_idx nil)

(def ^:dynamic heapify_down_vp nil)

(def ^:dynamic heapify_up_idx nil)

(def ^:dynamic heapify_up_p nil)

(def ^:dynamic insert_item_arr nil)

(def ^:dynamic insert_item_arr_len nil)

(def ^:dynamic insert_item_h nil)

(def ^:dynamic insert_item_pm nil)

(def ^:dynamic left_l nil)

(def ^:dynamic main_h nil)

(def ^:dynamic right_r nil)

(def ^:dynamic swap_arr nil)

(def ^:dynamic swap_h nil)

(def ^:dynamic swap_item_i nil)

(def ^:dynamic swap_item_j nil)

(def ^:dynamic swap_pm nil)

(def ^:dynamic swap_tmp nil)

(def ^:dynamic update_item_arr nil)

(def ^:dynamic update_item_h nil)

(def ^:dynamic update_item_index nil)

(def ^:dynamic update_item_pm nil)

(defn new_heap [new_heap_key]
  (try (throw (ex-info "return" {:v {:arr [] :key new_heap_key :pos_map {} :size 0}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parent [parent_i]
  (try (if (> parent_i 0) (quot (- parent_i 1) 2) (- 1)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn left [left_i left_size]
  (binding [left_l nil] (try (do (set! left_l (+ (* 2 left_i) 1)) (if (< left_l left_size) left_l (- 1))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn right [right_i right_size]
  (binding [right_r nil] (try (do (set! right_r (+ (* 2 right_i) 2)) (if (< right_r right_size) right_r (- 1))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn swap [swap_h_p swap_i swap_j]
  (binding [swap_arr nil swap_h nil swap_item_i nil swap_item_j nil swap_pm nil swap_tmp nil] (do (set! swap_h swap_h_p) (set! swap_arr (:arr swap_h)) (set! swap_item_i (nth (nth swap_arr swap_i) 0)) (set! swap_item_j (nth (nth swap_arr swap_j) 0)) (set! swap_pm (:pos_map swap_h)) (set! swap_pm (assoc swap_pm swap_item_i (+ swap_j 1))) (set! swap_pm (assoc swap_pm swap_item_j (+ swap_i 1))) (set! swap_h (assoc swap_h :pos_map swap_pm)) (set! swap_tmp (nth swap_arr swap_i)) (set! swap_arr (assoc swap_arr swap_i (nth swap_arr swap_j))) (set! swap_arr (assoc swap_arr swap_j swap_tmp)) (set! swap_h (assoc swap_h :arr swap_arr)))))

(defn cmp [cmp_h cmp_i cmp_j]
  (binding [cmp_arr nil] (try (do (set! cmp_arr (:arr cmp_h)) (throw (ex-info "return" {:v (< (nth (nth cmp_arr cmp_i) 1) (nth (nth cmp_arr cmp_j) 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_valid_parent [get_valid_parent_h get_valid_parent_i]
  (binding [get_valid_parent_l nil get_valid_parent_r nil get_valid_parent_vp nil] (try (do (set! get_valid_parent_vp get_valid_parent_i) (set! get_valid_parent_l (left get_valid_parent_i (:size get_valid_parent_h))) (when (and (not= get_valid_parent_l (- 0 1)) (= (cmp get_valid_parent_h get_valid_parent_l get_valid_parent_vp) false)) (set! get_valid_parent_vp get_valid_parent_l)) (set! get_valid_parent_r (right get_valid_parent_i (:size get_valid_parent_h))) (when (and (not= get_valid_parent_r (- 0 1)) (= (cmp get_valid_parent_h get_valid_parent_r get_valid_parent_vp) false)) (set! get_valid_parent_vp get_valid_parent_r)) (throw (ex-info "return" {:v get_valid_parent_vp}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn heapify_up [heapify_up_h heapify_up_index]
  (binding [heapify_up_idx nil heapify_up_p nil] (do (set! heapify_up_idx heapify_up_index) (set! heapify_up_p (parent heapify_up_idx)) (while (and (not= heapify_up_p (- 0 1)) (= (cmp heapify_up_h heapify_up_idx heapify_up_p) false)) (do (swap heapify_up_h heapify_up_idx heapify_up_p) (set! heapify_up_idx heapify_up_p) (set! heapify_up_p (parent heapify_up_p)))))))

(defn heapify_down [heapify_down_h heapify_down_index]
  (binding [heapify_down_idx nil heapify_down_vp nil] (do (set! heapify_down_idx heapify_down_index) (set! heapify_down_vp (get_valid_parent heapify_down_h heapify_down_idx)) (while (not= heapify_down_vp heapify_down_idx) (do (swap heapify_down_h heapify_down_idx heapify_down_vp) (set! heapify_down_idx heapify_down_vp) (set! heapify_down_vp (get_valid_parent heapify_down_h heapify_down_idx)))))))

(defn update_item [update_item_h_p update_item_item update_item_item_value]
  (binding [update_item_arr nil update_item_h nil update_item_index nil update_item_pm nil] (try (do (set! update_item_h update_item_h_p) (set! update_item_pm (:pos_map update_item_h)) (when (= (get update_item_pm update_item_item) 0) (throw (ex-info "return" {:v nil}))) (set! update_item_index (- (get update_item_pm update_item_item) 1)) (set! update_item_arr (:arr update_item_h)) (set! update_item_arr (assoc update_item_arr update_item_index [update_item_item ((:key update_item_h) update_item_item_value)])) (set! update_item_h (assoc update_item_h :arr update_item_arr)) (set! update_item_h (assoc update_item_h :pos_map update_item_pm)) (heapify_up update_item_h update_item_index) (heapify_down update_item_h update_item_index)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn delete_item [delete_item_h_p delete_item_item]
  (binding [delete_item_arr nil delete_item_h nil delete_item_index nil delete_item_last_index nil delete_item_moved nil delete_item_pm nil] (try (do (set! delete_item_h delete_item_h_p) (set! delete_item_pm (:pos_map delete_item_h)) (when (= (get delete_item_pm delete_item_item) 0) (throw (ex-info "return" {:v nil}))) (set! delete_item_index (- (get delete_item_pm delete_item_item) 1)) (set! delete_item_pm (assoc delete_item_pm delete_item_item 0)) (set! delete_item_arr (:arr delete_item_h)) (set! delete_item_last_index (- (:size delete_item_h) 1)) (when (not= delete_item_index delete_item_last_index) (do (set! delete_item_arr (assoc delete_item_arr delete_item_index (nth delete_item_arr delete_item_last_index))) (set! delete_item_moved (nth (nth delete_item_arr delete_item_index) 0)) (set! delete_item_pm (assoc delete_item_pm delete_item_moved (+ delete_item_index 1))))) (set! delete_item_h (assoc delete_item_h :size (- (:size delete_item_h) 1))) (set! delete_item_h (assoc delete_item_h :arr delete_item_arr)) (set! delete_item_h (assoc delete_item_h :pos_map delete_item_pm)) (when (> (:size delete_item_h) delete_item_index) (do (heapify_up delete_item_h delete_item_index) (heapify_down delete_item_h delete_item_index)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert_item [insert_item_h_p insert_item_item insert_item_item_value]
  (binding [insert_item_arr nil insert_item_arr_len nil insert_item_h nil insert_item_pm nil] (do (set! insert_item_h insert_item_h_p) (set! insert_item_arr (:arr insert_item_h)) (set! insert_item_arr_len (count insert_item_arr)) (if (= insert_item_arr_len (:size insert_item_h)) (set! insert_item_arr (conj insert_item_arr [insert_item_item ((:key insert_item_h) insert_item_item_value)])) (set! insert_item_arr (assoc insert_item_arr (:size insert_item_h) [insert_item_item ((:key insert_item_h) insert_item_item_value)]))) (set! insert_item_pm (:pos_map insert_item_h)) (set! insert_item_pm (assoc insert_item_pm insert_item_item (+ (:size insert_item_h) 1))) (set! insert_item_h (assoc insert_item_h :size (+ (:size insert_item_h) 1))) (set! insert_item_h (assoc insert_item_h :arr insert_item_arr)) (set! insert_item_h (assoc insert_item_h :pos_map insert_item_pm)) (heapify_up insert_item_h (- (:size insert_item_h) 1)))))

(defn get_top [get_top_h]
  (binding [get_top_arr nil] (try (do (set! get_top_arr (:arr get_top_h)) (if (> (:size get_top_h) 0) (nth get_top_arr 0) [])) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn extract_top [extract_top_h]
  (binding [extract_top_top nil] (try (do (set! extract_top_top (get_top extract_top_h)) (when (> (count extract_top_top) 0) (delete_item extract_top_h (nth extract_top_top 0))) (throw (ex-info "return" {:v extract_top_top}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn identity [identity_x]
  (try (throw (ex-info "return" {:v identity_x})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn negate [negate_x]
  (try (throw (ex-info "return" {:v (- 0 negate_x)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_h (new_heap identity))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (insert_item main_h 5 34)
      (insert_item main_h 6 31)
      (insert_item main_h 7 37)
      (println (str (get_top main_h)))
      (println (str (extract_top main_h)))
      (println (str (extract_top main_h)))
      (println (str (extract_top main_h)))
      (def main_h (new_heap negate))
      (insert_item main_h 5 34)
      (insert_item main_h 6 31)
      (insert_item main_h 7 37)
      (println (str (get_top main_h)))
      (println (str (extract_top main_h)))
      (println (str (extract_top main_h)))
      (println (str (extract_top main_h)))
      (insert_item main_h 8 45)
      (insert_item main_h 9 40)
      (insert_item main_h 10 50)
      (println (str (get_top main_h)))
      (update_item main_h 10 30)
      (println (str (get_top main_h)))
      (delete_item main_h 10)
      (println (str (get_top main_h)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
