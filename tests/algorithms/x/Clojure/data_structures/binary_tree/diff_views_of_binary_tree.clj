(ns main (:refer-clojure :exclude [make_tree index_of sort_pairs right_view left_view top_view bottom_view]))

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

(declare make_tree index_of sort_pairs right_view left_view top_view bottom_view)

(def ^:dynamic bottom_view_hd nil)

(def ^:dynamic bottom_view_hds nil)

(def ^:dynamic bottom_view_idx nil)

(def ^:dynamic bottom_view_pos nil)

(def ^:dynamic bottom_view_queue_hd nil)

(def ^:dynamic bottom_view_queue_idx nil)

(def ^:dynamic bottom_view_vals nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic left_view_i nil)

(def ^:dynamic left_view_idx nil)

(def ^:dynamic left_view_queue nil)

(def ^:dynamic left_view_res nil)

(def ^:dynamic left_view_size nil)

(def ^:dynamic right_view_i nil)

(def ^:dynamic right_view_idx nil)

(def ^:dynamic right_view_queue nil)

(def ^:dynamic right_view_res nil)

(def ^:dynamic right_view_size nil)

(def ^:dynamic sort_pairs_hd_tmp nil)

(def ^:dynamic sort_pairs_hds nil)

(def ^:dynamic sort_pairs_i nil)

(def ^:dynamic sort_pairs_j nil)

(def ^:dynamic sort_pairs_val_tmp nil)

(def ^:dynamic sort_pairs_vals nil)

(def ^:dynamic top_view_hd nil)

(def ^:dynamic top_view_hds nil)

(def ^:dynamic top_view_idx nil)

(def ^:dynamic top_view_queue_hd nil)

(def ^:dynamic top_view_queue_idx nil)

(def ^:dynamic top_view_vals nil)

(def ^:dynamic main_NIL (- 0 1))

(defn make_tree []
  (try (throw (ex-info "return" {:v {:values [3 9 20 15 7] :lefts [1 main_NIL 3 main_NIL main_NIL] :rights [2 main_NIL 4 main_NIL main_NIL] :root 0}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn index_of [index_of_xs index_of_x]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_xs)) (do (when (= (nth index_of_xs index_of_i) index_of_x) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v main_NIL}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_pairs [sort_pairs_hds_p sort_pairs_vals_p]
  (binding [sort_pairs_hd_tmp nil sort_pairs_hds nil sort_pairs_i nil sort_pairs_j nil sort_pairs_val_tmp nil sort_pairs_vals nil] (do (set! sort_pairs_hds sort_pairs_hds_p) (set! sort_pairs_vals sort_pairs_vals_p) (set! sort_pairs_i 0) (while (< sort_pairs_i (count sort_pairs_hds)) (do (set! sort_pairs_j sort_pairs_i) (while (and (> sort_pairs_j 0) (> (nth sort_pairs_hds (- sort_pairs_j 1)) (nth sort_pairs_hds sort_pairs_j))) (do (set! sort_pairs_hd_tmp (nth sort_pairs_hds (- sort_pairs_j 1))) (set! sort_pairs_hds (assoc sort_pairs_hds (- sort_pairs_j 1) (nth sort_pairs_hds sort_pairs_j))) (set! sort_pairs_hds (assoc sort_pairs_hds sort_pairs_j sort_pairs_hd_tmp)) (set! sort_pairs_val_tmp (nth sort_pairs_vals (- sort_pairs_j 1))) (set! sort_pairs_vals (assoc sort_pairs_vals (- sort_pairs_j 1) (nth sort_pairs_vals sort_pairs_j))) (set! sort_pairs_vals (assoc sort_pairs_vals sort_pairs_j sort_pairs_val_tmp)) (set! sort_pairs_j (- sort_pairs_j 1)))) (set! sort_pairs_i (+ sort_pairs_i 1)))))))

(defn right_view [right_view_t]
  (binding [right_view_i nil right_view_idx nil right_view_queue nil right_view_res nil right_view_size nil] (try (do (set! right_view_res []) (set! right_view_queue [(:root right_view_t)]) (while (> (count right_view_queue) 0) (do (set! right_view_size (count right_view_queue)) (set! right_view_i 0) (while (< right_view_i right_view_size) (do (set! right_view_idx (nth right_view_queue right_view_i)) (when (not= (get (:lefts right_view_t) right_view_idx) main_NIL) (set! right_view_queue (conj right_view_queue (get (:lefts right_view_t) right_view_idx)))) (when (not= (get (:rights right_view_t) right_view_idx) main_NIL) (set! right_view_queue (conj right_view_queue (get (:rights right_view_t) right_view_idx)))) (set! right_view_i (+ right_view_i 1)))) (set! right_view_res (conj right_view_res (get (:values right_view_t) (nth right_view_queue (- right_view_size 1))))) (set! right_view_queue (subvec right_view_queue right_view_size (count right_view_queue))))) (throw (ex-info "return" {:v right_view_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn left_view [left_view_t]
  (binding [left_view_i nil left_view_idx nil left_view_queue nil left_view_res nil left_view_size nil] (try (do (set! left_view_res []) (set! left_view_queue [(:root left_view_t)]) (while (> (count left_view_queue) 0) (do (set! left_view_size (count left_view_queue)) (set! left_view_i 0) (while (< left_view_i left_view_size) (do (set! left_view_idx (nth left_view_queue left_view_i)) (when (not= (get (:lefts left_view_t) left_view_idx) main_NIL) (set! left_view_queue (conj left_view_queue (get (:lefts left_view_t) left_view_idx)))) (when (not= (get (:rights left_view_t) left_view_idx) main_NIL) (set! left_view_queue (conj left_view_queue (get (:rights left_view_t) left_view_idx)))) (set! left_view_i (+ left_view_i 1)))) (set! left_view_res (conj left_view_res (get (:values left_view_t) (nth left_view_queue 0)))) (set! left_view_queue (subvec left_view_queue left_view_size (count left_view_queue))))) (throw (ex-info "return" {:v left_view_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn top_view [top_view_t]
  (binding [top_view_hd nil top_view_hds nil top_view_idx nil top_view_queue_hd nil top_view_queue_idx nil top_view_vals nil] (try (do (set! top_view_hds []) (set! top_view_vals []) (set! top_view_queue_idx [(:root top_view_t)]) (set! top_view_queue_hd [0]) (while (> (count top_view_queue_idx) 0) (do (set! top_view_idx (nth top_view_queue_idx 0)) (set! top_view_queue_idx (subvec top_view_queue_idx 1 (count top_view_queue_idx))) (set! top_view_hd (nth top_view_queue_hd 0)) (set! top_view_queue_hd (subvec top_view_queue_hd 1 (count top_view_queue_hd))) (when (= (index_of top_view_hds top_view_hd) main_NIL) (do (set! top_view_hds (conj top_view_hds top_view_hd)) (set! top_view_vals (conj top_view_vals (get (:values top_view_t) top_view_idx))))) (when (not= (get (:lefts top_view_t) top_view_idx) main_NIL) (do (set! top_view_queue_idx (conj top_view_queue_idx (get (:lefts top_view_t) top_view_idx))) (set! top_view_queue_hd (conj top_view_queue_hd (- top_view_hd 1))))) (when (not= (get (:rights top_view_t) top_view_idx) main_NIL) (do (set! top_view_queue_idx (conj top_view_queue_idx (get (:rights top_view_t) top_view_idx))) (set! top_view_queue_hd (conj top_view_queue_hd (+ top_view_hd 1))))))) (sort_pairs top_view_hds top_view_vals) (throw (ex-info "return" {:v top_view_vals}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bottom_view [bottom_view_t]
  (binding [bottom_view_hd nil bottom_view_hds nil bottom_view_idx nil bottom_view_pos nil bottom_view_queue_hd nil bottom_view_queue_idx nil bottom_view_vals nil] (try (do (set! bottom_view_hds []) (set! bottom_view_vals []) (set! bottom_view_queue_idx [(:root bottom_view_t)]) (set! bottom_view_queue_hd [0]) (while (> (count bottom_view_queue_idx) 0) (do (set! bottom_view_idx (nth bottom_view_queue_idx 0)) (set! bottom_view_queue_idx (subvec bottom_view_queue_idx 1 (count bottom_view_queue_idx))) (set! bottom_view_hd (nth bottom_view_queue_hd 0)) (set! bottom_view_queue_hd (subvec bottom_view_queue_hd 1 (count bottom_view_queue_hd))) (set! bottom_view_pos (index_of bottom_view_hds bottom_view_hd)) (if (= bottom_view_pos main_NIL) (do (set! bottom_view_hds (conj bottom_view_hds bottom_view_hd)) (set! bottom_view_vals (conj bottom_view_vals (get (:values bottom_view_t) bottom_view_idx)))) (set! bottom_view_vals (assoc bottom_view_vals bottom_view_pos (get (:values bottom_view_t) bottom_view_idx)))) (when (not= (get (:lefts bottom_view_t) bottom_view_idx) main_NIL) (do (set! bottom_view_queue_idx (conj bottom_view_queue_idx (get (:lefts bottom_view_t) bottom_view_idx))) (set! bottom_view_queue_hd (conj bottom_view_queue_hd (- bottom_view_hd 1))))) (when (not= (get (:rights bottom_view_t) bottom_view_idx) main_NIL) (do (set! bottom_view_queue_idx (conj bottom_view_queue_idx (get (:rights bottom_view_t) bottom_view_idx))) (set! bottom_view_queue_hd (conj bottom_view_queue_hd (+ bottom_view_hd 1))))))) (sort_pairs bottom_view_hds bottom_view_vals) (throw (ex-info "return" {:v bottom_view_vals}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_tree (make_tree))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (right_view main_tree))
      (println (left_view main_tree))
      (println (top_view main_tree))
      (println (bottom_view main_tree))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
