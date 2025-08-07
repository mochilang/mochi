(ns main (:refer-clojure :exclude [init_int_array init_bool_array left right build update query segtree_to_string]))

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

(declare init_int_array init_bool_array left right build update query segtree_to_string)

(def ^:dynamic build_lv nil)

(def ^:dynamic build_mid nil)

(def ^:dynamic build_rv nil)

(def ^:dynamic build_segment_tree nil)

(def ^:dynamic init_bool_array_arr nil)

(def ^:dynamic init_bool_array_i nil)

(def ^:dynamic init_int_array_arr nil)

(def ^:dynamic init_int_array_i nil)

(def ^:dynamic query_flag nil)

(def ^:dynamic query_lazy nil)

(def ^:dynamic query_mid nil)

(def ^:dynamic query_q1 nil)

(def ^:dynamic query_q2 nil)

(def ^:dynamic query_segment_tree nil)

(def ^:dynamic segtree_to_string_i nil)

(def ^:dynamic segtree_to_string_res nil)

(def ^:dynamic segtree_to_string_v nil)

(def ^:dynamic update_flag nil)

(def ^:dynamic update_lazy nil)

(def ^:dynamic update_lv nil)

(def ^:dynamic update_mid nil)

(def ^:dynamic update_rv nil)

(def ^:dynamic update_segment_tree nil)

(defn init_int_array [init_int_array_n]
  (binding [init_int_array_arr nil init_int_array_i nil] (try (do (set! init_int_array_arr []) (set! init_int_array_i 0) (while (< init_int_array_i (+ (* 4 init_int_array_n) 5)) (do (set! init_int_array_arr (conj init_int_array_arr 0)) (set! init_int_array_i (+ init_int_array_i 1)))) (throw (ex-info "return" {:v init_int_array_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn init_bool_array [init_bool_array_n]
  (binding [init_bool_array_arr nil init_bool_array_i nil] (try (do (set! init_bool_array_arr []) (set! init_bool_array_i 0) (while (< init_bool_array_i (+ (* 4 init_bool_array_n) 5)) (do (set! init_bool_array_arr (conj init_bool_array_arr false)) (set! init_bool_array_i (+ init_bool_array_i 1)))) (throw (ex-info "return" {:v init_bool_array_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn left [left_idx]
  (try (throw (ex-info "return" {:v (* left_idx 2)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn right [right_idx]
  (try (throw (ex-info "return" {:v (+ (* right_idx 2) 1)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn build [build_segment_tree_p build_idx build_l build_r build_a]
  (binding [build_lv nil build_mid nil build_rv nil build_segment_tree nil] (do (set! build_segment_tree build_segment_tree_p) (if (= build_l build_r) (set! build_segment_tree (assoc build_segment_tree build_idx (nth build_a (- build_l 1)))) (do (set! build_mid (quot (+ build_l build_r) 2)) (build build_segment_tree (left build_idx) build_l build_mid build_a) (build build_segment_tree (right build_idx) (+ build_mid 1) build_r build_a) (set! build_lv (nth build_segment_tree (left build_idx))) (set! build_rv (nth build_segment_tree (right build_idx))) (if (> build_lv build_rv) (set! build_segment_tree (assoc build_segment_tree build_idx build_lv)) (set! build_segment_tree (assoc build_segment_tree build_idx build_rv))))))))

(defn update [update_segment_tree_p update_lazy_p update_flag_p update_idx update_l update_r update_a update_b update_val]
  (binding [update_flag nil update_lazy nil update_lv nil update_mid nil update_rv nil update_segment_tree nil] (try (do (set! update_segment_tree update_segment_tree_p) (set! update_lazy update_lazy_p) (set! update_flag update_flag_p) (when (nth update_flag update_idx) (do (set! update_segment_tree (assoc update_segment_tree update_idx (nth update_lazy update_idx))) (set! update_flag (assoc update_flag update_idx false)) (when (not= update_l update_r) (do (set! update_lazy (assoc update_lazy (left update_idx) (nth update_lazy update_idx))) (set! update_lazy (assoc update_lazy (right update_idx) (nth update_lazy update_idx))) (set! update_flag (assoc update_flag (left update_idx) true)) (set! update_flag (assoc update_flag (right update_idx) true)))))) (when (or (< update_r update_a) (> update_l update_b)) (throw (ex-info "return" {:v nil}))) (when (and (>= update_l update_a) (<= update_r update_b)) (do (set! update_segment_tree (assoc update_segment_tree update_idx update_val)) (when (not= update_l update_r) (do (set! update_lazy (assoc update_lazy (left update_idx) update_val)) (set! update_lazy (assoc update_lazy (right update_idx) update_val)) (set! update_flag (assoc update_flag (left update_idx) true)) (set! update_flag (assoc update_flag (right update_idx) true)))) (throw (ex-info "return" {:v nil})))) (set! update_mid (quot (+ update_l update_r) 2)) (update update_segment_tree update_lazy update_flag (left update_idx) update_l update_mid update_a update_b update_val) (update update_segment_tree update_lazy update_flag (right update_idx) (+ update_mid 1) update_r update_a update_b update_val) (set! update_lv (nth update_segment_tree (left update_idx))) (set! update_rv (nth update_segment_tree (right update_idx))) (if (> update_lv update_rv) (set! update_segment_tree (assoc update_segment_tree update_idx update_lv)) (set! update_segment_tree (assoc update_segment_tree update_idx update_rv)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_NEG_INF (- 1000000000))

(defn query [query_segment_tree_p query_lazy_p query_flag_p query_idx query_l query_r query_a query_b]
  (binding [query_flag nil query_lazy nil query_mid nil query_q1 nil query_q2 nil query_segment_tree nil] (try (do (set! query_segment_tree query_segment_tree_p) (set! query_lazy query_lazy_p) (set! query_flag query_flag_p) (when (nth query_flag query_idx) (do (set! query_segment_tree (assoc query_segment_tree query_idx (nth query_lazy query_idx))) (set! query_flag (assoc query_flag query_idx false)) (when (not= query_l query_r) (do (set! query_lazy (assoc query_lazy (left query_idx) (nth query_lazy query_idx))) (set! query_lazy (assoc query_lazy (right query_idx) (nth query_lazy query_idx))) (set! query_flag (assoc query_flag (left query_idx) true)) (set! query_flag (assoc query_flag (right query_idx) true)))))) (when (or (< query_r query_a) (> query_l query_b)) (throw (ex-info "return" {:v main_NEG_INF}))) (when (and (>= query_l query_a) (<= query_r query_b)) (throw (ex-info "return" {:v (nth query_segment_tree query_idx)}))) (set! query_mid (quot (+ query_l query_r) 2)) (set! query_q1 (query query_segment_tree query_lazy query_flag (left query_idx) query_l query_mid query_a query_b)) (set! query_q2 (query query_segment_tree query_lazy query_flag (right query_idx) (+ query_mid 1) query_r query_a query_b)) (if (> query_q1 query_q2) (throw (ex-info "return" {:v query_q1})) (throw (ex-info "return" {:v query_q2})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn segtree_to_string [segtree_to_string_segment_tree segtree_to_string_lazy segtree_to_string_flag segtree_to_string_n]
  (binding [segtree_to_string_i nil segtree_to_string_res nil segtree_to_string_v nil] (try (do (set! segtree_to_string_res "[") (set! segtree_to_string_i 1) (while (<= segtree_to_string_i segtree_to_string_n) (do (set! segtree_to_string_v (query segtree_to_string_segment_tree segtree_to_string_lazy segtree_to_string_flag 1 1 segtree_to_string_n segtree_to_string_i segtree_to_string_i)) (set! segtree_to_string_res (str segtree_to_string_res (str segtree_to_string_v))) (when (< segtree_to_string_i segtree_to_string_n) (set! segtree_to_string_res (str segtree_to_string_res ", "))) (set! segtree_to_string_i (+ segtree_to_string_i 1)))) (set! segtree_to_string_res (str segtree_to_string_res "]")) (throw (ex-info "return" {:v segtree_to_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_A [1 2 (- 4) 7 3 (- 5) 6 11 (- 20) 9 14 15 5 2 (- 8)])

(def ^:dynamic main_n 15)

(def ^:dynamic main_segment_tree (init_int_array main_n))

(def ^:dynamic main_lazy (init_int_array main_n))

(def ^:dynamic main_flag (init_bool_array main_n))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (build main_segment_tree 1 1 main_n main_A)
      (println (query main_segment_tree main_lazy main_flag 1 1 main_n 4 6))
      (println (query main_segment_tree main_lazy main_flag 1 1 main_n 7 11))
      (println (query main_segment_tree main_lazy main_flag 1 1 main_n 7 12))
      (update main_segment_tree main_lazy main_flag 1 1 main_n 1 3 111)
      (println (query main_segment_tree main_lazy main_flag 1 1 main_n 1 15))
      (update main_segment_tree main_lazy main_flag 1 1 main_n 7 8 235)
      (println (segtree_to_string main_segment_tree main_lazy main_flag main_n))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
