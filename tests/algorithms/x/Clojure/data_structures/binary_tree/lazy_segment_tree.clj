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

(defn toi [s]
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare init_int_array init_bool_array left right build update query segtree_to_string)

(declare _read_file)

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
  (binding [build_segment_tree build_segment_tree_p build_lv nil build_mid nil build_rv nil] (do (try (if (= build_l build_r) (set! build_segment_tree (assoc build_segment_tree build_idx (nth build_a (- build_l 1)))) (do (set! build_mid (quot (+ build_l build_r) 2)) (let [__res (build build_segment_tree (left build_idx) build_l build_mid build_a)] (do (set! build_segment_tree build_segment_tree) __res)) (let [__res (build build_segment_tree (right build_idx) (+ build_mid 1) build_r build_a)] (do (set! build_segment_tree build_segment_tree) __res)) (set! build_lv (nth build_segment_tree (left build_idx))) (set! build_rv (nth build_segment_tree (right build_idx))) (if (> build_lv build_rv) (set! build_segment_tree (assoc build_segment_tree build_idx build_lv)) (set! build_segment_tree (assoc build_segment_tree build_idx build_rv))))) (finally (alter-var-root (var build_segment_tree) (constantly build_segment_tree)))) build_segment_tree)))

(defn update [update_segment_tree_p update_lazy_p update_flag_p update_idx update_l update_r update_a update_b update_val]
  (binding [update_segment_tree update_segment_tree_p update_lazy update_lazy_p update_flag update_flag_p update_lv nil update_mid nil update_rv nil] (try (do (when (nth update_flag update_idx) (do (set! update_segment_tree (assoc update_segment_tree update_idx (nth update_lazy update_idx))) (set! update_flag (assoc update_flag update_idx false)) (when (not= update_l update_r) (do (set! update_lazy (assoc update_lazy (left update_idx) (nth update_lazy update_idx))) (set! update_lazy (assoc update_lazy (right update_idx) (nth update_lazy update_idx))) (set! update_flag (assoc update_flag (left update_idx) true)) (set! update_flag (assoc update_flag (right update_idx) true)))))) (when (or (< update_r update_a) (> update_l update_b)) (throw (ex-info "return" {:v nil}))) (when (and (>= update_l update_a) (<= update_r update_b)) (do (set! update_segment_tree (assoc update_segment_tree update_idx update_val)) (when (not= update_l update_r) (do (set! update_lazy (assoc update_lazy (left update_idx) update_val)) (set! update_lazy (assoc update_lazy (right update_idx) update_val)) (set! update_flag (assoc update_flag (left update_idx) true)) (set! update_flag (assoc update_flag (right update_idx) true)))) (throw (ex-info "return" {:v nil})))) (set! update_mid (quot (+ update_l update_r) 2)) (let [__res (update update_segment_tree update_lazy update_flag (left update_idx) update_l update_mid update_a update_b update_val)] (do (set! update_segment_tree update_segment_tree) (set! update_lazy update_lazy) (set! update_flag update_flag) __res)) (let [__res (update update_segment_tree update_lazy update_flag (right update_idx) (+ update_mid 1) update_r update_a update_b update_val)] (do (set! update_segment_tree update_segment_tree) (set! update_lazy update_lazy) (set! update_flag update_flag) __res)) (set! update_lv (nth update_segment_tree (left update_idx))) (set! update_rv (nth update_segment_tree (right update_idx))) (if (> update_lv update_rv) (set! update_segment_tree (assoc update_segment_tree update_idx update_lv)) (set! update_segment_tree (assoc update_segment_tree update_idx update_rv)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var update_segment_tree) (constantly update_segment_tree)) (alter-var-root (var update_lazy) (constantly update_lazy)) (alter-var-root (var update_flag) (constantly update_flag))))))

(def ^:dynamic main_NEG_INF nil)

(defn query [query_segment_tree_p query_lazy_p query_flag_p query_idx query_l query_r query_a query_b]
  (binding [query_segment_tree query_segment_tree_p query_lazy query_lazy_p query_flag query_flag_p query_mid nil query_q1 nil query_q2 nil] (try (do (when (nth query_flag query_idx) (do (set! query_segment_tree (assoc query_segment_tree query_idx (nth query_lazy query_idx))) (set! query_flag (assoc query_flag query_idx false)) (when (not= query_l query_r) (do (set! query_lazy (assoc query_lazy (left query_idx) (nth query_lazy query_idx))) (set! query_lazy (assoc query_lazy (right query_idx) (nth query_lazy query_idx))) (set! query_flag (assoc query_flag (left query_idx) true)) (set! query_flag (assoc query_flag (right query_idx) true)))))) (when (or (< query_r query_a) (> query_l query_b)) (throw (ex-info "return" {:v main_NEG_INF}))) (when (and (>= query_l query_a) (<= query_r query_b)) (throw (ex-info "return" {:v (nth query_segment_tree query_idx)}))) (set! query_mid (quot (+ query_l query_r) 2)) (set! query_q1 (let [__res (query query_segment_tree query_lazy query_flag (left query_idx) query_l query_mid query_a query_b)] (do (set! query_segment_tree query_segment_tree) (set! query_lazy query_lazy) (set! query_flag query_flag) __res))) (set! query_q2 (let [__res (query query_segment_tree query_lazy query_flag (right query_idx) (+ query_mid 1) query_r query_a query_b)] (do (set! query_segment_tree query_segment_tree) (set! query_lazy query_lazy) (set! query_flag query_flag) __res))) (if (> query_q1 query_q2) (throw (ex-info "return" {:v query_q1})) (throw (ex-info "return" {:v query_q2})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var query_segment_tree) (constantly query_segment_tree)) (alter-var-root (var query_lazy) (constantly query_lazy)) (alter-var-root (var query_flag) (constantly query_flag))))))

(defn segtree_to_string [segtree_to_string_segment_tree segtree_to_string_lazy segtree_to_string_flag segtree_to_string_n]
  (binding [segtree_to_string_i nil segtree_to_string_res nil segtree_to_string_v nil] (try (do (set! segtree_to_string_res "[") (set! segtree_to_string_i 1) (while (<= segtree_to_string_i segtree_to_string_n) (do (set! segtree_to_string_v (let [__res (query segtree_to_string_segment_tree segtree_to_string_lazy segtree_to_string_flag 1 1 segtree_to_string_n segtree_to_string_i segtree_to_string_i)] (do (set! segtree_to_string_segment_tree query_segment_tree) (set! segtree_to_string_lazy query_lazy) (set! segtree_to_string_flag query_flag) __res))) (set! segtree_to_string_res (str segtree_to_string_res (mochi_str segtree_to_string_v))) (when (< segtree_to_string_i segtree_to_string_n) (set! segtree_to_string_res (str segtree_to_string_res ", "))) (set! segtree_to_string_i (+ segtree_to_string_i 1)))) (set! segtree_to_string_res (str segtree_to_string_res "]")) (throw (ex-info "return" {:v segtree_to_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_A nil)

(def ^:dynamic main_n nil)

(def ^:dynamic main_segment_tree nil)

(def ^:dynamic main_lazy nil)

(def ^:dynamic main_flag nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_NEG_INF) (constantly (- 1000000000)))
      (alter-var-root (var main_A) (constantly [1 2 (- 4) 7 3 (- 5) 6 11 (- 20) 9 14 15 5 2 (- 8)]))
      (alter-var-root (var main_n) (constantly 15))
      (alter-var-root (var main_segment_tree) (constantly (init_int_array main_n)))
      (alter-var-root (var main_lazy) (constantly (init_int_array main_n)))
      (alter-var-root (var main_flag) (constantly (init_bool_array main_n)))
      (let [__res (build main_segment_tree 1 1 main_n main_A)] (do (alter-var-root (var main_segment_tree) (constantly build_segment_tree)) __res))
      (println (let [__res (query main_segment_tree main_lazy main_flag 1 1 main_n 4 6)] (do (alter-var-root (var main_segment_tree) (constantly query_segment_tree)) (alter-var-root (var main_lazy) (constantly query_lazy)) (alter-var-root (var main_flag) (constantly query_flag)) __res)))
      (println (let [__res (query main_segment_tree main_lazy main_flag 1 1 main_n 7 11)] (do (alter-var-root (var main_segment_tree) (constantly query_segment_tree)) (alter-var-root (var main_lazy) (constantly query_lazy)) (alter-var-root (var main_flag) (constantly query_flag)) __res)))
      (println (let [__res (query main_segment_tree main_lazy main_flag 1 1 main_n 7 12)] (do (alter-var-root (var main_segment_tree) (constantly query_segment_tree)) (alter-var-root (var main_lazy) (constantly query_lazy)) (alter-var-root (var main_flag) (constantly query_flag)) __res)))
      (let [__res (update main_segment_tree main_lazy main_flag 1 1 main_n 1 3 111)] (do (alter-var-root (var main_segment_tree) (constantly update_segment_tree)) (alter-var-root (var main_lazy) (constantly update_lazy)) (alter-var-root (var main_flag) (constantly update_flag)) __res))
      (println (let [__res (query main_segment_tree main_lazy main_flag 1 1 main_n 1 15)] (do (alter-var-root (var main_segment_tree) (constantly query_segment_tree)) (alter-var-root (var main_lazy) (constantly query_lazy)) (alter-var-root (var main_flag) (constantly query_flag)) __res)))
      (let [__res (update main_segment_tree main_lazy main_flag 1 1 main_n 7 8 235)] (do (alter-var-root (var main_segment_tree) (constantly update_segment_tree)) (alter-var-root (var main_lazy) (constantly update_lazy)) (alter-var-root (var main_flag) (constantly update_flag)) __res))
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
