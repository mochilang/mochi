(ns main (:refer-clojure :exclude [fenwick_from_list fenwick_empty fenwick_get_array bit_and low_bit fenwick_next fenwick_prev fenwick_add fenwick_update fenwick_prefix fenwick_query fenwick_get fenwick_rank_query]))

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

(declare fenwick_from_list fenwick_empty fenwick_get_array bit_and low_bit fenwick_next fenwick_prev fenwick_add fenwick_update fenwick_prefix fenwick_query fenwick_get fenwick_rank_query)

(def ^:dynamic bit_and_bit nil)

(def ^:dynamic bit_and_res nil)

(def ^:dynamic bit_and_ua nil)

(def ^:dynamic bit_and_ub nil)

(def ^:dynamic fenwick_add_i nil)

(def ^:dynamic fenwick_add_tree nil)

(def ^:dynamic fenwick_empty_i nil)

(def ^:dynamic fenwick_empty_tree nil)

(def ^:dynamic fenwick_from_list_i nil)

(def ^:dynamic fenwick_from_list_j nil)

(def ^:dynamic fenwick_from_list_size nil)

(def ^:dynamic fenwick_from_list_tree nil)

(def ^:dynamic fenwick_get_array_arr nil)

(def ^:dynamic fenwick_get_array_i nil)

(def ^:dynamic fenwick_get_array_j nil)

(def ^:dynamic fenwick_prefix_r nil)

(def ^:dynamic fenwick_prefix_result nil)

(def ^:dynamic fenwick_rank_query_i nil)

(def ^:dynamic fenwick_rank_query_j nil)

(def ^:dynamic fenwick_rank_query_jj nil)

(def ^:dynamic fenwick_rank_query_v nil)

(def ^:dynamic fenwick_update_current nil)

(def ^:dynamic main_f nil)

(defn fenwick_from_list [fenwick_from_list_arr]
  (binding [fenwick_from_list_i nil fenwick_from_list_j nil fenwick_from_list_size nil fenwick_from_list_tree nil] (try (do (set! fenwick_from_list_size (count fenwick_from_list_arr)) (set! fenwick_from_list_tree []) (set! fenwick_from_list_i 0) (while (< fenwick_from_list_i fenwick_from_list_size) (do (set! fenwick_from_list_tree (conj fenwick_from_list_tree (nth fenwick_from_list_arr fenwick_from_list_i))) (set! fenwick_from_list_i (+ fenwick_from_list_i 1)))) (set! fenwick_from_list_i 1) (while (< fenwick_from_list_i fenwick_from_list_size) (do (set! fenwick_from_list_j (fenwick_next fenwick_from_list_i)) (when (< fenwick_from_list_j fenwick_from_list_size) (set! fenwick_from_list_tree (assoc fenwick_from_list_tree fenwick_from_list_j (+ (nth fenwick_from_list_tree fenwick_from_list_j) (nth fenwick_from_list_tree fenwick_from_list_i))))) (set! fenwick_from_list_i (+ fenwick_from_list_i 1)))) (throw (ex-info "return" {:v {:size fenwick_from_list_size :tree fenwick_from_list_tree}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fenwick_empty [fenwick_empty_size]
  (binding [fenwick_empty_i nil fenwick_empty_tree nil] (try (do (set! fenwick_empty_tree []) (set! fenwick_empty_i 0) (while (< fenwick_empty_i fenwick_empty_size) (do (set! fenwick_empty_tree (conj fenwick_empty_tree 0)) (set! fenwick_empty_i (+ fenwick_empty_i 1)))) (throw (ex-info "return" {:v {:size fenwick_empty_size :tree fenwick_empty_tree}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fenwick_get_array [fenwick_get_array_f]
  (binding [fenwick_get_array_arr nil fenwick_get_array_i nil fenwick_get_array_j nil] (try (do (set! fenwick_get_array_arr []) (set! fenwick_get_array_i 0) (while (< fenwick_get_array_i (:size fenwick_get_array_f)) (do (set! fenwick_get_array_arr (conj fenwick_get_array_arr (get (:tree fenwick_get_array_f) fenwick_get_array_i))) (set! fenwick_get_array_i (+ fenwick_get_array_i 1)))) (set! fenwick_get_array_i (- (:size fenwick_get_array_f) 1)) (while (> fenwick_get_array_i 0) (do (set! fenwick_get_array_j (fenwick_next fenwick_get_array_i)) (when (< fenwick_get_array_j (:size fenwick_get_array_f)) (set! fenwick_get_array_arr (assoc fenwick_get_array_arr fenwick_get_array_j (- (nth fenwick_get_array_arr fenwick_get_array_j) (nth fenwick_get_array_arr fenwick_get_array_i))))) (set! fenwick_get_array_i (- fenwick_get_array_i 1)))) (throw (ex-info "return" {:v fenwick_get_array_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bit_and [bit_and_a bit_and_b]
  (binding [bit_and_bit nil bit_and_res nil bit_and_ua nil bit_and_ub nil] (try (do (set! bit_and_ua bit_and_a) (set! bit_and_ub bit_and_b) (set! bit_and_res 0) (set! bit_and_bit 1) (while (or (not= bit_and_ua 0) (not= bit_and_ub 0)) (do (when (and (= (mod bit_and_ua 2) 1) (= (mod bit_and_ub 2) 1)) (set! bit_and_res (+ bit_and_res bit_and_bit))) (set! bit_and_ua (long (quot bit_and_ua 2))) (set! bit_and_ub (long (quot bit_and_ub 2))) (set! bit_and_bit (* bit_and_bit 2)))) (throw (ex-info "return" {:v bit_and_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn low_bit [low_bit_x]
  (try (if (= low_bit_x 0) 0 (- low_bit_x (bit_and low_bit_x (- low_bit_x 1)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fenwick_next [fenwick_next_index]
  (try (throw (ex-info "return" {:v (+ fenwick_next_index (low_bit fenwick_next_index))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fenwick_prev [fenwick_prev_index]
  (try (throw (ex-info "return" {:v (- fenwick_prev_index (low_bit fenwick_prev_index))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fenwick_add [fenwick_add_f fenwick_add_index fenwick_add_value]
  (binding [fenwick_add_i nil fenwick_add_tree nil] (try (do (set! fenwick_add_tree (:tree fenwick_add_f)) (when (= fenwick_add_index 0) (do (set! fenwick_add_tree (assoc fenwick_add_tree 0 (+ (nth fenwick_add_tree 0) fenwick_add_value))) (throw (ex-info "return" {:v {:size (:size fenwick_add_f) :tree fenwick_add_tree}})))) (set! fenwick_add_i fenwick_add_index) (while (< fenwick_add_i (:size fenwick_add_f)) (do (set! fenwick_add_tree (assoc fenwick_add_tree fenwick_add_i (+ (nth fenwick_add_tree fenwick_add_i) fenwick_add_value))) (set! fenwick_add_i (fenwick_next fenwick_add_i)))) (throw (ex-info "return" {:v {:size (:size fenwick_add_f) :tree fenwick_add_tree}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fenwick_update [fenwick_update_f fenwick_update_index fenwick_update_value]
  (binding [fenwick_update_current nil] (try (do (set! fenwick_update_current (fenwick_get fenwick_update_f fenwick_update_index)) (throw (ex-info "return" {:v (fenwick_add fenwick_update_f fenwick_update_index (- fenwick_update_value fenwick_update_current))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fenwick_prefix [fenwick_prefix_f fenwick_prefix_right]
  (binding [fenwick_prefix_r nil fenwick_prefix_result nil] (try (do (when (= fenwick_prefix_right 0) (throw (ex-info "return" {:v 0}))) (set! fenwick_prefix_result (get (:tree fenwick_prefix_f) 0)) (set! fenwick_prefix_r (- fenwick_prefix_right 1)) (while (> fenwick_prefix_r 0) (do (set! fenwick_prefix_result (+ fenwick_prefix_result (get (:tree fenwick_prefix_f) fenwick_prefix_r))) (set! fenwick_prefix_r (fenwick_prev fenwick_prefix_r)))) (throw (ex-info "return" {:v fenwick_prefix_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn fenwick_query [fenwick_query_f fenwick_query_left fenwick_query_right]
  (try (throw (ex-info "return" {:v (- (fenwick_prefix fenwick_query_f fenwick_query_right) (fenwick_prefix fenwick_query_f fenwick_query_left))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fenwick_get [fenwick_get_f fenwick_get_index]
  (try (throw (ex-info "return" {:v (fenwick_query fenwick_get_f fenwick_get_index (+ fenwick_get_index 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fenwick_rank_query [fenwick_rank_query_f fenwick_rank_query_value]
  (binding [fenwick_rank_query_i nil fenwick_rank_query_j nil fenwick_rank_query_jj nil fenwick_rank_query_v nil] (try (do (set! fenwick_rank_query_v (- fenwick_rank_query_value (get (:tree fenwick_rank_query_f) 0))) (when (< fenwick_rank_query_v 0) (throw (ex-info "return" {:v (- 1)}))) (set! fenwick_rank_query_j 1) (while (< (* fenwick_rank_query_j 2) (:size fenwick_rank_query_f)) (set! fenwick_rank_query_j (* fenwick_rank_query_j 2))) (set! fenwick_rank_query_i 0) (set! fenwick_rank_query_jj fenwick_rank_query_j) (while (> fenwick_rank_query_jj 0) (do (when (and (< (+ fenwick_rank_query_i fenwick_rank_query_jj) (:size fenwick_rank_query_f)) (<= (get (:tree fenwick_rank_query_f) (+ fenwick_rank_query_i fenwick_rank_query_jj)) fenwick_rank_query_v)) (do (set! fenwick_rank_query_v (- fenwick_rank_query_v (get (:tree fenwick_rank_query_f) (+ fenwick_rank_query_i fenwick_rank_query_jj)))) (set! fenwick_rank_query_i (+ fenwick_rank_query_i fenwick_rank_query_jj)))) (set! fenwick_rank_query_jj (quot fenwick_rank_query_jj 2)))) (throw (ex-info "return" {:v fenwick_rank_query_i}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_f_base (fenwick_from_list [1 2 3 4 5]))

(def ^:dynamic main_f (fenwick_from_list [1 2 3 4 5]))

(def ^:dynamic main_f2 (fenwick_from_list [1 2 3 4 5]))

(def ^:dynamic main_f3 (fenwick_from_list [1 2 0 3 0 5]))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (fenwick_get_array main_f_base))
      (def main_f (fenwick_add main_f 0 1))
      (def main_f (fenwick_add main_f 1 2))
      (def main_f (fenwick_add main_f 2 3))
      (def main_f (fenwick_add main_f 3 4))
      (def main_f (fenwick_add main_f 4 5))
      (println (fenwick_get_array main_f))
      (println (fenwick_prefix main_f2 3))
      (println (fenwick_query main_f2 1 4))
      (println (fenwick_rank_query main_f3 0))
      (println (fenwick_rank_query main_f3 2))
      (println (fenwick_rank_query main_f3 1))
      (println (fenwick_rank_query main_f3 3))
      (println (fenwick_rank_query main_f3 5))
      (println (fenwick_rank_query main_f3 6))
      (println (fenwick_rank_query main_f3 11))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
