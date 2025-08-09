(ns main (:refer-clojure :exclude [assign_ranks calculate_spearman_rank_correlation test_spearman main]))

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

(declare assign_ranks calculate_spearman_rank_correlation test_spearman main)

(def ^:dynamic assign_ranks_i nil)

(def ^:dynamic assign_ranks_j nil)

(def ^:dynamic assign_ranks_n nil)

(def ^:dynamic assign_ranks_rank nil)

(def ^:dynamic assign_ranks_ranks nil)

(def ^:dynamic calculate_spearman_rank_correlation_d_sq nil)

(def ^:dynamic calculate_spearman_rank_correlation_diff nil)

(def ^:dynamic calculate_spearman_rank_correlation_i nil)

(def ^:dynamic calculate_spearman_rank_correlation_n nil)

(def ^:dynamic calculate_spearman_rank_correlation_n_f nil)

(def ^:dynamic calculate_spearman_rank_correlation_rank1 nil)

(def ^:dynamic calculate_spearman_rank_correlation_rank2 nil)

(def ^:dynamic test_spearman_x nil)

(def ^:dynamic test_spearman_y_dec nil)

(def ^:dynamic test_spearman_y_inc nil)

(def ^:dynamic test_spearman_y_mix nil)

(defn assign_ranks [assign_ranks_data]
  (binding [assign_ranks_i nil assign_ranks_j nil assign_ranks_n nil assign_ranks_rank nil assign_ranks_ranks nil] (try (do (set! assign_ranks_ranks []) (set! assign_ranks_n (count assign_ranks_data)) (set! assign_ranks_i 0) (while (< assign_ranks_i assign_ranks_n) (do (set! assign_ranks_rank 1) (set! assign_ranks_j 0) (while (< assign_ranks_j assign_ranks_n) (do (when (or (< (nth assign_ranks_data assign_ranks_j) (nth assign_ranks_data assign_ranks_i)) (and (= (nth assign_ranks_data assign_ranks_j) (nth assign_ranks_data assign_ranks_i)) (< assign_ranks_j assign_ranks_i))) (set! assign_ranks_rank (+ assign_ranks_rank 1))) (set! assign_ranks_j (+ assign_ranks_j 1)))) (set! assign_ranks_ranks (conj assign_ranks_ranks assign_ranks_rank)) (set! assign_ranks_i (+ assign_ranks_i 1)))) (throw (ex-info "return" {:v assign_ranks_ranks}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn calculate_spearman_rank_correlation [calculate_spearman_rank_correlation_var1 calculate_spearman_rank_correlation_var2]
  (binding [calculate_spearman_rank_correlation_d_sq nil calculate_spearman_rank_correlation_diff nil calculate_spearman_rank_correlation_i nil calculate_spearman_rank_correlation_n nil calculate_spearman_rank_correlation_n_f nil calculate_spearman_rank_correlation_rank1 nil calculate_spearman_rank_correlation_rank2 nil] (try (do (when (not= (count calculate_spearman_rank_correlation_var1) (count calculate_spearman_rank_correlation_var2)) (throw (Exception. "Lists must have equal length"))) (set! calculate_spearman_rank_correlation_n (count calculate_spearman_rank_correlation_var1)) (set! calculate_spearman_rank_correlation_rank1 (assign_ranks calculate_spearman_rank_correlation_var1)) (set! calculate_spearman_rank_correlation_rank2 (assign_ranks calculate_spearman_rank_correlation_var2)) (set! calculate_spearman_rank_correlation_i 0) (set! calculate_spearman_rank_correlation_d_sq 0.0) (while (< calculate_spearman_rank_correlation_i calculate_spearman_rank_correlation_n) (do (set! calculate_spearman_rank_correlation_diff (double (- (nth calculate_spearman_rank_correlation_rank1 calculate_spearman_rank_correlation_i) (nth calculate_spearman_rank_correlation_rank2 calculate_spearman_rank_correlation_i)))) (set! calculate_spearman_rank_correlation_d_sq (+ calculate_spearman_rank_correlation_d_sq (* calculate_spearman_rank_correlation_diff calculate_spearman_rank_correlation_diff))) (set! calculate_spearman_rank_correlation_i (+ calculate_spearman_rank_correlation_i 1)))) (set! calculate_spearman_rank_correlation_n_f (double calculate_spearman_rank_correlation_n)) (throw (ex-info "return" {:v (- 1.0 (/ (* 6.0 calculate_spearman_rank_correlation_d_sq) (* calculate_spearman_rank_correlation_n_f (- (* calculate_spearman_rank_correlation_n_f calculate_spearman_rank_correlation_n_f) 1.0))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_spearman []
  (binding [test_spearman_x nil test_spearman_y_dec nil test_spearman_y_inc nil test_spearman_y_mix nil] (do (set! test_spearman_x [1.0 2.0 3.0 4.0 5.0]) (set! test_spearman_y_inc [2.0 4.0 6.0 8.0 10.0]) (when (not= (calculate_spearman_rank_correlation test_spearman_x test_spearman_y_inc) 1.0) (throw (Exception. "case1"))) (set! test_spearman_y_dec [5.0 4.0 3.0 2.0 1.0]) (when (not= (calculate_spearman_rank_correlation test_spearman_x test_spearman_y_dec) (- 1.0)) (throw (Exception. "case2"))) (set! test_spearman_y_mix [5.0 1.0 2.0 9.0 5.0]) (when (not= (calculate_spearman_rank_correlation test_spearman_x test_spearman_y_mix) 0.6) (throw (Exception. "case3"))))))

(defn main []
  (do (test_spearman) (println (str (calculate_spearman_rank_correlation [1.0 2.0 3.0 4.0 5.0] [2.0 4.0 6.0 8.0 10.0]))) (println (str (calculate_spearman_rank_correlation [1.0 2.0 3.0 4.0 5.0] [5.0 4.0 3.0 2.0 1.0]))) (println (str (calculate_spearman_rank_correlation [1.0 2.0 3.0 4.0 5.0] [5.0 1.0 2.0 9.0 5.0])))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
