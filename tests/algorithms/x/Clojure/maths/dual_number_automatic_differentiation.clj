(ns main (:refer-clojure :exclude [make_dual dual_from_list dual_add dual_add_real dual_mul dual_mul_real dual_pow factorial differentiate test_differentiate main]))

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

(declare make_dual dual_from_list dual_add dual_add_real dual_mul dual_mul_real dual_pow factorial differentiate test_differentiate main)

(def ^:dynamic differentiate_d nil)

(def ^:dynamic differentiate_result nil)

(def ^:dynamic dual_add_diff nil)

(def ^:dynamic dual_add_diff2 nil)

(def ^:dynamic dual_add_i nil)

(def ^:dynamic dual_add_idx nil)

(def ^:dynamic dual_add_j nil)

(def ^:dynamic dual_add_k nil)

(def ^:dynamic dual_add_k2 nil)

(def ^:dynamic dual_add_new_duals nil)

(def ^:dynamic dual_add_o_dual nil)

(def ^:dynamic dual_add_real_ds nil)

(def ^:dynamic dual_add_real_i nil)

(def ^:dynamic dual_add_s_dual nil)

(def ^:dynamic dual_mul_i nil)

(def ^:dynamic dual_mul_idx nil)

(def ^:dynamic dual_mul_j nil)

(def ^:dynamic dual_mul_k nil)

(def ^:dynamic dual_mul_l nil)

(def ^:dynamic dual_mul_new_duals nil)

(def ^:dynamic dual_mul_new_len nil)

(def ^:dynamic dual_mul_pos nil)

(def ^:dynamic dual_mul_real_ds nil)

(def ^:dynamic dual_mul_real_i nil)

(def ^:dynamic dual_mul_val nil)

(def ^:dynamic dual_pow_i nil)

(def ^:dynamic dual_pow_res nil)

(def ^:dynamic factorial_i nil)

(def ^:dynamic factorial_res nil)

(def ^:dynamic main_res nil)

(def ^:dynamic make_dual_ds nil)

(def ^:dynamic make_dual_i nil)

(defn make_dual [make_dual_real make_dual_rank]
  (binding [make_dual_ds nil make_dual_i nil] (try (do (set! make_dual_ds []) (set! make_dual_i 0) (while (< make_dual_i make_dual_rank) (do (set! make_dual_ds (conj make_dual_ds 1.0)) (set! make_dual_i (+ make_dual_i 1)))) (throw (ex-info "return" {:v {:duals make_dual_ds :real make_dual_real}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dual_from_list [dual_from_list_real dual_from_list_ds]
  (try (throw (ex-info "return" {:v {:duals dual_from_list_ds :real dual_from_list_real}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dual_add [dual_add_a dual_add_b]
  (binding [dual_add_diff nil dual_add_diff2 nil dual_add_i nil dual_add_idx nil dual_add_j nil dual_add_k nil dual_add_k2 nil dual_add_new_duals nil dual_add_o_dual nil dual_add_s_dual nil] (try (do (set! dual_add_s_dual []) (set! dual_add_i 0) (while (< dual_add_i (count (:duals dual_add_a))) (do (set! dual_add_s_dual (conj dual_add_s_dual (get (:duals dual_add_a) dual_add_i))) (set! dual_add_i (+ dual_add_i 1)))) (set! dual_add_o_dual []) (set! dual_add_j 0) (while (< dual_add_j (count (:duals dual_add_b))) (do (set! dual_add_o_dual (conj dual_add_o_dual (get (:duals dual_add_b) dual_add_j))) (set! dual_add_j (+ dual_add_j 1)))) (if (> (count dual_add_s_dual) (count dual_add_o_dual)) (do (set! dual_add_diff (- (count dual_add_s_dual) (count dual_add_o_dual))) (set! dual_add_k 0) (while (< dual_add_k dual_add_diff) (do (set! dual_add_o_dual (conj dual_add_o_dual 1.0)) (set! dual_add_k (+ dual_add_k 1))))) (when (< (count dual_add_s_dual) (count dual_add_o_dual)) (do (set! dual_add_diff2 (- (count dual_add_o_dual) (count dual_add_s_dual))) (set! dual_add_k2 0) (while (< dual_add_k2 dual_add_diff2) (do (set! dual_add_s_dual (conj dual_add_s_dual 1.0)) (set! dual_add_k2 (+ dual_add_k2 1))))))) (set! dual_add_new_duals []) (set! dual_add_idx 0) (while (< dual_add_idx (count dual_add_s_dual)) (do (set! dual_add_new_duals (conj dual_add_new_duals (+ (nth dual_add_s_dual dual_add_idx) (nth dual_add_o_dual dual_add_idx)))) (set! dual_add_idx (+ dual_add_idx 1)))) (throw (ex-info "return" {:v {:duals dual_add_new_duals :real (+ (:real dual_add_a) (:real dual_add_b))}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dual_add_real [dual_add_real_a dual_add_real_b]
  (binding [dual_add_real_ds nil dual_add_real_i nil] (try (do (set! dual_add_real_ds []) (set! dual_add_real_i 0) (while (< dual_add_real_i (count (:duals dual_add_real_a))) (do (set! dual_add_real_ds (conj dual_add_real_ds (get (:duals dual_add_real_a) dual_add_real_i))) (set! dual_add_real_i (+ dual_add_real_i 1)))) (throw (ex-info "return" {:v {:duals dual_add_real_ds :real (+ (:real dual_add_real_a) dual_add_real_b)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dual_mul [dual_mul_a dual_mul_b]
  (binding [dual_mul_i nil dual_mul_idx nil dual_mul_j nil dual_mul_k nil dual_mul_l nil dual_mul_new_duals nil dual_mul_new_len nil dual_mul_pos nil dual_mul_val nil] (try (do (set! dual_mul_new_len (+ (+ (count (:duals dual_mul_a)) (count (:duals dual_mul_b))) 1)) (set! dual_mul_new_duals []) (set! dual_mul_idx 0) (while (< dual_mul_idx dual_mul_new_len) (do (set! dual_mul_new_duals (conj dual_mul_new_duals 0.0)) (set! dual_mul_idx (+ dual_mul_idx 1)))) (set! dual_mul_i 0) (while (< dual_mul_i (count (:duals dual_mul_a))) (do (set! dual_mul_j 0) (while (< dual_mul_j (count (:duals dual_mul_b))) (do (set! dual_mul_pos (+ (+ dual_mul_i dual_mul_j) 1)) (set! dual_mul_val (+ (nth dual_mul_new_duals dual_mul_pos) (* (get (:duals dual_mul_a) dual_mul_i) (get (:duals dual_mul_b) dual_mul_j)))) (set! dual_mul_new_duals (assoc dual_mul_new_duals dual_mul_pos dual_mul_val)) (set! dual_mul_j (+ dual_mul_j 1)))) (set! dual_mul_i (+ dual_mul_i 1)))) (set! dual_mul_k 0) (while (< dual_mul_k (count (:duals dual_mul_a))) (do (set! dual_mul_val (+ (nth dual_mul_new_duals dual_mul_k) (* (get (:duals dual_mul_a) dual_mul_k) (:real dual_mul_b)))) (set! dual_mul_new_duals (assoc dual_mul_new_duals dual_mul_k dual_mul_val)) (set! dual_mul_k (+ dual_mul_k 1)))) (set! dual_mul_l 0) (while (< dual_mul_l (count (:duals dual_mul_b))) (do (set! dual_mul_val (+ (nth dual_mul_new_duals dual_mul_l) (* (get (:duals dual_mul_b) dual_mul_l) (:real dual_mul_a)))) (set! dual_mul_new_duals (assoc dual_mul_new_duals dual_mul_l dual_mul_val)) (set! dual_mul_l (+ dual_mul_l 1)))) (throw (ex-info "return" {:v {:duals dual_mul_new_duals :real (* (:real dual_mul_a) (:real dual_mul_b))}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dual_mul_real [dual_mul_real_a dual_mul_real_b]
  (binding [dual_mul_real_ds nil dual_mul_real_i nil] (try (do (set! dual_mul_real_ds []) (set! dual_mul_real_i 0) (while (< dual_mul_real_i (count (:duals dual_mul_real_a))) (do (set! dual_mul_real_ds (conj dual_mul_real_ds (* (get (:duals dual_mul_real_a) dual_mul_real_i) dual_mul_real_b))) (set! dual_mul_real_i (+ dual_mul_real_i 1)))) (throw (ex-info "return" {:v {:duals dual_mul_real_ds :real (* (:real dual_mul_real_a) dual_mul_real_b)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dual_pow [dual_pow_x dual_pow_n]
  (binding [dual_pow_i nil dual_pow_res nil] (try (do (when (< dual_pow_n 0) (throw (Exception. "power must be a positive integer"))) (when (= dual_pow_n 0) (throw (ex-info "return" {:v {:duals [] :real 1.0}}))) (set! dual_pow_res dual_pow_x) (set! dual_pow_i 1) (while (< dual_pow_i dual_pow_n) (do (set! dual_pow_res (dual_mul dual_pow_res dual_pow_x)) (set! dual_pow_i (+ dual_pow_i 1)))) (throw (ex-info "return" {:v dual_pow_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn factorial [factorial_n]
  (binding [factorial_i nil factorial_res nil] (try (do (set! factorial_res 1.0) (set! factorial_i 2) (while (<= factorial_i factorial_n) (do (set! factorial_res (* factorial_res (double factorial_i))) (set! factorial_i (+ factorial_i 1)))) (throw (ex-info "return" {:v factorial_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn differentiate [differentiate_func differentiate_position differentiate_order]
  (binding [differentiate_d nil differentiate_result nil] (try (do (set! differentiate_d (make_dual differentiate_position 1)) (set! differentiate_result (differentiate_func differentiate_d)) (if (= differentiate_order 0) (:real differentiate_result) (* (get (:duals differentiate_result) (- differentiate_order 1)) (factorial differentiate_order)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn f1 [f1_x]
  (try (throw (ex-info "return" {:v (dual_pow f1_x 2)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn f2 [f2_x]
  (try (throw (ex-info "return" {:v (dual_mul (dual_pow f2_x 2) (dual_pow f2_x 4))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn f3 [f3_y]
  (try (throw (ex-info "return" {:v (dual_mul_real (dual_pow (dual_add_real f3_y 3.0) 6) 0.5)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn f4 [f4_y]
  (try (throw (ex-info "return" {:v (dual_pow f4_y 2)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_differentiate []
  (try (do (when (not= (differentiate f1 2.0 2) 2.0) (throw (Exception. "f1 failed"))) (when (not= (differentiate f2 9.0 2) 196830.0) (throw (Exception. "f2 failed"))) (when (not= (differentiate f3 3.5 4) 7605.0) (throw (Exception. "f3 failed"))) (when (not= (differentiate f4 4.0 3) 0.0) (throw (Exception. "f4 failed")))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn f [f_y]
  (try (throw (ex-info "return" {:v (dual_mul (dual_pow f_y 2) (dual_pow f_y 4))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_res nil] (try (do (test_differentiate) (set! main_res (differentiate f 9.0 2)) (println main_res)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

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
