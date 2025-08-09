(ns main (:refer-clojure :exclude [key joint_probability_distribution expectation variance covariance sqrtApprox standard_deviation main]))

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

(declare key joint_probability_distribution expectation variance covariance sqrtApprox standard_deviation main)

(def ^:dynamic covariance_diff_x nil)

(def ^:dynamic covariance_diff_y nil)

(def ^:dynamic covariance_i nil)

(def ^:dynamic covariance_j nil)

(def ^:dynamic covariance_mean_x nil)

(def ^:dynamic covariance_mean_y nil)

(def ^:dynamic covariance_total nil)

(def ^:dynamic expectation_i nil)

(def ^:dynamic expectation_total nil)

(def ^:dynamic joint_probability_distribution_i nil)

(def ^:dynamic joint_probability_distribution_j nil)

(def ^:dynamic joint_probability_distribution_k nil)

(def ^:dynamic joint_probability_distribution_result nil)

(def ^:dynamic main_cov nil)

(def ^:dynamic main_ex nil)

(def ^:dynamic main_ey nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_j nil)

(def ^:dynamic main_jpd nil)

(def ^:dynamic main_k nil)

(def ^:dynamic main_prob nil)

(def ^:dynamic main_vx nil)

(def ^:dynamic main_vy nil)

(def ^:dynamic main_x_probabilities nil)

(def ^:dynamic main_x_values nil)

(def ^:dynamic main_y_probabilities nil)

(def ^:dynamic main_y_values nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic variance_diff nil)

(def ^:dynamic variance_i nil)

(def ^:dynamic variance_mean nil)

(def ^:dynamic variance_total nil)

(defn key [key_x key_y]
  (try (throw (ex-info "return" {:v (str (str (str key_x) ",") (str key_y))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn joint_probability_distribution [joint_probability_distribution_x_values joint_probability_distribution_y_values joint_probability_distribution_x_probabilities joint_probability_distribution_y_probabilities]
  (binding [joint_probability_distribution_i nil joint_probability_distribution_j nil joint_probability_distribution_k nil joint_probability_distribution_result nil] (try (do (set! joint_probability_distribution_result {}) (set! joint_probability_distribution_i 0) (while (< joint_probability_distribution_i (count joint_probability_distribution_x_values)) (do (set! joint_probability_distribution_j 0) (while (< joint_probability_distribution_j (count joint_probability_distribution_y_values)) (do (set! joint_probability_distribution_k (key (nth joint_probability_distribution_x_values joint_probability_distribution_i) (nth joint_probability_distribution_y_values joint_probability_distribution_j))) (set! joint_probability_distribution_result (assoc joint_probability_distribution_result joint_probability_distribution_k (* (nth joint_probability_distribution_x_probabilities joint_probability_distribution_i) (nth joint_probability_distribution_y_probabilities joint_probability_distribution_j)))) (set! joint_probability_distribution_j (+ joint_probability_distribution_j 1)))) (set! joint_probability_distribution_i (+ joint_probability_distribution_i 1)))) (throw (ex-info "return" {:v joint_probability_distribution_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn expectation [expectation_values expectation_probabilities]
  (binding [expectation_i nil expectation_total nil] (try (do (set! expectation_total 0.0) (set! expectation_i 0) (while (< expectation_i (count expectation_values)) (do (set! expectation_total (+ expectation_total (* (double (nth expectation_values expectation_i)) (nth expectation_probabilities expectation_i)))) (set! expectation_i (+ expectation_i 1)))) (throw (ex-info "return" {:v expectation_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn variance [variance_values variance_probabilities]
  (binding [variance_diff nil variance_i nil variance_mean nil variance_total nil] (try (do (set! variance_mean (expectation variance_values variance_probabilities)) (set! variance_total 0.0) (set! variance_i 0) (while (< variance_i (count variance_values)) (do (set! variance_diff (- (double (nth variance_values variance_i)) variance_mean)) (set! variance_total (+ variance_total (* (* variance_diff variance_diff) (nth variance_probabilities variance_i)))) (set! variance_i (+ variance_i 1)))) (throw (ex-info "return" {:v variance_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn covariance [covariance_x_values covariance_y_values covariance_x_probabilities covariance_y_probabilities]
  (binding [covariance_diff_x nil covariance_diff_y nil covariance_i nil covariance_j nil covariance_mean_x nil covariance_mean_y nil covariance_total nil] (try (do (set! covariance_mean_x (expectation covariance_x_values covariance_x_probabilities)) (set! covariance_mean_y (expectation covariance_y_values covariance_y_probabilities)) (set! covariance_total 0.0) (set! covariance_i 0) (while (< covariance_i (count covariance_x_values)) (do (set! covariance_j 0) (while (< covariance_j (count covariance_y_values)) (do (set! covariance_diff_x (- (double (nth covariance_x_values covariance_i)) covariance_mean_x)) (set! covariance_diff_y (- (double (nth covariance_y_values covariance_j)) covariance_mean_y)) (set! covariance_total (+ covariance_total (* (* (* covariance_diff_x covariance_diff_y) (nth covariance_x_probabilities covariance_i)) (nth covariance_y_probabilities covariance_j)))) (set! covariance_j (+ covariance_j 1)))) (set! covariance_i (+ covariance_i 1)))) (throw (ex-info "return" {:v covariance_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (when (<= sqrtApprox_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn standard_deviation [standard_deviation_v]
  (try (throw (ex-info "return" {:v (sqrtApprox standard_deviation_v)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_cov nil main_ex nil main_ey nil main_i nil main_j nil main_jpd nil main_k nil main_prob nil main_vx nil main_vy nil main_x_probabilities nil main_x_values nil main_y_probabilities nil main_y_values nil] (do (set! main_x_values [1 2]) (set! main_y_values [(- 2) 5 8]) (set! main_x_probabilities [0.7 0.3]) (set! main_y_probabilities [0.3 0.5 0.2]) (set! main_jpd (joint_probability_distribution main_x_values main_y_values main_x_probabilities main_y_probabilities)) (set! main_i 0) (while (< main_i (count main_x_values)) (do (set! main_j 0) (while (< main_j (count main_y_values)) (do (set! main_k (key (nth main_x_values main_i) (nth main_y_values main_j))) (set! main_prob (get main_jpd main_k)) (println (str (str main_k "=") (str main_prob))) (set! main_j (+ main_j 1)))) (set! main_i (+ main_i 1)))) (set! main_ex (expectation main_x_values main_x_probabilities)) (set! main_ey (expectation main_y_values main_y_probabilities)) (set! main_vx (variance main_x_values main_x_probabilities)) (set! main_vy (variance main_y_values main_y_probabilities)) (set! main_cov (covariance main_x_values main_y_values main_x_probabilities main_y_probabilities)) (println (str "Ex=" (str main_ex))) (println (str "Ey=" (str main_ey))) (println (str "Vx=" (str main_vx))) (println (str "Vy=" (str main_vy))) (println (str "Cov=" (str main_cov))) (println (str "Sx=" (str (standard_deviation main_vx)))) (println (str "Sy=" (str (standard_deviation main_vy)))))))

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
