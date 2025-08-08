(ns main (:refer-clojure :exclude [calc_profit test_sorted test_negative_max_weight test_negative_profit_value test_negative_weight_value test_null_max_weight test_unequal_list_length]))

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

(declare calc_profit test_sorted test_negative_max_weight test_negative_profit_value test_negative_weight_value test_null_max_weight test_unequal_list_length)

(def ^:dynamic calc_profit_gain nil)

(def ^:dynamic calc_profit_i nil)

(def ^:dynamic calc_profit_idx nil)

(def ^:dynamic calc_profit_j nil)

(def ^:dynamic calc_profit_k nil)

(def ^:dynamic calc_profit_limit nil)

(def ^:dynamic calc_profit_max_ratio nil)

(def ^:dynamic calc_profit_ratio nil)

(def ^:dynamic calc_profit_used nil)

(def ^:dynamic test_negative_max_weight_profit nil)

(def ^:dynamic test_negative_max_weight_res nil)

(def ^:dynamic test_negative_max_weight_weight nil)

(def ^:dynamic test_negative_profit_value_profit nil)

(def ^:dynamic test_negative_profit_value_res nil)

(def ^:dynamic test_negative_profit_value_weight nil)

(def ^:dynamic test_negative_weight_value_profit nil)

(def ^:dynamic test_negative_weight_value_res nil)

(def ^:dynamic test_negative_weight_value_weight nil)

(def ^:dynamic test_null_max_weight_profit nil)

(def ^:dynamic test_null_max_weight_res nil)

(def ^:dynamic test_null_max_weight_weight nil)

(def ^:dynamic test_sorted_profit nil)

(def ^:dynamic test_sorted_res nil)

(def ^:dynamic test_sorted_weight nil)

(def ^:dynamic test_unequal_list_length_profit nil)

(def ^:dynamic test_unequal_list_length_res nil)

(def ^:dynamic test_unequal_list_length_weight nil)

(defn calc_profit [calc_profit_profit calc_profit_weight calc_profit_max_weight]
  (binding [calc_profit_gain nil calc_profit_i nil calc_profit_idx nil calc_profit_j nil calc_profit_k nil calc_profit_limit nil calc_profit_max_ratio nil calc_profit_ratio nil calc_profit_used nil] (try (do (when (not= (count calc_profit_profit) (count calc_profit_weight)) (throw (ex-info "return" {:v {:error "The length of profit and weight must be same." :ok false :value 0.0}}))) (when (<= calc_profit_max_weight 0) (throw (ex-info "return" {:v {:error "max_weight must greater than zero." :ok false :value 0.0}}))) (set! calc_profit_i 0) (while (< calc_profit_i (count calc_profit_profit)) (do (when (< (nth calc_profit_profit calc_profit_i) 0) (throw (ex-info "return" {:v {:error "Profit can not be negative." :ok false :value 0.0}}))) (when (< (nth calc_profit_weight calc_profit_i) 0) (throw (ex-info "return" {:v {:error "Weight can not be negative." :ok false :value 0.0}}))) (set! calc_profit_i (+ calc_profit_i 1)))) (set! calc_profit_used []) (set! calc_profit_j 0) (while (< calc_profit_j (count calc_profit_profit)) (do (set! calc_profit_used (conj calc_profit_used false)) (set! calc_profit_j (+ calc_profit_j 1)))) (set! calc_profit_limit 0) (set! calc_profit_gain 0.0) (loop [while_flag_1 true] (when (and while_flag_1 (< calc_profit_limit calc_profit_max_weight)) (do (set! calc_profit_max_ratio (- 1.0)) (set! calc_profit_idx (- 0 1)) (set! calc_profit_k 0) (while (< calc_profit_k (count calc_profit_profit)) (do (when (not (nth calc_profit_used calc_profit_k)) (do (set! calc_profit_ratio (quot (double (nth calc_profit_profit calc_profit_k)) (double (nth calc_profit_weight calc_profit_k)))) (when (> calc_profit_ratio calc_profit_max_ratio) (do (set! calc_profit_max_ratio calc_profit_ratio) (set! calc_profit_idx calc_profit_k))))) (set! calc_profit_k (+ calc_profit_k 1)))) (cond (= calc_profit_idx (- 0 1)) (recur false) :else (do (set! calc_profit_used (assoc calc_profit_used calc_profit_idx true)) (if (>= (- calc_profit_max_weight calc_profit_limit) (nth calc_profit_weight calc_profit_idx)) (do (set! calc_profit_limit (+ calc_profit_limit (nth calc_profit_weight calc_profit_idx))) (set! calc_profit_gain (+ calc_profit_gain (double (nth calc_profit_profit calc_profit_idx))))) (do (set! calc_profit_gain (+ calc_profit_gain (* (quot (double (- calc_profit_max_weight calc_profit_limit)) (double (nth calc_profit_weight calc_profit_idx))) (double (nth calc_profit_profit calc_profit_idx))))) (recur false))) (recur while_flag_1)))))) (throw (ex-info "return" {:v {:error "" :ok true :value calc_profit_gain}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_sorted []
  (binding [test_sorted_profit nil test_sorted_res nil test_sorted_weight nil] (try (do (set! test_sorted_profit [10 20 30 40 50 60]) (set! test_sorted_weight [2 4 6 8 10 12]) (set! test_sorted_res (calc_profit test_sorted_profit test_sorted_weight 100)) (throw (ex-info "return" {:v (and (:ok test_sorted_res) (= (:value test_sorted_res) 210.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_negative_max_weight []
  (binding [test_negative_max_weight_profit nil test_negative_max_weight_res nil test_negative_max_weight_weight nil] (try (do (set! test_negative_max_weight_profit [10 20 30 40 50 60]) (set! test_negative_max_weight_weight [2 4 6 8 10 12]) (set! test_negative_max_weight_res (calc_profit test_negative_max_weight_profit test_negative_max_weight_weight (- 15))) (throw (ex-info "return" {:v (and (not (:ok test_negative_max_weight_res)) (= (:error test_negative_max_weight_res) "max_weight must greater than zero."))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_negative_profit_value []
  (binding [test_negative_profit_value_profit nil test_negative_profit_value_res nil test_negative_profit_value_weight nil] (try (do (set! test_negative_profit_value_profit [10 (- 20) 30 40 50 60]) (set! test_negative_profit_value_weight [2 4 6 8 10 12]) (set! test_negative_profit_value_res (calc_profit test_negative_profit_value_profit test_negative_profit_value_weight 15)) (throw (ex-info "return" {:v (and (not (:ok test_negative_profit_value_res)) (= (:error test_negative_profit_value_res) "Profit can not be negative."))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_negative_weight_value []
  (binding [test_negative_weight_value_profit nil test_negative_weight_value_res nil test_negative_weight_value_weight nil] (try (do (set! test_negative_weight_value_profit [10 20 30 40 50 60]) (set! test_negative_weight_value_weight [2 (- 4) 6 (- 8) 10 12]) (set! test_negative_weight_value_res (calc_profit test_negative_weight_value_profit test_negative_weight_value_weight 15)) (throw (ex-info "return" {:v (and (not (:ok test_negative_weight_value_res)) (= (:error test_negative_weight_value_res) "Weight can not be negative."))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_null_max_weight []
  (binding [test_null_max_weight_profit nil test_null_max_weight_res nil test_null_max_weight_weight nil] (try (do (set! test_null_max_weight_profit [10 20 30 40 50 60]) (set! test_null_max_weight_weight [2 4 6 8 10 12]) (set! test_null_max_weight_res (calc_profit test_null_max_weight_profit test_null_max_weight_weight 0)) (throw (ex-info "return" {:v (and (not (:ok test_null_max_weight_res)) (= (:error test_null_max_weight_res) "max_weight must greater than zero."))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_unequal_list_length []
  (binding [test_unequal_list_length_profit nil test_unequal_list_length_res nil test_unequal_list_length_weight nil] (try (do (set! test_unequal_list_length_profit [10 20 30 40 50]) (set! test_unequal_list_length_weight [2 4 6 8 10 12]) (set! test_unequal_list_length_res (calc_profit test_unequal_list_length_profit test_unequal_list_length_weight 100)) (throw (ex-info "return" {:v (and (not (:ok test_unequal_list_length_res)) (= (:error test_unequal_list_length_res) "The length of profit and weight must be same."))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (test_sorted))
      (println (test_negative_max_weight))
      (println (test_negative_profit_value))
      (println (test_negative_weight_value))
      (println (test_null_max_weight))
      (println (test_unequal_list_length))
      (println true)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
