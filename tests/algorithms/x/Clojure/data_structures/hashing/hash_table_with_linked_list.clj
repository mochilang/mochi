(ns main (:refer-clojure :exclude [make_table hash_function prepend set_value count_empty balanced_factor collision_resolution insert main]))

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

(declare make_table hash_function prepend set_value count_empty balanced_factor collision_resolution insert main)

(def ^:dynamic balanced_factor_i nil)

(def ^:dynamic balanced_factor_total nil)

(def ^:dynamic collision_resolution_new_key nil)

(def ^:dynamic collision_resolution_steps nil)

(def ^:dynamic count_empty_i nil)

(def ^:dynamic count_v nil)

(def ^:dynamic hash_function_res nil)

(def ^:dynamic insert_dest nil)

(def ^:dynamic insert_key nil)

(def ^:dynamic main_ht nil)

(def ^:dynamic make_table_i nil)

(def ^:dynamic make_table_vals nil)

(def ^:dynamic prepend_i nil)

(def ^:dynamic prepend_result nil)

(def ^:dynamic set_value_current nil)

(def ^:dynamic set_value_ht nil)

(def ^:dynamic set_value_ks nil)

(def ^:dynamic set_value_updated nil)

(def ^:dynamic set_value_vals nil)

(defn make_table [make_table_size_table make_table_charge_factor]
  (binding [make_table_i nil make_table_vals nil] (try (do (set! make_table_vals []) (set! make_table_i 0) (while (< make_table_i make_table_size_table) (do (set! make_table_vals (conj make_table_vals [])) (set! make_table_i (+ make_table_i 1)))) (throw (ex-info "return" {:v {:charge_factor make_table_charge_factor :keys {} :size_table make_table_size_table :values make_table_vals}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hash_function [hash_function_ht hash_function_key]
  (binding [hash_function_res nil] (try (do (set! hash_function_res (mod hash_function_key (:size_table hash_function_ht))) (when (< hash_function_res 0) (set! hash_function_res (+ hash_function_res (:size_table hash_function_ht)))) (throw (ex-info "return" {:v hash_function_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn prepend [prepend_lst prepend_value]
  (binding [prepend_i nil prepend_result nil] (try (do (set! prepend_result [prepend_value]) (set! prepend_i 0) (while (< prepend_i (count prepend_lst)) (do (set! prepend_result (conj prepend_result (nth prepend_lst prepend_i))) (set! prepend_i (+ prepend_i 1)))) (throw (ex-info "return" {:v prepend_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn set_value [set_value_ht_p set_value_key set_value_data]
  (binding [set_value_current nil set_value_ht nil set_value_ks nil set_value_updated nil set_value_vals nil] (do (set! set_value_ht set_value_ht_p) (set! set_value_current (get (:values set_value_ht) set_value_key)) (set! set_value_updated (prepend set_value_current set_value_data)) (set! set_value_vals (:values set_value_ht)) (set! set_value_vals (assoc set_value_vals set_value_key set_value_updated)) (set! set_value_ht (assoc set_value_ht :values set_value_vals)) (set! set_value_ks (:keys set_value_ht)) (set! set_value_ks (assoc set_value_ks set_value_key set_value_updated)) (set! set_value_ht (assoc set_value_ht :keys set_value_ks)))))

(defn count_empty [count_empty_ht]
  (binding [count_empty_i nil count_v nil] (try (do (set! count_v 0) (set! count_empty_i 0) (while (< count_empty_i (count (:values count_empty_ht))) (do (when (= (count (get (:values count_empty_ht) count_empty_i)) 0) (set! count_v (+ count_v 1))) (set! count_empty_i (+ count_empty_i 1)))) (throw (ex-info "return" {:v count_v}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn balanced_factor [balanced_factor_ht]
  (binding [balanced_factor_i nil balanced_factor_total nil] (try (do (set! balanced_factor_total 0) (set! balanced_factor_i 0) (while (< balanced_factor_i (count (:values balanced_factor_ht))) (do (set! balanced_factor_total (+ balanced_factor_total (- (:charge_factor balanced_factor_ht) (count (get (:values balanced_factor_ht) balanced_factor_i))))) (set! balanced_factor_i (+ balanced_factor_i 1)))) (throw (ex-info "return" {:v (* (quot (double balanced_factor_total) (double (:size_table balanced_factor_ht))) (double (:charge_factor balanced_factor_ht)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn collision_resolution [collision_resolution_ht collision_resolution_key]
  (binding [collision_resolution_new_key nil collision_resolution_steps nil] (try (do (when (not (and (= (count (get (:values collision_resolution_ht) collision_resolution_key)) (:charge_factor collision_resolution_ht)) (= (count_empty collision_resolution_ht) 0))) (throw (ex-info "return" {:v collision_resolution_key}))) (set! collision_resolution_new_key (mod (+ collision_resolution_key 1) (:size_table collision_resolution_ht))) (set! collision_resolution_steps 0) (while (and (= (count (get (:values collision_resolution_ht) collision_resolution_new_key)) (:charge_factor collision_resolution_ht)) (< collision_resolution_steps (- (:size_table collision_resolution_ht) 1))) (do (set! collision_resolution_new_key (mod (+ collision_resolution_new_key 1) (:size_table collision_resolution_ht))) (set! collision_resolution_steps (+ collision_resolution_steps 1)))) (if (< (count (get (:values collision_resolution_ht) collision_resolution_new_key)) (:charge_factor collision_resolution_ht)) collision_resolution_new_key (- 1))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert [insert_ht insert_data]
  (binding [insert_dest nil insert_key nil] (try (do (set! insert_key (hash_function insert_ht insert_data)) (when (or (= (count (get (:values insert_ht) insert_key)) 0) (< (count (get (:values insert_ht) insert_key)) (:charge_factor insert_ht))) (do (set_value insert_ht insert_key insert_data) (throw (ex-info "return" {:v nil})))) (set! insert_dest (collision_resolution insert_ht insert_key)) (if (>= insert_dest 0) (set_value insert_ht insert_dest insert_data) (println "table full"))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_ht nil] (do (set! main_ht (make_table 3 2)) (insert main_ht 10) (insert main_ht 20) (insert main_ht 30) (insert main_ht 40) (insert main_ht 50) (println (str (:values main_ht))) (println (str (balanced_factor main_ht))))))

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
