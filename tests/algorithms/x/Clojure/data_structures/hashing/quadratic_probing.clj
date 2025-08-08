(ns main (:refer-clojure :exclude [create_hash_table hash_function balanced_factor collision_resolution insert_data int_to_string keys_to_string]))

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

(declare create_hash_table hash_function balanced_factor collision_resolution insert_data int_to_string keys_to_string)

(def ^:dynamic balanced_factor_i nil)

(def ^:dynamic collision_resolution_i nil)

(def ^:dynamic collision_resolution_new_key nil)

(def ^:dynamic count_v nil)

(def ^:dynamic create_hash_table_i nil)

(def ^:dynamic create_hash_table_vals nil)

(def ^:dynamic first_v nil)

(def ^:dynamic insert_data_key nil)

(def ^:dynamic insert_data_new_key nil)

(def ^:dynamic insert_data_table nil)

(def ^:dynamic insert_data_vals nil)

(def ^:dynamic int_to_string_ch nil)

(def ^:dynamic int_to_string_digit nil)

(def ^:dynamic int_to_string_neg nil)

(def ^:dynamic int_to_string_num nil)

(def ^:dynamic int_to_string_res nil)

(def ^:dynamic keys_to_string_i nil)

(def ^:dynamic keys_to_string_result nil)

(def ^:dynamic keys_to_string_v nil)

(defn create_hash_table [create_hash_table_size]
  (binding [create_hash_table_i nil create_hash_table_vals nil] (try (do (set! create_hash_table_vals []) (set! create_hash_table_i 0) (while (< create_hash_table_i create_hash_table_size) (do (set! create_hash_table_vals (conj create_hash_table_vals nil)) (set! create_hash_table_i (+ create_hash_table_i 1)))) (throw (ex-info "return" {:v {:lim_charge 0.75 :size_table create_hash_table_size :values create_hash_table_vals}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hash_function [hash_function_table hash_function_key]
  (try (throw (ex-info "return" {:v (mod hash_function_key (:size_table hash_function_table))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn balanced_factor [balanced_factor_table]
  (binding [balanced_factor_i nil count_v nil] (try (do (set! count_v 0) (set! balanced_factor_i 0) (while (< balanced_factor_i (count (:values balanced_factor_table))) (do (when (not= (get (:values balanced_factor_table) balanced_factor_i) nil) (set! count_v (+ count_v 1))) (set! balanced_factor_i (+ balanced_factor_i 1)))) (throw (ex-info "return" {:v (quot (double count_v) (double (:size_table balanced_factor_table)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn collision_resolution [collision_resolution_table collision_resolution_key]
  (binding [collision_resolution_i nil collision_resolution_new_key nil] (try (do (set! collision_resolution_i 1) (set! collision_resolution_new_key (hash_function collision_resolution_table (+ collision_resolution_key (* collision_resolution_i collision_resolution_i)))) (while (and (not= (get (:values collision_resolution_table) collision_resolution_new_key) nil) (not= (get (:values collision_resolution_table) collision_resolution_new_key) collision_resolution_key)) (do (set! collision_resolution_i (+ collision_resolution_i 1)) (when (>= (balanced_factor collision_resolution_table) (:lim_charge collision_resolution_table)) (throw (ex-info "return" {:v (:size_table collision_resolution_table)}))) (set! collision_resolution_new_key (hash_function collision_resolution_table (+ collision_resolution_key (* collision_resolution_i collision_resolution_i)))))) (throw (ex-info "return" {:v collision_resolution_new_key}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert_data [insert_data_table_p insert_data_data]
  (binding [insert_data_key nil insert_data_new_key nil insert_data_table nil insert_data_vals nil] (try (do (set! insert_data_table insert_data_table_p) (set! insert_data_key (hash_function insert_data_table insert_data_data)) (set! insert_data_vals (:values insert_data_table)) (if (= (nth insert_data_vals insert_data_key) nil) (set! insert_data_vals (assoc insert_data_vals insert_data_key insert_data_data)) (if (= (nth insert_data_vals insert_data_key) insert_data_data) (do (set! insert_data_table (assoc insert_data_table :values insert_data_vals)) (throw (ex-info "return" {:v nil}))) (do (set! insert_data_new_key (collision_resolution insert_data_table insert_data_key)) (when (and (< insert_data_new_key (count insert_data_vals)) (= (nth insert_data_vals insert_data_new_key) nil)) (set! insert_data_vals (assoc insert_data_vals insert_data_new_key insert_data_data)))))) (set! insert_data_table (assoc insert_data_table :values insert_data_vals))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn int_to_string [int_to_string_n]
  (binding [int_to_string_ch nil int_to_string_digit nil int_to_string_neg nil int_to_string_num nil int_to_string_res nil] (try (do (when (= int_to_string_n 0) (throw (ex-info "return" {:v "0"}))) (set! int_to_string_num int_to_string_n) (set! int_to_string_neg false) (when (< int_to_string_num 0) (do (set! int_to_string_neg true) (set! int_to_string_num (- int_to_string_num)))) (set! int_to_string_res "") (while (> int_to_string_num 0) (do (set! int_to_string_digit (mod int_to_string_num 10)) (set! int_to_string_ch (subs "0123456789" int_to_string_digit (min (+ int_to_string_digit 1) (count "0123456789")))) (set! int_to_string_res (str int_to_string_ch int_to_string_res)) (set! int_to_string_num (quot int_to_string_num 10)))) (when int_to_string_neg (set! int_to_string_res (str "-" int_to_string_res))) (throw (ex-info "return" {:v int_to_string_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn keys_to_string [keys_to_string_table]
  (binding [first_v nil keys_to_string_i nil keys_to_string_result nil keys_to_string_v nil] (try (do (set! keys_to_string_result "{") (set! first_v true) (set! keys_to_string_i 0) (while (< keys_to_string_i (count (:values keys_to_string_table))) (do (set! keys_to_string_v (get (:values keys_to_string_table) keys_to_string_i)) (when (not= keys_to_string_v nil) (do (when (not first_v) (set! keys_to_string_result (str keys_to_string_result ", "))) (set! keys_to_string_result (str (str (str keys_to_string_result (int_to_string keys_to_string_i)) ": ") (int_to_string keys_to_string_v))) (set! first_v false))) (set! keys_to_string_i (+ keys_to_string_i 1)))) (set! keys_to_string_result (str keys_to_string_result "}")) (throw (ex-info "return" {:v keys_to_string_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_qp (create_hash_table 8))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (insert_data main_qp 0)
      (insert_data main_qp 999)
      (insert_data main_qp 111)
      (println (keys_to_string main_qp))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
