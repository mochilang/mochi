(ns main (:refer-clojure :exclude [repeat_int repeat_bool set_int set_bool create_table hash_function is_prime next_prime set_value collision_resolution rehashing insert_data keys main]))

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

(declare repeat_int repeat_bool set_int set_bool create_table hash_function is_prime next_prime set_value collision_resolution rehashing insert_data keys main)

(def ^:dynamic collision_resolution_new_key nil)

(def ^:dynamic collision_resolution_steps nil)

(def ^:dynamic hash_function_k nil)

(def ^:dynamic insert_data_key nil)

(def ^:dynamic insert_data_new_key nil)

(def ^:dynamic insert_data_resized nil)

(def ^:dynamic is_prime_i nil)

(def ^:dynamic keys_i nil)

(def ^:dynamic keys_res nil)

(def ^:dynamic main_ht nil)

(def ^:dynamic next_prime_candidate nil)

(def ^:dynamic rehashing_i nil)

(def ^:dynamic rehashing_new_ht nil)

(def ^:dynamic rehashing_new_size nil)

(def ^:dynamic rehashing_survivors nil)

(def ^:dynamic repeat_bool_i nil)

(def ^:dynamic repeat_bool_res nil)

(def ^:dynamic repeat_int_i nil)

(def ^:dynamic repeat_int_res nil)

(def ^:dynamic set_bool_i nil)

(def ^:dynamic set_bool_res nil)

(def ^:dynamic set_int_i nil)

(def ^:dynamic set_int_res nil)

(def ^:dynamic set_value_new_filled nil)

(def ^:dynamic set_value_new_values nil)

(defn repeat_int [repeat_int_n repeat_int_val]
  (binding [repeat_int_i nil repeat_int_res nil] (try (do (set! repeat_int_res nil) (set! repeat_int_i 0) (while (< repeat_int_i repeat_int_n) (do (set! repeat_int_res (conj repeat_int_res repeat_int_val)) (set! repeat_int_i (+ repeat_int_i 1)))) (throw (ex-info "return" {:v repeat_int_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn repeat_bool [repeat_bool_n repeat_bool_val]
  (binding [repeat_bool_i nil repeat_bool_res nil] (try (do (set! repeat_bool_res nil) (set! repeat_bool_i 0) (while (< repeat_bool_i repeat_bool_n) (do (set! repeat_bool_res (conj repeat_bool_res repeat_bool_val)) (set! repeat_bool_i (+ repeat_bool_i 1)))) (throw (ex-info "return" {:v repeat_bool_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn set_int [set_int_xs set_int_idx set_int_value]
  (binding [set_int_i nil set_int_res nil] (try (do (set! set_int_res nil) (set! set_int_i 0) (while (< set_int_i (count set_int_xs)) (do (if (= set_int_i set_int_idx) (set! set_int_res (conj set_int_res set_int_value)) (set! set_int_res (conj set_int_res (nth set_int_xs set_int_i)))) (set! set_int_i (+ set_int_i 1)))) (throw (ex-info "return" {:v set_int_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn set_bool [set_bool_xs set_bool_idx set_bool_value]
  (binding [set_bool_i nil set_bool_res nil] (try (do (set! set_bool_res nil) (set! set_bool_i 0) (while (< set_bool_i (count set_bool_xs)) (do (if (= set_bool_i set_bool_idx) (set! set_bool_res (conj set_bool_res set_bool_value)) (set! set_bool_res (conj set_bool_res (nth set_bool_xs set_bool_i)))) (set! set_bool_i (+ set_bool_i 1)))) (throw (ex-info "return" {:v set_bool_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn create_table [create_table_size_table create_table_charge_factor create_table_lim_charge]
  (try (throw (ex-info "return" {:v {:charge_factor create_table_charge_factor :filled (repeat_bool create_table_size_table false) :lim_charge create_table_lim_charge :size_table create_table_size_table :values (repeat_int create_table_size_table 0)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hash_function [hash_function_ht hash_function_key]
  (binding [hash_function_k nil] (try (do (set! hash_function_k (mod hash_function_key (:size_table hash_function_ht))) (when (< hash_function_k 0) (set! hash_function_k (+ hash_function_k (:size_table hash_function_ht)))) (throw (ex-info "return" {:v hash_function_k}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_prime [is_prime_n]
  (binding [is_prime_i nil] (try (do (when (< is_prime_n 2) (throw (ex-info "return" {:v false}))) (when (= (mod is_prime_n 2) 0) (throw (ex-info "return" {:v (= is_prime_n 2)}))) (set! is_prime_i 3) (while (<= (* is_prime_i is_prime_i) is_prime_n) (do (when (= (mod is_prime_n is_prime_i) 0) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 2)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn next_prime [next_prime_value next_prime_factor]
  (binding [next_prime_candidate nil] (try (do (set! next_prime_candidate (+ (* next_prime_value next_prime_factor) 1)) (while (not (is_prime next_prime_candidate)) (set! next_prime_candidate (+ next_prime_candidate 1))) (throw (ex-info "return" {:v next_prime_candidate}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn set_value [set_value_ht set_value_key set_value_data]
  (binding [set_value_new_filled nil set_value_new_values nil] (try (do (set! set_value_new_values (set_int (:values set_value_ht) set_value_key set_value_data)) (set! set_value_new_filled (set_bool (:filled set_value_ht) set_value_key true)) (throw (ex-info "return" {:v {:charge_factor (:charge_factor set_value_ht) :filled set_value_new_filled :lim_charge (:lim_charge set_value_ht) :size_table (:size_table set_value_ht) :values set_value_new_values}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn collision_resolution [collision_resolution_ht collision_resolution_key]
  (binding [collision_resolution_new_key nil collision_resolution_steps nil] (try (do (set! collision_resolution_new_key (hash_function collision_resolution_ht (+ collision_resolution_key 1))) (set! collision_resolution_steps 0) (while (get (:filled collision_resolution_ht) collision_resolution_new_key) (do (set! collision_resolution_new_key (hash_function collision_resolution_ht (+ collision_resolution_new_key 1))) (set! collision_resolution_steps (+ collision_resolution_steps 1)) (when (>= collision_resolution_steps (:size_table collision_resolution_ht)) (throw (ex-info "return" {:v (- 1)}))))) (throw (ex-info "return" {:v collision_resolution_new_key}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rehashing [rehashing_ht]
  (binding [rehashing_i nil rehashing_new_ht nil rehashing_new_size nil rehashing_survivors nil] (try (do (set! rehashing_survivors nil) (set! rehashing_i 0) (while (< rehashing_i (count (:values rehashing_ht))) (do (when (get (:filled rehashing_ht) rehashing_i) (set! rehashing_survivors (conj rehashing_survivors (get (:values rehashing_ht) rehashing_i)))) (set! rehashing_i (+ rehashing_i 1)))) (set! rehashing_new_size (next_prime (:size_table rehashing_ht) 2)) (set! rehashing_new_ht (create_table rehashing_new_size (:charge_factor rehashing_ht) (:lim_charge rehashing_ht))) (set! rehashing_i 0) (while (< rehashing_i (count rehashing_survivors)) (do (set! rehashing_new_ht (insert_data rehashing_new_ht (nth rehashing_survivors rehashing_i))) (set! rehashing_i (+ rehashing_i 1)))) (throw (ex-info "return" {:v rehashing_new_ht}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert_data [insert_data_ht insert_data_data]
  (binding [insert_data_key nil insert_data_new_key nil insert_data_resized nil] (try (do (set! insert_data_key (hash_function insert_data_ht insert_data_data)) (when (not (get (:filled insert_data_ht) insert_data_key)) (throw (ex-info "return" {:v (set_value insert_data_ht insert_data_key insert_data_data)}))) (when (= (get (:values insert_data_ht) insert_data_key) insert_data_data) (throw (ex-info "return" {:v insert_data_ht}))) (set! insert_data_new_key (collision_resolution insert_data_ht insert_data_key)) (when (>= insert_data_new_key 0) (throw (ex-info "return" {:v (set_value insert_data_ht insert_data_new_key insert_data_data)}))) (set! insert_data_resized (rehashing insert_data_ht)) (throw (ex-info "return" {:v (insert_data insert_data_resized insert_data_data)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn keys [keys_ht]
  (binding [keys_i nil keys_res nil] (try (do (set! keys_res nil) (set! keys_i 0) (while (< keys_i (count (:values keys_ht))) (do (when (get (:filled keys_ht) keys_i) (set! keys_res (conj keys_res [keys_i (get (:values keys_ht) keys_i)]))) (set! keys_i (+ keys_i 1)))) (throw (ex-info "return" {:v keys_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_ht nil] (do (set! main_ht (create_table 3 1 0.75)) (set! main_ht (insert_data main_ht 17)) (set! main_ht (insert_data main_ht 18)) (set! main_ht (insert_data main_ht 99)) (println (keys main_ht)))))

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
