(ns main (:refer-clojure :exclude [make_buckets hashmap_new bucket_index next_index try_set is_full is_sparse resize size_up size_down add_item hashmap_set hashmap_get hashmap_del hashmap_len hashmap_repr]))

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

(declare make_buckets hashmap_new bucket_index next_index try_set is_full is_sparse resize size_up size_down add_item hashmap_set hashmap_get hashmap_del hashmap_len hashmap_repr)

(declare _read_file)

(def ^:dynamic add_item_i nil)

(def ^:dynamic add_item_ind nil)

(def ^:dynamic bucket_index_ind nil)

(def ^:dynamic first_v nil)

(def ^:dynamic hashmap_del_buckets nil)

(def ^:dynamic hashmap_del_hm nil)

(def ^:dynamic hashmap_del_i nil)

(def ^:dynamic hashmap_del_ind nil)

(def ^:dynamic hashmap_del_it nil)

(def ^:dynamic hashmap_get_buckets nil)

(def ^:dynamic hashmap_get_i nil)

(def ^:dynamic hashmap_get_ind nil)

(def ^:dynamic hashmap_get_it nil)

(def ^:dynamic hashmap_repr_b nil)

(def ^:dynamic hashmap_repr_i nil)

(def ^:dynamic hashmap_repr_out nil)

(def ^:dynamic is_full_limit nil)

(def ^:dynamic is_sparse_limit nil)

(def ^:dynamic make_buckets_buckets nil)

(def ^:dynamic make_buckets_i nil)

(def ^:dynamic resize_hm nil)

(def ^:dynamic resize_i nil)

(def ^:dynamic resize_it nil)

(def ^:dynamic resize_old nil)

(def ^:dynamic try_set_b nil)

(def ^:dynamic try_set_buckets nil)

(def ^:dynamic try_set_hm nil)

(defn make_buckets [make_buckets_n]
  (binding [make_buckets_buckets nil make_buckets_i nil] (try (do (set! make_buckets_buckets []) (set! make_buckets_i 0) (while (< make_buckets_i make_buckets_n) (do (set! make_buckets_buckets (conj make_buckets_buckets {:key 0 :state 0 :val 0})) (set! make_buckets_i (+ make_buckets_i 1)))) (throw (ex-info "return" {:v make_buckets_buckets}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hashmap_new [hashmap_new_initial_size]
  (try (throw (ex-info "return" {:v {:buckets (make_buckets hashmap_new_initial_size) :cap_den 4 :cap_num 3 :initial_size hashmap_new_initial_size :len 0}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bucket_index [bucket_index_hm bucket_index_key]
  (binding [bucket_index_ind nil] (try (do (set! bucket_index_ind (mod bucket_index_key (count (:buckets bucket_index_hm)))) (when (< bucket_index_ind 0) (set! bucket_index_ind (+ bucket_index_ind (count (:buckets bucket_index_hm))))) (throw (ex-info "return" {:v bucket_index_ind}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn next_index [next_index_hm next_index_ind]
  (try (throw (ex-info "return" {:v (mod (+ next_index_ind 1) (count (:buckets next_index_hm)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn try_set [try_set_hm_p try_set_ind try_set_key try_set_val]
  (binding [try_set_hm try_set_hm_p try_set_b nil try_set_buckets nil] (try (do (set! try_set_buckets (:buckets try_set_hm)) (set! try_set_b (get try_set_buckets try_set_ind)) (when (or (= (:state try_set_b) 0) (= (:state try_set_b) 2)) (do (set! try_set_buckets (assoc try_set_buckets try_set_ind {:key try_set_key :state 1 :val try_set_val})) (set! try_set_hm (assoc try_set_hm :buckets try_set_buckets)) (set! try_set_hm (assoc try_set_hm :len (+ (:len try_set_hm) 1))) (throw (ex-info "return" {:v true})))) (when (= (:key try_set_b) try_set_key) (do (set! try_set_buckets (assoc try_set_buckets try_set_ind {:key try_set_key :state 1 :val try_set_val})) (set! try_set_hm (assoc try_set_hm :buckets try_set_buckets)) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var try_set_hm) (constantly try_set_hm))))))

(defn is_full [is_full_hm]
  (binding [is_full_limit nil] (try (do (set! is_full_limit (quot (* (count (:buckets is_full_hm)) (:cap_num is_full_hm)) (:cap_den is_full_hm))) (throw (ex-info "return" {:v (>= (:len is_full_hm) is_full_limit)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_sparse [is_sparse_hm]
  (binding [is_sparse_limit nil] (try (do (when (<= (count (:buckets is_sparse_hm)) (:initial_size is_sparse_hm)) (throw (ex-info "return" {:v false}))) (set! is_sparse_limit (quot (* (count (:buckets is_sparse_hm)) (:cap_num is_sparse_hm)) (* 2 (:cap_den is_sparse_hm)))) (throw (ex-info "return" {:v (< (:len is_sparse_hm) is_sparse_limit)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn resize [resize_hm_p resize_new_size]
  (binding [resize_hm resize_hm_p resize_i nil resize_it nil resize_old nil] (do (try (do (set! resize_old (:buckets resize_hm)) (set! resize_hm (assoc resize_hm :buckets (make_buckets resize_new_size))) (set! resize_hm (assoc resize_hm :len 0)) (set! resize_i 0) (while (< resize_i (count resize_old)) (do (set! resize_it (get resize_old resize_i)) (when (= (:state resize_it) 1) (add_item resize_hm (:key resize_it) (:val resize_it))) (set! resize_i (+ resize_i 1))))) (finally (alter-var-root (var resize_hm) (constantly resize_hm)))) resize_hm)))

(defn size_up [size_up_hm]
  (do (let [__res (resize size_up_hm (* (count (:buckets size_up_hm)) 2))] (do (set! size_up_hm resize_hm) __res)) size_up_hm))

(defn size_down [size_down_hm]
  (do (let [__res (resize size_down_hm (quot (count (:buckets size_down_hm)) 2))] (do (set! size_down_hm resize_hm) __res)) size_down_hm))

(defn add_item [add_item_hm add_item_key add_item_val]
  (binding [add_item_i nil add_item_ind nil] (do (set! add_item_ind (bucket_index add_item_hm add_item_key)) (set! add_item_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< add_item_i (count (:buckets add_item_hm)))) (do (if (let [__res (try_set add_item_hm add_item_ind add_item_key add_item_val)] (do (set! add_item_hm try_set_hm) __res)) (recur false) (do (set! add_item_ind (next_index add_item_hm add_item_ind)) (set! add_item_i (+ add_item_i 1)))) (cond :else (recur while_flag_1))))) add_item_hm)))

(defn hashmap_set [hashmap_set_hm hashmap_set_key hashmap_set_val]
  (do (when (is_full hashmap_set_hm) (size_up hashmap_set_hm)) (add_item hashmap_set_hm hashmap_set_key hashmap_set_val) hashmap_set_hm))

(defn hashmap_get [hashmap_get_hm hashmap_get_key]
  (binding [hashmap_get_buckets nil hashmap_get_i nil hashmap_get_ind nil hashmap_get_it nil] (try (do (set! hashmap_get_buckets (:buckets hashmap_get_hm)) (set! hashmap_get_ind (bucket_index hashmap_get_hm hashmap_get_key)) (set! hashmap_get_i 0) (loop [while_flag_2 true] (when (and while_flag_2 (< hashmap_get_i (count hashmap_get_buckets))) (do (set! hashmap_get_it (get hashmap_get_buckets hashmap_get_ind)) (if (= (:state hashmap_get_it) 0) (recur false) (do (when (and (= (:state hashmap_get_it) 1) (= (:key hashmap_get_it) hashmap_get_key)) (throw (ex-info "return" {:v (:val hashmap_get_it)}))) (set! hashmap_get_ind (next_index hashmap_get_hm hashmap_get_ind)) (set! hashmap_get_i (+ hashmap_get_i 1)))) (cond :else (recur while_flag_2))))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hashmap_del [hashmap_del_hm_p hashmap_del_key]
  (binding [hashmap_del_hm hashmap_del_hm_p hashmap_del_buckets nil hashmap_del_i nil hashmap_del_ind nil hashmap_del_it nil] (try (do (set! hashmap_del_buckets (:buckets hashmap_del_hm)) (set! hashmap_del_ind (bucket_index hashmap_del_hm hashmap_del_key)) (set! hashmap_del_i 0) (loop [while_flag_3 true] (when (and while_flag_3 (< hashmap_del_i (count hashmap_del_buckets))) (do (set! hashmap_del_it (get hashmap_del_buckets hashmap_del_ind)) (when (= (:state hashmap_del_it) 0) (do (println (str "KeyError: " (mochi_str hashmap_del_key))) (throw (ex-info "return" {:v nil})))) (if (and (= (:state hashmap_del_it) 1) (= (:key hashmap_del_it) hashmap_del_key)) (do (set! hashmap_del_buckets (assoc hashmap_del_buckets hashmap_del_ind {:key 0 :state 2 :val 0})) (set! hashmap_del_hm (assoc hashmap_del_hm :buckets hashmap_del_buckets)) (set! hashmap_del_hm (assoc hashmap_del_hm :len (- (:len hashmap_del_hm) 1))) (recur false)) (do (set! hashmap_del_ind (next_index hashmap_del_hm hashmap_del_ind)) (set! hashmap_del_i (+ hashmap_del_i 1)))) (cond :else (recur while_flag_3))))) (when (is_sparse hashmap_del_hm) (size_down hashmap_del_hm))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var hashmap_del_hm) (constantly hashmap_del_hm))))))

(defn hashmap_len [hashmap_len_hm]
  (try (throw (ex-info "return" {:v (:len hashmap_len_hm)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hashmap_repr [hashmap_repr_hm]
  (binding [first_v nil hashmap_repr_b nil hashmap_repr_i nil hashmap_repr_out nil] (try (do (set! hashmap_repr_out "HashMap(") (set! first_v true) (set! hashmap_repr_i 0) (while (< hashmap_repr_i (count (:buckets hashmap_repr_hm))) (do (set! hashmap_repr_b (get (:buckets hashmap_repr_hm) hashmap_repr_i)) (when (= (:state hashmap_repr_b) 1) (do (if (not first_v) (set! hashmap_repr_out (str hashmap_repr_out ", ")) (set! first_v false)) (set! hashmap_repr_out (str (str (str hashmap_repr_out (mochi_str (:key hashmap_repr_b))) ": ") (mochi_str (:val hashmap_repr_b)))))) (set! hashmap_repr_i (+ hashmap_repr_i 1)))) (set! hashmap_repr_out (str hashmap_repr_out ")")) (throw (ex-info "return" {:v hashmap_repr_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_hm nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_hm) (constantly (hashmap_new 5)))
      (hashmap_set main_hm 1 10)
      (hashmap_set main_hm 2 20)
      (hashmap_set main_hm 3 30)
      (println (hashmap_repr main_hm))
      (println (mochi_str (hashmap_get main_hm 2)))
      (let [__res (hashmap_del main_hm 1)] (do (alter-var-root (var main_hm) (constantly hashmap_del_hm)) __res))
      (println (hashmap_repr main_hm))
      (println (mochi_str (hashmap_len main_hm)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
