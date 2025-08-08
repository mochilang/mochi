(ns main (:refer-clojure :exclude [make_hash_map hm_len hm_set hm_get hm_del test_add_items test_overwrite_items test_delete_items test_access_absent_items test_add_with_resize_up test_add_with_resize_down]))

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

(declare make_hash_map hm_len hm_set hm_get hm_del test_add_items test_overwrite_items test_delete_items test_access_absent_items test_add_with_resize_up test_add_with_resize_down)

(def ^:dynamic hm_del_e nil)

(def ^:dynamic hm_del_entries nil)

(def ^:dynamic hm_del_i nil)

(def ^:dynamic hm_del_new_entries nil)

(def ^:dynamic hm_del_removed nil)

(def ^:dynamic hm_get_e nil)

(def ^:dynamic hm_get_i nil)

(def ^:dynamic hm_set_e nil)

(def ^:dynamic hm_set_entries nil)

(def ^:dynamic hm_set_i nil)

(def ^:dynamic hm_set_new_entries nil)

(def ^:dynamic hm_set_updated nil)

(def ^:dynamic test_access_absent_items_d1 nil)

(def ^:dynamic test_access_absent_items_d2 nil)

(def ^:dynamic test_access_absent_items_d3 nil)

(def ^:dynamic test_access_absent_items_g1 nil)

(def ^:dynamic test_access_absent_items_g2 nil)

(def ^:dynamic test_access_absent_items_h nil)

(def ^:dynamic test_add_items_a nil)

(def ^:dynamic test_add_items_b nil)

(def ^:dynamic test_add_items_h nil)

(def ^:dynamic test_add_with_resize_down_a nil)

(def ^:dynamic test_add_with_resize_down_d nil)

(def ^:dynamic test_add_with_resize_down_h nil)

(def ^:dynamic test_add_with_resize_down_i nil)

(def ^:dynamic test_add_with_resize_down_j nil)

(def ^:dynamic test_add_with_resize_down_s nil)

(def ^:dynamic test_add_with_resize_up_h nil)

(def ^:dynamic test_add_with_resize_up_i nil)

(def ^:dynamic test_add_with_resize_up_s nil)

(def ^:dynamic test_delete_items_d1 nil)

(def ^:dynamic test_delete_items_d2 nil)

(def ^:dynamic test_delete_items_d3 nil)

(def ^:dynamic test_delete_items_h nil)

(def ^:dynamic test_overwrite_items_a nil)

(def ^:dynamic test_overwrite_items_h nil)

(defn make_hash_map []
  (try (throw (ex-info "return" {:v {:entries []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hm_len [hm_len_m]
  (try (throw (ex-info "return" {:v (count (:entries hm_len_m))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hm_set [hm_set_m hm_set_key hm_set_value]
  (binding [hm_set_e nil hm_set_entries nil hm_set_i nil hm_set_new_entries nil hm_set_updated nil] (try (do (set! hm_set_entries (:entries hm_set_m)) (set! hm_set_updated false) (set! hm_set_new_entries []) (set! hm_set_i 0) (while (< hm_set_i (count hm_set_entries)) (do (set! hm_set_e (nth hm_set_entries hm_set_i)) (if (= (:key hm_set_e) hm_set_key) (do (set! hm_set_new_entries (conj hm_set_new_entries {:key hm_set_key :value hm_set_value})) (set! hm_set_updated true)) (set! hm_set_new_entries (conj hm_set_new_entries hm_set_e))) (set! hm_set_i (+ hm_set_i 1)))) (when (not hm_set_updated) (set! hm_set_new_entries (conj hm_set_new_entries {:key hm_set_key :value hm_set_value}))) (throw (ex-info "return" {:v {:entries hm_set_new_entries}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hm_get [hm_get_m hm_get_key]
  (binding [hm_get_e nil hm_get_i nil] (try (do (set! hm_get_i 0) (while (< hm_get_i (count (:entries hm_get_m))) (do (set! hm_get_e (get (:entries hm_get_m) hm_get_i)) (when (= (:key hm_get_e) hm_get_key) (throw (ex-info "return" {:v {:found true :value (:value hm_get_e)}}))) (set! hm_get_i (+ hm_get_i 1)))) (throw (ex-info "return" {:v {:found false :value ""}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn hm_del [hm_del_m hm_del_key]
  (binding [hm_del_e nil hm_del_entries nil hm_del_i nil hm_del_new_entries nil hm_del_removed nil] (try (do (set! hm_del_entries (:entries hm_del_m)) (set! hm_del_new_entries []) (set! hm_del_removed false) (set! hm_del_i 0) (while (< hm_del_i (count hm_del_entries)) (do (set! hm_del_e (nth hm_del_entries hm_del_i)) (if (= (:key hm_del_e) hm_del_key) (set! hm_del_removed true) (set! hm_del_new_entries (conj hm_del_new_entries hm_del_e))) (set! hm_del_i (+ hm_del_i 1)))) (if hm_del_removed {:map {:entries hm_del_new_entries} :ok true} {:map hm_del_m :ok false})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_add_items []
  (binding [test_add_items_a nil test_add_items_b nil test_add_items_h nil] (try (do (set! test_add_items_h (make_hash_map)) (set! test_add_items_h (hm_set test_add_items_h "key_a" "val_a")) (set! test_add_items_h (hm_set test_add_items_h "key_b" "val_b")) (set! test_add_items_a (hm_get test_add_items_h "key_a")) (set! test_add_items_b (hm_get test_add_items_h "key_b")) (throw (ex-info "return" {:v (and (and (and (and (= (hm_len test_add_items_h) 2) (:found test_add_items_a)) (:found test_add_items_b)) (= (:value test_add_items_a) "val_a")) (= (:value test_add_items_b) "val_b"))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_overwrite_items []
  (binding [test_overwrite_items_a nil test_overwrite_items_h nil] (try (do (set! test_overwrite_items_h (make_hash_map)) (set! test_overwrite_items_h (hm_set test_overwrite_items_h "key_a" "val_a")) (set! test_overwrite_items_h (hm_set test_overwrite_items_h "key_a" "val_b")) (set! test_overwrite_items_a (hm_get test_overwrite_items_h "key_a")) (throw (ex-info "return" {:v (and (and (= (hm_len test_overwrite_items_h) 1) (:found test_overwrite_items_a)) (= (:value test_overwrite_items_a) "val_b"))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_delete_items []
  (binding [test_delete_items_d1 nil test_delete_items_d2 nil test_delete_items_d3 nil test_delete_items_h nil] (try (do (set! test_delete_items_h (make_hash_map)) (set! test_delete_items_h (hm_set test_delete_items_h "key_a" "val_a")) (set! test_delete_items_h (hm_set test_delete_items_h "key_b" "val_b")) (set! test_delete_items_d1 (hm_del test_delete_items_h "key_a")) (set! test_delete_items_h (:map test_delete_items_d1)) (set! test_delete_items_d2 (hm_del test_delete_items_h "key_b")) (set! test_delete_items_h (:map test_delete_items_d2)) (set! test_delete_items_h (hm_set test_delete_items_h "key_a" "val_a")) (set! test_delete_items_d3 (hm_del test_delete_items_h "key_a")) (set! test_delete_items_h (:map test_delete_items_d3)) (throw (ex-info "return" {:v (= (hm_len test_delete_items_h) 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_access_absent_items []
  (binding [test_access_absent_items_d1 nil test_access_absent_items_d2 nil test_access_absent_items_d3 nil test_access_absent_items_g1 nil test_access_absent_items_g2 nil test_access_absent_items_h nil] (try (do (set! test_access_absent_items_h (make_hash_map)) (set! test_access_absent_items_g1 (hm_get test_access_absent_items_h "key_a")) (set! test_access_absent_items_d1 (hm_del test_access_absent_items_h "key_a")) (set! test_access_absent_items_h (:map test_access_absent_items_d1)) (set! test_access_absent_items_h (hm_set test_access_absent_items_h "key_a" "val_a")) (set! test_access_absent_items_d2 (hm_del test_access_absent_items_h "key_a")) (set! test_access_absent_items_h (:map test_access_absent_items_d2)) (set! test_access_absent_items_d3 (hm_del test_access_absent_items_h "key_a")) (set! test_access_absent_items_h (:map test_access_absent_items_d3)) (set! test_access_absent_items_g2 (hm_get test_access_absent_items_h "key_a")) (throw (ex-info "return" {:v (and (and (and (and (and (= (:found test_access_absent_items_g1) false) (= (:ok test_access_absent_items_d1) false)) (:ok test_access_absent_items_d2)) (= (:ok test_access_absent_items_d3) false)) (= (:found test_access_absent_items_g2) false)) (= (hm_len test_access_absent_items_h) 0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_add_with_resize_up []
  (binding [test_add_with_resize_up_h nil test_add_with_resize_up_i nil test_add_with_resize_up_s nil] (try (do (set! test_add_with_resize_up_h (make_hash_map)) (set! test_add_with_resize_up_i 0) (while (< test_add_with_resize_up_i 5) (do (set! test_add_with_resize_up_s (str test_add_with_resize_up_i)) (set! test_add_with_resize_up_h (hm_set test_add_with_resize_up_h test_add_with_resize_up_s test_add_with_resize_up_s)) (set! test_add_with_resize_up_i (+ test_add_with_resize_up_i 1)))) (throw (ex-info "return" {:v (= (hm_len test_add_with_resize_up_h) 5)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_add_with_resize_down []
  (binding [test_add_with_resize_down_a nil test_add_with_resize_down_d nil test_add_with_resize_down_h nil test_add_with_resize_down_i nil test_add_with_resize_down_j nil test_add_with_resize_down_s nil] (try (do (set! test_add_with_resize_down_h (make_hash_map)) (set! test_add_with_resize_down_i 0) (while (< test_add_with_resize_down_i 5) (do (set! test_add_with_resize_down_s (str test_add_with_resize_down_i)) (set! test_add_with_resize_down_h (hm_set test_add_with_resize_down_h test_add_with_resize_down_s test_add_with_resize_down_s)) (set! test_add_with_resize_down_i (+ test_add_with_resize_down_i 1)))) (set! test_add_with_resize_down_j 0) (while (< test_add_with_resize_down_j 5) (do (set! test_add_with_resize_down_s (str test_add_with_resize_down_j)) (set! test_add_with_resize_down_d (hm_del test_add_with_resize_down_h test_add_with_resize_down_s)) (set! test_add_with_resize_down_h (:map test_add_with_resize_down_d)) (set! test_add_with_resize_down_j (+ test_add_with_resize_down_j 1)))) (set! test_add_with_resize_down_h (hm_set test_add_with_resize_down_h "key_a" "val_b")) (set! test_add_with_resize_down_a (hm_get test_add_with_resize_down_h "key_a")) (throw (ex-info "return" {:v (and (and (= (hm_len test_add_with_resize_down_h) 1) (:found test_add_with_resize_down_a)) (= (:value test_add_with_resize_down_a) "val_b"))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (test_add_items))
      (println (test_overwrite_items))
      (println (test_delete_items))
      (println (test_access_absent_items))
      (println (test_add_with_resize_up))
      (println (test_add_with_resize_down))
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
