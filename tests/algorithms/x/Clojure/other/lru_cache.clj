(ns main (:refer-clojure :exclude [new_list dll_add dll_remove new_cache lru_get lru_put cache_info print_result main]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare new_list dll_add dll_remove new_cache lru_get lru_put cache_info print_result main)

(def ^:dynamic dll_add_lst nil)

(def ^:dynamic dll_add_node nil)

(def ^:dynamic dll_add_nodes nil)

(def ^:dynamic dll_add_prev_idx nil)

(def ^:dynamic dll_add_prev_node nil)

(def ^:dynamic dll_add_tail_idx nil)

(def ^:dynamic dll_add_tail_node nil)

(def ^:dynamic dll_remove_lst nil)

(def ^:dynamic dll_remove_next_idx nil)

(def ^:dynamic dll_remove_next_node nil)

(def ^:dynamic dll_remove_node nil)

(def ^:dynamic dll_remove_nodes nil)

(def ^:dynamic dll_remove_prev_idx nil)

(def ^:dynamic dll_remove_prev_node nil)

(def ^:dynamic lru_get_cache nil)

(def ^:dynamic lru_get_idx nil)

(def ^:dynamic lru_get_key_str nil)

(def ^:dynamic lru_get_node nil)

(def ^:dynamic lru_get_value nil)

(def ^:dynamic lru_put_cache nil)

(def ^:dynamic lru_put_first_idx nil)

(def ^:dynamic lru_put_first_node nil)

(def ^:dynamic lru_put_head_node nil)

(def ^:dynamic lru_put_idx nil)

(def ^:dynamic lru_put_key_str nil)

(def ^:dynamic lru_put_m nil)

(def ^:dynamic lru_put_mdel nil)

(def ^:dynamic lru_put_new_node nil)

(def ^:dynamic lru_put_node nil)

(def ^:dynamic lru_put_nodes nil)

(def ^:dynamic lru_put_old_key nil)

(def ^:dynamic main_cache nil)

(def ^:dynamic main_r1 nil)

(def ^:dynamic main_r2 nil)

(def ^:dynamic main_r3 nil)

(def ^:dynamic main_r4 nil)

(def ^:dynamic main_r5 nil)

(def ^:dynamic new_cache_empty_map nil)

(def ^:dynamic new_list_head nil)

(def ^:dynamic new_list_nodes nil)

(def ^:dynamic new_list_tail nil)

(defn new_list []
  (binding [new_list_head nil new_list_nodes nil new_list_tail nil] (try (do (set! new_list_nodes []) (set! new_list_head {:key 0 :next 1 :prev (- 0 1) :value 0}) (set! new_list_tail {:key 0 :next (- 0 1) :prev 0 :value 0}) (set! new_list_nodes (conj new_list_nodes new_list_head)) (set! new_list_nodes (conj new_list_nodes new_list_tail)) (throw (ex-info "return" {:v {:head 0 :nodes new_list_nodes :tail 1}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dll_add [dll_add_lst_p dll_add_idx]
  (binding [dll_add_lst nil dll_add_node nil dll_add_nodes nil dll_add_prev_idx nil dll_add_prev_node nil dll_add_tail_idx nil dll_add_tail_node nil] (try (do (set! dll_add_lst dll_add_lst_p) (set! dll_add_nodes (:nodes dll_add_lst)) (set! dll_add_tail_idx (:tail dll_add_lst)) (set! dll_add_tail_node (get dll_add_nodes dll_add_tail_idx)) (set! dll_add_prev_idx (:prev dll_add_tail_node)) (set! dll_add_node (get dll_add_nodes dll_add_idx)) (set! dll_add_node (assoc dll_add_node :prev dll_add_prev_idx)) (set! dll_add_node (assoc dll_add_node :next dll_add_tail_idx)) (set! dll_add_nodes (assoc dll_add_nodes dll_add_idx dll_add_node)) (set! dll_add_prev_node (get dll_add_nodes dll_add_prev_idx)) (set! dll_add_prev_node (assoc dll_add_prev_node :next dll_add_idx)) (set! dll_add_nodes (assoc dll_add_nodes dll_add_prev_idx dll_add_prev_node)) (set! dll_add_tail_node (assoc dll_add_tail_node :prev dll_add_idx)) (set! dll_add_nodes (assoc dll_add_nodes dll_add_tail_idx dll_add_tail_node)) (set! dll_add_lst (assoc dll_add_lst :nodes dll_add_nodes)) (throw (ex-info "return" {:v dll_add_lst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dll_remove [dll_remove_lst_p dll_remove_idx]
  (binding [dll_remove_lst nil dll_remove_next_idx nil dll_remove_next_node nil dll_remove_node nil dll_remove_nodes nil dll_remove_prev_idx nil dll_remove_prev_node nil] (try (do (set! dll_remove_lst dll_remove_lst_p) (set! dll_remove_nodes (:nodes dll_remove_lst)) (set! dll_remove_node (get dll_remove_nodes dll_remove_idx)) (set! dll_remove_prev_idx (:prev dll_remove_node)) (set! dll_remove_next_idx (:next dll_remove_node)) (when (or (= dll_remove_prev_idx (- 0 1)) (= dll_remove_next_idx (- 0 1))) (throw (ex-info "return" {:v dll_remove_lst}))) (set! dll_remove_prev_node (get dll_remove_nodes dll_remove_prev_idx)) (set! dll_remove_prev_node (assoc dll_remove_prev_node :next dll_remove_next_idx)) (set! dll_remove_nodes (assoc dll_remove_nodes dll_remove_prev_idx dll_remove_prev_node)) (set! dll_remove_next_node (get dll_remove_nodes dll_remove_next_idx)) (set! dll_remove_next_node (assoc dll_remove_next_node :prev dll_remove_prev_idx)) (set! dll_remove_nodes (assoc dll_remove_nodes dll_remove_next_idx dll_remove_next_node)) (set! dll_remove_node (assoc dll_remove_node :prev (- 0 1))) (set! dll_remove_node (assoc dll_remove_node :next (- 0 1))) (set! dll_remove_nodes (assoc dll_remove_nodes dll_remove_idx dll_remove_node)) (set! dll_remove_lst (assoc dll_remove_lst :nodes dll_remove_nodes)) (throw (ex-info "return" {:v dll_remove_lst}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn new_cache [new_cache_cap]
  (binding [new_cache_empty_map nil] (try (do (set! new_cache_empty_map {}) (throw (ex-info "return" {:v {:cache new_cache_empty_map :capacity new_cache_cap :hits 0 :list (new_list) :misses 0 :num_keys 0}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lru_get [lru_get_c lru_get_key]
  (binding [lru_get_cache nil lru_get_idx nil lru_get_key_str nil lru_get_node nil lru_get_value nil] (try (do (set! lru_get_cache lru_get_c) (set! lru_get_key_str (str lru_get_key)) (when (in lru_get_key_str (:cache lru_get_cache)) (do (set! lru_get_idx (get (:cache lru_get_cache) lru_get_key_str)) (when (not= lru_get_idx (- 0 1)) (do (set! lru_get_cache (assoc lru_get_cache :hits (+ (:hits lru_get_cache) 1))) (set! lru_get_node (get (:nodes (:list lru_get_cache)) lru_get_idx)) (set! lru_get_value (:value lru_get_node)) (set! lru_get_cache (assoc lru_get_cache :list (dll_remove (:list lru_get_cache) lru_get_idx))) (set! lru_get_cache (assoc lru_get_cache :list (dll_add (:list lru_get_cache) lru_get_idx))) (throw (ex-info "return" {:v {:cache lru_get_cache :ok true :value lru_get_value}})))))) (set! lru_get_cache (assoc lru_get_cache :misses (+ (:misses lru_get_cache) 1))) (throw (ex-info "return" {:v {:cache lru_get_cache :ok false :value 0}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lru_put [lru_put_c lru_put_key lru_put_value]
  (binding [lru_put_cache nil lru_put_first_idx nil lru_put_first_node nil lru_put_head_node nil lru_put_idx nil lru_put_key_str nil lru_put_m nil lru_put_mdel nil lru_put_new_node nil lru_put_node nil lru_put_nodes nil lru_put_old_key nil] (try (do (set! lru_put_cache lru_put_c) (set! lru_put_key_str (str lru_put_key)) (if (not (in lru_put_key_str (:cache lru_put_cache))) (do (when (>= (:num_keys lru_put_cache) (:capacity lru_put_cache)) (do (set! lru_put_head_node (get (:nodes (:list lru_put_cache)) (:head (:list lru_put_cache)))) (set! lru_put_first_idx (:next lru_put_head_node)) (set! lru_put_first_node (get (:nodes (:list lru_put_cache)) lru_put_first_idx)) (set! lru_put_old_key (:key lru_put_first_node)) (set! lru_put_cache (assoc lru_put_cache :list (dll_remove (:list lru_put_cache) lru_put_first_idx))) (set! lru_put_mdel (:cache lru_put_cache)) (set! lru_put_mdel (assoc lru_put_mdel (str lru_put_old_key) (- 0 1))) (set! lru_put_cache (assoc lru_put_cache :cache lru_put_mdel)) (set! lru_put_cache (assoc lru_put_cache :num_keys (- (:num_keys lru_put_cache) 1))))) (set! lru_put_nodes (:nodes (:list lru_put_cache))) (set! lru_put_new_node {:key lru_put_key :next (- 0 1) :prev (- 0 1) :value lru_put_value}) (set! lru_put_nodes (conj lru_put_nodes lru_put_new_node)) (set! lru_put_idx (- (count lru_put_nodes) 1)) (set! lru_put_cache (assoc-in lru_put_cache [:list :nodes] lru_put_nodes)) (set! lru_put_cache (assoc lru_put_cache :list (dll_add (:list lru_put_cache) lru_put_idx))) (set! lru_put_m (:cache lru_put_cache)) (set! lru_put_m (assoc lru_put_m lru_put_key_str lru_put_idx)) (set! lru_put_cache (assoc lru_put_cache :cache lru_put_m)) (set! lru_put_cache (assoc lru_put_cache :num_keys (+ (:num_keys lru_put_cache) 1)))) (do (set! lru_put_m (:cache lru_put_cache)) (set! lru_put_idx (get lru_put_m lru_put_key_str)) (set! lru_put_nodes (:nodes (:list lru_put_cache))) (set! lru_put_node (get lru_put_nodes lru_put_idx)) (set! lru_put_node (assoc lru_put_node :value lru_put_value)) (set! lru_put_nodes (assoc lru_put_nodes lru_put_idx lru_put_node)) (set! lru_put_cache (assoc-in lru_put_cache [:list :nodes] lru_put_nodes)) (set! lru_put_cache (assoc lru_put_cache :list (dll_remove (:list lru_put_cache) lru_put_idx))) (set! lru_put_cache (assoc lru_put_cache :list (dll_add (:list lru_put_cache) lru_put_idx))) (set! lru_put_cache (assoc lru_put_cache :cache lru_put_m)))) (throw (ex-info "return" {:v lru_put_cache}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cache_info [cache_info_cache]
  (try (throw (ex-info "return" {:v (str (str (str (str (str (str (str (str "CacheInfo(hits=" (str (:hits cache_info_cache))) ", misses=") (str (:misses cache_info_cache))) ", capacity=") (str (:capacity cache_info_cache))) ", current size=") (str (:num_keys cache_info_cache))) ")")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn print_result [print_result_res]
  (do (if (:ok print_result_res) (println (str (:value print_result_res))) (println "None")) print_result_res))

(defn main []
  (binding [main_cache nil main_r1 nil main_r2 nil main_r3 nil main_r4 nil main_r5 nil] (do (set! main_cache (new_cache 2)) (set! main_cache (lru_put main_cache 1 1)) (set! main_cache (lru_put main_cache 2 2)) (set! main_r1 (lru_get main_cache 1)) (set! main_cache (:cache main_r1)) (print_result main_r1) (set! main_cache (lru_put main_cache 3 3)) (set! main_r2 (lru_get main_cache 2)) (set! main_cache (:cache main_r2)) (print_result main_r2) (set! main_cache (lru_put main_cache 4 4)) (set! main_r3 (lru_get main_cache 1)) (set! main_cache (:cache main_r3)) (print_result main_r3) (set! main_r4 (lru_get main_cache 3)) (set! main_cache (:cache main_r4)) (print_result main_r4) (set! main_r5 (lru_get main_cache 4)) (set! main_cache (:cache main_r5)) (print_result main_r5) (println (cache_info main_cache)))))

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
