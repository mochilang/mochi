(ns main (:refer-clojure :exclude [lfu_new find_entry lfu_get remove_lfu lfu_put cache_info main]))

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

(declare lfu_new find_entry lfu_get remove_lfu lfu_put cache_info main)

(def ^:dynamic find_entry_e nil)

(def ^:dynamic find_entry_i nil)

(def ^:dynamic lfu_get_e nil)

(def ^:dynamic lfu_get_entries nil)

(def ^:dynamic lfu_get_idx nil)

(def ^:dynamic lfu_get_new_cache nil)

(def ^:dynamic lfu_get_new_tick nil)

(def ^:dynamic lfu_put_e nil)

(def ^:dynamic lfu_put_entries nil)

(def ^:dynamic lfu_put_idx nil)

(def ^:dynamic lfu_put_new_entry nil)

(def ^:dynamic lfu_put_new_tick nil)

(def ^:dynamic main_cache nil)

(def ^:dynamic main_r nil)

(def ^:dynamic remove_lfu_e nil)

(def ^:dynamic remove_lfu_i nil)

(def ^:dynamic remove_lfu_j nil)

(def ^:dynamic remove_lfu_m nil)

(def ^:dynamic remove_lfu_min_idx nil)

(def ^:dynamic remove_lfu_res nil)

(defn lfu_new [lfu_new_cap]
  (try (throw (ex-info "return" {:v {:capacity lfu_new_cap :entries [] :hits 0 :miss 0 :tick 0}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn find_entry [find_entry_entries find_entry_key]
  (binding [find_entry_e nil find_entry_i nil] (try (do (set! find_entry_i 0) (while (< find_entry_i (count find_entry_entries)) (do (set! find_entry_e (nth find_entry_entries find_entry_i)) (when (= (:key find_entry_e) find_entry_key) (throw (ex-info "return" {:v find_entry_i}))) (set! find_entry_i (+ find_entry_i 1)))) (throw (ex-info "return" {:v (- 0 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lfu_get [lfu_get_cache lfu_get_key]
  (binding [lfu_get_e nil lfu_get_entries nil lfu_get_idx nil lfu_get_new_cache nil lfu_get_new_tick nil] (try (do (set! lfu_get_idx (find_entry (:entries lfu_get_cache) lfu_get_key)) (when (= lfu_get_idx (- 0 1)) (do (set! lfu_get_new_cache {:capacity (:capacity lfu_get_cache) :entries (:entries lfu_get_cache) :hits (:hits lfu_get_cache) :miss (+ (:miss lfu_get_cache) 1) :tick (:tick lfu_get_cache)}) (throw (ex-info "return" {:v {:cache lfu_get_new_cache :ok false :value 0}})))) (set! lfu_get_entries (:entries lfu_get_cache)) (set! lfu_get_e (get lfu_get_entries lfu_get_idx)) (set! lfu_get_e (assoc lfu_get_e :freq (+ (:freq lfu_get_e) 1))) (set! lfu_get_new_tick (+ (:tick lfu_get_cache) 1)) (set! lfu_get_e (assoc lfu_get_e :order lfu_get_new_tick)) (set! lfu_get_entries (assoc lfu_get_entries lfu_get_idx lfu_get_e)) (set! lfu_get_new_cache {:capacity (:capacity lfu_get_cache) :entries lfu_get_entries :hits (+ (:hits lfu_get_cache) 1) :miss (:miss lfu_get_cache) :tick lfu_get_new_tick}) (throw (ex-info "return" {:v {:cache lfu_get_new_cache :ok true :value (:val lfu_get_e)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_lfu [remove_lfu_entries]
  (binding [remove_lfu_e nil remove_lfu_i nil remove_lfu_j nil remove_lfu_m nil remove_lfu_min_idx nil remove_lfu_res nil] (try (do (when (= (count remove_lfu_entries) 0) (throw (ex-info "return" {:v remove_lfu_entries}))) (set! remove_lfu_min_idx 0) (set! remove_lfu_i 1) (while (< remove_lfu_i (count remove_lfu_entries)) (do (set! remove_lfu_e (nth remove_lfu_entries remove_lfu_i)) (set! remove_lfu_m (nth remove_lfu_entries remove_lfu_min_idx)) (when (or (< (:freq remove_lfu_e) (:freq remove_lfu_m)) (and (= (:freq remove_lfu_e) (:freq remove_lfu_m)) (< (:order remove_lfu_e) (:order remove_lfu_m)))) (set! remove_lfu_min_idx remove_lfu_i)) (set! remove_lfu_i (+ remove_lfu_i 1)))) (set! remove_lfu_res []) (set! remove_lfu_j 0) (while (< remove_lfu_j (count remove_lfu_entries)) (do (when (not= remove_lfu_j remove_lfu_min_idx) (set! remove_lfu_res (conj remove_lfu_res (nth remove_lfu_entries remove_lfu_j)))) (set! remove_lfu_j (+ remove_lfu_j 1)))) (throw (ex-info "return" {:v remove_lfu_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lfu_put [lfu_put_cache lfu_put_key lfu_put_value]
  (binding [lfu_put_e nil lfu_put_entries nil lfu_put_idx nil lfu_put_new_entry nil lfu_put_new_tick nil] (try (do (set! lfu_put_entries (:entries lfu_put_cache)) (set! lfu_put_idx (find_entry lfu_put_entries lfu_put_key)) (when (not= lfu_put_idx (- 0 1)) (do (set! lfu_put_e (get lfu_put_entries lfu_put_idx)) (set! lfu_put_e (assoc lfu_put_e :val lfu_put_value)) (set! lfu_put_e (assoc lfu_put_e :freq (+ (:freq lfu_put_e) 1))) (set! lfu_put_new_tick (+ (:tick lfu_put_cache) 1)) (set! lfu_put_e (assoc lfu_put_e :order lfu_put_new_tick)) (set! lfu_put_entries (assoc lfu_put_entries lfu_put_idx lfu_put_e)) (throw (ex-info "return" {:v {:capacity (:capacity lfu_put_cache) :entries lfu_put_entries :hits (:hits lfu_put_cache) :miss (:miss lfu_put_cache) :tick lfu_put_new_tick}})))) (when (>= (count lfu_put_entries) (:capacity lfu_put_cache)) (set! lfu_put_entries (remove_lfu lfu_put_entries))) (set! lfu_put_new_tick (+ (:tick lfu_put_cache) 1)) (set! lfu_put_new_entry {:freq 1 :key lfu_put_key :order lfu_put_new_tick :val lfu_put_value}) (set! lfu_put_entries (conj lfu_put_entries lfu_put_new_entry)) (throw (ex-info "return" {:v {:capacity (:capacity lfu_put_cache) :entries lfu_put_entries :hits (:hits lfu_put_cache) :miss (:miss lfu_put_cache) :tick lfu_put_new_tick}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn cache_info [cache_info_cache]
  (try (throw (ex-info "return" {:v (str (str (str (str (str (str (str (str "CacheInfo(hits=" (str (:hits cache_info_cache))) ", misses=") (str (:miss cache_info_cache))) ", capacity=") (str (:capacity cache_info_cache))) ", current_size=") (str (count (:entries cache_info_cache)))) ")")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (binding [main_cache nil main_r nil] (do (set! main_cache (lfu_new 2)) (set! main_cache (lfu_put main_cache 1 1)) (set! main_cache (lfu_put main_cache 2 2)) (set! main_r (lfu_get main_cache 1)) (set! main_cache (:cache main_r)) (if (:ok main_r) (println (str (:value main_r))) (println "None")) (set! main_cache (lfu_put main_cache 3 3)) (set! main_r (lfu_get main_cache 2)) (set! main_cache (:cache main_r)) (if (:ok main_r) (println (str (:value main_r))) (println "None")) (set! main_cache (lfu_put main_cache 4 4)) (set! main_r (lfu_get main_cache 1)) (set! main_cache (:cache main_r)) (if (:ok main_r) (println (str (:value main_r))) (println "None")) (set! main_r (lfu_get main_cache 3)) (set! main_cache (:cache main_r)) (if (:ok main_r) (println (str (:value main_r))) (println "None")) (set! main_r (lfu_get main_cache 4)) (set! main_cache (:cache main_r)) (if (:ok main_r) (println (str (:value main_r))) (println "None")) (println (cache_info main_cache)))))

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
