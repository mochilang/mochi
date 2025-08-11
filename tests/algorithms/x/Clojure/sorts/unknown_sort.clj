(ns main (:refer-clojure :exclude [list_min list_max remove_once reverse_list merge_sort test_merge_sort main]))

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

(declare list_min list_max remove_once reverse_list merge_sort test_merge_sort main)

(def ^:dynamic list_max_i nil)

(def ^:dynamic list_max_m nil)

(def ^:dynamic list_min_i nil)

(def ^:dynamic list_min_m nil)

(def ^:dynamic merge_sort_coll nil)

(def ^:dynamic merge_sort_end nil)

(def ^:dynamic merge_sort_mn nil)

(def ^:dynamic merge_sort_mx nil)

(def ^:dynamic merge_sort_start nil)

(def ^:dynamic remove_once_i nil)

(def ^:dynamic remove_once_removed nil)

(def ^:dynamic remove_once_res nil)

(def ^:dynamic reverse_list_i nil)

(def ^:dynamic reverse_list_res nil)

(defn list_min [list_min_xs]
  (binding [list_min_i nil list_min_m nil] (try (do (set! list_min_i 1) (set! list_min_m (nth list_min_xs 0)) (while (< list_min_i (count list_min_xs)) (do (when (< (nth list_min_xs list_min_i) list_min_m) (set! list_min_m (nth list_min_xs list_min_i))) (set! list_min_i (+ list_min_i 1)))) (throw (ex-info "return" {:v list_min_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_max [list_max_xs]
  (binding [list_max_i nil list_max_m nil] (try (do (set! list_max_i 1) (set! list_max_m (nth list_max_xs 0)) (while (< list_max_i (count list_max_xs)) (do (when (> (nth list_max_xs list_max_i) list_max_m) (set! list_max_m (nth list_max_xs list_max_i))) (set! list_max_i (+ list_max_i 1)))) (throw (ex-info "return" {:v list_max_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_once [remove_once_xs remove_once_value]
  (binding [remove_once_i nil remove_once_removed nil remove_once_res nil] (try (do (set! remove_once_res []) (set! remove_once_removed false) (set! remove_once_i 0) (while (< remove_once_i (count remove_once_xs)) (do (if (and (not remove_once_removed) (= (nth remove_once_xs remove_once_i) remove_once_value)) (set! remove_once_removed true) (set! remove_once_res (conj remove_once_res (nth remove_once_xs remove_once_i)))) (set! remove_once_i (+ remove_once_i 1)))) (throw (ex-info "return" {:v remove_once_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_list [reverse_list_xs]
  (binding [reverse_list_i nil reverse_list_res nil] (try (do (set! reverse_list_res []) (set! reverse_list_i (- (count reverse_list_xs) 1)) (while (>= reverse_list_i 0) (do (set! reverse_list_res (conj reverse_list_res (nth reverse_list_xs reverse_list_i))) (set! reverse_list_i (- reverse_list_i 1)))) (throw (ex-info "return" {:v reverse_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn merge_sort [merge_sort_collection]
  (binding [merge_sort_coll nil merge_sort_end nil merge_sort_mn nil merge_sort_mx nil merge_sort_start nil] (try (do (set! merge_sort_start []) (set! merge_sort_end []) (set! merge_sort_coll merge_sort_collection) (while (> (count merge_sort_coll) 1) (do (set! merge_sort_mn (list_min merge_sort_coll)) (set! merge_sort_mx (list_max merge_sort_coll)) (set! merge_sort_start (conj merge_sort_start merge_sort_mn)) (set! merge_sort_end (conj merge_sort_end merge_sort_mx)) (set! merge_sort_coll (remove_once merge_sort_coll merge_sort_mn)) (set! merge_sort_coll (remove_once merge_sort_coll merge_sort_mx)))) (set! merge_sort_end (reverse_list merge_sort_end)) (throw (ex-info "return" {:v (concat (concat merge_sort_start merge_sort_coll) merge_sort_end)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_merge_sort []
  (do (when (not= (merge_sort [0 5 3 2 2]) [0 2 2 3 5]) (throw (Exception. "case1 failed"))) (when (not= (merge_sort []) []) (throw (Exception. "case2 failed"))) (when (not= (merge_sort [(- 2) (- 5) (- 45)]) [(- 45) (- 5) (- 2)]) (throw (Exception. "case3 failed")))))

(defn main []
  (do (test_merge_sort) (println (str (merge_sort [0 5 3 2 2])))))

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
