(ns main (:refer-clojure :exclude [rand rand_range shuffle is_sorted bogo_sort]))

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

(declare rand rand_range shuffle is_sorted bogo_sort)

(def ^:dynamic bogo_sort_res nil)

(def ^:dynamic is_sorted_i nil)

(def ^:dynamic shuffle_i nil)

(def ^:dynamic shuffle_j nil)

(def ^:dynamic shuffle_list_int nil)

(def ^:dynamic shuffle_tmp nil)

(def ^:dynamic main_seed 1)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rand_range [rand_range_max]
  (try (throw (ex-info "return" {:v (mod (rand) rand_range_max)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn shuffle [shuffle_list_int_p]
  (binding [shuffle_i nil shuffle_j nil shuffle_list_int nil shuffle_tmp nil] (try (do (set! shuffle_list_int shuffle_list_int_p) (set! shuffle_i (- (count shuffle_list_int) 1)) (while (> shuffle_i 0) (do (set! shuffle_j (rand_range (+ shuffle_i 1))) (set! shuffle_tmp (nth shuffle_list_int shuffle_i)) (set! shuffle_list_int (assoc shuffle_list_int shuffle_i (nth shuffle_list_int shuffle_j))) (set! shuffle_list_int (assoc shuffle_list_int shuffle_j shuffle_tmp)) (set! shuffle_i (- shuffle_i 1)))) (throw (ex-info "return" {:v shuffle_list_int}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_sorted [is_sorted_list_int]
  (binding [is_sorted_i nil] (try (do (set! is_sorted_i 0) (while (< is_sorted_i (- (count is_sorted_list_int) 1)) (do (when (> (nth is_sorted_list_int is_sorted_i) (nth is_sorted_list_int (+ is_sorted_i 1))) (throw (ex-info "return" {:v false}))) (set! is_sorted_i (+ is_sorted_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bogo_sort [bogo_sort_list_int]
  (binding [bogo_sort_res nil] (try (do (set! bogo_sort_res bogo_sort_list_int) (while (not (is_sorted bogo_sort_res)) (set! bogo_sort_res (shuffle bogo_sort_res))) (throw (ex-info "return" {:v bogo_sort_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_data [3 2 1])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (bogo_sort main_data)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
