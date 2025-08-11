(ns main (:refer-clojure :exclude [make_list min_value max_value pigeon_sort]))

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

(declare make_list min_value max_value pigeon_sort)

(def ^:dynamic make_list_i nil)

(def ^:dynamic make_list_result nil)

(def ^:dynamic max_value_i nil)

(def ^:dynamic max_value_m nil)

(def ^:dynamic min_value_i nil)

(def ^:dynamic min_value_m nil)

(def ^:dynamic pigeon_sort_array nil)

(def ^:dynamic pigeon_sort_array_index nil)

(def ^:dynamic pigeon_sort_h nil)

(def ^:dynamic pigeon_sort_holes nil)

(def ^:dynamic pigeon_sort_holes_range nil)

(def ^:dynamic pigeon_sort_holes_repeat nil)

(def ^:dynamic pigeon_sort_i nil)

(def ^:dynamic pigeon_sort_index nil)

(def ^:dynamic pigeon_sort_mn nil)

(def ^:dynamic pigeon_sort_mx nil)

(defn make_list [make_list_n make_list_value]
  (binding [make_list_i nil make_list_result nil] (try (do (set! make_list_result []) (set! make_list_i 0) (while (< make_list_i make_list_n) (do (set! make_list_result (conj make_list_result make_list_value)) (set! make_list_i (+ make_list_i 1)))) (throw (ex-info "return" {:v make_list_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn min_value [min_value_arr]
  (binding [min_value_i nil min_value_m nil] (try (do (set! min_value_m (nth min_value_arr 0)) (set! min_value_i 1) (while (< min_value_i (count min_value_arr)) (do (when (< (nth min_value_arr min_value_i) min_value_m) (set! min_value_m (nth min_value_arr min_value_i))) (set! min_value_i (+ min_value_i 1)))) (throw (ex-info "return" {:v min_value_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_value [max_value_arr]
  (binding [max_value_i nil max_value_m nil] (try (do (set! max_value_m (nth max_value_arr 0)) (set! max_value_i 1) (while (< max_value_i (count max_value_arr)) (do (when (> (nth max_value_arr max_value_i) max_value_m) (set! max_value_m (nth max_value_arr max_value_i))) (set! max_value_i (+ max_value_i 1)))) (throw (ex-info "return" {:v max_value_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pigeon_sort [pigeon_sort_array_p]
  (binding [pigeon_sort_array nil pigeon_sort_array_index nil pigeon_sort_h nil pigeon_sort_holes nil pigeon_sort_holes_range nil pigeon_sort_holes_repeat nil pigeon_sort_i nil pigeon_sort_index nil pigeon_sort_mn nil pigeon_sort_mx nil] (try (do (set! pigeon_sort_array pigeon_sort_array_p) (when (= (count pigeon_sort_array) 0) (throw (ex-info "return" {:v pigeon_sort_array}))) (set! pigeon_sort_mn (min_value pigeon_sort_array)) (set! pigeon_sort_mx (max_value pigeon_sort_array)) (set! pigeon_sort_holes_range (+ (- pigeon_sort_mx pigeon_sort_mn) 1)) (set! pigeon_sort_holes (make_list pigeon_sort_holes_range 0)) (set! pigeon_sort_holes_repeat (make_list pigeon_sort_holes_range 0)) (set! pigeon_sort_i 0) (while (< pigeon_sort_i (count pigeon_sort_array)) (do (set! pigeon_sort_index (- (nth pigeon_sort_array pigeon_sort_i) pigeon_sort_mn)) (set! pigeon_sort_holes (assoc pigeon_sort_holes pigeon_sort_index (nth pigeon_sort_array pigeon_sort_i))) (set! pigeon_sort_holes_repeat (assoc pigeon_sort_holes_repeat pigeon_sort_index (+ (nth pigeon_sort_holes_repeat pigeon_sort_index) 1))) (set! pigeon_sort_i (+ pigeon_sort_i 1)))) (set! pigeon_sort_array_index 0) (set! pigeon_sort_h 0) (while (< pigeon_sort_h pigeon_sort_holes_range) (do (while (> (nth pigeon_sort_holes_repeat pigeon_sort_h) 0) (do (set! pigeon_sort_array (assoc pigeon_sort_array pigeon_sort_array_index (nth pigeon_sort_holes pigeon_sort_h))) (set! pigeon_sort_array_index (+ pigeon_sort_array_index 1)) (set! pigeon_sort_holes_repeat (assoc pigeon_sort_holes_repeat pigeon_sort_h (- (nth pigeon_sort_holes_repeat pigeon_sort_h) 1))))) (set! pigeon_sort_h (+ pigeon_sort_h 1)))) (throw (ex-info "return" {:v pigeon_sort_array}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (pigeon_sort [0 5 3 2 2])))
      (println (str (pigeon_sort [])))
      (println (str (pigeon_sort [(- 2) (- 5) (- 45)])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
