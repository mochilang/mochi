(ns main (:refer-clojure :exclude [new_heap swap sift_up sift_down insert peek is_empty delete_min main]))

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

(declare new_heap swap sift_up sift_down insert peek is_empty delete_min main)

(declare _read_file)

(def ^:dynamic delete_min_d nil)

(def ^:dynamic delete_min_min nil)

(def ^:dynamic insert_d nil)

(def ^:dynamic main_d1 nil)

(def ^:dynamic main_d2 nil)

(def ^:dynamic main_d3 nil)

(def ^:dynamic main_h nil)

(def ^:dynamic sift_down_i nil)

(def ^:dynamic sift_down_left nil)

(def ^:dynamic sift_down_n nil)

(def ^:dynamic sift_down_right nil)

(def ^:dynamic sift_down_smallest nil)

(def ^:dynamic sift_up_i nil)

(def ^:dynamic sift_up_parent nil)

(def ^:dynamic swap_data nil)

(def ^:dynamic swap_tmp nil)

(defn new_heap []
  (try (throw (ex-info "return" {:v {:data []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn swap [swap_data_p swap_i swap_j]
  (binding [swap_data swap_data_p swap_tmp nil] (do (try (do (set! swap_tmp (nth swap_data swap_i)) (set! swap_data (assoc swap_data swap_i (nth swap_data swap_j))) (set! swap_data (assoc swap_data swap_j swap_tmp))) (finally (alter-var-root (var swap_data) (constantly swap_data)))) swap_data)))

(defn sift_up [sift_up_data sift_up_idx]
  (binding [sift_up_i nil sift_up_parent nil] (do (set! sift_up_i sift_up_idx) (loop [while_flag_1 true] (when (and while_flag_1 (> sift_up_i 0)) (do (set! sift_up_parent (quot (- sift_up_i 1) 2)) (if (<= (nth sift_up_data sift_up_parent) (nth sift_up_data sift_up_i)) (recur false) (do (let [__res (swap sift_up_data sift_up_parent sift_up_i)] (do (set! sift_up_data swap_data) __res)) (set! sift_up_i sift_up_parent))) (cond :else (recur while_flag_1))))) sift_up_data)))

(defn sift_down [sift_down_data sift_down_idx]
  (binding [sift_down_i nil sift_down_left nil sift_down_n nil sift_down_right nil sift_down_smallest nil] (do (set! sift_down_i sift_down_idx) (set! sift_down_n (count sift_down_data)) (loop [while_flag_2 true] (when (and while_flag_2 true) (do (set! sift_down_left (+ (* 2 sift_down_i) 1)) (set! sift_down_right (+ sift_down_left 1)) (set! sift_down_smallest sift_down_i) (when (and (< sift_down_left sift_down_n) (< (nth sift_down_data sift_down_left) (nth sift_down_data sift_down_smallest))) (set! sift_down_smallest sift_down_left)) (when (and (< sift_down_right sift_down_n) (< (nth sift_down_data sift_down_right) (nth sift_down_data sift_down_smallest))) (set! sift_down_smallest sift_down_right)) (if (= sift_down_smallest sift_down_i) (recur false) (do (let [__res (swap sift_down_data sift_down_i sift_down_smallest)] (do (set! sift_down_data swap_data) __res)) (set! sift_down_i sift_down_smallest))) (cond :else (recur while_flag_2))))) sift_down_data)))

(defn insert [insert_heap insert_v]
  (binding [insert_d nil] (try (do (set! insert_d (:data insert_heap)) (set! insert_d (conj insert_d insert_v)) (sift_up insert_d (- (count insert_d) 1)) (throw (ex-info "return" {:v {:data insert_d}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn peek [peek_heap]
  (try (throw (ex-info "return" {:v (get (:data peek_heap) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_empty [is_empty_heap]
  (try (throw (ex-info "return" {:v (= (count (:data is_empty_heap)) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn delete_min [delete_min_heap]
  (binding [delete_min_d nil delete_min_min nil] (try (do (set! delete_min_d (:data delete_min_heap)) (set! delete_min_min (get delete_min_d 0)) (set! delete_min_d (assoc delete_min_d 0 (get delete_min_d (- (count delete_min_d) 1)))) (set! delete_min_d (subvec delete_min_d 0 (- (count delete_min_d) 1))) (when (> (count delete_min_d) 0) (sift_down delete_min_d 0)) (throw (ex-info "return" {:v {:heap {:data delete_min_d} :value delete_min_min}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_d1 nil main_d2 nil main_d3 nil main_h nil] (do (set! main_h (new_heap)) (set! main_h (insert main_h 10)) (set! main_h (insert main_h 3)) (set! main_h (insert main_h 7)) (println (mochi_str (peek main_h))) (set! main_d1 (delete_min main_h)) (set! main_h (:heap main_d1)) (println (mochi_str (:value main_d1))) (set! main_d2 (delete_min main_h)) (set! main_h (:heap main_d2)) (println (mochi_str (:value main_d2))) (set! main_d3 (delete_min main_h)) (set! main_h (:heap main_d3)) (println (mochi_str (:value main_d3))))))

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
