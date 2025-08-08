(ns main (:refer-clojure :exclude [empty_list length is_empty to_string insert_nth insert_head insert_tail delete_nth delete_head delete_tail delete_value main]))

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

(declare empty_list length is_empty to_string insert_nth insert_head insert_tail delete_nth delete_head delete_tail delete_value main)

(def ^:dynamic delete_nth_i nil)

(def ^:dynamic delete_nth_removed nil)

(def ^:dynamic delete_nth_res nil)

(def ^:dynamic delete_value_found nil)

(def ^:dynamic delete_value_idx nil)

(def ^:dynamic insert_nth_i nil)

(def ^:dynamic insert_nth_res nil)

(def ^:dynamic main_dll nil)

(def ^:dynamic main_res nil)

(def ^:dynamic to_string_i nil)

(def ^:dynamic to_string_s nil)

(defn empty_list []
  (try (throw (ex-info "return" {:v {:data []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn length [length_list]
  (try (throw (ex-info "return" {:v (count (:data length_list))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_empty [is_empty_list]
  (try (throw (ex-info "return" {:v (= (count (:data is_empty_list)) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_string [to_string_list]
  (binding [to_string_i nil to_string_s nil] (try (do (when (= (count (:data to_string_list)) 0) (throw (ex-info "return" {:v ""}))) (set! to_string_s (str (get (:data to_string_list) 0))) (set! to_string_i 1) (while (< to_string_i (count (:data to_string_list))) (do (set! to_string_s (str (str to_string_s "->") (str (get (:data to_string_list) to_string_i)))) (set! to_string_i (+ to_string_i 1)))) (throw (ex-info "return" {:v to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert_nth [insert_nth_list insert_nth_index insert_nth_value]
  (binding [insert_nth_i nil insert_nth_res nil] (try (do (when (or (< insert_nth_index 0) (> insert_nth_index (count (:data insert_nth_list)))) (throw (Exception. "index out of range"))) (set! insert_nth_res []) (set! insert_nth_i 0) (while (< insert_nth_i insert_nth_index) (do (set! insert_nth_res (conj insert_nth_res (get (:data insert_nth_list) insert_nth_i))) (set! insert_nth_i (+ insert_nth_i 1)))) (set! insert_nth_res (conj insert_nth_res insert_nth_value)) (while (< insert_nth_i (count (:data insert_nth_list))) (do (set! insert_nth_res (conj insert_nth_res (get (:data insert_nth_list) insert_nth_i))) (set! insert_nth_i (+ insert_nth_i 1)))) (throw (ex-info "return" {:v {:data insert_nth_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert_head [insert_head_list insert_head_value]
  (try (throw (ex-info "return" {:v (insert_nth insert_head_list 0 insert_head_value)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn insert_tail [insert_tail_list insert_tail_value]
  (try (throw (ex-info "return" {:v (insert_nth insert_tail_list (count (:data insert_tail_list)) insert_tail_value)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn delete_nth [delete_nth_list delete_nth_index]
  (binding [delete_nth_i nil delete_nth_removed nil delete_nth_res nil] (try (do (when (or (< delete_nth_index 0) (>= delete_nth_index (count (:data delete_nth_list)))) (throw (Exception. "index out of range"))) (set! delete_nth_res []) (set! delete_nth_i 0) (set! delete_nth_removed 0) (while (< delete_nth_i (count (:data delete_nth_list))) (do (if (= delete_nth_i delete_nth_index) (set! delete_nth_removed (get (:data delete_nth_list) delete_nth_i)) (set! delete_nth_res (conj delete_nth_res (get (:data delete_nth_list) delete_nth_i)))) (set! delete_nth_i (+ delete_nth_i 1)))) (throw (ex-info "return" {:v {:list {:data delete_nth_res} :value delete_nth_removed}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn delete_head [delete_head_list]
  (try (throw (ex-info "return" {:v (delete_nth delete_head_list 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn delete_tail [delete_tail_list]
  (try (throw (ex-info "return" {:v (delete_nth delete_tail_list (- (count (:data delete_tail_list)) 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn delete_value [delete_value_list delete_value_value]
  (binding [delete_value_found nil delete_value_idx nil] (try (do (set! delete_value_idx 0) (set! delete_value_found false) (loop [while_flag_1 true] (when (and while_flag_1 (< delete_value_idx (count (:data delete_value_list)))) (cond (= (get (:data delete_value_list) delete_value_idx) delete_value_value) (do (set! delete_value_found true) (recur false)) :else (do (set! delete_value_idx (+ delete_value_idx 1)) (recur while_flag_1))))) (when (not delete_value_found) (throw (Exception. "value not found"))) (throw (ex-info "return" {:v (delete_nth delete_value_list delete_value_idx)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_dll nil main_res nil] (do (set! main_dll (empty_list)) (set! main_dll (insert_tail main_dll 1)) (set! main_dll (insert_tail main_dll 2)) (set! main_dll (insert_tail main_dll 3)) (println (to_string main_dll)) (set! main_dll (insert_head main_dll 0)) (println (to_string main_dll)) (set! main_dll (insert_nth main_dll 2 9)) (println (to_string main_dll)) (set! main_res (delete_nth main_dll 2)) (set! main_dll (:list main_res)) (println (:value main_res)) (println (to_string main_dll)) (set! main_res (delete_tail main_dll)) (set! main_dll (:list main_res)) (println (:value main_res)) (println (to_string main_dll)) (set! main_res (delete_value main_dll 1)) (set! main_dll (:list main_res)) (println (:value main_res)) (println (to_string main_dll)))))

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
