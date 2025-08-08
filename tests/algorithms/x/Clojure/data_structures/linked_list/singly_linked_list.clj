(ns main (:refer-clojure :exclude [empty_list length is_empty to_string insert_nth insert_head insert_tail delete_nth delete_head delete_tail get_item set_item reverse_list main]))

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

(declare empty_list length is_empty to_string insert_nth insert_head insert_tail delete_nth delete_head delete_tail get_item set_item reverse_list main)

(def ^:dynamic delete_nth_i nil)

(def ^:dynamic delete_nth_res nil)

(def ^:dynamic delete_nth_val nil)

(def ^:dynamic insert_nth_i nil)

(def ^:dynamic insert_nth_res nil)

(def ^:dynamic main_del nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_lst nil)

(def ^:dynamic reverse_list_i nil)

(def ^:dynamic reverse_list_res nil)

(def ^:dynamic set_item_i nil)

(def ^:dynamic set_item_res nil)

(def ^:dynamic to_string_i nil)

(def ^:dynamic to_string_s nil)

(defn empty_list []
  (try (throw (ex-info "return" {:v {:data []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn length [length_list]
  (try (throw (ex-info "return" {:v (count (:data length_list))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn is_empty [is_empty_list]
  (try (throw (ex-info "return" {:v (= (count (:data is_empty_list)) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_string [to_string_list]
  (binding [to_string_i nil to_string_s nil] (try (do (when (= (count (:data to_string_list)) 0) (throw (ex-info "return" {:v ""}))) (set! to_string_s (str (get (:data to_string_list) 0))) (set! to_string_i 1) (while (< to_string_i (count (:data to_string_list))) (do (set! to_string_s (str (str to_string_s " -> ") (str (get (:data to_string_list) to_string_i)))) (set! to_string_i (+ to_string_i 1)))) (throw (ex-info "return" {:v to_string_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert_nth [insert_nth_list insert_nth_index insert_nth_value]
  (binding [insert_nth_i nil insert_nth_res nil] (try (do (when (or (< insert_nth_index 0) (> insert_nth_index (count (:data insert_nth_list)))) (throw (Exception. "index out of range"))) (set! insert_nth_res []) (set! insert_nth_i 0) (while (< insert_nth_i insert_nth_index) (do (set! insert_nth_res (conj insert_nth_res (get (:data insert_nth_list) insert_nth_i))) (set! insert_nth_i (+ insert_nth_i 1)))) (set! insert_nth_res (conj insert_nth_res insert_nth_value)) (while (< insert_nth_i (count (:data insert_nth_list))) (do (set! insert_nth_res (conj insert_nth_res (get (:data insert_nth_list) insert_nth_i))) (set! insert_nth_i (+ insert_nth_i 1)))) (throw (ex-info "return" {:v {:data insert_nth_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insert_head [insert_head_list insert_head_value]
  (try (throw (ex-info "return" {:v (insert_nth insert_head_list 0 insert_head_value)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn insert_tail [insert_tail_list insert_tail_value]
  (try (throw (ex-info "return" {:v (insert_nth insert_tail_list (count (:data insert_tail_list)) insert_tail_value)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn delete_nth [delete_nth_list delete_nth_index]
  (binding [delete_nth_i nil delete_nth_res nil delete_nth_val nil] (try (do (when (or (< delete_nth_index 0) (>= delete_nth_index (count (:data delete_nth_list)))) (throw (Exception. "index out of range"))) (set! delete_nth_res []) (set! delete_nth_val 0) (set! delete_nth_i 0) (while (< delete_nth_i (count (:data delete_nth_list))) (do (if (= delete_nth_i delete_nth_index) (set! delete_nth_val (get (:data delete_nth_list) delete_nth_i)) (set! delete_nth_res (conj delete_nth_res (get (:data delete_nth_list) delete_nth_i)))) (set! delete_nth_i (+ delete_nth_i 1)))) (throw (ex-info "return" {:v {:list {:data delete_nth_res} :value delete_nth_val}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn delete_head [delete_head_list]
  (try (throw (ex-info "return" {:v (delete_nth delete_head_list 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn delete_tail [delete_tail_list]
  (try (throw (ex-info "return" {:v (delete_nth delete_tail_list (- (count (:data delete_tail_list)) 1))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn get_item [get_item_list get_item_index]
  (try (do (when (or (< get_item_index 0) (>= get_item_index (count (:data get_item_list)))) (throw (Exception. "index out of range"))) (throw (ex-info "return" {:v (get (:data get_item_list) get_item_index)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn set_item [set_item_list set_item_index set_item_value]
  (binding [set_item_i nil set_item_res nil] (try (do (when (or (< set_item_index 0) (>= set_item_index (count (:data set_item_list)))) (throw (Exception. "index out of range"))) (set! set_item_res []) (set! set_item_i 0) (while (< set_item_i (count (:data set_item_list))) (do (if (= set_item_i set_item_index) (set! set_item_res (conj set_item_res set_item_value)) (set! set_item_res (conj set_item_res (get (:data set_item_list) set_item_i)))) (set! set_item_i (+ set_item_i 1)))) (throw (ex-info "return" {:v {:data set_item_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn reverse_list [reverse_list_list]
  (binding [reverse_list_i nil reverse_list_res nil] (try (do (set! reverse_list_res []) (set! reverse_list_i (- (count (:data reverse_list_list)) 1)) (while (>= reverse_list_i 0) (do (set! reverse_list_res (conj reverse_list_res (get (:data reverse_list_list) reverse_list_i))) (set! reverse_list_i (- reverse_list_i 1)))) (throw (ex-info "return" {:v {:data reverse_list_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_del nil main_i nil main_lst nil] (do (set! main_lst (empty_list)) (set! main_i 1) (while (<= main_i 5) (do (set! main_lst (insert_tail main_lst main_i)) (set! main_i (+ main_i 1)))) (println (to_string main_lst)) (set! main_lst (insert_head main_lst 0)) (println (to_string main_lst)) (set! main_del (delete_head main_lst)) (set! main_lst (:list main_del)) (println (str (:value main_del))) (set! main_del (delete_tail main_lst)) (set! main_lst (:list main_del)) (println (str (:value main_del))) (set! main_del (delete_nth main_lst 2)) (set! main_lst (:list main_del)) (println (str (:value main_del))) (set! main_lst (set_item main_lst 1 99)) (println (str (get_item main_lst 1))) (set! main_lst (reverse_list main_lst)) (println (to_string main_lst)))))

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
