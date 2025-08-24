(ns main (:refer-clojure :exclude [set_seed randint rand_bool new_heap merge insert top pop is_empty to_sorted_list]))

(require 'clojure.set)

(defrecord Node [value left right])

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

(declare set_seed randint rand_bool new_heap merge insert top pop is_empty to_sorted_list)

(def ^:dynamic insert_idx nil)

(def ^:dynamic insert_node nil)

(def ^:dynamic merge_r1 nil)

(def ^:dynamic merge_r2 nil)

(def ^:dynamic merge_tmp nil)

(def ^:dynamic pop_l nil)

(def ^:dynamic pop_r nil)

(def ^:dynamic pop_result nil)

(def ^:dynamic to_sorted_list_res nil)

(declare _read_file)

(def ^:dynamic main_NIL nil)

(def ^:dynamic main_seed nil)

(defn set_seed [set_seed_s]
  (do (alter-var-root (var main_seed) (fn [_] set_seed_s)) set_seed_s))

(defn randint [randint_a randint_b]
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v (+ (mod main_seed (+ (- randint_b randint_a) 1)) randint_a)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn rand_bool []
  (try (throw (ex-info "return" {:v (= (randint 0 1) 1)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_nodes nil)

(def ^:dynamic main_root nil)

(defn new_heap []
  (do (alter-var-root (var main_nodes) (fn [_] [])) (alter-var-root (var main_root) (fn [_] main_NIL))))

(defn merge [merge_r1_p merge_r2_p]
  (binding [merge_r1 merge_r1_p merge_r2 merge_r2_p merge_tmp nil] (try (do (when (= merge_r1 main_NIL) (throw (ex-info "return" {:v merge_r2}))) (when (= merge_r2 main_NIL) (throw (ex-info "return" {:v merge_r1}))) (when (> (get (nth main_nodes merge_r1) "value") (get (nth main_nodes merge_r2) "value")) (do (set! merge_tmp merge_r1) (set! merge_r1 merge_r2) (set! merge_r2 merge_tmp))) (when (rand_bool) (do (set! merge_tmp (get (nth main_nodes merge_r1) "left")) (alter-var-root (var main_nodes) (fn [_] (assoc-in main_nodes [merge_r1 "left"] (get (nth main_nodes merge_r1) "right")))) (alter-var-root (var main_nodes) (fn [_] (assoc-in main_nodes [merge_r1 "right"] merge_tmp))))) (alter-var-root (var main_nodes) (fn [_] (assoc-in main_nodes [merge_r1 "left"] (let [__res (merge (get (nth main_nodes merge_r1) "left") merge_r2)] (do (set! merge_r2 merge_r2) __res))))) (throw (ex-info "return" {:v merge_r1}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var merge_r1) (constantly merge_r1)) (alter-var-root (var merge_r2) (constantly merge_r2))))))

(defn insert [insert_value]
  (binding [insert_idx nil insert_node nil] (do (set! insert_node {"left" main_NIL "right" main_NIL "value" insert_value}) (alter-var-root (var main_nodes) (fn [_] (conj main_nodes insert_node))) (set! insert_idx (- (count main_nodes) 1)) (alter-var-root (var main_root) (fn [_] (let [__res (merge main_root insert_idx)] (do (set! main_root merge_r1) (set! insert_idx merge_r2) __res)))) insert_value)))

(defn top []
  (try (if (= main_root main_NIL) 0 (get (nth main_nodes main_root) "value")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pop []
  (binding [pop_l nil pop_r nil pop_result nil] (try (do (set! pop_result (top)) (set! pop_l (get (nth main_nodes main_root) "left")) (set! pop_r (get (nth main_nodes main_root) "right")) (alter-var-root (var main_root) (fn [_] (let [__res (merge pop_l pop_r)] (do (set! pop_l merge_r1) (set! pop_r merge_r2) __res)))) (throw (ex-info "return" {:v pop_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_empty []
  (try (throw (ex-info "return" {:v (= main_root main_NIL)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_sorted_list []
  (binding [to_sorted_list_res nil] (try (do (set! to_sorted_list_res []) (while (not (is_empty)) (set! to_sorted_list_res (conj to_sorted_list_res (pop)))) (throw (ex-info "return" {:v to_sorted_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_NIL) (constantly -1))
      (alter-var-root (var main_seed) (constantly 1))
      (alter-var-root (var main_nodes) (constantly []))
      (alter-var-root (var main_root) (constantly main_NIL))
      (set_seed 1)
      (new_heap)
      (insert 2)
      (insert 3)
      (insert 1)
      (insert 5)
      (insert 1)
      (insert 7)
      (println (to_sorted_list))
      (new_heap)
      (insert 1)
      (insert (- 1))
      (insert 0)
      (println (to_sorted_list))
      (new_heap)
      (insert 3)
      (insert 1)
      (insert 3)
      (insert 7)
      (println (pop))
      (println (pop))
      (println (pop))
      (println (pop))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
