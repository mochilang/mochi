(ns main (:refer-clojure :exclude [insert_next rec_insertion_sort test_rec_insertion_sort main]))

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

(declare insert_next rec_insertion_sort test_rec_insertion_sort main)

(def ^:dynamic insert_next_arr nil)

(def ^:dynamic insert_next_j nil)

(def ^:dynamic insert_next_temp nil)

(def ^:dynamic main_numbers nil)

(def ^:dynamic rec_insertion_sort_arr nil)

(def ^:dynamic test_rec_insertion_sort_col1 nil)

(def ^:dynamic test_rec_insertion_sort_col2 nil)

(def ^:dynamic test_rec_insertion_sort_col3 nil)

(defn insert_next [insert_next_collection insert_next_index]
  (binding [insert_next_arr nil insert_next_j nil insert_next_temp nil] (try (do (set! insert_next_arr insert_next_collection) (when (or (>= insert_next_index (count insert_next_arr)) (<= (nth insert_next_arr (- insert_next_index 1)) (nth insert_next_arr insert_next_index))) (throw (ex-info "return" {:v insert_next_arr}))) (set! insert_next_j (- insert_next_index 1)) (set! insert_next_temp (nth insert_next_arr insert_next_j)) (set! insert_next_arr (assoc insert_next_arr insert_next_j (nth insert_next_arr insert_next_index))) (set! insert_next_arr (assoc insert_next_arr insert_next_index insert_next_temp)) (throw (ex-info "return" {:v (insert_next insert_next_arr (+ insert_next_index 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rec_insertion_sort [rec_insertion_sort_collection rec_insertion_sort_n]
  (binding [rec_insertion_sort_arr nil] (try (do (set! rec_insertion_sort_arr rec_insertion_sort_collection) (when (or (<= (count rec_insertion_sort_arr) 1) (<= rec_insertion_sort_n 1)) (throw (ex-info "return" {:v rec_insertion_sort_arr}))) (set! rec_insertion_sort_arr (insert_next rec_insertion_sort_arr (- rec_insertion_sort_n 1))) (throw (ex-info "return" {:v (rec_insertion_sort rec_insertion_sort_arr (- rec_insertion_sort_n 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_rec_insertion_sort []
  (binding [test_rec_insertion_sort_col1 nil test_rec_insertion_sort_col2 nil test_rec_insertion_sort_col3 nil] (do (set! test_rec_insertion_sort_col1 [1 2 1]) (set! test_rec_insertion_sort_col1 (rec_insertion_sort test_rec_insertion_sort_col1 (count test_rec_insertion_sort_col1))) (when (or (or (not= (nth test_rec_insertion_sort_col1 0) 1) (not= (nth test_rec_insertion_sort_col1 1) 1)) (not= (nth test_rec_insertion_sort_col1 2) 2)) (throw (Exception. "test1 failed"))) (set! test_rec_insertion_sort_col2 [2 1 0 (- 1) (- 2)]) (set! test_rec_insertion_sort_col2 (rec_insertion_sort test_rec_insertion_sort_col2 (count test_rec_insertion_sort_col2))) (when (not= (nth test_rec_insertion_sort_col2 0) (- 0 2)) (throw (Exception. "test2 failed"))) (when (not= (nth test_rec_insertion_sort_col2 1) (- 0 1)) (throw (Exception. "test2 failed"))) (when (not= (nth test_rec_insertion_sort_col2 2) 0) (throw (Exception. "test2 failed"))) (when (not= (nth test_rec_insertion_sort_col2 3) 1) (throw (Exception. "test2 failed"))) (when (not= (nth test_rec_insertion_sort_col2 4) 2) (throw (Exception. "test2 failed"))) (set! test_rec_insertion_sort_col3 [1]) (set! test_rec_insertion_sort_col3 (rec_insertion_sort test_rec_insertion_sort_col3 (count test_rec_insertion_sort_col3))) (when (not= (nth test_rec_insertion_sort_col3 0) 1) (throw (Exception. "test3 failed"))))))

(defn main []
  (binding [main_numbers nil] (do (test_rec_insertion_sort) (set! main_numbers [5 3 4 1 2]) (set! main_numbers (rec_insertion_sort main_numbers (count main_numbers))) (println (str main_numbers)))))

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
