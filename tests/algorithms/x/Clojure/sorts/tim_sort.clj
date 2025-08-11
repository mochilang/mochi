(ns main (:refer-clojure :exclude [copy_list insertion_sort merge tim_sort list_to_string]))

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

(declare copy_list insertion_sort merge tim_sort list_to_string)

(def ^:dynamic copy_list_k nil)

(def ^:dynamic copy_list_res nil)

(def ^:dynamic insertion_sort_arr nil)

(def ^:dynamic insertion_sort_idx nil)

(def ^:dynamic insertion_sort_jdx nil)

(def ^:dynamic insertion_sort_value nil)

(def ^:dynamic list_to_string_k nil)

(def ^:dynamic list_to_string_s nil)

(def ^:dynamic merge_i nil)

(def ^:dynamic merge_j nil)

(def ^:dynamic merge_result nil)

(def ^:dynamic tim_sort_current nil)

(def ^:dynamic tim_sort_i nil)

(def ^:dynamic tim_sort_n nil)

(def ^:dynamic tim_sort_r nil)

(def ^:dynamic tim_sort_result nil)

(def ^:dynamic tim_sort_runs nil)

(def ^:dynamic tim_sort_sorted_runs nil)

(defn copy_list [copy_list_xs]
  (binding [copy_list_k nil copy_list_res nil] (try (do (set! copy_list_res []) (set! copy_list_k 0) (while (< copy_list_k (count copy_list_xs)) (do (set! copy_list_res (conj copy_list_res (nth copy_list_xs copy_list_k))) (set! copy_list_k (+ copy_list_k 1)))) (throw (ex-info "return" {:v copy_list_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn insertion_sort [insertion_sort_xs]
  (binding [insertion_sort_arr nil insertion_sort_idx nil insertion_sort_jdx nil insertion_sort_value nil] (try (do (set! insertion_sort_arr (copy_list insertion_sort_xs)) (set! insertion_sort_idx 1) (while (< insertion_sort_idx (count insertion_sort_arr)) (do (set! insertion_sort_value (nth insertion_sort_arr insertion_sort_idx)) (set! insertion_sort_jdx (- insertion_sort_idx 1)) (while (and (>= insertion_sort_jdx 0) (> (nth insertion_sort_arr insertion_sort_jdx) insertion_sort_value)) (do (set! insertion_sort_arr (assoc insertion_sort_arr (+ insertion_sort_jdx 1) (nth insertion_sort_arr insertion_sort_jdx))) (set! insertion_sort_jdx (- insertion_sort_jdx 1)))) (set! insertion_sort_arr (assoc insertion_sort_arr (+ insertion_sort_jdx 1) insertion_sort_value)) (set! insertion_sort_idx (+ insertion_sort_idx 1)))) (throw (ex-info "return" {:v insertion_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn merge [merge_left merge_right]
  (binding [merge_i nil merge_j nil merge_result nil] (try (do (set! merge_result []) (set! merge_i 0) (set! merge_j 0) (while (and (< merge_i (count merge_left)) (< merge_j (count merge_right))) (if (< (nth merge_left merge_i) (nth merge_right merge_j)) (do (set! merge_result (conj merge_result (nth merge_left merge_i))) (set! merge_i (+ merge_i 1))) (do (set! merge_result (conj merge_result (nth merge_right merge_j))) (set! merge_j (+ merge_j 1))))) (while (< merge_i (count merge_left)) (do (set! merge_result (conj merge_result (nth merge_left merge_i))) (set! merge_i (+ merge_i 1)))) (while (< merge_j (count merge_right)) (do (set! merge_result (conj merge_result (nth merge_right merge_j))) (set! merge_j (+ merge_j 1)))) (throw (ex-info "return" {:v merge_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn tim_sort [tim_sort_xs]
  (binding [tim_sort_current nil tim_sort_i nil tim_sort_n nil tim_sort_r nil tim_sort_result nil tim_sort_runs nil tim_sort_sorted_runs nil] (try (do (set! tim_sort_n (count tim_sort_xs)) (set! tim_sort_runs []) (set! tim_sort_sorted_runs []) (set! tim_sort_current []) (set! tim_sort_current (conj tim_sort_current (nth tim_sort_xs 0))) (set! tim_sort_i 1) (while (< tim_sort_i tim_sort_n) (do (if (< (nth tim_sort_xs tim_sort_i) (nth tim_sort_xs (- tim_sort_i 1))) (do (set! tim_sort_runs (conj tim_sort_runs (copy_list tim_sort_current))) (set! tim_sort_current []) (set! tim_sort_current (conj tim_sort_current (nth tim_sort_xs tim_sort_i)))) (set! tim_sort_current (conj tim_sort_current (nth tim_sort_xs tim_sort_i)))) (set! tim_sort_i (+ tim_sort_i 1)))) (set! tim_sort_runs (conj tim_sort_runs (copy_list tim_sort_current))) (set! tim_sort_r 0) (while (< tim_sort_r (count tim_sort_runs)) (do (set! tim_sort_sorted_runs (conj tim_sort_sorted_runs (insertion_sort (nth tim_sort_runs tim_sort_r)))) (set! tim_sort_r (+ tim_sort_r 1)))) (set! tim_sort_result []) (set! tim_sort_r 0) (while (< tim_sort_r (count tim_sort_sorted_runs)) (do (set! tim_sort_result (merge tim_sort_result (nth tim_sort_sorted_runs tim_sort_r))) (set! tim_sort_r (+ tim_sort_r 1)))) (throw (ex-info "return" {:v tim_sort_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn list_to_string [list_to_string_xs]
  (binding [list_to_string_k nil list_to_string_s nil] (try (do (set! list_to_string_s "[") (set! list_to_string_k 0) (while (< list_to_string_k (count list_to_string_xs)) (do (set! list_to_string_s (str list_to_string_s (str (nth list_to_string_xs list_to_string_k)))) (when (< list_to_string_k (- (count list_to_string_xs) 1)) (set! list_to_string_s (str list_to_string_s ", "))) (set! list_to_string_k (+ list_to_string_k 1)))) (throw (ex-info "return" {:v (str list_to_string_s "]")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_sample [5 9 10 3 (- 4) 5 178 92 46 (- 18) 0 7])

(def ^:dynamic main_sorted_sample (tim_sort main_sample))

(def ^:dynamic main_sample2 [3 2 1])

(def ^:dynamic main_sorted_sample2 (tim_sort main_sample2))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (list_to_string main_sorted_sample))
      (println (list_to_string main_sorted_sample2))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
