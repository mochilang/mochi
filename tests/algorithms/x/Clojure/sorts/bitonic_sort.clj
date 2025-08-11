(ns main (:refer-clojure :exclude [set_at_int comp_and_swap bitonic_merge bitonic_sort main]))

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

(declare set_at_int comp_and_swap bitonic_merge bitonic_sort main)

(def ^:dynamic bitonic_merge_k nil)

(def ^:dynamic bitonic_merge_mid nil)

(def ^:dynamic bitonic_merge_res nil)

(def ^:dynamic bitonic_sort_mid nil)

(def ^:dynamic bitonic_sort_res nil)

(def ^:dynamic comp_and_swap_res nil)

(def ^:dynamic comp_and_swap_xi nil)

(def ^:dynamic comp_and_swap_xj nil)

(def ^:dynamic main_asc nil)

(def ^:dynamic main_data nil)

(def ^:dynamic main_desc nil)

(def ^:dynamic set_at_int_i nil)

(def ^:dynamic set_at_int_res nil)

(defn set_at_int [set_at_int_xs set_at_int_idx set_at_int_value]
  (binding [set_at_int_i nil set_at_int_res nil] (try (do (set! set_at_int_res []) (set! set_at_int_i 0) (while (< set_at_int_i (count set_at_int_xs)) (do (if (= set_at_int_i set_at_int_idx) (set! set_at_int_res (conj set_at_int_res set_at_int_value)) (set! set_at_int_res (conj set_at_int_res (nth set_at_int_xs set_at_int_i)))) (set! set_at_int_i (+ set_at_int_i 1)))) (throw (ex-info "return" {:v set_at_int_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn comp_and_swap [comp_and_swap_arr comp_and_swap_i comp_and_swap_j comp_and_swap_dir]
  (binding [comp_and_swap_res nil comp_and_swap_xi nil comp_and_swap_xj nil] (try (do (set! comp_and_swap_res comp_and_swap_arr) (set! comp_and_swap_xi (nth comp_and_swap_arr comp_and_swap_i)) (set! comp_and_swap_xj (nth comp_and_swap_arr comp_and_swap_j)) (when (or (and (= comp_and_swap_dir 1) (> comp_and_swap_xi comp_and_swap_xj)) (and (= comp_and_swap_dir 0) (< comp_and_swap_xi comp_and_swap_xj))) (do (set! comp_and_swap_res (set_at_int comp_and_swap_res comp_and_swap_i comp_and_swap_xj)) (set! comp_and_swap_res (set_at_int comp_and_swap_res comp_and_swap_j comp_and_swap_xi)))) (throw (ex-info "return" {:v comp_and_swap_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bitonic_merge [bitonic_merge_arr bitonic_merge_low bitonic_merge_length bitonic_merge_dir]
  (binding [bitonic_merge_k nil bitonic_merge_mid nil bitonic_merge_res nil] (try (do (set! bitonic_merge_res bitonic_merge_arr) (when (> bitonic_merge_length 1) (do (set! bitonic_merge_mid (/ bitonic_merge_length 2)) (set! bitonic_merge_k bitonic_merge_low) (while (< bitonic_merge_k (+ bitonic_merge_low bitonic_merge_mid)) (do (set! bitonic_merge_res (comp_and_swap bitonic_merge_res bitonic_merge_k (+ bitonic_merge_k bitonic_merge_mid) bitonic_merge_dir)) (set! bitonic_merge_k (+ bitonic_merge_k 1)))) (set! bitonic_merge_res (bitonic_merge bitonic_merge_res bitonic_merge_low bitonic_merge_mid bitonic_merge_dir)) (set! bitonic_merge_res (bitonic_merge bitonic_merge_res (+ bitonic_merge_low bitonic_merge_mid) bitonic_merge_mid bitonic_merge_dir)))) (throw (ex-info "return" {:v bitonic_merge_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn bitonic_sort [bitonic_sort_arr bitonic_sort_low bitonic_sort_length bitonic_sort_dir]
  (binding [bitonic_sort_mid nil bitonic_sort_res nil] (try (do (set! bitonic_sort_res bitonic_sort_arr) (when (> bitonic_sort_length 1) (do (set! bitonic_sort_mid (/ bitonic_sort_length 2)) (set! bitonic_sort_res (bitonic_sort bitonic_sort_res bitonic_sort_low bitonic_sort_mid 1)) (set! bitonic_sort_res (bitonic_sort bitonic_sort_res (+ bitonic_sort_low bitonic_sort_mid) bitonic_sort_mid 0)) (set! bitonic_sort_res (bitonic_merge bitonic_sort_res bitonic_sort_low bitonic_sort_length bitonic_sort_dir)))) (throw (ex-info "return" {:v bitonic_sort_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_asc nil main_data nil main_desc nil] (do (set! main_data [12 34 92 (- 23) 0 (- 121) (- 167) 145]) (set! main_asc (bitonic_sort main_data 0 (count main_data) 1)) (println (str main_asc)) (set! main_desc (bitonic_merge main_asc 0 (count main_asc) 0)) (println (str main_desc)))))

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
