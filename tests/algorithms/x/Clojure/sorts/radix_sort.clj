(ns main (:refer-clojure :exclude [make_buckets max_value radix_sort]))

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

(declare make_buckets max_value radix_sort)

(def ^:dynamic make_buckets_buckets nil)

(def ^:dynamic make_buckets_i nil)

(def ^:dynamic max_value_i nil)

(def ^:dynamic max_value_max_val nil)

(def ^:dynamic radix_sort_a nil)

(def ^:dynamic radix_sort_b nil)

(def ^:dynamic radix_sort_bucket nil)

(def ^:dynamic radix_sort_buckets nil)

(def ^:dynamic radix_sort_i nil)

(def ^:dynamic radix_sort_j nil)

(def ^:dynamic radix_sort_list_of_ints nil)

(def ^:dynamic radix_sort_max_digit nil)

(def ^:dynamic radix_sort_placement nil)

(def ^:dynamic radix_sort_tmp nil)

(def ^:dynamic radix_sort_value nil)

(def ^:dynamic main_RADIX 10)

(defn make_buckets []
  (binding [make_buckets_buckets nil make_buckets_i nil] (try (do (set! make_buckets_buckets []) (set! make_buckets_i 0) (while (< make_buckets_i main_RADIX) (do (set! make_buckets_buckets (conj make_buckets_buckets [])) (set! make_buckets_i (+ make_buckets_i 1)))) (throw (ex-info "return" {:v make_buckets_buckets}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_value [max_value_xs]
  (binding [max_value_i nil max_value_max_val nil] (try (do (set! max_value_max_val (nth max_value_xs 0)) (set! max_value_i 1) (while (< max_value_i (count max_value_xs)) (do (when (> (nth max_value_xs max_value_i) max_value_max_val) (set! max_value_max_val (nth max_value_xs max_value_i))) (set! max_value_i (+ max_value_i 1)))) (throw (ex-info "return" {:v max_value_max_val}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn radix_sort [radix_sort_list_of_ints_p]
  (binding [radix_sort_a nil radix_sort_b nil radix_sort_bucket nil radix_sort_buckets nil radix_sort_i nil radix_sort_j nil radix_sort_list_of_ints nil radix_sort_max_digit nil radix_sort_placement nil radix_sort_tmp nil radix_sort_value nil] (try (do (set! radix_sort_list_of_ints radix_sort_list_of_ints_p) (set! radix_sort_placement 1) (set! radix_sort_max_digit (max_value radix_sort_list_of_ints)) (while (<= radix_sort_placement radix_sort_max_digit) (do (set! radix_sort_buckets (make_buckets)) (set! radix_sort_i 0) (while (< radix_sort_i (count radix_sort_list_of_ints)) (do (set! radix_sort_value (nth radix_sort_list_of_ints radix_sort_i)) (set! radix_sort_tmp (mod (/ radix_sort_value radix_sort_placement) main_RADIX)) (set! radix_sort_buckets (assoc radix_sort_buckets radix_sort_tmp (conj (nth radix_sort_buckets radix_sort_tmp) radix_sort_value))) (set! radix_sort_i (+ radix_sort_i 1)))) (set! radix_sort_a 0) (set! radix_sort_b 0) (while (< radix_sort_b main_RADIX) (do (set! radix_sort_bucket (nth radix_sort_buckets radix_sort_b)) (set! radix_sort_j 0) (while (< radix_sort_j (count radix_sort_bucket)) (do (set! radix_sort_list_of_ints (assoc radix_sort_list_of_ints radix_sort_a (nth radix_sort_bucket radix_sort_j))) (set! radix_sort_a (+ radix_sort_a 1)) (set! radix_sort_j (+ radix_sort_j 1)))) (set! radix_sort_b (+ radix_sort_b 1)))) (set! radix_sort_placement (* radix_sort_placement main_RADIX)))) (throw (ex-info "return" {:v radix_sort_list_of_ints}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (radix_sort [0 5 3 2 2])))
      (println (str (radix_sort [1 100 10 1000])))
      (println (str (radix_sort [15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
