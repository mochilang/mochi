(ns main (:refer-clojure :exclude [ratio fractional_cover]))

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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare ratio fractional_cover)

(def ^:dynamic fractional_cover_idx nil)

(def ^:dynamic fractional_cover_item nil)

(def ^:dynamic fractional_cover_remaining nil)

(def ^:dynamic fractional_cover_sorted nil)

(def ^:dynamic fractional_cover_take nil)

(def ^:dynamic fractional_cover_total nil)

(defn ratio [ratio_item]
  (try (throw (ex-info "return" {:v (quot (double (:value ratio_item)) (double (:weight ratio_item)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fractional_cover [fractional_cover_items fractional_cover_capacity]
  (binding [fractional_cover_idx nil fractional_cover_item nil fractional_cover_remaining nil fractional_cover_sorted nil fractional_cover_take nil fractional_cover_total nil] (try (do (when (< fractional_cover_capacity 0) (throw (Exception. "Capacity cannot be negative"))) (set! fractional_cover_total 0.0) (set! fractional_cover_remaining fractional_cover_capacity) (set! fractional_cover_sorted (for [it (sort-by (fn [it] (- (ratio it))) fractional_cover_items)] it)) (set! fractional_cover_idx 0) (while (and (< fractional_cover_idx (count fractional_cover_sorted)) (> fractional_cover_remaining 0)) (do (set! fractional_cover_item (nth fractional_cover_sorted fractional_cover_idx)) (set! fractional_cover_take (if (< (:weight fractional_cover_item) fractional_cover_remaining) (:weight fractional_cover_item) fractional_cover_remaining)) (set! fractional_cover_total (+ fractional_cover_total (* (double fractional_cover_take) (ratio fractional_cover_item)))) (set! fractional_cover_remaining (- fractional_cover_remaining fractional_cover_take)) (set! fractional_cover_idx (+ fractional_cover_idx 1)))) (throw (ex-info "return" {:v fractional_cover_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_items1 [{:value 60 :weight 10} {:value 100 :weight 20} {:value 120 :weight 30}])

(def ^:dynamic main_items2 [{:value 100 :weight 20} {:value 120 :weight 30} {:value 60 :weight 10}])

(def ^:dynamic main_items3 [])

(def ^:dynamic main_items4 [{:value 60 :weight 10}])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (fractional_cover main_items1 50)))
      (println (str (fractional_cover main_items2 25)))
      (println (str (fractional_cover main_items3 50)))
      (println (str (fractional_cover main_items4 5)))
      (println (str (fractional_cover main_items4 1)))
      (println (str (fractional_cover main_items4 0)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
