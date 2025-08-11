(ns main (:refer-clojure :exclude [stooge stooge_sort]))

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

(declare stooge stooge_sort)

(def ^:dynamic stooge_arr nil)

(def ^:dynamic stooge_t nil)

(def ^:dynamic stooge_tmp nil)

(defn stooge [stooge_arr_p stooge_i stooge_h]
  (binding [stooge_arr nil stooge_t nil stooge_tmp nil] (try (do (set! stooge_arr stooge_arr_p) (when (>= stooge_i stooge_h) (throw (ex-info "return" {:v nil}))) (when (> (nth stooge_arr stooge_i) (nth stooge_arr stooge_h)) (do (set! stooge_tmp (nth stooge_arr stooge_i)) (set! stooge_arr (assoc stooge_arr stooge_i (nth stooge_arr stooge_h))) (set! stooge_arr (assoc stooge_arr stooge_h stooge_tmp)))) (when (> (+ (- stooge_h stooge_i) 1) 2) (do (set! stooge_t (long (/ (+ (- stooge_h stooge_i) 1) 3))) (stooge stooge_arr stooge_i (- stooge_h stooge_t)) (stooge stooge_arr (+ stooge_i stooge_t) stooge_h) (stooge stooge_arr stooge_i (- stooge_h stooge_t))))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn stooge_sort [stooge_sort_arr]
  (try (do (stooge stooge_sort_arr 0 (- (count stooge_sort_arr) 1)) (throw (ex-info "return" {:v stooge_sort_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (stooge_sort [18 0 (- 7) (- 1) 2 2])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
