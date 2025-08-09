(ns main (:refer-clojure :exclude [abs_val abs_min abs_max abs_max_sort test_abs_val main]))

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

(declare abs_val abs_min abs_max abs_max_sort test_abs_val main)

(def ^:dynamic abs_max_i nil)

(def ^:dynamic abs_max_idx nil)

(def ^:dynamic abs_max_j nil)

(def ^:dynamic abs_max_sort_a nil)

(def ^:dynamic abs_max_sort_arr nil)

(def ^:dynamic abs_max_sort_b nil)

(def ^:dynamic abs_max_sort_i nil)

(def ^:dynamic abs_max_sort_n nil)

(def ^:dynamic abs_max_sort_temp nil)

(def ^:dynamic abs_min_i nil)

(def ^:dynamic abs_min_idx nil)

(def ^:dynamic abs_min_j nil)

(def ^:dynamic test_abs_val_a nil)

(defn abs_val [abs_val_num]
  (try (if (< abs_val_num 0.0) (- abs_val_num) abs_val_num) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn abs_min [abs_min_x]
  (binding [abs_min_i nil abs_min_idx nil abs_min_j nil] (try (do (when (= (count abs_min_x) 0) (throw (Exception. "abs_min() arg is an empty sequence"))) (set! abs_min_j (nth abs_min_x 0)) (set! abs_min_idx 0) (while (< abs_min_idx (count abs_min_x)) (do (set! abs_min_i (nth abs_min_x abs_min_idx)) (when (< (abs_val (float abs_min_i)) (abs_val (float abs_min_j))) (set! abs_min_j abs_min_i)) (set! abs_min_idx (+ abs_min_idx 1)))) (throw (ex-info "return" {:v abs_min_j}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abs_max [abs_max_x]
  (binding [abs_max_i nil abs_max_idx nil abs_max_j nil] (try (do (when (= (count abs_max_x) 0) (throw (Exception. "abs_max() arg is an empty sequence"))) (set! abs_max_j (nth abs_max_x 0)) (set! abs_max_idx 0) (while (< abs_max_idx (count abs_max_x)) (do (set! abs_max_i (nth abs_max_x abs_max_idx)) (when (> (abs_val (float abs_max_i)) (abs_val (float abs_max_j))) (set! abs_max_j abs_max_i)) (set! abs_max_idx (+ abs_max_idx 1)))) (throw (ex-info "return" {:v abs_max_j}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abs_max_sort [abs_max_sort_x]
  (binding [abs_max_sort_a nil abs_max_sort_arr nil abs_max_sort_b nil abs_max_sort_i nil abs_max_sort_n nil abs_max_sort_temp nil] (try (do (when (= (count abs_max_sort_x) 0) (throw (Exception. "abs_max_sort() arg is an empty sequence"))) (set! abs_max_sort_arr []) (set! abs_max_sort_i 0) (while (< abs_max_sort_i (count abs_max_sort_x)) (do (set! abs_max_sort_arr (conj abs_max_sort_arr (nth abs_max_sort_x abs_max_sort_i))) (set! abs_max_sort_i (+ abs_max_sort_i 1)))) (set! abs_max_sort_n (count abs_max_sort_arr)) (set! abs_max_sort_a 0) (while (< abs_max_sort_a abs_max_sort_n) (do (set! abs_max_sort_b 0) (while (< abs_max_sort_b (- (- abs_max_sort_n abs_max_sort_a) 1)) (do (when (> (abs_val (float (nth abs_max_sort_arr abs_max_sort_b))) (abs_val (float (nth abs_max_sort_arr (+ abs_max_sort_b 1))))) (do (set! abs_max_sort_temp (nth abs_max_sort_arr abs_max_sort_b)) (set! abs_max_sort_arr (assoc abs_max_sort_arr abs_max_sort_b (nth abs_max_sort_arr (+ abs_max_sort_b 1)))) (set! abs_max_sort_arr (assoc abs_max_sort_arr (+ abs_max_sort_b 1) abs_max_sort_temp)))) (set! abs_max_sort_b (+ abs_max_sort_b 1)))) (set! abs_max_sort_a (+ abs_max_sort_a 1)))) (throw (ex-info "return" {:v (nth abs_max_sort_arr (- abs_max_sort_n 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_abs_val []
  (binding [test_abs_val_a nil] (do (when (not= (abs_val 0.0) 0.0) (throw (Exception. "abs_val(0) failed"))) (when (not= (abs_val 34.0) 34.0) (throw (Exception. "abs_val(34) failed"))) (when (not= (abs_val (- 100000000000.0)) 100000000000.0) (throw (Exception. "abs_val large failed"))) (set! test_abs_val_a [(- 3) (- 1) 2 (- 11)]) (when (not= (abs_max test_abs_val_a) (- 11)) (throw (Exception. "abs_max failed"))) (when (not= (abs_max_sort test_abs_val_a) (- 11)) (throw (Exception. "abs_max_sort failed"))) (when (not= (abs_min test_abs_val_a) (- 1)) (throw (Exception. "abs_min failed"))))))

(defn main []
  (do (test_abs_val) (println (abs_val (- 34.0)))))

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
