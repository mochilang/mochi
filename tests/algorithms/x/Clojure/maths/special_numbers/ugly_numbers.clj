(ns main (:refer-clojure :exclude [ugly_numbers]))

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

(declare ugly_numbers)

(def ^:dynamic count_v nil)

(def ^:dynamic ugly_numbers_i2 nil)

(def ^:dynamic ugly_numbers_i3 nil)

(def ^:dynamic ugly_numbers_i5 nil)

(def ^:dynamic ugly_numbers_next_2 nil)

(def ^:dynamic ugly_numbers_next_3 nil)

(def ^:dynamic ugly_numbers_next_5 nil)

(def ^:dynamic ugly_numbers_next_num nil)

(def ^:dynamic ugly_numbers_ugly_nums nil)

(defn ugly_numbers [ugly_numbers_n]
  (binding [count_v nil ugly_numbers_i2 nil ugly_numbers_i3 nil ugly_numbers_i5 nil ugly_numbers_next_2 nil ugly_numbers_next_3 nil ugly_numbers_next_5 nil ugly_numbers_next_num nil ugly_numbers_ugly_nums nil] (try (do (when (<= ugly_numbers_n 0) (throw (ex-info "return" {:v 1}))) (set! ugly_numbers_ugly_nums []) (set! ugly_numbers_ugly_nums (conj ugly_numbers_ugly_nums 1)) (set! ugly_numbers_i2 0) (set! ugly_numbers_i3 0) (set! ugly_numbers_i5 0) (set! ugly_numbers_next_2 2) (set! ugly_numbers_next_3 3) (set! ugly_numbers_next_5 5) (set! count_v 1) (while (< count_v ugly_numbers_n) (do (set! ugly_numbers_next_num (if (< ugly_numbers_next_2 ugly_numbers_next_3) (if (< ugly_numbers_next_2 ugly_numbers_next_5) ugly_numbers_next_2 ugly_numbers_next_5) (if (< ugly_numbers_next_3 ugly_numbers_next_5) ugly_numbers_next_3 ugly_numbers_next_5))) (set! ugly_numbers_ugly_nums (conj ugly_numbers_ugly_nums ugly_numbers_next_num)) (when (= ugly_numbers_next_num ugly_numbers_next_2) (do (set! ugly_numbers_i2 (+ ugly_numbers_i2 1)) (set! ugly_numbers_next_2 (* (nth ugly_numbers_ugly_nums ugly_numbers_i2) 2)))) (when (= ugly_numbers_next_num ugly_numbers_next_3) (do (set! ugly_numbers_i3 (+ ugly_numbers_i3 1)) (set! ugly_numbers_next_3 (* (nth ugly_numbers_ugly_nums ugly_numbers_i3) 3)))) (when (= ugly_numbers_next_num ugly_numbers_next_5) (do (set! ugly_numbers_i5 (+ ugly_numbers_i5 1)) (set! ugly_numbers_next_5 (* (nth ugly_numbers_ugly_nums ugly_numbers_i5) 5)))) (set! count_v (+ count_v 1)))) (throw (ex-info "return" {:v (nth ugly_numbers_ugly_nums (- (count ugly_numbers_ugly_nums) 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (ugly_numbers 100)))
      (println (str (ugly_numbers 0)))
      (println (str (ugly_numbers 20)))
      (println (str (ugly_numbers (- 5))))
      (println (str (ugly_numbers 200)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
