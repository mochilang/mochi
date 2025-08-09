(ns main (:refer-clojure :exclude [floor test_floor main]))

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

(declare floor test_floor main)

(def ^:dynamic floor_i nil)

(def ^:dynamic test_floor_expected nil)

(def ^:dynamic test_floor_idx nil)

(def ^:dynamic test_floor_nums nil)

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (int floor_x)) (if (>= (- floor_x (float floor_i)) 0.0) floor_i (- floor_i 1))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_floor []
  (binding [test_floor_expected nil test_floor_idx nil test_floor_nums nil] (do (set! test_floor_nums [1.0 (- 1.0) 0.0 0.0 1.1 (- 1.1) 1.0 (- 1.0) 1000000000.0]) (set! test_floor_expected [1 (- 1) 0 0 1 (- 2) 1 (- 1) 1000000000]) (set! test_floor_idx 0) (while (< test_floor_idx (count test_floor_nums)) (do (when (not= (floor (nth test_floor_nums test_floor_idx)) (nth test_floor_expected test_floor_idx)) (throw (Exception. "floor test failed"))) (set! test_floor_idx (+ test_floor_idx 1)))))))

(defn main []
  (do (test_floor) (println (str (floor (- 1.1))))))

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
