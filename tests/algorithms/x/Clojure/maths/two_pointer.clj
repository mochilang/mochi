(ns main (:refer-clojure :exclude [two_pointer test_two_pointer main]))

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

(declare two_pointer test_two_pointer main)

(def ^:dynamic two_pointer_i nil)

(def ^:dynamic two_pointer_j nil)

(def ^:dynamic two_pointer_s nil)

(defn two_pointer [two_pointer_nums two_pointer_target]
  (binding [two_pointer_i nil two_pointer_j nil two_pointer_s nil] (try (do (set! two_pointer_i 0) (set! two_pointer_j (- (count two_pointer_nums) 1)) (while (< two_pointer_i two_pointer_j) (do (set! two_pointer_s (+ (nth two_pointer_nums two_pointer_i) (nth two_pointer_nums two_pointer_j))) (when (= two_pointer_s two_pointer_target) (throw (ex-info "return" {:v [two_pointer_i two_pointer_j]}))) (if (< two_pointer_s two_pointer_target) (set! two_pointer_i (+ two_pointer_i 1)) (set! two_pointer_j (- two_pointer_j 1))))) (throw (ex-info "return" {:v []}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_two_pointer []
  (do (when (not= (two_pointer [2 7 11 15] 9) [0 1]) (throw (Exception. "case1"))) (when (not= (two_pointer [2 7 11 15] 17) [0 3]) (throw (Exception. "case2"))) (when (not= (two_pointer [2 7 11 15] 18) [1 2]) (throw (Exception. "case3"))) (when (not= (two_pointer [2 7 11 15] 26) [2 3]) (throw (Exception. "case4"))) (when (not= (two_pointer [1 3 3] 6) [1 2]) (throw (Exception. "case5"))) (when (not= (count (two_pointer [2 7 11 15] 8)) 0) (throw (Exception. "case6"))) (when (not= (count (two_pointer [0 3 6 9 12 15 18 21 24 27] 19)) 0) (throw (Exception. "case7"))) (when (not= (count (two_pointer [1 2 3] 6)) 0) (throw (Exception. "case8")))))

(defn main []
  (do (test_two_pointer) (println (two_pointer [2 7 11 15] 9))))

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
