(ns main (:refer-clojure :exclude [pow2 lucas_lehmer_test main]))

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

(declare pow2 lucas_lehmer_test main)

(def ^:dynamic lucas_lehmer_test_i nil)

(def ^:dynamic lucas_lehmer_test_m nil)

(def ^:dynamic lucas_lehmer_test_s nil)

(def ^:dynamic pow2_i nil)

(def ^:dynamic pow2_result nil)

(defn pow2 [pow2_p]
  (binding [pow2_i nil pow2_result nil] (try (do (set! pow2_result 1) (set! pow2_i 0) (while (< pow2_i pow2_p) (do (set! pow2_result (* pow2_result 2)) (set! pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn lucas_lehmer_test [lucas_lehmer_test_p]
  (binding [lucas_lehmer_test_i nil lucas_lehmer_test_m nil lucas_lehmer_test_s nil] (try (do (when (< lucas_lehmer_test_p 2) (throw (Exception. "p should not be less than 2!"))) (when (= lucas_lehmer_test_p 2) (throw (ex-info "return" {:v true}))) (set! lucas_lehmer_test_s 4) (set! lucas_lehmer_test_m (- (pow2 lucas_lehmer_test_p) 1)) (set! lucas_lehmer_test_i 0) (while (< lucas_lehmer_test_i (- lucas_lehmer_test_p 2)) (do (set! lucas_lehmer_test_s (mod (- (* lucas_lehmer_test_s lucas_lehmer_test_s) 2) lucas_lehmer_test_m)) (set! lucas_lehmer_test_i (+ lucas_lehmer_test_i 1)))) (throw (ex-info "return" {:v (= lucas_lehmer_test_s 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (str (lucas_lehmer_test 7))) (println (str (lucas_lehmer_test 11)))))

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
