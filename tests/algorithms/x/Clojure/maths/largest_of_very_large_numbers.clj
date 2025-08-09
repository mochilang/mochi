(ns main (:refer-clojure :exclude [ln log10 absf res test_res compare]))

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

(declare ln log10 absf res test_res compare)

(def ^:dynamic compare_r1 nil)

(def ^:dynamic compare_r2 nil)

(def ^:dynamic ln_k nil)

(def ^:dynamic ln_sum nil)

(def ^:dynamic ln_t nil)

(def ^:dynamic ln_term nil)

(defn ln [ln_x]
  (binding [ln_k nil ln_sum nil ln_t nil ln_term nil] (try (do (set! ln_t (/ (- ln_x 1.0) (+ ln_x 1.0))) (set! ln_term ln_t) (set! ln_sum 0.0) (set! ln_k 1) (while (<= ln_k 99) (do (set! ln_sum (+ ln_sum (/ ln_term (double ln_k)))) (set! ln_term (* (* ln_term ln_t) ln_t)) (set! ln_k (+ ln_k 2)))) (throw (ex-info "return" {:v (* 2.0 ln_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn log10 [log10_x]
  (try (throw (ex-info "return" {:v (/ (ln log10_x) (ln 10.0))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (- absf_x) absf_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn res [res_x res_y]
  (try (do (when (= res_x 0) (throw (ex-info "return" {:v 0.0}))) (when (= res_y 0) (throw (ex-info "return" {:v 1.0}))) (when (< res_x 0) (throw (Exception. "math domain error"))) (throw (ex-info "return" {:v (* (double res_y) (log10 (double res_x)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_res []
  (do (when (> (absf (- (res 5 7) 4.892790030352132)) 0.0000001) (throw (Exception. "res(5,7) failed"))) (when (not= (res 0 5) 0.0) (throw (Exception. "res(0,5) failed"))) (when (not= (res 3 0) 1.0) (throw (Exception. "res(3,0) failed")))))

(defn compare [compare_x1 compare_y1 compare_x2 compare_y2]
  (binding [compare_r1 nil compare_r2 nil] (try (do (set! compare_r1 (res compare_x1 compare_y1)) (set! compare_r2 (res compare_x2 compare_y2)) (when (> compare_r1 compare_r2) (throw (ex-info "return" {:v (str (str (str "Largest number is " (str compare_x1)) " ^ ") (str compare_y1))}))) (if (> compare_r2 compare_r1) (str (str (str "Largest number is " (str compare_x2)) " ^ ") (str compare_y2)) "Both are equal")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (test_res)
      (println (compare 5 7 4 8))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
