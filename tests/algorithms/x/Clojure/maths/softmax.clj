(ns main (:refer-clojure :exclude [exp_approx softmax abs_val approx_equal test_softmax main]))

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

(declare exp_approx softmax abs_val approx_equal test_softmax main)

(def ^:dynamic exp_approx_i nil)

(def ^:dynamic exp_approx_sum nil)

(def ^:dynamic exp_approx_term nil)

(def ^:dynamic softmax_exps nil)

(def ^:dynamic softmax_i nil)

(def ^:dynamic softmax_result nil)

(def ^:dynamic softmax_total nil)

(def ^:dynamic test_softmax_i nil)

(def ^:dynamic test_softmax_s1 nil)

(def ^:dynamic test_softmax_s2 nil)

(def ^:dynamic test_softmax_s3 nil)

(def ^:dynamic test_softmax_sum1 nil)

(defn exp_approx [exp_approx_x]
  (binding [exp_approx_i nil exp_approx_sum nil exp_approx_term nil] (try (do (set! exp_approx_term 1.0) (set! exp_approx_sum 1.0) (set! exp_approx_i 1) (while (< exp_approx_i 20) (do (set! exp_approx_term (quot (* exp_approx_term exp_approx_x) (double exp_approx_i))) (set! exp_approx_sum (+ exp_approx_sum exp_approx_term)) (set! exp_approx_i (+ exp_approx_i 1)))) (throw (ex-info "return" {:v exp_approx_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn softmax [softmax_vec]
  (binding [softmax_exps nil softmax_i nil softmax_result nil softmax_total nil] (try (do (set! softmax_exps []) (set! softmax_i 0) (while (< softmax_i (count softmax_vec)) (do (set! softmax_exps (conj softmax_exps (exp_approx (nth softmax_vec softmax_i)))) (set! softmax_i (+ softmax_i 1)))) (set! softmax_total 0.0) (set! softmax_i 0) (while (< softmax_i (count softmax_exps)) (do (set! softmax_total (+ softmax_total (nth softmax_exps softmax_i))) (set! softmax_i (+ softmax_i 1)))) (set! softmax_result []) (set! softmax_i 0) (while (< softmax_i (count softmax_exps)) (do (set! softmax_result (conj softmax_result (quot (nth softmax_exps softmax_i) softmax_total))) (set! softmax_i (+ softmax_i 1)))) (throw (ex-info "return" {:v softmax_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abs_val [abs_val_x]
  (try (if (< abs_val_x 0.0) (- abs_val_x) abs_val_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn approx_equal [approx_equal_a approx_equal_b]
  (try (throw (ex-info "return" {:v (< (abs_val (- approx_equal_a approx_equal_b)) 0.0001)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn test_softmax []
  (binding [test_softmax_i nil test_softmax_s1 nil test_softmax_s2 nil test_softmax_s3 nil test_softmax_sum1 nil] (do (set! test_softmax_s1 (softmax [1.0 2.0 3.0 4.0])) (set! test_softmax_sum1 0.0) (set! test_softmax_i 0) (while (< test_softmax_i (count test_softmax_s1)) (do (set! test_softmax_sum1 (+ test_softmax_sum1 (nth test_softmax_s1 test_softmax_i))) (set! test_softmax_i (+ test_softmax_i 1)))) (when (not (approx_equal test_softmax_sum1 1.0)) (throw (Exception. "sum test failed"))) (set! test_softmax_s2 (softmax [5.0 5.0])) (when (not (and (approx_equal (nth test_softmax_s2 0) 0.5) (approx_equal (nth test_softmax_s2 1) 0.5))) (throw (Exception. "equal elements test failed"))) (set! test_softmax_s3 (softmax [0.0])) (when (not (approx_equal (nth test_softmax_s3 0) 1.0)) (throw (Exception. "zero vector test failed"))))))

(defn main []
  (do (test_softmax) (println (str (softmax [1.0 2.0 3.0 4.0])))))

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
