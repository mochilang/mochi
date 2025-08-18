(ns main (:refer-clojure :exclude [exp_approx sigmoid swish sigmoid_linear_unit approx_equal approx_equal_list test_swish main]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare exp_approx sigmoid swish sigmoid_linear_unit approx_equal approx_equal_list test_swish main)

(def ^:dynamic approx_equal_diff nil)

(def ^:dynamic approx_equal_list_i nil)

(def ^:dynamic exp_approx_i nil)

(def ^:dynamic exp_approx_sum nil)

(def ^:dynamic exp_approx_term nil)

(def ^:dynamic sigmoid_i nil)

(def ^:dynamic sigmoid_result nil)

(def ^:dynamic sigmoid_s nil)

(def ^:dynamic sigmoid_v nil)

(def ^:dynamic swish_i nil)

(def ^:dynamic swish_result nil)

(def ^:dynamic swish_s nil)

(def ^:dynamic swish_v nil)

(def ^:dynamic test_swish_eps nil)

(def ^:dynamic test_swish_v nil)

(defn exp_approx [exp_approx_x]
  (binding [exp_approx_i nil exp_approx_sum nil exp_approx_term nil] (try (do (set! exp_approx_sum 1.0) (set! exp_approx_term 1.0) (set! exp_approx_i 1) (while (<= exp_approx_i 20) (do (set! exp_approx_term (quot (* exp_approx_term exp_approx_x) (double exp_approx_i))) (set! exp_approx_sum (+ exp_approx_sum exp_approx_term)) (set! exp_approx_i (+ exp_approx_i 1)))) (throw (ex-info "return" {:v exp_approx_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sigmoid [sigmoid_vector]
  (binding [sigmoid_i nil sigmoid_result nil sigmoid_s nil sigmoid_v nil] (try (do (set! sigmoid_result []) (set! sigmoid_i 0) (while (< sigmoid_i (count sigmoid_vector)) (do (set! sigmoid_v (nth sigmoid_vector sigmoid_i)) (set! sigmoid_s (/ 1.0 (+ 1.0 (exp_approx (- sigmoid_v))))) (set! sigmoid_result (conj sigmoid_result sigmoid_s)) (set! sigmoid_i (+ sigmoid_i 1)))) (throw (ex-info "return" {:v sigmoid_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn swish [swish_vector swish_beta]
  (binding [swish_i nil swish_result nil swish_s nil swish_v nil] (try (do (set! swish_result []) (set! swish_i 0) (while (< swish_i (count swish_vector)) (do (set! swish_v (nth swish_vector swish_i)) (set! swish_s (/ 1.0 (+ 1.0 (exp_approx (* (- swish_beta) swish_v))))) (set! swish_result (conj swish_result (* swish_v swish_s))) (set! swish_i (+ swish_i 1)))) (throw (ex-info "return" {:v swish_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sigmoid_linear_unit [sigmoid_linear_unit_vector]
  (try (throw (ex-info "return" {:v (swish sigmoid_linear_unit_vector 1.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn approx_equal [approx_equal_a approx_equal_b approx_equal_eps]
  (binding [approx_equal_diff nil] (try (do (set! approx_equal_diff (if (> approx_equal_a approx_equal_b) (- approx_equal_a approx_equal_b) (- approx_equal_b approx_equal_a))) (throw (ex-info "return" {:v (< approx_equal_diff approx_equal_eps)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn approx_equal_list [approx_equal_list_a approx_equal_list_b approx_equal_list_eps]
  (binding [approx_equal_list_i nil] (try (do (when (not= (count approx_equal_list_a) (count approx_equal_list_b)) (throw (ex-info "return" {:v false}))) (set! approx_equal_list_i 0) (while (< approx_equal_list_i (count approx_equal_list_a)) (do (when (not (approx_equal (nth approx_equal_list_a approx_equal_list_i) (nth approx_equal_list_b approx_equal_list_i) approx_equal_list_eps)) (throw (ex-info "return" {:v false}))) (set! approx_equal_list_i (+ approx_equal_list_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn test_swish []
  (binding [test_swish_eps nil test_swish_v nil] (do (set! test_swish_v [(- 1.0) 1.0 2.0]) (set! test_swish_eps 0.001) (when (not (approx_equal_list (sigmoid test_swish_v) [0.26894142 0.73105858 0.88079708] test_swish_eps)) (throw (Exception. "sigmoid incorrect"))) (when (not (approx_equal_list (sigmoid_linear_unit test_swish_v) [(- 0.26894142) 0.73105858 1.76159416] test_swish_eps)) (throw (Exception. "sigmoid_linear_unit incorrect"))) (when (not (approx_equal_list (swish test_swish_v 2.0) [(- 0.11920292) 0.88079708 1.96402758] test_swish_eps)) (throw (Exception. "swish incorrect"))) (when (not (approx_equal_list (swish [(- 2.0)] 1.0) [(- 0.23840584)] test_swish_eps)) (throw (Exception. "swish with parameter 1 incorrect"))))))

(defn main []
  (do (test_swish) (println (mochi_str (sigmoid [(- 1.0) 1.0 2.0]))) (println (mochi_str (sigmoid_linear_unit [(- 1.0) 1.0 2.0]))) (println (mochi_str (swish [(- 1.0) 1.0 2.0] 2.0))) (println (mochi_str (swish [(- 2.0)] 1.0)))))

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
