(ns main (:refer-clojure :exclude [exp_approx sigmoid sigmoid_derivative new_network feedforward train predict example main]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare exp_approx sigmoid sigmoid_derivative new_network feedforward train predict example main)

(declare _read_file)

(def ^:dynamic example_inputs nil)

(def ^:dynamic example_net nil)

(def ^:dynamic example_outputs nil)

(def ^:dynamic example_result nil)

(def ^:dynamic exp_approx_i nil)

(def ^:dynamic exp_approx_sum nil)

(def ^:dynamic exp_approx_term nil)

(def ^:dynamic feedforward_hidden1 nil)

(def ^:dynamic feedforward_hidden2 nil)

(def ^:dynamic feedforward_i nil)

(def ^:dynamic feedforward_j nil)

(def ^:dynamic feedforward_j2 nil)

(def ^:dynamic feedforward_k nil)

(def ^:dynamic feedforward_k2 nil)

(def ^:dynamic feedforward_out nil)

(def ^:dynamic feedforward_sum1 nil)

(def ^:dynamic feedforward_sum2 nil)

(def ^:dynamic feedforward_sum3 nil)

(def ^:dynamic predict_out nil)

(def ^:dynamic train_delta_hidden1 nil)

(def ^:dynamic train_delta_hidden2 nil)

(def ^:dynamic train_delta_output nil)

(def ^:dynamic train_dh2 nil)

(def ^:dynamic train_error nil)

(def ^:dynamic train_hidden1 nil)

(def ^:dynamic train_hidden2 nil)

(def ^:dynamic train_i nil)

(def ^:dynamic train_i2 nil)

(def ^:dynamic train_inp nil)

(def ^:dynamic train_iter nil)

(def ^:dynamic train_j nil)

(def ^:dynamic train_j2 nil)

(def ^:dynamic train_k nil)

(def ^:dynamic train_k3 nil)

(def ^:dynamic train_k4 nil)

(def ^:dynamic train_k5 nil)

(def ^:dynamic train_k6 nil)

(def ^:dynamic train_k7 nil)

(def ^:dynamic train_net nil)

(def ^:dynamic train_new_w1 nil)

(def ^:dynamic train_new_w2 nil)

(def ^:dynamic train_new_w3 nil)

(def ^:dynamic train_output nil)

(def ^:dynamic train_row nil)

(def ^:dynamic train_row2 nil)

(def ^:dynamic train_s nil)

(def ^:dynamic train_sum1 nil)

(def ^:dynamic train_sum2 nil)

(def ^:dynamic train_sum3 nil)

(def ^:dynamic train_sumdh nil)

(def ^:dynamic train_target nil)

(def ^:dynamic train_w1row nil)

(def ^:dynamic train_w2row nil)

(def ^:dynamic train_w3row nil)

(defn exp_approx [exp_approx_x]
  (binding [exp_approx_i nil exp_approx_sum nil exp_approx_term nil] (try (do (set! exp_approx_sum 1.0) (set! exp_approx_term 1.0) (set! exp_approx_i 1) (while (< exp_approx_i 10) (do (set! exp_approx_term (/ (*' exp_approx_term exp_approx_x) (float exp_approx_i))) (set! exp_approx_sum (+' exp_approx_sum exp_approx_term)) (set! exp_approx_i (+' exp_approx_i 1)))) (throw (ex-info "return" {:v exp_approx_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sigmoid [sigmoid_x]
  (try (throw (ex-info "return" {:v (/ 1.0 (+' 1.0 (exp_approx (- sigmoid_x))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sigmoid_derivative [sigmoid_derivative_x]
  (try (throw (ex-info "return" {:v (*' sigmoid_derivative_x (- 1.0 sigmoid_derivative_x))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn new_network []
  (try (throw (ex-info "return" {:v {:w1 [[0.1 0.2 0.3 0.4] [0.5 0.6 0.7 0.8] [0.9 1.0 1.1 1.2]] :w2 [[0.1 0.2 0.3] [0.4 0.5 0.6] [0.7 0.8 0.9] [1.0 1.1 1.2]] :w3 [[0.1] [0.2] [0.3]]}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn feedforward [feedforward_net feedforward_input]
  (binding [feedforward_hidden1 nil feedforward_hidden2 nil feedforward_i nil feedforward_j nil feedforward_j2 nil feedforward_k nil feedforward_k2 nil feedforward_out nil feedforward_sum1 nil feedforward_sum2 nil feedforward_sum3 nil] (try (do (set! feedforward_hidden1 []) (set! feedforward_j 0) (while (< feedforward_j 4) (do (set! feedforward_sum1 0.0) (set! feedforward_i 0) (while (< feedforward_i 3) (do (set! feedforward_sum1 (+' feedforward_sum1 (*' (nth feedforward_input feedforward_i) (get (get (:w1 feedforward_net) feedforward_i) feedforward_j)))) (set! feedforward_i (+' feedforward_i 1)))) (set! feedforward_hidden1 (conj feedforward_hidden1 (sigmoid feedforward_sum1))) (set! feedforward_j (+' feedforward_j 1)))) (set! feedforward_hidden2 []) (set! feedforward_k 0) (while (< feedforward_k 3) (do (set! feedforward_sum2 0.0) (set! feedforward_j2 0) (while (< feedforward_j2 4) (do (set! feedforward_sum2 (+' feedforward_sum2 (*' (nth feedforward_hidden1 feedforward_j2) (get (get (:w2 feedforward_net) feedforward_j2) feedforward_k)))) (set! feedforward_j2 (+' feedforward_j2 1)))) (set! feedforward_hidden2 (conj feedforward_hidden2 (sigmoid feedforward_sum2))) (set! feedforward_k (+' feedforward_k 1)))) (set! feedforward_sum3 0.0) (set! feedforward_k2 0) (while (< feedforward_k2 3) (do (set! feedforward_sum3 (+' feedforward_sum3 (*' (nth feedforward_hidden2 feedforward_k2) (get (get (:w3 feedforward_net) feedforward_k2) 0)))) (set! feedforward_k2 (+' feedforward_k2 1)))) (set! feedforward_out (sigmoid feedforward_sum3)) (throw (ex-info "return" {:v feedforward_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn train [train_net_p train_inputs train_outputs train_iterations]
  (binding [train_net train_net_p train_delta_hidden1 nil train_delta_hidden2 nil train_delta_output nil train_dh2 nil train_error nil train_hidden1 nil train_hidden2 nil train_i nil train_i2 nil train_inp nil train_iter nil train_j nil train_j2 nil train_k nil train_k3 nil train_k4 nil train_k5 nil train_k6 nil train_k7 nil train_new_w1 nil train_new_w2 nil train_new_w3 nil train_output nil train_row nil train_row2 nil train_s nil train_sum1 nil train_sum2 nil train_sum3 nil train_sumdh nil train_target nil train_w1row nil train_w2row nil train_w3row nil] (do (try (do (set! train_iter 0) (while (< train_iter train_iterations) (do (set! train_s 0) (while (< train_s (count train_inputs)) (do (set! train_inp (nth train_inputs train_s)) (set! train_target (nth train_outputs train_s)) (set! train_hidden1 []) (set! train_j 0) (while (< train_j 4) (do (set! train_sum1 0.0) (set! train_i 0) (while (< train_i 3) (do (set! train_sum1 (+' train_sum1 (*' (nth train_inp train_i) (get (get (:w1 train_net) train_i) train_j)))) (set! train_i (+' train_i 1)))) (set! train_hidden1 (conj train_hidden1 (sigmoid train_sum1))) (set! train_j (+' train_j 1)))) (set! train_hidden2 []) (set! train_k 0) (while (< train_k 3) (do (set! train_sum2 0.0) (set! train_j2 0) (while (< train_j2 4) (do (set! train_sum2 (+' train_sum2 (*' (nth train_hidden1 train_j2) (get (get (:w2 train_net) train_j2) train_k)))) (set! train_j2 (+' train_j2 1)))) (set! train_hidden2 (conj train_hidden2 (sigmoid train_sum2))) (set! train_k (+' train_k 1)))) (set! train_sum3 0.0) (set! train_k3 0) (while (< train_k3 3) (do (set! train_sum3 (+' train_sum3 (*' (nth train_hidden2 train_k3) (get (get (:w3 train_net) train_k3) 0)))) (set! train_k3 (+' train_k3 1)))) (set! train_output (sigmoid train_sum3)) (set! train_error (- train_target train_output)) (set! train_delta_output (*' train_error (sigmoid_derivative train_output))) (set! train_new_w3 []) (set! train_k4 0) (while (< train_k4 3) (do (set! train_w3row (get (:w3 train_net) train_k4)) (set! train_w3row (assoc train_w3row 0 (+' (get train_w3row 0) (*' (nth train_hidden2 train_k4) train_delta_output)))) (set! train_new_w3 (conj train_new_w3 train_w3row)) (set! train_k4 (+' train_k4 1)))) (set! train_net (assoc train_net :w3 train_new_w3)) (set! train_delta_hidden2 []) (set! train_k5 0) (while (< train_k5 3) (do (set! train_row (get (:w3 train_net) train_k5)) (set! train_dh2 (*' (*' (get train_row 0) train_delta_output) (sigmoid_derivative (nth train_hidden2 train_k5)))) (set! train_delta_hidden2 (conj train_delta_hidden2 train_dh2)) (set! train_k5 (+' train_k5 1)))) (set! train_new_w2 []) (set! train_j 0) (while (< train_j 4) (do (set! train_w2row (get (:w2 train_net) train_j)) (set! train_k6 0) (while (< train_k6 3) (do (set! train_w2row (assoc train_w2row train_k6 (+' (get train_w2row train_k6) (*' (nth train_hidden1 train_j) (nth train_delta_hidden2 train_k6))))) (set! train_k6 (+' train_k6 1)))) (set! train_new_w2 (conj train_new_w2 train_w2row)) (set! train_j (+' train_j 1)))) (set! train_net (assoc train_net :w2 train_new_w2)) (set! train_delta_hidden1 []) (set! train_j 0) (while (< train_j 4) (do (set! train_sumdh 0.0) (set! train_k7 0) (while (< train_k7 3) (do (set! train_row2 (get (:w2 train_net) train_j)) (set! train_sumdh (+' train_sumdh (*' (get train_row2 train_k7) (nth train_delta_hidden2 train_k7)))) (set! train_k7 (+' train_k7 1)))) (set! train_delta_hidden1 (conj train_delta_hidden1 (*' train_sumdh (sigmoid_derivative (nth train_hidden1 train_j))))) (set! train_j (+' train_j 1)))) (set! train_new_w1 []) (set! train_i2 0) (while (< train_i2 3) (do (set! train_w1row (get (:w1 train_net) train_i2)) (set! train_j 0) (while (< train_j 4) (do (set! train_w1row (assoc train_w1row train_j (+' (get train_w1row train_j) (*' (nth train_inp train_i2) (nth train_delta_hidden1 train_j))))) (set! train_j (+' train_j 1)))) (set! train_new_w1 (conj train_new_w1 train_w1row)) (set! train_i2 (+' train_i2 1)))) (set! train_net (assoc train_net :w1 train_new_w1)) (set! train_s (+' train_s 1)))) (set! train_iter (+' train_iter 1))))) (finally (alter-var-root (var train_net) (constantly train_net)))) train_net)))

(defn predict [predict_net predict_input]
  (binding [predict_out nil] (try (do (set! predict_out (feedforward predict_net predict_input)) (if (> predict_out 0.6) 1 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn example []
  (binding [example_inputs nil example_net nil example_outputs nil example_result nil] (try (do (set! example_inputs [[0.0 0.0 0.0] [0.0 0.0 1.0] [0.0 1.0 0.0] [0.0 1.0 1.0] [1.0 0.0 0.0] [1.0 0.0 1.0] [1.0 1.0 0.0] [1.0 1.0 1.0]]) (set! example_outputs [0.0 1.0 1.0 0.0 1.0 0.0 0.0 1.0]) (set! example_net (new_network)) (let [__res (train example_net example_inputs example_outputs 10)] (do (set! example_net train_net) __res)) (set! example_result (predict example_net [1.0 1.0 1.0])) (println (mochi_str example_result)) (throw (ex-info "return" {:v example_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (example))

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
