(ns main (:refer-clojure :exclude [exp_taylor sigmoid train predict wrapper]))

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

(declare exp_taylor sigmoid train predict wrapper)

(def ^:dynamic exp_taylor_i nil)

(def ^:dynamic exp_taylor_sum nil)

(def ^:dynamic exp_taylor_term nil)

(def ^:dynamic predict_h1 nil)

(def ^:dynamic predict_h2 nil)

(def ^:dynamic predict_i nil)

(def ^:dynamic predict_label nil)

(def ^:dynamic predict_out nil)

(def ^:dynamic predict_preds nil)

(def ^:dynamic predict_x0 nil)

(def ^:dynamic predict_x1 nil)

(def ^:dynamic predict_z1 nil)

(def ^:dynamic predict_z2 nil)

(def ^:dynamic predict_z3 nil)

(def ^:dynamic train_d1 nil)

(def ^:dynamic train_d2 nil)

(def ^:dynamic train_e nil)

(def ^:dynamic train_error nil)

(def ^:dynamic train_h1 nil)

(def ^:dynamic train_h2 nil)

(def ^:dynamic train_i nil)

(def ^:dynamic train_out nil)

(def ^:dynamic train_target nil)

(def ^:dynamic train_x0 nil)

(def ^:dynamic train_x1 nil)

(def ^:dynamic train_z1 nil)

(def ^:dynamic train_z2 nil)

(def ^:dynamic train_z3 nil)

(defn exp_taylor [exp_taylor_x]
  (binding [exp_taylor_i nil exp_taylor_sum nil exp_taylor_term nil] (try (do (set! exp_taylor_term 1.0) (set! exp_taylor_sum 1.0) (set! exp_taylor_i 1.0) (while (< exp_taylor_i 20.0) (do (set! exp_taylor_term (quot (* exp_taylor_term exp_taylor_x) exp_taylor_i)) (set! exp_taylor_sum (+ exp_taylor_sum exp_taylor_term)) (set! exp_taylor_i (+ exp_taylor_i 1.0)))) (throw (ex-info "return" {:v exp_taylor_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sigmoid [sigmoid_x]
  (try (throw (ex-info "return" {:v (/ 1.0 (+ 1.0 (exp_taylor (- sigmoid_x))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_X [[0.0 0.0] [1.0 1.0] [1.0 0.0] [0.0 1.0]])

(def ^:dynamic main_Y [0.0 1.0 0.0 0.0])

(def ^:dynamic main_test_data [[0.0 0.0] [0.0 1.0] [1.0 1.0]])

(def ^:dynamic main_w1 [[0.5 (- 0.5)] [0.5 0.5]])

(def ^:dynamic main_b1 [0.0 0.0])

(def ^:dynamic main_w2 [0.5 (- 0.5)])

(def ^:dynamic main_b2 0.0)

(defn train [train_epochs train_lr]
  (binding [train_d1 nil train_d2 nil train_e nil train_error nil train_h1 nil train_h2 nil train_i nil train_out nil train_target nil train_x0 nil train_x1 nil train_z1 nil train_z2 nil train_z3 nil] (do (set! train_e 0) (while (< train_e train_epochs) (do (set! train_i 0) (while (< train_i (count main_X)) (do (set! train_x0 (nth (nth main_X train_i) 0)) (set! train_x1 (nth (nth main_X train_i) 1)) (set! train_target (nth main_Y train_i)) (set! train_z1 (+ (+ (* (nth (nth main_w1 0) 0) train_x0) (* (nth (nth main_w1 1) 0) train_x1)) (nth main_b1 0))) (set! train_z2 (+ (+ (* (nth (nth main_w1 0) 1) train_x0) (* (nth (nth main_w1 1) 1) train_x1)) (nth main_b1 1))) (set! train_h1 (sigmoid train_z1)) (set! train_h2 (sigmoid train_z2)) (set! train_z3 (+ (+ (* (nth main_w2 0) train_h1) (* (nth main_w2 1) train_h2)) main_b2)) (set! train_out (sigmoid train_z3)) (set! train_error (- train_out train_target)) (set! train_d1 (* (* (* train_h1 (- 1.0 train_h1)) (nth main_w2 0)) train_error)) (set! train_d2 (* (* (* train_h2 (- 1.0 train_h2)) (nth main_w2 1)) train_error)) (alter-var-root (var main_w2) (fn [_] (assoc main_w2 0 (- (nth main_w2 0) (* (* train_lr train_error) train_h1))))) (alter-var-root (var main_w2) (fn [_] (assoc main_w2 1 (- (nth main_w2 1) (* (* train_lr train_error) train_h2))))) (alter-var-root (var main_b2) (fn [_] (- main_b2 (* train_lr train_error)))) (alter-var-root (var main_w1) (fn [_] (assoc-in main_w1 [0 0] (- (nth (nth main_w1 0) 0) (* (* train_lr train_d1) train_x0))))) (alter-var-root (var main_w1) (fn [_] (assoc-in main_w1 [1 0] (- (nth (nth main_w1 1) 0) (* (* train_lr train_d1) train_x1))))) (alter-var-root (var main_b1) (fn [_] (assoc main_b1 0 (- (nth main_b1 0) (* train_lr train_d1))))) (alter-var-root (var main_w1) (fn [_] (assoc-in main_w1 [0 1] (- (nth (nth main_w1 0) 1) (* (* train_lr train_d2) train_x0))))) (alter-var-root (var main_w1) (fn [_] (assoc-in main_w1 [1 1] (- (nth (nth main_w1 1) 1) (* (* train_lr train_d2) train_x1))))) (alter-var-root (var main_b1) (fn [_] (assoc main_b1 1 (- (nth main_b1 1) (* train_lr train_d2))))) (set! train_i (+ train_i 1)))) (set! train_e (+ train_e 1)))) train_epochs)))

(defn predict [predict_samples]
  (binding [predict_h1 nil predict_h2 nil predict_i nil predict_label nil predict_out nil predict_preds nil predict_x0 nil predict_x1 nil predict_z1 nil predict_z2 nil predict_z3 nil] (try (do (set! predict_preds []) (set! predict_i 0) (while (< predict_i (count predict_samples)) (do (set! predict_x0 (nth (nth predict_samples predict_i) 0)) (set! predict_x1 (nth (nth predict_samples predict_i) 1)) (set! predict_z1 (+ (+ (* (nth (nth main_w1 0) 0) predict_x0) (* (nth (nth main_w1 1) 0) predict_x1)) (nth main_b1 0))) (set! predict_z2 (+ (+ (* (nth (nth main_w1 0) 1) predict_x0) (* (nth (nth main_w1 1) 1) predict_x1)) (nth main_b1 1))) (set! predict_h1 (sigmoid predict_z1)) (set! predict_h2 (sigmoid predict_z2)) (set! predict_z3 (+ (+ (* (nth main_w2 0) predict_h1) (* (nth main_w2 1) predict_h2)) main_b2)) (set! predict_out (sigmoid predict_z3)) (set! predict_label 0) (when (>= predict_out 0.5) (set! predict_label 1)) (set! predict_preds (conj predict_preds predict_label)) (set! predict_i (+ predict_i 1)))) (throw (ex-info "return" {:v predict_preds}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn wrapper [wrapper_y]
  (try (throw (ex-info "return" {:v wrapper_y})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_preds (wrapper (predict main_test_data)))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (train 4000 0.5)
      (println (str main_preds))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
