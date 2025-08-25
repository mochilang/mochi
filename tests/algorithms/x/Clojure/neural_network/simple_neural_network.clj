(ns main (:refer-clojure :exclude [rand randint expApprox sigmoid sigmoid_derivative forward_propagation]))

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

(declare rand randint expApprox sigmoid sigmoid_derivative forward_propagation)

(declare _read_file)

(def ^:dynamic expApprox_is_neg nil)

(def ^:dynamic expApprox_n nil)

(def ^:dynamic expApprox_sum nil)

(def ^:dynamic expApprox_term nil)

(def ^:dynamic expApprox_y nil)

(def ^:dynamic forward_propagation_i nil)

(def ^:dynamic forward_propagation_layer_1 nil)

(def ^:dynamic forward_propagation_layer_1_delta nil)

(def ^:dynamic forward_propagation_layer_1_error nil)

(def ^:dynamic forward_propagation_weight nil)

(def ^:dynamic main_seed nil)

(def ^:dynamic main_seed nil)

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+' (*' main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn randint [randint_low randint_high]
  (try (throw (ex-info "return" {:v (+' (mod (rand) (+' (- randint_high randint_low) 1)) randint_low)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn expApprox [expApprox_x]
  (binding [expApprox_is_neg nil expApprox_n nil expApprox_sum nil expApprox_term nil expApprox_y nil] (try (do (set! expApprox_y expApprox_x) (set! expApprox_is_neg false) (when (< expApprox_x 0.0) (do (set! expApprox_is_neg true) (set! expApprox_y (- expApprox_x)))) (set! expApprox_term 1.0) (set! expApprox_sum 1.0) (set! expApprox_n 1) (while (< expApprox_n 30) (do (set! expApprox_term (/ (*' expApprox_term expApprox_y) (double expApprox_n))) (set! expApprox_sum (+' expApprox_sum expApprox_term)) (set! expApprox_n (+' expApprox_n 1)))) (if expApprox_is_neg (/ 1.0 expApprox_sum) expApprox_sum)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sigmoid [sigmoid_x]
  (try (throw (ex-info "return" {:v (/ 1.0 (+' 1.0 (expApprox (- sigmoid_x))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sigmoid_derivative [sigmoid_derivative_sig_val]
  (try (throw (ex-info "return" {:v (*' sigmoid_derivative_sig_val (- 1.0 sigmoid_derivative_sig_val))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def ^:dynamic main_INITIAL_VALUE nil)

(defn forward_propagation [forward_propagation_expected forward_propagation_number_propagations]
  (binding [forward_propagation_i nil forward_propagation_layer_1 nil forward_propagation_layer_1_delta nil forward_propagation_layer_1_error nil forward_propagation_weight nil] (try (do (set! forward_propagation_weight (- (*' 2.0 (double (randint 1 100))) 1.0)) (set! forward_propagation_layer_1 0.0) (set! forward_propagation_i 0) (while (< forward_propagation_i forward_propagation_number_propagations) (do (set! forward_propagation_layer_1 (sigmoid (*' main_INITIAL_VALUE forward_propagation_weight))) (set! forward_propagation_layer_1_error (- (/ (double forward_propagation_expected) 100.0) forward_propagation_layer_1)) (set! forward_propagation_layer_1_delta (*' forward_propagation_layer_1_error (sigmoid_derivative forward_propagation_layer_1))) (set! forward_propagation_weight (+' forward_propagation_weight (*' main_INITIAL_VALUE forward_propagation_layer_1_delta))) (set! forward_propagation_i (+' forward_propagation_i 1)))) (throw (ex-info "return" {:v (*' forward_propagation_layer_1 100.0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_result nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_seed) (constantly 1))
      (alter-var-root (var main_INITIAL_VALUE) (constantly 0.02))
      (alter-var-root (var main_seed) (constantly 1))
      (alter-var-root (var main_result) (constantly (forward_propagation 32 450000)))
      (println main_result)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
