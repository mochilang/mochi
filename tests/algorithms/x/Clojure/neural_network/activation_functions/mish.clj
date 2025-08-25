(ns main (:refer-clojure :exclude [exp_approx ln_series ln softplus tanh_approx mish main]))

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

(declare exp_approx ln_series ln softplus tanh_approx mish main)

(declare _read_file)

(def ^:dynamic exp_approx_n nil)

(def ^:dynamic exp_approx_neg nil)

(def ^:dynamic exp_approx_sum nil)

(def ^:dynamic exp_approx_term nil)

(def ^:dynamic exp_approx_y nil)

(def ^:dynamic ln_k nil)

(def ^:dynamic ln_series_acc nil)

(def ^:dynamic ln_series_n nil)

(def ^:dynamic ln_series_t nil)

(def ^:dynamic ln_series_term nil)

(def ^:dynamic ln_y nil)

(def ^:dynamic main_v1 nil)

(def ^:dynamic main_v2 nil)

(def ^:dynamic mish_i nil)

(def ^:dynamic mish_result nil)

(def ^:dynamic mish_sp nil)

(def ^:dynamic mish_x nil)

(def ^:dynamic mish_y nil)

(defn exp_approx [exp_approx_x]
  (binding [exp_approx_n nil exp_approx_neg nil exp_approx_sum nil exp_approx_term nil exp_approx_y nil] (try (do (set! exp_approx_neg false) (set! exp_approx_y exp_approx_x) (when (< exp_approx_x 0.0) (do (set! exp_approx_neg true) (set! exp_approx_y (- exp_approx_x)))) (set! exp_approx_term 1.0) (set! exp_approx_sum 1.0) (set! exp_approx_n 1) (while (< exp_approx_n 30) (do (set! exp_approx_term (/ (*' exp_approx_term exp_approx_y) (double exp_approx_n))) (set! exp_approx_sum (+' exp_approx_sum exp_approx_term)) (set! exp_approx_n (+' exp_approx_n 1)))) (if exp_approx_neg (/ 1.0 exp_approx_sum) exp_approx_sum)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ln_series [ln_series_x]
  (binding [ln_series_acc nil ln_series_n nil ln_series_t nil ln_series_term nil] (try (do (set! ln_series_t (/ (- ln_series_x 1.0) (+' ln_series_x 1.0))) (set! ln_series_term ln_series_t) (set! ln_series_acc 0.0) (set! ln_series_n 1) (while (<= ln_series_n 19) (do (set! ln_series_acc (+' ln_series_acc (/ ln_series_term (double ln_series_n)))) (set! ln_series_term (*' (*' ln_series_term ln_series_t) ln_series_t)) (set! ln_series_n (+' ln_series_n 2)))) (throw (ex-info "return" {:v (*' 2.0 ln_series_acc)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ln [ln_x]
  (binding [ln_k nil ln_y nil] (try (do (set! ln_y ln_x) (set! ln_k 0) (while (>= ln_y 10.0) (do (set! ln_y (/ ln_y 10.0)) (set! ln_k (+' ln_k 1)))) (while (< ln_y 1.0) (do (set! ln_y (*' ln_y 10.0)) (set! ln_k (- ln_k 1)))) (throw (ex-info "return" {:v (+' (ln_series ln_y) (*' (double ln_k) (ln_series 10.0)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn softplus [softplus_x]
  (try (throw (ex-info "return" {:v (ln (+' 1.0 (exp_approx softplus_x)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn tanh_approx [tanh_approx_x]
  (try (throw (ex-info "return" {:v (- (/ 2.0 (+' 1.0 (exp_approx (*' (- 2.0) tanh_approx_x)))) 1.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mish [mish_vector]
  (binding [mish_i nil mish_result nil mish_sp nil mish_x nil mish_y nil] (try (do (set! mish_result []) (set! mish_i 0) (while (< mish_i (count mish_vector)) (do (set! mish_x (nth mish_vector mish_i)) (set! mish_sp (softplus mish_x)) (set! mish_y (*' mish_x (tanh_approx mish_sp))) (set! mish_result (conj mish_result mish_y)) (set! mish_i (+' mish_i 1)))) (throw (ex-info "return" {:v mish_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_v1 nil main_v2 nil] (do (set! main_v1 [2.3 0.6 (- 2.0) (- 3.8)]) (set! main_v2 [(- 9.2) (- 0.3) 0.45 (- 4.56)]) (println (mochi_str (mish main_v1))) (println (mochi_str (mish main_v2))))))

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
