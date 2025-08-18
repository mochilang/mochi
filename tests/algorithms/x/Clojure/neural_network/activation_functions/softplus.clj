(ns main (:refer-clojure :exclude [ln exp softplus main]))

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

(declare ln exp softplus main)

(def ^:dynamic exp_n nil)

(def ^:dynamic exp_sum nil)

(def ^:dynamic exp_term nil)

(def ^:dynamic ln_denom nil)

(def ^:dynamic ln_k nil)

(def ^:dynamic ln_sum nil)

(def ^:dynamic ln_term nil)

(def ^:dynamic ln_y nil)

(def ^:dynamic ln_y2 nil)

(def ^:dynamic main_r1 nil)

(def ^:dynamic main_r2 nil)

(def ^:dynamic main_v1 nil)

(def ^:dynamic main_v2 nil)

(def ^:dynamic softplus_i nil)

(def ^:dynamic softplus_result nil)

(def ^:dynamic softplus_value nil)

(def ^:dynamic softplus_x nil)

(defn ln [ln_x]
  (binding [ln_denom nil ln_k nil ln_sum nil ln_term nil ln_y nil ln_y2 nil] (try (do (when (<= ln_x 0.0) (throw (Exception. "ln domain error"))) (set! ln_y (/ (- ln_x 1.0) (+ ln_x 1.0))) (set! ln_y2 (* ln_y ln_y)) (set! ln_term ln_y) (set! ln_sum 0.0) (set! ln_k 0) (while (< ln_k 10) (do (set! ln_denom (double (+ (* 2 ln_k) 1))) (set! ln_sum (+ ln_sum (quot ln_term ln_denom))) (set! ln_term (* ln_term ln_y2)) (set! ln_k (+ ln_k 1)))) (throw (ex-info "return" {:v (* 2.0 ln_sum)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn exp [exp_x]
  (binding [exp_n nil exp_sum nil exp_term nil] (try (do (set! exp_term 1.0) (set! exp_sum 1.0) (set! exp_n 1) (while (< exp_n 20) (do (set! exp_term (quot (* exp_term exp_x) (double exp_n))) (set! exp_sum (+ exp_sum exp_term)) (set! exp_n (+ exp_n 1)))) (throw (ex-info "return" {:v exp_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn softplus [softplus_vector]
  (binding [softplus_i nil softplus_result nil softplus_value nil softplus_x nil] (try (do (set! softplus_result []) (set! softplus_i 0) (while (< softplus_i (count softplus_vector)) (do (set! softplus_x (nth softplus_vector softplus_i)) (set! softplus_value (ln (+ 1.0 (exp softplus_x)))) (set! softplus_result (conj softplus_result softplus_value)) (set! softplus_i (+ softplus_i 1)))) (throw (ex-info "return" {:v softplus_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_r1 nil main_r2 nil main_v1 nil main_v2 nil] (do (set! main_v1 [2.3 0.6 (- 2.0) (- 3.8)]) (set! main_v2 [(- 9.2) (- 0.3) 0.45 (- 4.56)]) (set! main_r1 (softplus main_v1)) (set! main_r2 (softplus main_v2)) (println main_r1) (println main_r2))))

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
