(ns main (:refer-clojure :exclude [dot new_svc fit predict]))

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

(declare dot new_svc fit predict)

(def ^:dynamic dot_i nil)

(def ^:dynamic dot_s nil)

(def ^:dynamic fit_b nil)

(def ^:dynamic fit_epoch nil)

(def ^:dynamic fit_i nil)

(def ^:dynamic fit_j nil)

(def ^:dynamic fit_k nil)

(def ^:dynamic fit_n_features nil)

(def ^:dynamic fit_prod nil)

(def ^:dynamic fit_w nil)

(def ^:dynamic fit_x nil)

(def ^:dynamic fit_y nil)

(def ^:dynamic predict_s nil)

(defn dot [dot_a dot_b]
  (binding [dot_i nil dot_s nil] (try (do (set! dot_s 0.0) (set! dot_i 0) (while (< dot_i (count dot_a)) (do (set! dot_s (+ dot_s (* (nth dot_a dot_i) (nth dot_b dot_i)))) (set! dot_i (+ dot_i 1)))) (throw (ex-info "return" {:v dot_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn new_svc [new_svc_lr new_svc_lambda new_svc_epochs]
  (try (throw (ex-info "return" {:v {:bias 0.0 :epochs new_svc_epochs :lambda new_svc_lambda :lr new_svc_lr :weights []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fit [fit_model fit_xs fit_ys]
  (binding [fit_b nil fit_epoch nil fit_i nil fit_j nil fit_k nil fit_n_features nil fit_prod nil fit_w nil fit_x nil fit_y nil] (try (do (set! fit_n_features (count (nth fit_xs 0))) (set! fit_w []) (set! fit_i 0) (while (< fit_i fit_n_features) (do (set! fit_w (conj fit_w 0.0)) (set! fit_i (+ fit_i 1)))) (set! fit_b 0.0) (set! fit_epoch 0) (while (< fit_epoch (:epochs fit_model)) (do (set! fit_j 0) (while (< fit_j (count fit_xs)) (do (set! fit_x (nth fit_xs fit_j)) (set! fit_y (double (nth fit_ys fit_j))) (set! fit_prod (+ (dot fit_w fit_x) fit_b)) (if (< (* fit_y fit_prod) 1.0) (do (set! fit_k 0) (while (< fit_k (count fit_w)) (do (set! fit_w (assoc fit_w fit_k (+ (nth fit_w fit_k) (* (:lr fit_model) (- (* fit_y (nth fit_x fit_k)) (* (* 2.0 (:lambda fit_model)) (nth fit_w fit_k))))))) (set! fit_k (+ fit_k 1)))) (set! fit_b (+ fit_b (* (:lr fit_model) fit_y)))) (do (set! fit_k 0) (while (< fit_k (count fit_w)) (do (set! fit_w (assoc fit_w fit_k (- (nth fit_w fit_k) (* (:lr fit_model) (* (* 2.0 (:lambda fit_model)) (nth fit_w fit_k)))))) (set! fit_k (+ fit_k 1)))))) (set! fit_j (+ fit_j 1)))) (set! fit_epoch (+ fit_epoch 1)))) (throw (ex-info "return" {:v {:bias fit_b :epochs (:epochs fit_model) :lambda (:lambda fit_model) :lr (:lr fit_model) :weights fit_w}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn predict [predict_model predict_x]
  (binding [predict_s nil] (try (do (set! predict_s (+ (dot (:weights predict_model) predict_x) (:bias predict_model))) (if (>= predict_s 0.0) (throw (ex-info "return" {:v 1})) (throw (ex-info "return" {:v (- 1)})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_xs [[0.0 1.0] [0.0 2.0] [1.0 1.0] [1.0 2.0]])

(def ^:dynamic main_ys [1 1 (- 1) (- 1)])

(def ^:dynamic main_base (new_svc 0.01 0.01 1000))

(def ^:dynamic main_model (fit main_base main_xs main_ys))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (predict main_model [0.0 1.0]))
      (println (predict main_model [1.0 1.0]))
      (println (predict main_model [2.0 2.0]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
