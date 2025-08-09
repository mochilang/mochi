(ns main (:refer-clojure :exclude [dot maxf minf absf predict_raw smo_train predict]))

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

(declare dot maxf minf absf predict_raw smo_train predict)

(def ^:dynamic dot_i nil)

(def ^:dynamic dot_sum nil)

(def ^:dynamic predict_alphas nil)

(def ^:dynamic predict_b nil)

(def ^:dynamic predict_raw_i nil)

(def ^:dynamic predict_raw_res nil)

(def ^:dynamic predict_val nil)

(def ^:dynamic smo_train_Ei nil)

(def ^:dynamic smo_train_Ej nil)

(def ^:dynamic smo_train_H nil)

(def ^:dynamic smo_train_L nil)

(def ^:dynamic smo_train_alpha1_old nil)

(def ^:dynamic smo_train_alpha2_old nil)

(def ^:dynamic smo_train_alphas nil)

(def ^:dynamic smo_train_b nil)

(def ^:dynamic smo_train_b1 nil)

(def ^:dynamic smo_train_b2 nil)

(def ^:dynamic smo_train_eta nil)

(def ^:dynamic smo_train_i nil)

(def ^:dynamic smo_train_i1 nil)

(def ^:dynamic smo_train_i2 nil)

(def ^:dynamic smo_train_m nil)

(def ^:dynamic smo_train_num_changed nil)

(def ^:dynamic smo_train_passes nil)

(defn dot [dot_a dot_b]
  (binding [dot_i nil dot_sum nil] (try (do (set! dot_sum 0.0) (set! dot_i 0) (while (< dot_i (count dot_a)) (do (set! dot_sum (+ dot_sum (* (nth dot_a dot_i) (nth dot_b dot_i)))) (set! dot_i (+ dot_i 1)))) (throw (ex-info "return" {:v dot_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn maxf [maxf_a maxf_b]
  (try (if (> maxf_a maxf_b) maxf_a maxf_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn minf [minf_a minf_b]
  (try (if (< minf_a minf_b) minf_a minf_b) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn absf [absf_x]
  (try (if (>= absf_x 0.0) absf_x (- 0.0 absf_x)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn predict_raw [predict_raw_samples predict_raw_labels predict_raw_alphas predict_raw_b predict_raw_x]
  (binding [predict_raw_i nil predict_raw_res nil] (try (do (set! predict_raw_res 0.0) (set! predict_raw_i 0) (while (< predict_raw_i (count predict_raw_samples)) (do (set! predict_raw_res (+ predict_raw_res (* (* (nth predict_raw_alphas predict_raw_i) (nth predict_raw_labels predict_raw_i)) (dot (nth predict_raw_samples predict_raw_i) predict_raw_x)))) (set! predict_raw_i (+ predict_raw_i 1)))) (throw (ex-info "return" {:v (+ predict_raw_res predict_raw_b)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn smo_train [smo_train_samples smo_train_labels smo_train_c smo_train_tol smo_train_max_passes]
  (binding [smo_train_Ei nil smo_train_Ej nil smo_train_H nil smo_train_L nil smo_train_alpha1_old nil smo_train_alpha2_old nil smo_train_alphas nil smo_train_b nil smo_train_b1 nil smo_train_b2 nil smo_train_eta nil smo_train_i nil smo_train_i1 nil smo_train_i2 nil smo_train_m nil smo_train_num_changed nil smo_train_passes nil] (try (do (set! smo_train_m (count smo_train_samples)) (set! smo_train_alphas []) (set! smo_train_i 0) (while (< smo_train_i smo_train_m) (do (set! smo_train_alphas (conj smo_train_alphas 0.0)) (set! smo_train_i (+ smo_train_i 1)))) (set! smo_train_b 0.0) (set! smo_train_passes 0) (while (< smo_train_passes smo_train_max_passes) (do (set! smo_train_num_changed 0) (set! smo_train_i1 0) (loop [while_flag_1 true] (when (and while_flag_1 (< smo_train_i1 smo_train_m)) (do (set! smo_train_Ei (- (predict_raw smo_train_samples smo_train_labels smo_train_alphas smo_train_b (nth smo_train_samples smo_train_i1)) (nth smo_train_labels smo_train_i1))) (when (or (and (< (* (nth smo_train_labels smo_train_i1) smo_train_Ei) (- 0.0 smo_train_tol)) (< (nth smo_train_alphas smo_train_i1) smo_train_c)) (and (> (* (nth smo_train_labels smo_train_i1) smo_train_Ei) smo_train_tol) (> (nth smo_train_alphas smo_train_i1) 0.0))) (do (set! smo_train_i2 (mod (+ smo_train_i1 1) smo_train_m)) (set! smo_train_Ej (- (predict_raw smo_train_samples smo_train_labels smo_train_alphas smo_train_b (nth smo_train_samples smo_train_i2)) (nth smo_train_labels smo_train_i2))) (set! smo_train_alpha1_old (nth smo_train_alphas smo_train_i1)) (set! smo_train_alpha2_old (nth smo_train_alphas smo_train_i2)) (set! smo_train_L 0.0) (set! smo_train_H 0.0) (if (not= (nth smo_train_labels smo_train_i1) (nth smo_train_labels smo_train_i2)) (do (set! smo_train_L (maxf 0.0 (- smo_train_alpha2_old smo_train_alpha1_old))) (set! smo_train_H (minf smo_train_c (- (+ smo_train_c smo_train_alpha2_old) smo_train_alpha1_old)))) (do (set! smo_train_L (maxf 0.0 (- (+ smo_train_alpha2_old smo_train_alpha1_old) smo_train_c))) (set! smo_train_H (minf smo_train_c (+ smo_train_alpha2_old smo_train_alpha1_old))))) (when (= smo_train_L smo_train_H) (do (set! smo_train_i1 (+ smo_train_i1 1)) (recur true))) (set! smo_train_eta (- (- (* 2.0 (dot (nth smo_train_samples smo_train_i1) (nth smo_train_samples smo_train_i2))) (dot (nth smo_train_samples smo_train_i1) (nth smo_train_samples smo_train_i1))) (dot (nth smo_train_samples smo_train_i2) (nth smo_train_samples smo_train_i2)))) (when (>= smo_train_eta 0.0) (do (set! smo_train_i1 (+ smo_train_i1 1)) (recur true))) (set! smo_train_alphas (assoc smo_train_alphas smo_train_i2 (- smo_train_alpha2_old (quot (* (nth smo_train_labels smo_train_i2) (- smo_train_Ei smo_train_Ej)) smo_train_eta)))) (when (> (nth smo_train_alphas smo_train_i2) smo_train_H) (set! smo_train_alphas (assoc smo_train_alphas smo_train_i2 smo_train_H))) (when (< (nth smo_train_alphas smo_train_i2) smo_train_L) (set! smo_train_alphas (assoc smo_train_alphas smo_train_i2 smo_train_L))) (when (< (absf (- (nth smo_train_alphas smo_train_i2) smo_train_alpha2_old)) 0.00001) (do (set! smo_train_i1 (+ smo_train_i1 1)) (recur true))) (set! smo_train_alphas (assoc smo_train_alphas smo_train_i1 (+ smo_train_alpha1_old (* (* (nth smo_train_labels smo_train_i1) (nth smo_train_labels smo_train_i2)) (- smo_train_alpha2_old (nth smo_train_alphas smo_train_i2)))))) (set! smo_train_b1 (- (- (- smo_train_b smo_train_Ei) (* (* (nth smo_train_labels smo_train_i1) (- (nth smo_train_alphas smo_train_i1) smo_train_alpha1_old)) (dot (nth smo_train_samples smo_train_i1) (nth smo_train_samples smo_train_i1)))) (* (* (nth smo_train_labels smo_train_i2) (- (nth smo_train_alphas smo_train_i2) smo_train_alpha2_old)) (dot (nth smo_train_samples smo_train_i1) (nth smo_train_samples smo_train_i2))))) (set! smo_train_b2 (- (- (- smo_train_b smo_train_Ej) (* (* (nth smo_train_labels smo_train_i1) (- (nth smo_train_alphas smo_train_i1) smo_train_alpha1_old)) (dot (nth smo_train_samples smo_train_i1) (nth smo_train_samples smo_train_i2)))) (* (* (nth smo_train_labels smo_train_i2) (- (nth smo_train_alphas smo_train_i2) smo_train_alpha2_old)) (dot (nth smo_train_samples smo_train_i2) (nth smo_train_samples smo_train_i2))))) (if (and (> (nth smo_train_alphas smo_train_i1) 0.0) (< (nth smo_train_alphas smo_train_i1) smo_train_c)) (set! smo_train_b smo_train_b1) (if (and (> (nth smo_train_alphas smo_train_i2) 0.0) (< (nth smo_train_alphas smo_train_i2) smo_train_c)) (set! smo_train_b smo_train_b2) (set! smo_train_b (/ (+ smo_train_b1 smo_train_b2) 2.0)))) (set! smo_train_num_changed (+ smo_train_num_changed 1)))) (set! smo_train_i1 (+ smo_train_i1 1)) (cond :else (recur while_flag_1))))) (if (= smo_train_num_changed 0) (set! smo_train_passes (+ smo_train_passes 1)) (set! smo_train_passes 0)))) (throw (ex-info "return" {:v [smo_train_alphas [smo_train_b]]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn predict [predict_samples predict_labels predict_model predict_x]
  (binding [predict_alphas nil predict_b nil predict_val nil] (try (do (set! predict_alphas (nth predict_model 0)) (set! predict_b (nth (nth predict_model 1) 0)) (set! predict_val (predict_raw predict_samples predict_labels predict_alphas predict_b predict_x)) (if (>= predict_val 0.0) 1.0 (- 1.0))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_samples [[2.0 2.0] [1.5 1.5] [0.0 0.0] [0.5 0.0]])

(def ^:dynamic main_labels [1.0 1.0 (- 1.0) (- 1.0)])

(def ^:dynamic main_model (smo_train main_samples main_labels 1.0 0.001 10))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (predict main_samples main_labels main_model [1.5 1.0]))
      (println (predict main_samples main_labels main_model [0.2 0.1]))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
