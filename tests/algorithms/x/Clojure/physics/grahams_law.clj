(ns main (:refer-clojure :exclude [to_float round6 sqrtApprox validate effusion_ratio first_effusion_rate second_effusion_rate first_molar_mass second_molar_mass]))

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

(declare to_float round6 sqrtApprox validate effusion_ratio first_effusion_rate second_effusion_rate first_molar_mass second_molar_mass)

(def ^:dynamic first_molar_mass_ratio nil)

(def ^:dynamic round6_factor nil)

(def ^:dynamic second_molar_mass_ratio nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic validate_i nil)

(defn to_float [to_float_x]
  (try (throw (ex-info "return" {:v (* to_float_x 1.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn round6 [round6_x]
  (binding [round6_factor nil] (try (do (set! round6_factor 1000000.0) (throw (ex-info "return" {:v (/ (to_float (toi (+ (* round6_x round6_factor) 0.5))) round6_factor)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn validate [validate_values]
  (binding [validate_i nil] (try (do (when (= (count validate_values) 0) (throw (ex-info "return" {:v false}))) (set! validate_i 0) (while (< validate_i (count validate_values)) (do (when (<= (nth validate_values validate_i) 0.0) (throw (ex-info "return" {:v false}))) (set! validate_i (+ validate_i 1)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn effusion_ratio [effusion_ratio_m1 effusion_ratio_m2]
  (try (do (when (not (validate [effusion_ratio_m1 effusion_ratio_m2])) (do (println "ValueError: Molar mass values must greater than 0.") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (round6 (sqrtApprox (quot effusion_ratio_m2 effusion_ratio_m1)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn first_effusion_rate [first_effusion_rate_rate first_effusion_rate_m1 first_effusion_rate_m2]
  (try (do (when (not (validate [first_effusion_rate_rate first_effusion_rate_m1 first_effusion_rate_m2])) (do (println "ValueError: Molar mass and effusion rate values must greater than 0.") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (round6 (* first_effusion_rate_rate (sqrtApprox (quot first_effusion_rate_m2 first_effusion_rate_m1))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn second_effusion_rate [second_effusion_rate_rate second_effusion_rate_m1 second_effusion_rate_m2]
  (try (do (when (not (validate [second_effusion_rate_rate second_effusion_rate_m1 second_effusion_rate_m2])) (do (println "ValueError: Molar mass and effusion rate values must greater than 0.") (throw (ex-info "return" {:v 0.0})))) (throw (ex-info "return" {:v (round6 (/ second_effusion_rate_rate (sqrtApprox (quot second_effusion_rate_m2 second_effusion_rate_m1))))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn first_molar_mass [first_molar_mass_mass first_molar_mass_r1 first_molar_mass_r2]
  (binding [first_molar_mass_ratio nil] (try (do (when (not (validate [first_molar_mass_mass first_molar_mass_r1 first_molar_mass_r2])) (do (println "ValueError: Molar mass and effusion rate values must greater than 0.") (throw (ex-info "return" {:v 0.0})))) (set! first_molar_mass_ratio (quot first_molar_mass_r1 first_molar_mass_r2)) (throw (ex-info "return" {:v (round6 (quot first_molar_mass_mass (* first_molar_mass_ratio first_molar_mass_ratio)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn second_molar_mass [second_molar_mass_mass second_molar_mass_r1 second_molar_mass_r2]
  (binding [second_molar_mass_ratio nil] (try (do (when (not (validate [second_molar_mass_mass second_molar_mass_r1 second_molar_mass_r2])) (do (println "ValueError: Molar mass and effusion rate values must greater than 0.") (throw (ex-info "return" {:v 0.0})))) (set! second_molar_mass_ratio (quot second_molar_mass_r1 second_molar_mass_r2)) (throw (ex-info "return" {:v (round6 (quot (* second_molar_mass_ratio second_molar_mass_ratio) second_molar_mass_mass))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (effusion_ratio 2.016 4.002))
      (println (first_effusion_rate 1.0 2.016 4.002))
      (println (second_effusion_rate 1.0 2.016 4.002))
      (println (first_molar_mass 2.0 1.408943 0.709752))
      (println (second_molar_mass 2.0 1.408943 0.709752))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
