(ns main (:refer-clojure :exclude [sqrt avg_speed_of_molecule mps_speed_of_molecule]))

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

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sqrt avg_speed_of_molecule mps_speed_of_molecule)

(def ^:dynamic avg_speed_of_molecule_expr nil)

(def ^:dynamic avg_speed_of_molecule_s nil)

(def ^:dynamic mps_speed_of_molecule_expr nil)

(def ^:dynamic mps_speed_of_molecule_s nil)

(def ^:dynamic sqrt_guess nil)

(def ^:dynamic sqrt_i nil)

(def ^:dynamic main_PI 3.141592653589793)

(def ^:dynamic main_R 8.31446261815324)

(defn sqrt [sqrt_x]
  (binding [sqrt_guess nil sqrt_i nil] (try (do (when (<= sqrt_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_guess sqrt_x) (set! sqrt_i 0) (while (< sqrt_i 20) (do (set! sqrt_guess (/ (+ sqrt_guess (/ sqrt_x sqrt_guess)) 2.0)) (set! sqrt_i (+ sqrt_i 1)))) (throw (ex-info "return" {:v sqrt_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn avg_speed_of_molecule [avg_speed_of_molecule_temperature avg_speed_of_molecule_molar_mass]
  (binding [avg_speed_of_molecule_expr nil avg_speed_of_molecule_s nil] (try (do (when (< avg_speed_of_molecule_temperature 0.0) (throw (Exception. "Absolute temperature cannot be less than 0 K"))) (when (<= avg_speed_of_molecule_molar_mass 0.0) (throw (Exception. "Molar mass should be greater than 0 kg/mol"))) (set! avg_speed_of_molecule_expr (/ (* (* 8.0 main_R) avg_speed_of_molecule_temperature) (* main_PI avg_speed_of_molecule_molar_mass))) (set! avg_speed_of_molecule_s (sqrt avg_speed_of_molecule_expr)) (throw (ex-info "return" {:v avg_speed_of_molecule_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn mps_speed_of_molecule [mps_speed_of_molecule_temperature mps_speed_of_molecule_molar_mass]
  (binding [mps_speed_of_molecule_expr nil mps_speed_of_molecule_s nil] (try (do (when (< mps_speed_of_molecule_temperature 0.0) (throw (Exception. "Absolute temperature cannot be less than 0 K"))) (when (<= mps_speed_of_molecule_molar_mass 0.0) (throw (Exception. "Molar mass should be greater than 0 kg/mol"))) (set! mps_speed_of_molecule_expr (/ (* (* 2.0 main_R) mps_speed_of_molecule_temperature) mps_speed_of_molecule_molar_mass)) (set! mps_speed_of_molecule_s (sqrt mps_speed_of_molecule_expr)) (throw (ex-info "return" {:v mps_speed_of_molecule_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (avg_speed_of_molecule 273.0 0.028)))
      (println (str (avg_speed_of_molecule 300.0 0.032)))
      (println (str (mps_speed_of_molecule 273.0 0.028)))
      (println (str (mps_speed_of_molecule 300.0 0.032)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
