(ns main (:refer-clojure :exclude [sqrt rms_speed_of_molecule]))

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

(declare sqrt rms_speed_of_molecule)

(def ^:dynamic rms_speed_of_molecule_num nil)

(def ^:dynamic rms_speed_of_molecule_result nil)

(def ^:dynamic rms_speed_of_molecule_val nil)

(def ^:dynamic sqrt_guess nil)

(def ^:dynamic sqrt_i nil)

(def ^:dynamic main_UNIVERSAL_GAS_CONSTANT 8.3144598)

(defn sqrt [sqrt_x]
  (binding [sqrt_guess nil sqrt_i nil] (try (do (when (<= sqrt_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_guess sqrt_x) (set! sqrt_i 0) (while (< sqrt_i 10) (do (set! sqrt_guess (/ (+ sqrt_guess (/ sqrt_x sqrt_guess)) 2.0)) (set! sqrt_i (+ sqrt_i 1)))) (throw (ex-info "return" {:v sqrt_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rms_speed_of_molecule [rms_speed_of_molecule_temperature rms_speed_of_molecule_molar_mass]
  (binding [rms_speed_of_molecule_num nil rms_speed_of_molecule_result nil rms_speed_of_molecule_val nil] (try (do (when (< rms_speed_of_molecule_temperature 0.0) (throw (Exception. "Temperature cannot be less than 0 K"))) (when (<= rms_speed_of_molecule_molar_mass 0.0) (throw (Exception. "Molar mass cannot be less than or equal to 0 kg/mol"))) (set! rms_speed_of_molecule_num (* (* 3.0 main_UNIVERSAL_GAS_CONSTANT) rms_speed_of_molecule_temperature)) (set! rms_speed_of_molecule_val (/ rms_speed_of_molecule_num rms_speed_of_molecule_molar_mass)) (set! rms_speed_of_molecule_result (sqrt rms_speed_of_molecule_val)) (throw (ex-info "return" {:v rms_speed_of_molecule_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_vrms (rms_speed_of_molecule 300.0 28.0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "rms_speed_of_molecule(100, 2) = " (str (rms_speed_of_molecule 100.0 2.0))))
      (println (str "rms_speed_of_molecule(273, 12) = " (str (rms_speed_of_molecule 273.0 12.0))))
      (println (str (str "Vrms of Nitrogen gas at 300 K is " (str main_vrms)) " m/s"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
