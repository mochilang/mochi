(ns main (:refer-clojure :exclude [sqrt terminal_velocity]))

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

(declare sqrt terminal_velocity)

(def ^:dynamic sqrt_guess nil)

(def ^:dynamic sqrt_i nil)

(def ^:dynamic terminal_velocity_denominator nil)

(def ^:dynamic terminal_velocity_numerator nil)

(def ^:dynamic terminal_velocity_result nil)

(def ^:dynamic main_G nil)

(defn sqrt [sqrt_x]
  (binding [sqrt_guess nil sqrt_i nil] (try (do (when (<= sqrt_x 0.0) (throw (ex-info "return" {:v 0.0}))) (set! sqrt_guess sqrt_x) (set! sqrt_i 0) (while (< sqrt_i 10) (do (set! sqrt_guess (/ (+ sqrt_guess (quot sqrt_x sqrt_guess)) 2.0)) (set! sqrt_i (+ sqrt_i 1)))) (throw (ex-info "return" {:v sqrt_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn terminal_velocity [terminal_velocity_mass terminal_velocity_density terminal_velocity_area terminal_velocity_drag_coefficient]
  (binding [terminal_velocity_denominator nil terminal_velocity_numerator nil terminal_velocity_result nil] (try (do (when (or (or (or (<= terminal_velocity_mass 0.0) (<= terminal_velocity_density 0.0)) (<= terminal_velocity_area 0.0)) (<= terminal_velocity_drag_coefficient 0.0)) (throw (Exception. "mass, density, area and the drag coefficient all need to be positive"))) (set! terminal_velocity_numerator (* (* 2.0 terminal_velocity_mass) main_G)) (set! terminal_velocity_denominator (* (* terminal_velocity_density terminal_velocity_area) terminal_velocity_drag_coefficient)) (set! terminal_velocity_result (sqrt (quot terminal_velocity_numerator terminal_velocity_denominator))) (throw (ex-info "return" {:v terminal_velocity_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_G) (constantly 9.80665))
      (println (mochi_str (terminal_velocity 1.0 25.0 0.6 0.77)))
      (println (mochi_str (terminal_velocity 2.0 100.0 0.45 0.23)))
      (println (mochi_str (terminal_velocity 5.0 50.0 0.2 0.5)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
