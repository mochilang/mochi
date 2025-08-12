(ns main (:refer-clojure :exclude [_mod cos radians abs_val malus_law main]))

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

(declare _mod cos radians abs_val malus_law main)

(def ^:dynamic cos_y nil)

(def ^:dynamic cos_y2 nil)

(def ^:dynamic cos_y4 nil)

(def ^:dynamic cos_y6 nil)

(def ^:dynamic malus_law_c nil)

(def ^:dynamic malus_law_theta nil)

(def ^:dynamic main_PI 3.141592653589793)

(def ^:dynamic main_TWO_PI 6.283185307179586)

(defn _mod [_mod_x _mod_m]
  (try (throw (ex-info "return" {:v (- _mod_x (* (floor (/ _mod_x _mod_m)) _mod_m))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn cos [cos_x]
  (binding [cos_y nil cos_y2 nil cos_y4 nil cos_y6 nil] (try (do (set! cos_y (- (_mod (+ cos_x main_PI) main_TWO_PI) main_PI)) (set! cos_y2 (* cos_y cos_y)) (set! cos_y4 (* cos_y2 cos_y2)) (set! cos_y6 (* cos_y4 cos_y2)) (throw (ex-info "return" {:v (- (+ (- 1.0 (/ cos_y2 2.0)) (/ cos_y4 24.0)) (/ cos_y6 720.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn radians [radians_deg]
  (try (throw (ex-info "return" {:v (/ (* radians_deg main_PI) 180.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn abs_val [abs_val_x]
  (try (if (< abs_val_x 0.0) (- abs_val_x) abs_val_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn malus_law [malus_law_initial_intensity malus_law_angle]
  (binding [malus_law_c nil malus_law_theta nil] (try (do (when (< malus_law_initial_intensity 0.0) (throw (Exception. "The value of intensity cannot be negative"))) (when (or (< malus_law_angle 0.0) (> malus_law_angle 360.0)) (throw (Exception. "In Malus Law, the angle is in the range 0-360 degrees"))) (set! malus_law_theta (radians malus_law_angle)) (set! malus_law_c (cos malus_law_theta)) (throw (ex-info "return" {:v (* malus_law_initial_intensity (* malus_law_c malus_law_c))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (println (str (malus_law 100.0 60.0))))

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
