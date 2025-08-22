(ns main (:refer-clojure :exclude [_mod sin deg_to_rad floor pow10 round check_args horizontal_distance max_height total_time]))

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

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare _mod sin deg_to_rad floor pow10 round check_args horizontal_distance max_height total_time)

(def ^:dynamic floor_i nil)

(def ^:dynamic horizontal_distance_radians nil)

(def ^:dynamic max_height_radians nil)

(def ^:dynamic max_height_s nil)

(def ^:dynamic pow10_i nil)

(def ^:dynamic pow10_result nil)

(def ^:dynamic round_m nil)

(def ^:dynamic round_y nil)

(def ^:dynamic sin_y nil)

(def ^:dynamic sin_y2 nil)

(def ^:dynamic sin_y3 nil)

(def ^:dynamic sin_y5 nil)

(def ^:dynamic sin_y7 nil)

(def ^:dynamic total_time_radians nil)

(def ^:dynamic main_PI nil)

(def ^:dynamic main_TWO_PI nil)

(def ^:dynamic main_g nil)

(defn _mod [_mod_x _mod_m]
  (try (throw (ex-info "return" {:v (- _mod_x (* (double (toi (/ _mod_x _mod_m))) _mod_m))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sin [sin_x]
  (binding [sin_y nil sin_y2 nil sin_y3 nil sin_y5 nil sin_y7 nil] (try (do (set! sin_y (- (_mod (+ sin_x main_PI) main_TWO_PI) main_PI)) (set! sin_y2 (* sin_y sin_y)) (set! sin_y3 (* sin_y2 sin_y)) (set! sin_y5 (* sin_y3 sin_y2)) (set! sin_y7 (* sin_y5 sin_y2)) (throw (ex-info "return" {:v (- (+ (- sin_y (/ sin_y3 6.0)) (/ sin_y5 120.0)) (/ sin_y7 5040.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn deg_to_rad [deg_to_rad_deg]
  (try (throw (ex-info "return" {:v (/ (* deg_to_rad_deg main_PI) 180.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn floor [floor_x]
  (binding [floor_i nil] (try (do (set! floor_i (long floor_x)) (when (> (double floor_i) floor_x) (set! floor_i (- floor_i 1))) (throw (ex-info "return" {:v (double floor_i)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pow10 [pow10_n]
  (binding [pow10_i nil pow10_result nil] (try (do (set! pow10_result 1.0) (set! pow10_i 0) (while (< pow10_i pow10_n) (do (set! pow10_result (* pow10_result 10.0)) (set! pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn round [round_x round_n]
  (binding [round_m nil round_y nil] (try (do (set! round_m (pow10 round_n)) (set! round_y (floor (+ (* round_x round_m) 0.5))) (throw (ex-info "return" {:v (/ round_y round_m)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn check_args [check_args_init_velocity check_args_angle]
  (do (when (or (> check_args_angle 90.0) (< check_args_angle 1.0)) (throw (Exception. "Invalid angle. Range is 1-90 degrees."))) (when (< check_args_init_velocity 0.0) (throw (Exception. "Invalid velocity. Should be a positive number."))) check_args_init_velocity))

(defn horizontal_distance [horizontal_distance_init_velocity horizontal_distance_angle]
  (binding [horizontal_distance_radians nil] (try (do (check_args horizontal_distance_init_velocity horizontal_distance_angle) (set! horizontal_distance_radians (deg_to_rad (* 2.0 horizontal_distance_angle))) (throw (ex-info "return" {:v (round (/ (* (* horizontal_distance_init_velocity horizontal_distance_init_velocity) (sin horizontal_distance_radians)) main_g) 2)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn max_height [max_height_init_velocity max_height_angle]
  (binding [max_height_radians nil max_height_s nil] (try (do (check_args max_height_init_velocity max_height_angle) (set! max_height_radians (deg_to_rad max_height_angle)) (set! max_height_s (sin max_height_radians)) (throw (ex-info "return" {:v (round (/ (* (* (* max_height_init_velocity max_height_init_velocity) max_height_s) max_height_s) (* 2.0 main_g)) 2)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn total_time [total_time_init_velocity total_time_angle]
  (binding [total_time_radians nil] (try (do (check_args total_time_init_velocity total_time_angle) (set! total_time_radians (deg_to_rad total_time_angle)) (throw (ex-info "return" {:v (round (/ (* (* 2.0 total_time_init_velocity) (sin total_time_radians)) main_g) 2)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_v0 nil)

(def ^:dynamic main_angle nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_PI) (constantly 3.141592653589793))
      (alter-var-root (var main_TWO_PI) (constantly 6.283185307179586))
      (alter-var-root (var main_g) (constantly 9.80665))
      (alter-var-root (var main_v0) (constantly 25.0))
      (alter-var-root (var main_angle) (constantly 20.0))
      (println (horizontal_distance main_v0 main_angle))
      (println (max_height main_v0 main_angle))
      (println (total_time main_v0 main_angle))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
