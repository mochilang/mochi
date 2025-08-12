(ns main (:refer-clojure :exclude [make_body update_velocity update_position make_body_system sqrtApprox update_system main]))

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

(declare make_body update_velocity update_position make_body_system sqrtApprox update_system main)

(def ^:dynamic main_b1 nil)

(def ^:dynamic main_b1_after nil)

(def ^:dynamic main_b2 nil)

(def ^:dynamic main_b2_after nil)

(def ^:dynamic main_b3 nil)

(def ^:dynamic main_b4 nil)

(def ^:dynamic main_pos1x nil)

(def ^:dynamic main_pos1y nil)

(def ^:dynamic main_pos2x nil)

(def ^:dynamic main_pos2y nil)

(def ^:dynamic main_sys1 nil)

(def ^:dynamic main_sys2 nil)

(def ^:dynamic main_vel1x nil)

(def ^:dynamic main_vel1y nil)

(def ^:dynamic main_vel2x nil)

(def ^:dynamic main_vel2y nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(def ^:dynamic update_position_body nil)

(def ^:dynamic update_system_bodies nil)

(def ^:dynamic update_system_body nil)

(def ^:dynamic update_system_body1 nil)

(def ^:dynamic update_system_body2 nil)

(def ^:dynamic update_system_denom nil)

(def ^:dynamic update_system_dif_x nil)

(def ^:dynamic update_system_dif_y nil)

(def ^:dynamic update_system_distance nil)

(def ^:dynamic update_system_distance_sq nil)

(def ^:dynamic update_system_force_x nil)

(def ^:dynamic update_system_force_y nil)

(def ^:dynamic update_system_i nil)

(def ^:dynamic update_system_j nil)

(def ^:dynamic update_system_system nil)

(def ^:dynamic update_velocity_body nil)

(defn make_body [make_body_px make_body_py make_body_vx make_body_vy make_body_mass]
  (try (throw (ex-info "return" {:v {:mass make_body_mass :position_x make_body_px :position_y make_body_py :velocity_x make_body_vx :velocity_y make_body_vy}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn update_velocity [update_velocity_body_p update_velocity_force_x update_velocity_force_y update_velocity_delta_time]
  (binding [update_velocity_body update_velocity_body_p] (try (do (set! update_velocity_body (assoc update_velocity_body :velocity_x (+ (:velocity_x update_velocity_body) (* update_velocity_force_x update_velocity_delta_time)))) (set! update_velocity_body (assoc update_velocity_body :velocity_y (+ (:velocity_y update_velocity_body) (* update_velocity_force_y update_velocity_delta_time)))) (throw (ex-info "return" {:v update_velocity_body}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var update_velocity_body) (constantly update_velocity_body))))))

(defn update_position [update_position_body_p update_position_delta_time]
  (binding [update_position_body update_position_body_p] (try (do (set! update_position_body (assoc update_position_body :position_x (+ (:position_x update_position_body) (* (:velocity_x update_position_body) update_position_delta_time)))) (set! update_position_body (assoc update_position_body :position_y (+ (:position_y update_position_body) (* (:velocity_y update_position_body) update_position_delta_time)))) (throw (ex-info "return" {:v update_position_body}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var update_position_body) (constantly update_position_body))))))

(defn make_body_system [make_body_system_bodies make_body_system_g make_body_system_tf make_body_system_sf]
  (try (throw (ex-info "return" {:v {:bodies make_body_system_bodies :gravitation_constant make_body_system_g :softening_factor make_body_system_sf :time_factor make_body_system_tf}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (/ sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn update_system [update_system_system_p update_system_delta_time]
  (binding [update_system_system update_system_system_p update_system_bodies nil update_system_body nil update_system_body1 nil update_system_body2 nil update_system_denom nil update_system_dif_x nil update_system_dif_y nil update_system_distance nil update_system_distance_sq nil update_system_force_x nil update_system_force_y nil update_system_i nil update_system_j nil] (try (do (set! update_system_bodies (:bodies update_system_system)) (set! update_system_i 0) (while (< update_system_i (count update_system_bodies)) (do (set! update_system_body1 (get update_system_bodies update_system_i)) (set! update_system_force_x 0.0) (set! update_system_force_y 0.0) (set! update_system_j 0) (while (< update_system_j (count update_system_bodies)) (do (when (not= update_system_i update_system_j) (do (set! update_system_body2 (get update_system_bodies update_system_j)) (set! update_system_dif_x (- (:position_x update_system_body2) (:position_x update_system_body1))) (set! update_system_dif_y (- (:position_y update_system_body2) (:position_y update_system_body1))) (set! update_system_distance_sq (+ (+ (* update_system_dif_x update_system_dif_x) (* update_system_dif_y update_system_dif_y)) (:softening_factor update_system_system))) (set! update_system_distance (sqrtApprox update_system_distance_sq)) (set! update_system_denom (* (* update_system_distance update_system_distance) update_system_distance)) (set! update_system_force_x (+ update_system_force_x (/ (* (* (:gravitation_constant update_system_system) (:mass update_system_body2)) update_system_dif_x) update_system_denom))) (set! update_system_force_y (+ update_system_force_y (/ (* (* (:gravitation_constant update_system_system) (:mass update_system_body2)) update_system_dif_y) update_system_denom))))) (set! update_system_j (+ update_system_j 1)))) (set! update_system_body1 (let [__res (update_velocity update_system_body1 update_system_force_x update_system_force_y (* update_system_delta_time (:time_factor update_system_system)))] (do (set! update_system_body1 update_velocity_body) __res))) (set! update_system_bodies (assoc update_system_bodies update_system_i update_system_body1)) (set! update_system_i (+ update_system_i 1)))) (set! update_system_i 0) (while (< update_system_i (count update_system_bodies)) (do (set! update_system_body (get update_system_bodies update_system_i)) (set! update_system_body (let [__res (update_position update_system_body (* update_system_delta_time (:time_factor update_system_system)))] (do (set! update_system_body update_position_body) __res))) (set! update_system_bodies (assoc update_system_bodies update_system_i update_system_body)) (set! update_system_i (+ update_system_i 1)))) (set! update_system_system (assoc update_system_system :bodies update_system_bodies)) (throw (ex-info "return" {:v update_system_system}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var update_system_system) (constantly update_system_system))))))

(defn main []
  (binding [main_b1 nil main_b1_after nil main_b2 nil main_b2_after nil main_b3 nil main_b4 nil main_pos1x nil main_pos1y nil main_pos2x nil main_pos2y nil main_sys1 nil main_sys2 nil main_vel1x nil main_vel1y nil main_vel2x nil main_vel2y nil] (do (set! main_b1 (make_body 0.0 0.0 0.0 0.0 1.0)) (set! main_b2 (make_body 10.0 0.0 0.0 0.0 1.0)) (set! main_sys1 (make_body_system [main_b1 main_b2] 1.0 1.0 0.0)) (set! main_sys1 (let [__res (update_system main_sys1 1.0)] (do (set! main_sys1 update_system_system) __res))) (set! main_b1_after (get (:bodies main_sys1) 0)) (set! main_pos1x (:position_x main_b1_after)) (set! main_pos1y (:position_y main_b1_after)) (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) {"x" main_pos1x "y" main_pos1y})) (set! main_vel1x (:velocity_x main_b1_after)) (set! main_vel1y (:velocity_y main_b1_after)) (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) {"vx" main_vel1x "vy" main_vel1y})) (set! main_b3 (make_body (- 10.0) 0.0 0.0 0.0 1.0)) (set! main_b4 (make_body 10.0 0.0 0.0 0.0 4.0)) (set! main_sys2 (make_body_system [main_b3 main_b4] 1.0 10.0 0.0)) (set! main_sys2 (let [__res (update_system main_sys2 1.0)] (do (set! main_sys2 update_system_system) __res))) (set! main_b2_after (get (:bodies main_sys2) 0)) (set! main_pos2x (:position_x main_b2_after)) (set! main_pos2y (:position_y main_b2_after)) (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) {"x" main_pos2x "y" main_pos2y})) (set! main_vel2x (:velocity_x main_b2_after)) (set! main_vel2y (:velocity_y main_b2_after)) (println ((fn json_str [x] (cond (map? x) (str "{" (clojure.string/join "," (map (fn [[k v]] (str "\"" (name k) "\":" (json_str v))) x)) "}") (sequential? x) (str "[" (clojure.string/join "," (map json_str x)) "]") (string? x) (pr-str x) :else (str x))) {"vx" main_vel2x "vy" main_vel2y})))))

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
