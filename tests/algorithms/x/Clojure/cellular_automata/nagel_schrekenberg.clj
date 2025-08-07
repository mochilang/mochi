(ns main (:refer-clojure :exclude [rand randint random construct_highway get_distance update simulate main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare rand randint random construct_highway get_distance update simulate main)

(def ^:dynamic construct_highway_highway nil)

(def ^:dynamic construct_highway_i nil)

(def ^:dynamic construct_highway_initial_speed nil)

(def ^:dynamic construct_highway_row nil)

(def ^:dynamic construct_highway_speed nil)

(def ^:dynamic construct_highway_step nil)

(def ^:dynamic get_distance_distance nil)

(def ^:dynamic get_distance_i nil)

(def ^:dynamic main_ex1 nil)

(def ^:dynamic main_ex2 nil)

(def ^:dynamic randint_r nil)

(def ^:dynamic simulate_highway nil)

(def ^:dynamic simulate_i nil)

(def ^:dynamic simulate_index nil)

(def ^:dynamic simulate_j nil)

(def ^:dynamic simulate_k nil)

(def ^:dynamic simulate_next_speeds nil)

(def ^:dynamic simulate_number_of_cells nil)

(def ^:dynamic simulate_real_next nil)

(def ^:dynamic simulate_speed nil)

(def ^:dynamic update_car_index nil)

(def ^:dynamic update_dn nil)

(def ^:dynamic update_i nil)

(def ^:dynamic update_new_speed nil)

(def ^:dynamic update_next_highway nil)

(def ^:dynamic update_number_of_cells nil)

(def ^:dynamic update_speed nil)

(def ^:dynamic main_seed 1)

(def ^:dynamic main_NEG_ONE (- 1))

(defn rand []
  (try (do (alter-var-root (var main_seed) (fn [_] (mod (+ (* main_seed 1103515245) 12345) 2147483648))) (throw (ex-info "return" {:v main_seed}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn randint [randint_a randint_b]
  (binding [randint_r nil] (try (do (set! randint_r (rand)) (throw (ex-info "return" {:v (+ randint_a (mod randint_r (+ (- randint_b randint_a) 1)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn random []
  (try (throw (ex-info "return" {:v (/ (* 1.0 (rand)) 2147483648.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn construct_highway [construct_highway_number_of_cells construct_highway_frequency construct_highway_initial_speed_p construct_highway_random_frequency construct_highway_random_speed construct_highway_max_speed]
  (binding [construct_highway_highway nil construct_highway_i nil construct_highway_initial_speed nil construct_highway_row nil construct_highway_speed nil construct_highway_step nil] (try (do (set! construct_highway_initial_speed construct_highway_initial_speed_p) (set! construct_highway_row []) (set! construct_highway_i 0) (while (< construct_highway_i construct_highway_number_of_cells) (do (set! construct_highway_row (conj construct_highway_row (- 1))) (set! construct_highway_i (+ construct_highway_i 1)))) (set! construct_highway_highway []) (set! construct_highway_highway (conj construct_highway_highway construct_highway_row)) (set! construct_highway_i 0) (when (< construct_highway_initial_speed 0) (set! construct_highway_initial_speed 0)) (while (< construct_highway_i construct_highway_number_of_cells) (do (set! construct_highway_speed construct_highway_initial_speed) (when construct_highway_random_speed (set! construct_highway_speed (randint 0 construct_highway_max_speed))) (set! construct_highway_highway (assoc-in construct_highway_highway [0 construct_highway_i] construct_highway_speed)) (set! construct_highway_step construct_highway_frequency) (when construct_highway_random_frequency (set! construct_highway_step (randint 1 (* construct_highway_max_speed 2)))) (set! construct_highway_i (+ construct_highway_i construct_highway_step)))) (throw (ex-info "return" {:v construct_highway_highway}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn get_distance [get_distance_highway_now get_distance_car_index]
  (binding [get_distance_distance nil get_distance_i nil] (try (do (set! get_distance_distance 0) (set! get_distance_i (+ get_distance_car_index 1)) (while (< get_distance_i (count get_distance_highway_now)) (do (when (> (nth get_distance_highway_now get_distance_i) main_NEG_ONE) (throw (ex-info "return" {:v get_distance_distance}))) (set! get_distance_distance (+ get_distance_distance 1)) (set! get_distance_i (+ get_distance_i 1)))) (throw (ex-info "return" {:v (+ get_distance_distance (get_distance get_distance_highway_now (- 1)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn update [update_highway_now update_probability update_max_speed]
  (binding [update_car_index nil update_dn nil update_i nil update_new_speed nil update_next_highway nil update_number_of_cells nil update_speed nil] (try (do (set! update_number_of_cells (count update_highway_now)) (set! update_next_highway []) (set! update_i 0) (while (< update_i update_number_of_cells) (do (set! update_next_highway (conj update_next_highway (- 1))) (set! update_i (+ update_i 1)))) (set! update_car_index 0) (while (< update_car_index update_number_of_cells) (do (set! update_speed (nth update_highway_now update_car_index)) (when (> update_speed main_NEG_ONE) (do (set! update_new_speed (+ update_speed 1)) (when (> update_new_speed update_max_speed) (set! update_new_speed update_max_speed)) (set! update_dn (- (get_distance update_highway_now update_car_index) 1)) (when (> update_new_speed update_dn) (set! update_new_speed update_dn)) (when (< (random) update_probability) (do (set! update_new_speed (- update_new_speed 1)) (when (< update_new_speed 0) (set! update_new_speed 0)))) (set! update_next_highway (assoc update_next_highway update_car_index update_new_speed)))) (set! update_car_index (+ update_car_index 1)))) (throw (ex-info "return" {:v update_next_highway}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn simulate [simulate_highway_p simulate_number_of_update simulate_probability simulate_max_speed]
  (binding [simulate_highway nil simulate_i nil simulate_index nil simulate_j nil simulate_k nil simulate_next_speeds nil simulate_number_of_cells nil simulate_real_next nil simulate_speed nil] (try (do (set! simulate_highway simulate_highway_p) (set! simulate_number_of_cells (count (nth simulate_highway 0))) (set! simulate_i 0) (while (< simulate_i simulate_number_of_update) (do (set! simulate_next_speeds (update (nth simulate_highway simulate_i) simulate_probability simulate_max_speed)) (set! simulate_real_next []) (set! simulate_j 0) (while (< simulate_j simulate_number_of_cells) (do (set! simulate_real_next (conj simulate_real_next (- 1))) (set! simulate_j (+ simulate_j 1)))) (set! simulate_k 0) (while (< simulate_k simulate_number_of_cells) (do (set! simulate_speed (nth simulate_next_speeds simulate_k)) (when (> simulate_speed main_NEG_ONE) (do (set! simulate_index (mod (+ simulate_k simulate_speed) simulate_number_of_cells)) (set! simulate_real_next (assoc simulate_real_next simulate_index simulate_speed)))) (set! simulate_k (+ simulate_k 1)))) (set! simulate_highway (conj simulate_highway simulate_real_next)) (set! simulate_i (+ simulate_i 1)))) (throw (ex-info "return" {:v simulate_highway}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_ex1 nil main_ex2 nil] (do (set! main_ex1 (simulate (construct_highway 6 3 0 false false 2) 2 0.0 2)) (println (str main_ex1)) (set! main_ex2 (simulate (construct_highway 5 2 (- 2) false false 2) 3 0.0 2)) (println (str main_ex2)))))

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
