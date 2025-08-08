(ns main (:refer-clojure :exclude [sqrtApprox rand_float pow_float distance choose_weighted city_select pheromone_update remove_value ant_colony]))

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

(declare sqrtApprox rand_float pow_float distance choose_weighted city_select pheromone_update remove_value ant_colony)

(def ^:dynamic ant_colony_a nil)

(def ^:dynamic ant_colony_ants_route nil)

(def ^:dynamic ant_colony_best_distance nil)

(def ^:dynamic ant_colony_best_path nil)

(def ^:dynamic ant_colony_current nil)

(def ^:dynamic ant_colony_dist nil)

(def ^:dynamic ant_colony_i nil)

(def ^:dynamic ant_colony_iter nil)

(def ^:dynamic ant_colony_j nil)

(def ^:dynamic ant_colony_k nil)

(def ^:dynamic ant_colony_n nil)

(def ^:dynamic ant_colony_next_city nil)

(def ^:dynamic ant_colony_pheromone nil)

(def ^:dynamic ant_colony_r nil)

(def ^:dynamic ant_colony_route nil)

(def ^:dynamic ant_colony_row nil)

(def ^:dynamic ant_colony_unvisited nil)

(def ^:dynamic choose_weighted_accum nil)

(def ^:dynamic choose_weighted_i nil)

(def ^:dynamic choose_weighted_r nil)

(def ^:dynamic choose_weighted_total nil)

(def ^:dynamic city_select_city nil)

(def ^:dynamic city_select_dist nil)

(def ^:dynamic city_select_i nil)

(def ^:dynamic city_select_prob nil)

(def ^:dynamic city_select_probs nil)

(def ^:dynamic city_select_trail nil)

(def ^:dynamic distance_dx nil)

(def ^:dynamic distance_dy nil)

(def ^:dynamic pheromone_update_a nil)

(def ^:dynamic pheromone_update_delta nil)

(def ^:dynamic pheromone_update_i nil)

(def ^:dynamic pheromone_update_j nil)

(def ^:dynamic pheromone_update_n nil)

(def ^:dynamic pheromone_update_pheromone nil)

(def ^:dynamic pheromone_update_r nil)

(def ^:dynamic pheromone_update_route nil)

(def ^:dynamic pheromone_update_total nil)

(def ^:dynamic pheromone_update_u nil)

(def ^:dynamic pheromone_update_v nil)

(def ^:dynamic pow_float_e nil)

(def ^:dynamic pow_float_i nil)

(def ^:dynamic pow_float_result nil)

(def ^:dynamic remove_value_i nil)

(def ^:dynamic remove_value_res nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (set! sqrtApprox_guess (/ sqrtApprox_x 2.0)) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rand_float []
  (try (throw (ex-info "return" {:v (/ (double (mod (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647))) 1000000)) 1000000.0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pow_float [pow_float_base pow_float_exp]
  (binding [pow_float_e nil pow_float_i nil pow_float_result nil] (try (do (set! pow_float_result 1.0) (set! pow_float_i 0) (set! pow_float_e (long pow_float_exp)) (while (< pow_float_i pow_float_e) (do (set! pow_float_result (* pow_float_result pow_float_base)) (set! pow_float_i (+ pow_float_i 1)))) (throw (ex-info "return" {:v pow_float_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn distance [distance_city1 distance_city2]
  (binding [distance_dx nil distance_dy nil] (try (do (set! distance_dx (double (- (nth distance_city1 0) (nth distance_city2 0)))) (set! distance_dy (double (- (nth distance_city1 1) (nth distance_city2 1)))) (throw (ex-info "return" {:v (sqrtApprox (+ (* distance_dx distance_dx) (* distance_dy distance_dy)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn choose_weighted [choose_weighted_options choose_weighted_weights]
  (binding [choose_weighted_accum nil choose_weighted_i nil choose_weighted_r nil choose_weighted_total nil] (try (do (set! choose_weighted_total 0.0) (set! choose_weighted_i 0) (while (< choose_weighted_i (count choose_weighted_weights)) (do (set! choose_weighted_total (+ choose_weighted_total (nth choose_weighted_weights choose_weighted_i))) (set! choose_weighted_i (+ choose_weighted_i 1)))) (set! choose_weighted_r (* (rand_float) choose_weighted_total)) (set! choose_weighted_accum 0.0) (set! choose_weighted_i 0) (while (< choose_weighted_i (count choose_weighted_weights)) (do (set! choose_weighted_accum (+ choose_weighted_accum (nth choose_weighted_weights choose_weighted_i))) (when (<= choose_weighted_r choose_weighted_accum) (throw (ex-info "return" {:v (nth choose_weighted_options choose_weighted_i)}))) (set! choose_weighted_i (+ choose_weighted_i 1)))) (throw (ex-info "return" {:v (nth choose_weighted_options (- (count choose_weighted_options) 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn city_select [city_select_pheromone city_select_current city_select_unvisited city_select_alpha city_select_beta city_select_cities]
  (binding [city_select_city nil city_select_dist nil city_select_i nil city_select_prob nil city_select_probs nil city_select_trail nil] (try (do (set! city_select_probs []) (set! city_select_i 0) (while (< city_select_i (count city_select_unvisited)) (do (set! city_select_city (nth city_select_unvisited city_select_i)) (set! city_select_dist (distance (get city_select_cities city_select_city) (get city_select_cities city_select_current))) (set! city_select_trail (nth (nth city_select_pheromone city_select_city) city_select_current)) (set! city_select_prob (* (pow_float city_select_trail city_select_alpha) (pow_float (/ 1.0 city_select_dist) city_select_beta))) (set! city_select_probs (conj city_select_probs city_select_prob)) (set! city_select_i (+ city_select_i 1)))) (throw (ex-info "return" {:v (choose_weighted city_select_unvisited city_select_probs)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pheromone_update [pheromone_update_pheromone_p pheromone_update_cities pheromone_update_evaporation pheromone_update_ants_route pheromone_update_q]
  (binding [pheromone_update_a nil pheromone_update_delta nil pheromone_update_i nil pheromone_update_j nil pheromone_update_n nil pheromone_update_pheromone nil pheromone_update_r nil pheromone_update_route nil pheromone_update_total nil pheromone_update_u nil pheromone_update_v nil] (try (do (set! pheromone_update_pheromone pheromone_update_pheromone_p) (set! pheromone_update_n (count pheromone_update_pheromone)) (set! pheromone_update_i 0) (while (< pheromone_update_i pheromone_update_n) (do (set! pheromone_update_j 0) (while (< pheromone_update_j pheromone_update_n) (do (set! pheromone_update_pheromone (assoc-in pheromone_update_pheromone [pheromone_update_i pheromone_update_j] (* (nth (nth pheromone_update_pheromone pheromone_update_i) pheromone_update_j) pheromone_update_evaporation))) (set! pheromone_update_j (+ pheromone_update_j 1)))) (set! pheromone_update_i (+ pheromone_update_i 1)))) (set! pheromone_update_a 0) (while (< pheromone_update_a (count pheromone_update_ants_route)) (do (set! pheromone_update_route (nth pheromone_update_ants_route pheromone_update_a)) (set! pheromone_update_total 0.0) (set! pheromone_update_r 0) (while (< pheromone_update_r (- (count pheromone_update_route) 1)) (do (set! pheromone_update_total (+ pheromone_update_total (distance (get pheromone_update_cities (nth pheromone_update_route pheromone_update_r)) (get pheromone_update_cities (nth pheromone_update_route (+ pheromone_update_r 1)))))) (set! pheromone_update_r (+ pheromone_update_r 1)))) (set! pheromone_update_delta (quot pheromone_update_q pheromone_update_total)) (set! pheromone_update_r 0) (while (< pheromone_update_r (- (count pheromone_update_route) 1)) (do (set! pheromone_update_u (nth pheromone_update_route pheromone_update_r)) (set! pheromone_update_v (nth pheromone_update_route (+ pheromone_update_r 1))) (set! pheromone_update_pheromone (assoc-in pheromone_update_pheromone [pheromone_update_u pheromone_update_v] (+ (nth (nth pheromone_update_pheromone pheromone_update_u) pheromone_update_v) pheromone_update_delta))) (set! pheromone_update_pheromone (assoc-in pheromone_update_pheromone [pheromone_update_v pheromone_update_u] (nth (nth pheromone_update_pheromone pheromone_update_u) pheromone_update_v))) (set! pheromone_update_r (+ pheromone_update_r 1)))) (set! pheromone_update_a (+ pheromone_update_a 1)))) (throw (ex-info "return" {:v pheromone_update_pheromone}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn remove_value [remove_value_lst remove_value_val]
  (binding [remove_value_i nil remove_value_res nil] (try (do (set! remove_value_res []) (set! remove_value_i 0) (while (< remove_value_i (count remove_value_lst)) (do (when (not= (nth remove_value_lst remove_value_i) remove_value_val) (set! remove_value_res (conj remove_value_res (nth remove_value_lst remove_value_i)))) (set! remove_value_i (+ remove_value_i 1)))) (throw (ex-info "return" {:v remove_value_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn ant_colony [ant_colony_cities ant_colony_ants_num ant_colony_iterations ant_colony_evaporation ant_colony_alpha ant_colony_beta ant_colony_q]
  (binding [ant_colony_a nil ant_colony_ants_route nil ant_colony_best_distance nil ant_colony_best_path nil ant_colony_current nil ant_colony_dist nil ant_colony_i nil ant_colony_iter nil ant_colony_j nil ant_colony_k nil ant_colony_n nil ant_colony_next_city nil ant_colony_pheromone nil ant_colony_r nil ant_colony_route nil ant_colony_row nil ant_colony_unvisited nil] (do (set! ant_colony_n (count ant_colony_cities)) (set! ant_colony_pheromone []) (set! ant_colony_i 0) (while (< ant_colony_i ant_colony_n) (do (set! ant_colony_row []) (set! ant_colony_j 0) (while (< ant_colony_j ant_colony_n) (do (set! ant_colony_row (conj ant_colony_row 1.0)) (set! ant_colony_j (+ ant_colony_j 1)))) (set! ant_colony_pheromone (conj ant_colony_pheromone ant_colony_row)) (set! ant_colony_i (+ ant_colony_i 1)))) (set! ant_colony_best_path []) (set! ant_colony_best_distance 1000000000.0) (set! ant_colony_iter 0) (while (< ant_colony_iter ant_colony_iterations) (do (set! ant_colony_ants_route []) (set! ant_colony_k 0) (while (< ant_colony_k ant_colony_ants_num) (do (set! ant_colony_route [0]) (set! ant_colony_unvisited []) (doseq [key (keys ant_colony_cities)] (when (not= key 0) (set! ant_colony_unvisited (conj ant_colony_unvisited key)))) (set! ant_colony_current 0) (while (> (count ant_colony_unvisited) 0) (do (set! ant_colony_next_city (city_select ant_colony_pheromone ant_colony_current ant_colony_unvisited ant_colony_alpha ant_colony_beta ant_colony_cities)) (set! ant_colony_route (conj ant_colony_route ant_colony_next_city)) (set! ant_colony_unvisited (remove_value ant_colony_unvisited ant_colony_next_city)) (set! ant_colony_current ant_colony_next_city))) (set! ant_colony_route (conj ant_colony_route 0)) (set! ant_colony_ants_route (conj ant_colony_ants_route ant_colony_route)) (set! ant_colony_k (+ ant_colony_k 1)))) (set! ant_colony_pheromone (pheromone_update ant_colony_pheromone ant_colony_cities ant_colony_evaporation ant_colony_ants_route ant_colony_q)) (set! ant_colony_a 0) (while (< ant_colony_a (count ant_colony_ants_route)) (do (set! ant_colony_route (nth ant_colony_ants_route ant_colony_a)) (set! ant_colony_dist 0.0) (set! ant_colony_r 0) (while (< ant_colony_r (- (count ant_colony_route) 1)) (do (set! ant_colony_dist (+ ant_colony_dist (distance (get ant_colony_cities (nth ant_colony_route ant_colony_r)) (get ant_colony_cities (nth ant_colony_route (+ ant_colony_r 1)))))) (set! ant_colony_r (+ ant_colony_r 1)))) (when (< ant_colony_dist ant_colony_best_distance) (do (set! ant_colony_best_distance ant_colony_dist) (set! ant_colony_best_path ant_colony_route))) (set! ant_colony_a (+ ant_colony_a 1)))) (set! ant_colony_iter (+ ant_colony_iter 1)))) (println (str "best_path = " (str ant_colony_best_path))) (println (str "best_distance = " (str ant_colony_best_distance))))))

(def ^:dynamic main_cities {0 [0 0] 1 [0 5] 2 [3 8] 3 [8 10] 4 [12 8] 5 [12 4] 6 [8 0] 7 [6 2]})

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (ant_colony main_cities 10 20 0.7 1.0 5.0 10.0)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
