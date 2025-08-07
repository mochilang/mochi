(ns main (:refer-clojure :exclude [abs sqrtApprox euclidean_distance_sqr column_based_sort dis_between_closest_pair dis_between_closest_in_strip closest_pair_of_points_sqr closest_pair_of_points]))

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

(declare abs sqrtApprox euclidean_distance_sqr column_based_sort dis_between_closest_pair dis_between_closest_in_strip closest_pair_of_points_sqr closest_pair_of_points)

(def ^:dynamic closest_pair_of_points_dist_sqr nil)

(def ^:dynamic closest_pair_of_points_points_sorted_on_x nil)

(def ^:dynamic closest_pair_of_points_points_sorted_on_y nil)

(def ^:dynamic closest_pair_of_points_sqr_best nil)

(def ^:dynamic closest_pair_of_points_sqr_i nil)

(def ^:dynamic closest_pair_of_points_sqr_left nil)

(def ^:dynamic closest_pair_of_points_sqr_mid nil)

(def ^:dynamic closest_pair_of_points_sqr_right nil)

(def ^:dynamic closest_pair_of_points_sqr_strip nil)

(def ^:dynamic closest_pair_of_points_sqr_strip_best nil)

(def ^:dynamic column_based_sort_i nil)

(def ^:dynamic column_based_sort_j nil)

(def ^:dynamic column_based_sort_points nil)

(def ^:dynamic column_based_sort_tmp nil)

(def ^:dynamic dis_between_closest_in_strip_current nil)

(def ^:dynamic dis_between_closest_in_strip_i nil)

(def ^:dynamic dis_between_closest_in_strip_i_start nil)

(def ^:dynamic dis_between_closest_in_strip_j nil)

(def ^:dynamic dis_between_closest_in_strip_j_start nil)

(def ^:dynamic dis_between_closest_in_strip_min_dis nil)

(def ^:dynamic dis_between_closest_pair_current nil)

(def ^:dynamic dis_between_closest_pair_i nil)

(def ^:dynamic dis_between_closest_pair_j nil)

(def ^:dynamic dis_between_closest_pair_min_dis nil)

(def ^:dynamic euclidean_distance_sqr_dx nil)

(def ^:dynamic euclidean_distance_sqr_dy nil)

(def ^:dynamic sqrtApprox_guess nil)

(def ^:dynamic sqrtApprox_i nil)

(defn abs [abs_x]
  (try (if (< abs_x 0.0) (- 0.0 abs_x) abs_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sqrtApprox [sqrtApprox_x]
  (binding [sqrtApprox_guess nil sqrtApprox_i nil] (try (do (set! sqrtApprox_guess sqrtApprox_x) (set! sqrtApprox_i 0) (while (< sqrtApprox_i 20) (do (set! sqrtApprox_guess (/ (+ sqrtApprox_guess (quot sqrtApprox_x sqrtApprox_guess)) 2.0)) (set! sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_guess}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn euclidean_distance_sqr [euclidean_distance_sqr_p1 euclidean_distance_sqr_p2]
  (binding [euclidean_distance_sqr_dx nil euclidean_distance_sqr_dy nil] (try (do (set! euclidean_distance_sqr_dx (- (nth euclidean_distance_sqr_p1 0) (nth euclidean_distance_sqr_p2 0))) (set! euclidean_distance_sqr_dy (- (nth euclidean_distance_sqr_p1 1) (nth euclidean_distance_sqr_p2 1))) (throw (ex-info "return" {:v (+ (* euclidean_distance_sqr_dx euclidean_distance_sqr_dx) (* euclidean_distance_sqr_dy euclidean_distance_sqr_dy))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn column_based_sort [column_based_sort_arr column_based_sort_column]
  (binding [column_based_sort_i nil column_based_sort_j nil column_based_sort_points nil column_based_sort_tmp nil] (try (do (set! column_based_sort_points column_based_sort_arr) (set! column_based_sort_i 0) (while (< column_based_sort_i (count column_based_sort_points)) (do (set! column_based_sort_j 0) (while (< column_based_sort_j (- (count column_based_sort_points) 1)) (do (when (> (nth (nth column_based_sort_points column_based_sort_j) column_based_sort_column) (nth (nth column_based_sort_points (+ column_based_sort_j 1)) column_based_sort_column)) (do (set! column_based_sort_tmp (nth column_based_sort_points column_based_sort_j)) (set! column_based_sort_points (assoc column_based_sort_points column_based_sort_j (nth column_based_sort_points (+ column_based_sort_j 1)))) (set! column_based_sort_points (assoc column_based_sort_points (+ column_based_sort_j 1) column_based_sort_tmp)))) (set! column_based_sort_j (+ column_based_sort_j 1)))) (set! column_based_sort_i (+ column_based_sort_i 1)))) (throw (ex-info "return" {:v column_based_sort_points}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dis_between_closest_pair [dis_between_closest_pair_points count_v dis_between_closest_pair_min_dis_p]
  (binding [dis_between_closest_pair_current nil dis_between_closest_pair_i nil dis_between_closest_pair_j nil dis_between_closest_pair_min_dis nil] (try (do (set! dis_between_closest_pair_min_dis dis_between_closest_pair_min_dis_p) (set! dis_between_closest_pair_i 0) (while (< dis_between_closest_pair_i (- count_v 1)) (do (set! dis_between_closest_pair_j (+ dis_between_closest_pair_i 1)) (while (< dis_between_closest_pair_j count_v) (do (set! dis_between_closest_pair_current (euclidean_distance_sqr (nth dis_between_closest_pair_points dis_between_closest_pair_i) (nth dis_between_closest_pair_points dis_between_closest_pair_j))) (when (< dis_between_closest_pair_current dis_between_closest_pair_min_dis) (set! dis_between_closest_pair_min_dis dis_between_closest_pair_current)) (set! dis_between_closest_pair_j (+ dis_between_closest_pair_j 1)))) (set! dis_between_closest_pair_i (+ dis_between_closest_pair_i 1)))) (throw (ex-info "return" {:v dis_between_closest_pair_min_dis}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dis_between_closest_in_strip [dis_between_closest_in_strip_points count_v dis_between_closest_in_strip_min_dis_p]
  (binding [dis_between_closest_in_strip_current nil dis_between_closest_in_strip_i nil dis_between_closest_in_strip_i_start nil dis_between_closest_in_strip_j nil dis_between_closest_in_strip_j_start nil dis_between_closest_in_strip_min_dis nil] (try (do (set! dis_between_closest_in_strip_min_dis dis_between_closest_in_strip_min_dis_p) (set! dis_between_closest_in_strip_i_start 0) (if (< 6 (- count_v 1)) (set! dis_between_closest_in_strip_i_start 6) (set! dis_between_closest_in_strip_i_start (- count_v 1))) (set! dis_between_closest_in_strip_i dis_between_closest_in_strip_i_start) (while (< dis_between_closest_in_strip_i count_v) (do (set! dis_between_closest_in_strip_j_start 0) (when (> (- dis_between_closest_in_strip_i 6) 0) (set! dis_between_closest_in_strip_j_start (- dis_between_closest_in_strip_i 6))) (set! dis_between_closest_in_strip_j dis_between_closest_in_strip_j_start) (while (< dis_between_closest_in_strip_j dis_between_closest_in_strip_i) (do (set! dis_between_closest_in_strip_current (euclidean_distance_sqr (nth dis_between_closest_in_strip_points dis_between_closest_in_strip_i) (nth dis_between_closest_in_strip_points dis_between_closest_in_strip_j))) (when (< dis_between_closest_in_strip_current dis_between_closest_in_strip_min_dis) (set! dis_between_closest_in_strip_min_dis dis_between_closest_in_strip_current)) (set! dis_between_closest_in_strip_j (+ dis_between_closest_in_strip_j 1)))) (set! dis_between_closest_in_strip_i (+ dis_between_closest_in_strip_i 1)))) (throw (ex-info "return" {:v dis_between_closest_in_strip_min_dis}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn closest_pair_of_points_sqr [closest_pair_of_points_sqr_px closest_pair_of_points_sqr_py count_v]
  (binding [closest_pair_of_points_sqr_best nil closest_pair_of_points_sqr_i nil closest_pair_of_points_sqr_left nil closest_pair_of_points_sqr_mid nil closest_pair_of_points_sqr_right nil closest_pair_of_points_sqr_strip nil closest_pair_of_points_sqr_strip_best nil] (try (do (when (<= count_v 3) (throw (ex-info "return" {:v (dis_between_closest_pair closest_pair_of_points_sqr_px count_v 1000000000000000000.0)}))) (set! closest_pair_of_points_sqr_mid (quot count_v 2)) (set! closest_pair_of_points_sqr_left (closest_pair_of_points_sqr closest_pair_of_points_sqr_px (subvec closest_pair_of_points_sqr_py 0 closest_pair_of_points_sqr_mid) closest_pair_of_points_sqr_mid)) (set! closest_pair_of_points_sqr_right (closest_pair_of_points_sqr closest_pair_of_points_sqr_py (subvec closest_pair_of_points_sqr_py closest_pair_of_points_sqr_mid count_v) (- count_v closest_pair_of_points_sqr_mid))) (set! closest_pair_of_points_sqr_best closest_pair_of_points_sqr_left) (when (< closest_pair_of_points_sqr_right closest_pair_of_points_sqr_best) (set! closest_pair_of_points_sqr_best closest_pair_of_points_sqr_right)) (set! closest_pair_of_points_sqr_strip []) (set! closest_pair_of_points_sqr_i 0) (while (< closest_pair_of_points_sqr_i (count closest_pair_of_points_sqr_px)) (do (when (< (abs (- (nth (nth closest_pair_of_points_sqr_px closest_pair_of_points_sqr_i) 0) (nth (nth closest_pair_of_points_sqr_px closest_pair_of_points_sqr_mid) 0))) closest_pair_of_points_sqr_best) (set! closest_pair_of_points_sqr_strip (conj closest_pair_of_points_sqr_strip (nth closest_pair_of_points_sqr_px closest_pair_of_points_sqr_i)))) (set! closest_pair_of_points_sqr_i (+ closest_pair_of_points_sqr_i 1)))) (set! closest_pair_of_points_sqr_strip_best (dis_between_closest_in_strip closest_pair_of_points_sqr_strip (count closest_pair_of_points_sqr_strip) closest_pair_of_points_sqr_best)) (when (< closest_pair_of_points_sqr_strip_best closest_pair_of_points_sqr_best) (set! closest_pair_of_points_sqr_best closest_pair_of_points_sqr_strip_best)) (throw (ex-info "return" {:v closest_pair_of_points_sqr_best}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn closest_pair_of_points [closest_pair_of_points_points count_v]
  (binding [closest_pair_of_points_dist_sqr nil closest_pair_of_points_points_sorted_on_x nil closest_pair_of_points_points_sorted_on_y nil] (try (do (set! closest_pair_of_points_points_sorted_on_x (column_based_sort closest_pair_of_points_points 0)) (set! closest_pair_of_points_points_sorted_on_y (column_based_sort closest_pair_of_points_points 1)) (set! closest_pair_of_points_dist_sqr (closest_pair_of_points_sqr closest_pair_of_points_points_sorted_on_x closest_pair_of_points_points_sorted_on_y count_v)) (throw (ex-info "return" {:v (sqrtApprox closest_pair_of_points_dist_sqr)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_points [[2.0 3.0] [12.0 30.0] [40.0 50.0] [5.0 1.0] [12.0 10.0] [3.0 4.0]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "Distance: " (str (closest_pair_of_points main_points (count main_points)))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
