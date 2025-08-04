(ns main (:refer-clojure :exclude [ccw sortPoints convexHull pointStr hullStr]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare ccw sortPoints convexHull pointStr hullStr)

(def ^:dynamic ccw_lhs nil)

(def ^:dynamic ccw_rhs nil)

(def ^:dynamic convexHull_h nil)

(def ^:dynamic convexHull_i nil)

(def ^:dynamic convexHull_ps nil)

(def ^:dynamic convexHull_pt nil)

(def ^:dynamic convexHull_t nil)

(def ^:dynamic hullStr_i nil)

(def ^:dynamic hullStr_s nil)

(def ^:dynamic sortPoints_arr nil)

(def ^:dynamic sortPoints_i nil)

(def ^:dynamic sortPoints_j nil)

(def ^:dynamic sortPoints_n nil)

(def ^:dynamic sortPoints_p nil)

(def ^:dynamic sortPoints_q nil)

(defn ccw [ccw_a ccw_b ccw_c]
  (binding [ccw_lhs nil ccw_rhs nil] (try (do (set! ccw_lhs (* (- (:x ccw_b) (:x ccw_a)) (- (:y ccw_c) (:y ccw_a)))) (set! ccw_rhs (* (- (:y ccw_b) (:y ccw_a)) (- (:x ccw_c) (:x ccw_a)))) (throw (ex-info "return" {:v (> ccw_lhs ccw_rhs)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sortPoints [sortPoints_ps]
  (binding [sortPoints_arr nil sortPoints_i nil sortPoints_j nil sortPoints_n nil sortPoints_p nil sortPoints_q nil] (try (do (set! sortPoints_arr sortPoints_ps) (set! sortPoints_n (count sortPoints_arr)) (set! sortPoints_i 0) (while (< sortPoints_i sortPoints_n) (do (set! sortPoints_j 0) (while (< sortPoints_j (- sortPoints_n 1)) (do (set! sortPoints_p (nth sortPoints_arr sortPoints_j)) (set! sortPoints_q (nth sortPoints_arr (+ sortPoints_j 1))) (when (or (> (:x sortPoints_p) (:x sortPoints_q)) (and (= (:x sortPoints_p) (:x sortPoints_q)) (> (:y sortPoints_p) (:y sortPoints_q)))) (do (set! sortPoints_arr (assoc sortPoints_arr sortPoints_j sortPoints_q)) (set! sortPoints_arr (assoc sortPoints_arr (+ sortPoints_j 1) sortPoints_p)))) (set! sortPoints_j (+ sortPoints_j 1)))) (set! sortPoints_i (+ sortPoints_i 1)))) (throw (ex-info "return" {:v sortPoints_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn convexHull [convexHull_ps_p]
  (binding [convexHull_h nil convexHull_i nil convexHull_ps nil convexHull_pt nil convexHull_t nil] (try (do (set! convexHull_ps convexHull_ps_p) (set! convexHull_ps (sortPoints convexHull_ps)) (set! convexHull_h []) (doseq [pt convexHull_ps] (do (while (and (>= (count convexHull_h) 2) (= (ccw (nth convexHull_h (- (count convexHull_h) 2)) (nth convexHull_h (- (count convexHull_h) 1)) pt) false)) (set! convexHull_h (subvec convexHull_h 0 (- (count convexHull_h) 1)))) (set! convexHull_h (conj convexHull_h pt)))) (set! convexHull_i (- (count convexHull_ps) 2)) (set! convexHull_t (+ (count convexHull_h) 1)) (while (>= convexHull_i 0) (do (set! convexHull_pt (nth convexHull_ps convexHull_i)) (while (and (>= (count convexHull_h) convexHull_t) (= (ccw (nth convexHull_h (- (count convexHull_h) 2)) (nth convexHull_h (- (count convexHull_h) 1)) convexHull_pt) false)) (set! convexHull_h (subvec convexHull_h 0 (- (count convexHull_h) 1)))) (set! convexHull_h (conj convexHull_h convexHull_pt)) (set! convexHull_i (- convexHull_i 1)))) (throw (ex-info "return" {:v (subvec convexHull_h 0 (- (count convexHull_h) 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pointStr [pointStr_p]
  (try (throw (ex-info "return" {:v (str (str (str (str "(" (str (:x pointStr_p))) ",") (str (:y pointStr_p))) ")")})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hullStr [hullStr_h]
  (binding [hullStr_i nil hullStr_s nil] (try (do (set! hullStr_s "[") (set! hullStr_i 0) (while (< hullStr_i (count hullStr_h)) (do (set! hullStr_s (str hullStr_s (pointStr (nth hullStr_h hullStr_i)))) (when (< hullStr_i (- (count hullStr_h) 1)) (set! hullStr_s (str hullStr_s " "))) (set! hullStr_i (+ hullStr_i 1)))) (set! hullStr_s (str hullStr_s "]")) (throw (ex-info "return" {:v hullStr_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_pts [{:x 16 :y 3} {:x 12 :y 17} {:x 0 :y 6} {:x (- 4) :y (- 6)} {:x 16 :y 6} {:x 16 :y (- 7)} {:x 16 :y (- 3)} {:x 17 :y (- 4)} {:x 5 :y 19} {:x 19 :y (- 8)} {:x 3 :y 16} {:x 12 :y 13} {:x 3 :y (- 4)} {:x 17 :y 5} {:x (- 3) :y 15} {:x (- 3) :y (- 9)} {:x 0 :y 11} {:x (- 9) :y (- 3)} {:x (- 4) :y (- 2)} {:x 12 :y 10}])

(def ^:dynamic main_hull (convexHull main_pts))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "Convex Hull: " (hullStr main_hull)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
