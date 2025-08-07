(ns main (:refer-clojure :exclude [cross sortPoints convex_hull]))

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

(declare cross sortPoints convex_hull)

(def ^:dynamic convex_hull_hull nil)

(def ^:dynamic convex_hull_i nil)

(def ^:dynamic convex_hull_j nil)

(def ^:dynamic convex_hull_lower nil)

(def ^:dynamic convex_hull_p nil)

(def ^:dynamic convex_hull_ps nil)

(def ^:dynamic convex_hull_upper nil)

(def ^:dynamic sortPoints_arr nil)

(def ^:dynamic sortPoints_i nil)

(def ^:dynamic sortPoints_j nil)

(def ^:dynamic sortPoints_n nil)

(def ^:dynamic sortPoints_p nil)

(def ^:dynamic sortPoints_q nil)

(defn cross [cross_o cross_a cross_b]
  (try (throw (ex-info "return" {:v (- (* (- (:x cross_a) (:x cross_o)) (- (:y cross_b) (:y cross_o))) (* (- (:y cross_a) (:y cross_o)) (- (:x cross_b) (:x cross_o))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sortPoints [sortPoints_ps]
  (binding [sortPoints_arr nil sortPoints_i nil sortPoints_j nil sortPoints_n nil sortPoints_p nil sortPoints_q nil] (try (do (set! sortPoints_arr sortPoints_ps) (set! sortPoints_n (count sortPoints_arr)) (set! sortPoints_i 0) (while (< sortPoints_i sortPoints_n) (do (set! sortPoints_j 0) (while (< sortPoints_j (- sortPoints_n 1)) (do (set! sortPoints_p (nth sortPoints_arr sortPoints_j)) (set! sortPoints_q (nth sortPoints_arr (+ sortPoints_j 1))) (when (or (> (:x sortPoints_p) (:x sortPoints_q)) (and (= (:x sortPoints_p) (:x sortPoints_q)) (> (:y sortPoints_p) (:y sortPoints_q)))) (do (set! sortPoints_arr (assoc sortPoints_arr sortPoints_j sortPoints_q)) (set! sortPoints_arr (assoc sortPoints_arr (+ sortPoints_j 1) sortPoints_p)))) (set! sortPoints_j (+ sortPoints_j 1)))) (set! sortPoints_i (+ sortPoints_i 1)))) (throw (ex-info "return" {:v sortPoints_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn convex_hull [convex_hull_ps_p]
  (binding [convex_hull_hull nil convex_hull_i nil convex_hull_j nil convex_hull_lower nil convex_hull_p nil convex_hull_ps nil convex_hull_upper nil] (try (do (set! convex_hull_ps convex_hull_ps_p) (set! convex_hull_ps (sortPoints convex_hull_ps)) (set! convex_hull_lower []) (doseq [p convex_hull_ps] (do (while (and (>= (count convex_hull_lower) 2) (<= (cross (nth convex_hull_lower (- (count convex_hull_lower) 2)) (nth convex_hull_lower (- (count convex_hull_lower) 1)) p) 0)) (set! convex_hull_lower (subvec convex_hull_lower 0 (- (count convex_hull_lower) 1)))) (set! convex_hull_lower (conj convex_hull_lower p)))) (set! convex_hull_upper []) (set! convex_hull_i (- (count convex_hull_ps) 1)) (while (>= convex_hull_i 0) (do (set! convex_hull_p (nth convex_hull_ps convex_hull_i)) (while (and (>= (count convex_hull_upper) 2) (<= (cross (nth convex_hull_upper (- (count convex_hull_upper) 2)) (nth convex_hull_upper (- (count convex_hull_upper) 1)) convex_hull_p) 0)) (set! convex_hull_upper (subvec convex_hull_upper 0 (- (count convex_hull_upper) 1)))) (set! convex_hull_upper (conj convex_hull_upper convex_hull_p)) (set! convex_hull_i (- convex_hull_i 1)))) (set! convex_hull_hull (subvec convex_hull_lower 0 (- (count convex_hull_lower) 1))) (set! convex_hull_j 0) (while (< convex_hull_j (- (count convex_hull_upper) 1)) (do (set! convex_hull_hull (conj convex_hull_hull (nth convex_hull_upper convex_hull_j))) (set! convex_hull_j (+ convex_hull_j 1)))) (throw (ex-info "return" {:v convex_hull_hull}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
