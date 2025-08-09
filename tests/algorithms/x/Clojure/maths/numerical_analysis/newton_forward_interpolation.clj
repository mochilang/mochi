(ns main (:refer-clojure :exclude [ucal factorial newton_forward_interpolation]))

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

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare ucal factorial newton_forward_interpolation)

(def ^:dynamic factorial_i nil)

(def ^:dynamic factorial_result nil)

(def ^:dynamic newton_forward_interpolation_i nil)

(def ^:dynamic newton_forward_interpolation_i1 nil)

(def ^:dynamic newton_forward_interpolation_j nil)

(def ^:dynamic newton_forward_interpolation_j1 nil)

(def ^:dynamic newton_forward_interpolation_k nil)

(def ^:dynamic newton_forward_interpolation_n nil)

(def ^:dynamic newton_forward_interpolation_row nil)

(def ^:dynamic newton_forward_interpolation_sum nil)

(def ^:dynamic newton_forward_interpolation_u nil)

(def ^:dynamic newton_forward_interpolation_y nil)

(def ^:dynamic ucal_i nil)

(def ^:dynamic ucal_temp nil)

(defn ucal [ucal_u ucal_p]
  (binding [ucal_i nil ucal_temp nil] (try (do (set! ucal_temp ucal_u) (set! ucal_i 1) (while (< ucal_i ucal_p) (do (set! ucal_temp (* ucal_temp (- ucal_u (double ucal_i)))) (set! ucal_i (+ ucal_i 1)))) (throw (ex-info "return" {:v ucal_temp}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn factorial [factorial_n]
  (binding [factorial_i nil factorial_result nil] (try (do (set! factorial_result 1.0) (set! factorial_i 2) (while (<= factorial_i factorial_n) (do (set! factorial_result (* factorial_result (double factorial_i))) (set! factorial_i (+ factorial_i 1)))) (throw (ex-info "return" {:v factorial_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn newton_forward_interpolation [newton_forward_interpolation_x newton_forward_interpolation_y0 newton_forward_interpolation_value]
  (binding [newton_forward_interpolation_i nil newton_forward_interpolation_i1 nil newton_forward_interpolation_j nil newton_forward_interpolation_j1 nil newton_forward_interpolation_k nil newton_forward_interpolation_n nil newton_forward_interpolation_row nil newton_forward_interpolation_sum nil newton_forward_interpolation_u nil newton_forward_interpolation_y nil] (try (do (set! newton_forward_interpolation_n (count newton_forward_interpolation_x)) (set! newton_forward_interpolation_y []) (set! newton_forward_interpolation_i 0) (while (< newton_forward_interpolation_i newton_forward_interpolation_n) (do (set! newton_forward_interpolation_row []) (set! newton_forward_interpolation_j 0) (while (< newton_forward_interpolation_j newton_forward_interpolation_n) (do (set! newton_forward_interpolation_row (conj newton_forward_interpolation_row 0.0)) (set! newton_forward_interpolation_j (+ newton_forward_interpolation_j 1)))) (set! newton_forward_interpolation_y (conj newton_forward_interpolation_y newton_forward_interpolation_row)) (set! newton_forward_interpolation_i (+ newton_forward_interpolation_i 1)))) (set! newton_forward_interpolation_i 0) (while (< newton_forward_interpolation_i newton_forward_interpolation_n) (do (set! newton_forward_interpolation_y (assoc-in newton_forward_interpolation_y [newton_forward_interpolation_i 0] (nth newton_forward_interpolation_y0 newton_forward_interpolation_i))) (set! newton_forward_interpolation_i (+ newton_forward_interpolation_i 1)))) (set! newton_forward_interpolation_i1 1) (while (< newton_forward_interpolation_i1 newton_forward_interpolation_n) (do (set! newton_forward_interpolation_j1 0) (while (< newton_forward_interpolation_j1 (- newton_forward_interpolation_n newton_forward_interpolation_i1)) (do (set! newton_forward_interpolation_y (assoc-in newton_forward_interpolation_y [newton_forward_interpolation_j1 newton_forward_interpolation_i1] (- (nth (nth newton_forward_interpolation_y (+ newton_forward_interpolation_j1 1)) (- newton_forward_interpolation_i1 1)) (nth (nth newton_forward_interpolation_y newton_forward_interpolation_j1) (- newton_forward_interpolation_i1 1))))) (set! newton_forward_interpolation_j1 (+ newton_forward_interpolation_j1 1)))) (set! newton_forward_interpolation_i1 (+ newton_forward_interpolation_i1 1)))) (set! newton_forward_interpolation_u (quot (- newton_forward_interpolation_value (nth newton_forward_interpolation_x 0)) (- (nth newton_forward_interpolation_x 1) (nth newton_forward_interpolation_x 0)))) (set! newton_forward_interpolation_sum (nth (nth newton_forward_interpolation_y 0) 0)) (set! newton_forward_interpolation_k 1) (while (< newton_forward_interpolation_k newton_forward_interpolation_n) (do (set! newton_forward_interpolation_sum (+ newton_forward_interpolation_sum (/ (* (ucal newton_forward_interpolation_u newton_forward_interpolation_k) (nth (nth newton_forward_interpolation_y 0) newton_forward_interpolation_k)) (factorial newton_forward_interpolation_k)))) (set! newton_forward_interpolation_k (+ newton_forward_interpolation_k 1)))) (throw (ex-info "return" {:v newton_forward_interpolation_sum}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_x_points [0.0 1.0 2.0 3.0])

(def ^:dynamic main_y_points [0.0 1.0 8.0 27.0])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (newton_forward_interpolation main_x_points main_y_points 1.5)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
