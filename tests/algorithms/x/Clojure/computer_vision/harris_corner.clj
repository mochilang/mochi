(ns main (:refer-clojure :exclude [zeros gradient harris]))

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

(declare zeros gradient harris)

(def ^:dynamic gradient_dx nil)

(def ^:dynamic gradient_dy nil)

(def ^:dynamic gradient_h nil)

(def ^:dynamic gradient_w nil)

(def ^:dynamic gradient_x nil)

(def ^:dynamic gradient_y nil)

(def ^:dynamic harris_corners nil)

(def ^:dynamic harris_det nil)

(def ^:dynamic harris_dx nil)

(def ^:dynamic harris_dy nil)

(def ^:dynamic harris_grads nil)

(def ^:dynamic harris_gx nil)

(def ^:dynamic harris_gy nil)

(def ^:dynamic harris_h nil)

(def ^:dynamic harris_ixx nil)

(def ^:dynamic harris_ixy nil)

(def ^:dynamic harris_iyy nil)

(def ^:dynamic harris_offset nil)

(def ^:dynamic harris_r nil)

(def ^:dynamic harris_trace nil)

(def ^:dynamic harris_w nil)

(def ^:dynamic harris_wxx nil)

(def ^:dynamic harris_wxy nil)

(def ^:dynamic harris_wyy nil)

(def ^:dynamic harris_x nil)

(def ^:dynamic harris_xx nil)

(def ^:dynamic harris_y nil)

(def ^:dynamic harris_yy nil)

(def ^:dynamic zeros_m nil)

(def ^:dynamic zeros_row nil)

(def ^:dynamic zeros_x nil)

(def ^:dynamic zeros_y nil)

(defn zeros [zeros_h zeros_w]
  (binding [zeros_m nil zeros_row nil zeros_x nil zeros_y nil] (try (do (set! zeros_m []) (set! zeros_y 0) (while (< zeros_y zeros_h) (do (set! zeros_row []) (set! zeros_x 0) (while (< zeros_x zeros_w) (do (set! zeros_row (conj zeros_row 0.0)) (set! zeros_x (+ zeros_x 1)))) (set! zeros_m (conj zeros_m zeros_row)) (set! zeros_y (+ zeros_y 1)))) (throw (ex-info "return" {:v zeros_m}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn gradient [gradient_img]
  (binding [gradient_dx nil gradient_dy nil gradient_h nil gradient_w nil gradient_x nil gradient_y nil] (try (do (set! gradient_h (count gradient_img)) (set! gradient_w (count (nth gradient_img 0))) (set! gradient_dx (zeros gradient_h gradient_w)) (set! gradient_dy (zeros gradient_h gradient_w)) (set! gradient_y 1) (while (< gradient_y (- gradient_h 1)) (do (set! gradient_x 1) (while (< gradient_x (- gradient_w 1)) (do (set! gradient_dx (assoc-in gradient_dx [gradient_y gradient_x] (- (double (nth (nth gradient_img gradient_y) (+ gradient_x 1))) (double (nth (nth gradient_img gradient_y) (- gradient_x 1)))))) (set! gradient_dy (assoc-in gradient_dy [gradient_y gradient_x] (- (double (nth (nth gradient_img (+ gradient_y 1)) gradient_x)) (double (nth (nth gradient_img (- gradient_y 1)) gradient_x))))) (set! gradient_x (+ gradient_x 1)))) (set! gradient_y (+ gradient_y 1)))) (throw (ex-info "return" {:v [gradient_dx gradient_dy]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn harris [harris_img harris_k harris_window harris_thresh]
  (binding [harris_corners nil harris_det nil harris_dx nil harris_dy nil harris_grads nil harris_gx nil harris_gy nil harris_h nil harris_ixx nil harris_ixy nil harris_iyy nil harris_offset nil harris_r nil harris_trace nil harris_w nil harris_wxx nil harris_wxy nil harris_wyy nil harris_x nil harris_xx nil harris_y nil harris_yy nil] (try (do (set! harris_h (count harris_img)) (set! harris_w (count (nth harris_img 0))) (set! harris_grads (gradient harris_img)) (set! harris_dx (nth harris_grads 0)) (set! harris_dy (nth harris_grads 1)) (set! harris_ixx (zeros harris_h harris_w)) (set! harris_iyy (zeros harris_h harris_w)) (set! harris_ixy (zeros harris_h harris_w)) (set! harris_y 0) (while (< harris_y harris_h) (do (set! harris_x 0) (while (< harris_x harris_w) (do (set! harris_gx (nth (nth harris_dx harris_y) harris_x)) (set! harris_gy (nth (nth harris_dy harris_y) harris_x)) (set! harris_ixx (assoc-in harris_ixx [harris_y harris_x] (* harris_gx harris_gx))) (set! harris_iyy (assoc-in harris_iyy [harris_y harris_x] (* harris_gy harris_gy))) (set! harris_ixy (assoc-in harris_ixy [harris_y harris_x] (* harris_gx harris_gy))) (set! harris_x (+ harris_x 1)))) (set! harris_y (+ harris_y 1)))) (set! harris_offset (quot harris_window 2)) (set! harris_corners []) (set! harris_y harris_offset) (while (< harris_y (- harris_h harris_offset)) (do (set! harris_x harris_offset) (while (< harris_x (- harris_w harris_offset)) (do (set! harris_wxx 0.0) (set! harris_wyy 0.0) (set! harris_wxy 0.0) (set! harris_yy (- harris_y harris_offset)) (while (<= harris_yy (+ harris_y harris_offset)) (do (set! harris_xx (- harris_x harris_offset)) (while (<= harris_xx (+ harris_x harris_offset)) (do (set! harris_wxx (+ harris_wxx (nth (nth harris_ixx harris_yy) harris_xx))) (set! harris_wyy (+ harris_wyy (nth (nth harris_iyy harris_yy) harris_xx))) (set! harris_wxy (+ harris_wxy (nth (nth harris_ixy harris_yy) harris_xx))) (set! harris_xx (+ harris_xx 1)))) (set! harris_yy (+ harris_yy 1)))) (set! harris_det (- (* harris_wxx harris_wyy) (* harris_wxy harris_wxy))) (set! harris_trace (+ harris_wxx harris_wyy)) (set! harris_r (- harris_det (* harris_k (* harris_trace harris_trace)))) (when (> harris_r harris_thresh) (set! harris_corners (conj harris_corners [harris_x harris_y]))) (set! harris_x (+ harris_x 1)))) (set! harris_y (+ harris_y 1)))) (throw (ex-info "return" {:v harris_corners}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_img [[1 1 1 1 1] [1 255 255 255 1] [1 255 0 255 1] [1 255 255 255 1] [1 1 1 1 1]])

(def ^:dynamic main_corners (harris main_img 0.04 3 10000000000.0))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println main_corners)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
