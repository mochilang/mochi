(ns main (:refer-clojure :exclude [_mod _sin _cos clearGrid drawPoint bresenham ftree render main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare _mod _sin _cos clearGrid drawPoint bresenham ftree render main)

(declare PI angle depth dx dy e2 err frac g grid height length line out rad row sx sy width x x2 y y2 y3 y4 y5 y6 y7)

(def PI 3.141592653589793)

(defn _mod [x m]
  (try (throw (ex-info "return" {:v (- x (* (double (int (/ x m))) m))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn _sin [x]
  (try (do (def y (- (_mod (+' x PI) (* 2.0 PI)) PI)) (def y2 (* y y)) (def y3 (* y2 y)) (def y5 (* y3 y2)) (def y7 (* y5 y2)) (throw (ex-info "return" {:v (- (+' (- y (/ y3 6.0)) (/ y5 120.0)) (/ y7 5040.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn _cos [x]
  (try (do (def y (- (_mod (+' x PI) (* 2.0 PI)) PI)) (def y2 (* y y)) (def y4 (* y2 y2)) (def y6 (* y4 y2)) (throw (ex-info "return" {:v (- (+' (- 1.0 (/ y2 2.0)) (/ y4 24.0)) (/ y6 720.0))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def width 80)

(def height 40)

(def depth 6)

(def angle 12.0)

(def length 12.0)

(def frac 0.8)

(defn clearGrid []
  (try (do (def g []) (def y 0) (while (< y height) (do (def row []) (def x 0) (while (< x width) (do (def row (conj row " ")) (def x (+' x 1)))) (def g (conj g row)) (def y (+' y 1)))) (throw (ex-info "return" {:v g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn drawPoint [g_p x y]
  (do (def g g_p) (when (and (and (and (>= x 0) (< x width)) (>= y 0)) (< y height)) (do (def row (nth g y)) (def row (assoc row x "#")) (def g (assoc g y row))))))

(defn bresenham [x0_p y0_p x1 y1 g]
  (do (def x0 x0_p) (def y0 y0_p) (def dx (- x1 x0)) (when (< dx 0) (def dx (- dx))) (def dy (- y1 y0)) (when (< dy 0) (def dy (- dy))) (def sx (- 1)) (when (< x0 x1) (def sx 1)) (def sy (- 1)) (when (< y0 y1) (def sy 1)) (def err (- dx dy)) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (drawPoint g x0 y0) (cond (and (= x0 x1) (= y0 y1)) (recur false) :else (do (def e2 (* 2 err)) (when (> e2 (- dy)) (do (def err (- err dy)) (def x0 (+' x0 sx)))) (when (< e2 dx) (do (def err (+' err dx)) (def y0 (+' y0 sy)))) (recur while_flag_1))))))))

(defn ftree [g x y dist dir d]
  (do (def rad (/ (* dir PI) 180.0)) (def x2 (+' x (* dist (_sin rad)))) (def y2 (- y (* dist (_cos rad)))) (bresenham (int x) (int y) (int x2) (int y2) g) (when (> d 0) (do (ftree g x2 y2 (* dist frac) (- dir angle) (- d 1)) (ftree g x2 y2 (* dist frac) (+' dir angle) (- d 1))))))

(defn render [g]
  (try (do (def out "") (def y 0) (while (< y height) (do (def line "") (def x 0) (while (< x width) (do (def line (str line (nth (nth g y) x))) (def x (+' x 1)))) (def out (str out line)) (when (< y (- height 1)) (def out (str out "\n"))) (def y (+' y 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def grid (clearGrid)) (ftree grid (double (/ width 2)) (double (- height 1)) length 0.0 depth) (println (render grid))))

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
