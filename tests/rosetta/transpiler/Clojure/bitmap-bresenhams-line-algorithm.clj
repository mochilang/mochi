(ns main (:refer-clojure :exclude [absi bresenham main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare absi bresenham main)

(defn absi [x]
  (try (if (< x 0) (- x) x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bresenham [x0 y0 x1 y1]
  (try (do (def dx (absi (- x1 x0))) (def dy (absi (- y1 y0))) (def sx (- 1)) (when (< x0 x1) (def sx 1)) (def sy (- 1)) (when (< y0 y1) (def sy 1)) (def err (- dx dy)) (def pts []) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def pts (conj pts {:x x0 :y y0})) (cond (and (= x0 x1) (= y0 y1)) (recur false) :else (do (def e2 (* 2 err)) (when (> e2 (- dy)) (do (def err (- err dy)) (def x0 (+ x0 sx)))) (when (< e2 dx) (do (def err (+ err dx)) (def y0 (+ y0 sy)))) (recur while_flag_1)))))) (throw (ex-info "return" {:v pts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def pts (bresenham 0 0 6 4)) (def i 0) (while (< i (count pts)) (do (def p (nth pts i)) (println (str (str (str (str "(" (str (:x p))) ",") (str (:y p))) ")")) (def i (+ i 1))))))

(defn -main []
  (main))

(-main)
