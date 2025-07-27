(ns main (:refer-clojure :exclude [initGrid set circle trimRight]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare initGrid set circle trimRight)

(defn initGrid [size]
  (try (do (def g []) (def y 0) (while (< y size) (do (def row []) (def x 0) (while (< x size) (do (def row (conj row " ")) (def x (+ x 1)))) (def g (conj g row)) (def y (+ y 1)))) (throw (ex-info "return" {:v g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn set [g x y]
  (when (and (and (and (>= x 0) (< x (count (nth g 0)))) (>= y 0)) (< y (count g))) (def g (assoc-in g [y x] "#"))))

(defn circle [r]
  (try (do (def size (+ (* r 2) 1)) (def g (initGrid size)) (def x r) (def y 0) (def err (- 1 r)) (while (<= y x) (do (set g (+ r x) (+ r y)) (set g (+ r y) (+ r x)) (set g (- r x) (+ r y)) (set g (- r y) (+ r x)) (set g (- r x) (- r y)) (set g (- r y) (- r x)) (set g (+ r x) (- r y)) (set g (+ r y) (- r x)) (def y (+ y 1)) (if (< err 0) (def err (+ (+ err (* 2 y)) 1)) (do (def x (- x 1)) (def err (+ (+ err (* 2 (- y x))) 1)))))) (throw (ex-info "return" {:v g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn trimRight [row]
  (try (do (def end (count row)) (while (and (> end 0) (= (nth row (- end 1)) " ")) (def end (- end 1))) (def s "") (def i 0) (while (< i end) (do (def s (str s (nth row i))) (def i (+ i 1)))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (def g (circle 10))
  (doseq [row g] (println (trimRight row))))

(-main)
