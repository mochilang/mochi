(ns main (:refer-clojure :exclude [sqrtApprox hypot circles]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare sqrtApprox hypot circles)

(declare circles_d circles_dx circles_dy circles_m circles_ox circles_oy circles_q main_Co main_CoR0 main_Diam main_Far main_R0 main_Two main_c1 main_c2 main_caseStr main_p1 main_p2 main_r main_res main_td sqrtApprox_g sqrtApprox_i)

(defn sqrtApprox [sqrtApprox_x]
  (try (do (def sqrtApprox_g sqrtApprox_x) (def sqrtApprox_i 0) (while (< sqrtApprox_i 40) (do (def sqrtApprox_g (/ (+ sqrtApprox_g (/ sqrtApprox_x sqrtApprox_g)) 2.0)) (def sqrtApprox_i (+ sqrtApprox_i 1)))) (throw (ex-info "return" {:v sqrtApprox_g}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn hypot [hypot_x hypot_y]
  (try (throw (ex-info "return" {:v (sqrtApprox (+ (* hypot_x hypot_x) (* hypot_y hypot_y)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_Two "Two circles.")

(def main_R0 "R==0.0 does not describe circles.")

(def main_Co "Coincident points describe an infinite number of circles.")

(def main_CoR0 "Coincident points with r==0.0 describe a degenerate circle.")

(def main_Diam "Points form a diameter and describe only a single circle.")

(def main_Far "Points too far apart to form circles.")

(defn circles [circles_p1 circles_p2 circles_r]
  (try (do (when (and (= (:x circles_p1) (:x circles_p2)) (= (:y circles_p1) (:y circles_p2))) (do (when (= circles_r 0.0) (throw (ex-info "return" {:v [circles_p1 circles_p1 "Coincident points with r==0.0 describe a degenerate circle."]}))) (throw (ex-info "return" {:v [circles_p1 circles_p2 "Coincident points describe an infinite number of circles."]})))) (when (= circles_r 0.0) (throw (ex-info "return" {:v [circles_p1 circles_p2 "R==0.0 does not describe circles."]}))) (def circles_dx (- (:x circles_p2) (:x circles_p1))) (def circles_dy (- (:y circles_p2) (:y circles_p1))) (def circles_q (hypot circles_dx circles_dy)) (when (> circles_q (* 2.0 circles_r)) (throw (ex-info "return" {:v [circles_p1 circles_p2 "Points too far apart to form circles."]}))) (def circles_m {:x (/ (+ (:x circles_p1) (:x circles_p2)) 2.0) :y (/ (+ (:y circles_p1) (:y circles_p2)) 2.0)}) (when (= circles_q (* 2.0 circles_r)) (throw (ex-info "return" {:v [circles_m circles_m "Points form a diameter and describe only a single circle."]}))) (def circles_d (sqrtApprox (- (* circles_r circles_r) (/ (* circles_q circles_q) 4.0)))) (def circles_ox (/ (* circles_d circles_dx) circles_q)) (def circles_oy (/ (* circles_d circles_dy) circles_q)) (throw (ex-info "return" {:v [{:x (- (:x circles_m) circles_oy) :y (+ (:y circles_m) circles_ox)} {:x (+ (:x circles_m) circles_oy) :y (- (:y circles_m) circles_ox)} "Two circles."]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_td [[{:x 0.1234 :y 0.9876} {:x 0.8765 :y 0.2345} 2.0] [{:x 0.0 :y 2.0} {:x 0.0 :y 0.0} 1.0] [{:x 0.1234 :y 0.9876} {:x 0.1234 :y 0.9876} 2.0] [{:x 0.1234 :y 0.9876} {:x 0.8765 :y 0.2345} 0.5] [{:x 0.1234 :y 0.9876} {:x 0.1234 :y 0.9876} 0.0]])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (doseq [tc main_td] (do (def main_p1 (nth tc 0)) (def main_p2 (nth tc 1)) (def main_r (nth tc 2)) (println (str (str (str (str "p1:  {" (str (:x main_p1))) " ") (str (:y main_p1))) "}")) (println (str (str (str (str "p2:  {" (str (:x main_p2))) " ") (str (:y main_p2))) "}")) (println (str "r:  " (str main_r))) (def main_res (circles main_p1 main_p2 main_r)) (def main_c1 (nth main_res 0)) (def main_c2 (nth main_res 1)) (def main_caseStr (nth main_res 2)) (println (str "   " main_caseStr)) (if (or (= main_caseStr "Points form a diameter and describe only a single circle.") (= main_caseStr "Coincident points with r==0.0 describe a degenerate circle.")) (println (str (str (str (str "   Center:  {" (str (:x main_c1))) " ") (str (:y main_c1))) "}")) (when (= main_caseStr "Two circles.") (do (println (str (str (str (str "   Center 1:  {" (str (:x main_c1))) " ") (str (:y main_c1))) "}")) (println (str (str (str (str "   Center 2:  {" (str (:x main_c2))) " ") (str (:y main_c2))) "}"))))) (println "")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
