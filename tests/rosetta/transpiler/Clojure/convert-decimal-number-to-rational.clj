(ns main (:refer-clojure :exclude [gcd parseRational main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare gcd parseRational main)

(def ^:dynamic gcd_t nil)

(def ^:dynamic gcd_x nil)

(def ^:dynamic gcd_y nil)

(def ^:dynamic main_inputs nil)

(def ^:dynamic main_r nil)

(def ^:dynamic parseRational_afterDot nil)

(def ^:dynamic parseRational_ch nil)

(def ^:dynamic parseRational_d nil)

(def ^:dynamic parseRational_denom nil)

(def ^:dynamic parseRational_fracPart nil)

(def ^:dynamic parseRational_g nil)

(def ^:dynamic parseRational_i nil)

(def ^:dynamic parseRational_intPart nil)

(def ^:dynamic parseRational_num nil)

(defn gcd [gcd_a gcd_b]
  (binding [gcd_t nil gcd_x nil gcd_y nil] (try (do (set! gcd_x gcd_a) (when (< gcd_x 0) (set! gcd_x (- gcd_x))) (set! gcd_y gcd_b) (when (< gcd_y 0) (set! gcd_y (- gcd_y))) (while (not= gcd_y 0) (do (set! gcd_t (mod gcd_x gcd_y)) (set! gcd_x gcd_y) (set! gcd_y gcd_t))) (throw (ex-info "return" {:v gcd_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn parseRational [parseRational_s]
  (binding [parseRational_afterDot nil parseRational_ch nil parseRational_d nil parseRational_denom nil parseRational_fracPart nil parseRational_g nil parseRational_i nil parseRational_intPart nil parseRational_num nil] (try (do (set! parseRational_intPart 0) (set! parseRational_fracPart 0) (set! parseRational_denom 1) (set! parseRational_afterDot false) (set! parseRational_i 0) (while (< parseRational_i (count parseRational_s)) (do (set! parseRational_ch (subs parseRational_s parseRational_i (+ parseRational_i 1))) (if (= parseRational_ch ".") (set! parseRational_afterDot true) (do (set! parseRational_d (- (Integer/parseInt parseRational_ch) (Integer/parseInt "0"))) (if (not parseRational_afterDot) (set! parseRational_intPart (+ (* parseRational_intPart 10) parseRational_d)) (do (set! parseRational_fracPart (+ (* parseRational_fracPart 10) parseRational_d)) (set! parseRational_denom (* parseRational_denom 10)))))) (set! parseRational_i (+ parseRational_i 1)))) (set! parseRational_num (+ (* parseRational_intPart parseRational_denom) parseRational_fracPart)) (set! parseRational_g (gcd parseRational_num parseRational_denom)) (throw (ex-info "return" {:v {"num" (long (/ parseRational_num parseRational_g)) "den" (long (/ parseRational_denom parseRational_g))}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_inputs nil main_r nil] (do (set! main_inputs ["0.9054054" "0.518518" "0.75"]) (doseq [s main_inputs] (do (set! main_r (parseRational s)) (println (str (str (str (str s " = ") (str (get main_r "num"))) "/") (str (get main_r "den")))))))))

(defn -main []
  (main))

(-main)
