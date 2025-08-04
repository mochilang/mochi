(ns main (:refer-clojure :exclude [det replaceCol]))

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

(declare det replaceCol)

(def ^:dynamic det_c nil)

(def ^:dynamic det_cc nil)

(def ^:dynamic det_n nil)

(def ^:dynamic det_r nil)

(def ^:dynamic det_row nil)

(def ^:dynamic det_sign nil)

(def ^:dynamic det_sub nil)

(def ^:dynamic det_total nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_j nil)

(def ^:dynamic main_s nil)

(def ^:dynamic main_x nil)

(def ^:dynamic replaceCol_c nil)

(def ^:dynamic replaceCol_r nil)

(def ^:dynamic replaceCol_res nil)

(def ^:dynamic replaceCol_row nil)

(defn det [det_m]
  (binding [det_c nil det_cc nil det_n nil det_r nil det_row nil det_sign nil det_sub nil det_total nil] (try (do (set! det_n (count det_m)) (when (= det_n 1) (throw (ex-info "return" {:v (nth (nth det_m 0) 0)}))) (set! det_total 0.0) (set! det_sign 1.0) (set! det_c 0) (while (< det_c det_n) (do (set! det_sub []) (set! det_r 1) (while (< det_r det_n) (do (set! det_row []) (set! det_cc 0) (while (< det_cc det_n) (do (when (not= det_cc det_c) (set! det_row (conj det_row (nth (nth det_m det_r) det_cc)))) (set! det_cc (+ det_cc 1)))) (set! det_sub (conj det_sub det_row)) (set! det_r (+ det_r 1)))) (set! det_total (+ det_total (* (* det_sign (nth (nth det_m 0) det_c)) (det det_sub)))) (set! det_sign (* det_sign (- 1.0))) (set! det_c (+ det_c 1)))) (throw (ex-info "return" {:v det_total}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn replaceCol [replaceCol_m replaceCol_col replaceCol_v]
  (binding [replaceCol_c nil replaceCol_r nil replaceCol_res nil replaceCol_row nil] (try (do (set! replaceCol_res []) (set! replaceCol_r 0) (while (< replaceCol_r (count replaceCol_m)) (do (set! replaceCol_row []) (set! replaceCol_c 0) (while (< replaceCol_c (count (nth replaceCol_m replaceCol_r))) (do (if (= replaceCol_c replaceCol_col) (set! replaceCol_row (conj replaceCol_row (nth replaceCol_v replaceCol_r))) (set! replaceCol_row (conj replaceCol_row (nth (nth replaceCol_m replaceCol_r) replaceCol_c)))) (set! replaceCol_c (+ replaceCol_c 1)))) (set! replaceCol_res (conj replaceCol_res replaceCol_row)) (set! replaceCol_r (+ replaceCol_r 1)))) (throw (ex-info "return" {:v replaceCol_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_m [[2.0 (- 1.0) 5.0 1.0] [3.0 2.0 2.0 (- 6.0)] [1.0 3.0 3.0 (- 1.0)] [5.0 (- 2.0) (- 3.0) 3.0]])

(def ^:dynamic main_v [(- 3.0) (- 32.0) (- 47.0) 49.0])

(def ^:dynamic main_d (det main_m))

(def ^:dynamic main_x [])

(def ^:dynamic main_i 0)

(def ^:dynamic main_s "[")

(def ^:dynamic main_j 0)

(defn -main []
  (while (< main_i (count main_v)) (do (def ^:dynamic main_mc (replaceCol main_m main_i main_v)) (def main_x (conj main_x (/ (det main_mc) main_d))) (def main_i (+ main_i 1))))
  (while (< main_j (count main_x)) (do (def main_s (str main_s (str (nth main_x main_j)))) (when (< main_j (- (count main_x) 1)) (def main_s (str main_s " "))) (def main_j (+ main_j 1))))
  (def main_s (str main_s "]"))
  (println main_s))

(-main)
