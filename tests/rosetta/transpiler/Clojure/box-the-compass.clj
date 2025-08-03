(ns main (:refer-clojure :exclude [padLeft padRight indexOf format2 cpx degrees2compasspoint]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare padLeft padRight indexOf format2 cpx degrees2compasspoint)

(declare cpx_x format2_idx format2_need format2_s indexOf_i main_compassPoint main_cp main_h main_headings main_i main_idx padLeft_n padLeft_res padRight_i padRight_out)

(defn padLeft [padLeft_s padLeft_w]
  (try (do (def padLeft_res "") (def padLeft_n (- padLeft_w (count padLeft_s))) (while (> padLeft_n 0) (do (def padLeft_res (str padLeft_res " ")) (def padLeft_n (- padLeft_n 1)))) (throw (ex-info "return" {:v (str padLeft_res padLeft_s)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padRight [padRight_s padRight_w]
  (try (do (def padRight_out padRight_s) (def padRight_i (count padRight_s)) (while (< padRight_i padRight_w) (do (def padRight_out (str padRight_out " ")) (def padRight_i (+ padRight_i 1)))) (throw (ex-info "return" {:v padRight_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn indexOf [indexOf_s indexOf_ch]
  (try (do (def indexOf_i 0) (while (< indexOf_i (count indexOf_s)) (do (when (= (subs indexOf_s indexOf_i (+ indexOf_i 1)) indexOf_ch) (throw (ex-info "return" {:v indexOf_i}))) (def indexOf_i (+ indexOf_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn format2 [format2_f]
  (try (do (def format2_s (str format2_f)) (def format2_idx (indexOf format2_s ".")) (if (< format2_idx 0) (def format2_s (str format2_s ".00")) (do (def format2_need (+ format2_idx 3)) (if (> (count format2_s) format2_need) (def format2_s (subs format2_s 0 format2_need)) (while (< (count format2_s) format2_need) (def format2_s (str format2_s "0")))))) (throw (ex-info "return" {:v format2_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn cpx [cpx_h]
  (try (do (def cpx_x (int (+ (/ cpx_h 11.25) 0.5))) (def cpx_x (mod cpx_x 32)) (when (< cpx_x 0) (def cpx_x (+ cpx_x 32))) (throw (ex-info "return" {:v cpx_x}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_compassPoint ["North" "North by east" "North-northeast" "Northeast by north" "Northeast" "Northeast by east" "East-northeast" "East by north" "East" "East by south" "East-southeast" "Southeast by east" "Southeast" "Southeast by south" "South-southeast" "South by east" "South" "South by west" "South-southwest" "Southwest by south" "Southwest" "Southwest by west" "West-southwest" "West by south" "West" "West by north" "West-northwest" "Northwest by west" "Northwest" "Northwest by north" "North-northwest" "North by west"])

(defn degrees2compasspoint [degrees2compasspoint_h]
  (try (throw (ex-info "return" {:v (nth main_compassPoint (cpx degrees2compasspoint_h))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_headings [0.0 16.87 16.88 33.75 50.62 50.63 67.5 84.37 84.38 101.25 118.12 118.13 135.0 151.87 151.88 168.75 185.62 185.63 202.5 219.37 219.38 236.25 253.12 253.13 270.0 286.87 286.88 303.75 320.62 320.63 337.5 354.37 354.38])

(def main_i 0)

(defn -main []
  (println "Index  Compass point         Degree")
  (while (< main_i (count main_headings)) (do (def main_h (nth main_headings main_i)) (def main_idx (+ (mod main_i 32) 1)) (def main_cp (degrees2compasspoint main_h)) (println (str (str (str (str (str (padLeft (str main_idx) 4) "   ") (padRight main_cp 19)) " ") (format2 main_h)) "°")) (def main_i (+ main_i 1)))))

(-main)
