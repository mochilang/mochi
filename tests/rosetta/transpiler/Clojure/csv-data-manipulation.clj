(ns main (:refer-clojure :exclude [join parseIntStr]))

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

(declare join parseIntStr)

(def ^:dynamic join_i nil)

(def ^:dynamic join_res nil)

(def ^:dynamic main_i nil)

(def ^:dynamic main_rows nil)

(def ^:dynamic main_sum nil)

(def ^:dynamic parseIntStr_digits nil)

(def ^:dynamic parseIntStr_i nil)

(def ^:dynamic parseIntStr_n nil)

(def ^:dynamic parseIntStr_neg nil)

(defn join [join_xs join_sep]
  (binding [join_i nil join_res nil] (try (do (set! join_res "") (set! join_i 0) (while (< join_i (count join_xs)) (do (when (> join_i 0) (set! join_res (str join_res join_sep))) (set! join_res (str join_res (nth join_xs join_i))) (set! join_i (+ join_i 1)))) (throw (ex-info "return" {:v join_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn parseIntStr [parseIntStr_str]
  (binding [parseIntStr_digits nil parseIntStr_i nil parseIntStr_n nil parseIntStr_neg nil] (try (do (set! parseIntStr_i 0) (set! parseIntStr_neg false) (when (and (> (count parseIntStr_str) 0) (= (subs parseIntStr_str 0 1) "-")) (do (set! parseIntStr_neg true) (set! parseIntStr_i 1))) (set! parseIntStr_n 0) (set! parseIntStr_digits {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9}) (while (< parseIntStr_i (count parseIntStr_str)) (do (set! parseIntStr_n (+ (* parseIntStr_n 10) (get parseIntStr_digits (subs parseIntStr_str parseIntStr_i (+ parseIntStr_i 1))))) (set! parseIntStr_i (+ parseIntStr_i 1)))) (when parseIntStr_neg (set! parseIntStr_n (- parseIntStr_n))) (throw (ex-info "return" {:v parseIntStr_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_rows [["A" "B" "C"] ["1" "2" "3"] ["4" "5" "6"] ["7" "8" "9"]])

(def ^:dynamic main_i 1)

(defn -main []
  (def main_rows (assoc main_rows 0 (conj (nth main_rows 0) "SUM")))
  (while (< main_i (count main_rows)) (do (def ^:dynamic main_sum 0) (doseq [s (nth main_rows main_i)] (def main_sum (+ main_sum (parseIntStr s)))) (def main_rows (assoc main_rows main_i (conj (nth main_rows main_i) (str main_sum)))) (def main_i (+ main_i 1))))
  (doseq [r main_rows] (println (join r ","))))

(-main)
