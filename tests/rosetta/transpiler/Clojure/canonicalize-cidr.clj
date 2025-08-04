(ns main (:refer-clojure :exclude [split join repeat parseIntStr toBinary binToInt padRight canonicalize]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare split join repeat parseIntStr toBinary binToInt padRight canonicalize)

(declare binToInt_i binToInt_n canonicalize_binParts canonicalize_binary canonicalize_canonParts canonicalize_dotted canonicalize_i canonicalize_parts canonicalize_size join_i join_res main_tests padRight_out parseIntStr_digits parseIntStr_i parseIntStr_n parseIntStr_neg repeat_i repeat_out split_cur split_i split_parts toBinary_b toBinary_i toBinary_val)

(defn split [split_s split_sep]
  (try (do (def split_parts []) (def split_cur "") (def split_i 0) (while (< split_i (count split_s)) (if (and (and (> (count split_sep) 0) (<= (+ split_i (count split_sep)) (count split_s))) (= (subs split_s split_i (+ split_i (count split_sep))) split_sep)) (do (def split_parts (conj split_parts split_cur)) (def split_cur "") (def split_i (+ split_i (count split_sep)))) (do (def split_cur (str split_cur (subs split_s split_i (+ split_i 1)))) (def split_i (+ split_i 1))))) (def split_parts (conj split_parts split_cur)) (throw (ex-info "return" {:v split_parts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn join [join_xs join_sep]
  (try (do (def join_res "") (def join_i 0) (while (< join_i (count join_xs)) (do (when (> join_i 0) (def join_res (str join_res join_sep))) (def join_res (str join_res (nth join_xs join_i))) (def join_i (+ join_i 1)))) (throw (ex-info "return" {:v join_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn repeat [repeat_ch repeat_n]
  (try (do (def repeat_out "") (def repeat_i 0) (while (< repeat_i repeat_n) (do (def repeat_out (str repeat_out repeat_ch)) (def repeat_i (+ repeat_i 1)))) (throw (ex-info "return" {:v repeat_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseIntStr [parseIntStr_str]
  (try (do (def parseIntStr_i 0) (def parseIntStr_neg false) (when (and (> (count parseIntStr_str) 0) (= (subs parseIntStr_str 0 1) "-")) (do (def parseIntStr_neg true) (def parseIntStr_i 1))) (def parseIntStr_n 0) (def parseIntStr_digits {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9}) (while (< parseIntStr_i (count parseIntStr_str)) (do (def parseIntStr_n (+ (* parseIntStr_n 10) (get parseIntStr_digits (subs parseIntStr_str parseIntStr_i (+ parseIntStr_i 1))))) (def parseIntStr_i (+ parseIntStr_i 1)))) (when parseIntStr_neg (def parseIntStr_n (- parseIntStr_n))) (throw (ex-info "return" {:v parseIntStr_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn toBinary [toBinary_n toBinary_bits]
  (try (do (def toBinary_b "") (def toBinary_val toBinary_n) (def toBinary_i 0) (while (< toBinary_i toBinary_bits) (do (def toBinary_b (str (str (mod toBinary_val 2)) toBinary_b)) (def toBinary_val (long (quot toBinary_val 2))) (def toBinary_i (+ toBinary_i 1)))) (throw (ex-info "return" {:v toBinary_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn binToInt [binToInt_bits]
  (try (do (def binToInt_n 0) (def binToInt_i 0) (while (< binToInt_i (count binToInt_bits)) (do (def binToInt_n (+ (* binToInt_n 2) (parseIntStr (subs binToInt_bits binToInt_i (+ binToInt_i 1))))) (def binToInt_i (+ binToInt_i 1)))) (throw (ex-info "return" {:v binToInt_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padRight [padRight_s padRight_width]
  (try (do (def padRight_out padRight_s) (while (< (count padRight_out) padRight_width) (def padRight_out (str padRight_out " "))) (throw (ex-info "return" {:v padRight_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn canonicalize [canonicalize_cidr]
  (try (do (def canonicalize_parts (split canonicalize_cidr "/")) (def canonicalize_dotted (nth canonicalize_parts 0)) (def canonicalize_size (parseIntStr (nth canonicalize_parts 1))) (def canonicalize_binParts []) (doseq [p (split canonicalize_dotted ".")] (def canonicalize_binParts (conj canonicalize_binParts (toBinary (parseIntStr p) 8)))) (def canonicalize_binary (join canonicalize_binParts "")) (def canonicalize_binary (str (subs canonicalize_binary 0 canonicalize_size) (repeat "0" (- 32 canonicalize_size)))) (def canonicalize_canonParts []) (def canonicalize_i 0) (while (< canonicalize_i (count canonicalize_binary)) (do (def canonicalize_canonParts (conj canonicalize_canonParts (str (binToInt (subs canonicalize_binary canonicalize_i (+ canonicalize_i 8)))))) (def canonicalize_i (+ canonicalize_i 8)))) (throw (ex-info "return" {:v (str (str (join canonicalize_canonParts ".") "/") (nth canonicalize_parts 1))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_tests ["87.70.141.1/22" "36.18.154.103/12" "62.62.197.11/29" "67.137.119.181/4" "161.214.74.21/24" "184.232.176.184/18"])

(defn -main []
  (doseq [t main_tests] (println (str (str (padRight t 18) " -> ") (canonicalize t)))))

(-main)
