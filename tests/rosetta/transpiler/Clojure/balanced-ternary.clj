(ns main (:refer-clojure :exclude [trimLeftZeros btString btToString btInt btToInt btNeg btAdd btMul padLeft show main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare trimLeftZeros btString btToString btInt btToInt btNeg btAdd btMul padLeft show main)

(defn trimLeftZeros [s]
  (try (do (def i 0) (while (and (< i (count s)) (= (subs s i (+ i 1)) "0")) (def i (+ i 1))) (throw (ex-info "return" {:v (subs s i (count s))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn btString [s]
  (try (do (def s (trimLeftZeros s)) (def b []) (def i (- (count s) 1)) (while (>= i 0) (do (def ch (subs s i (+ i 1))) (if (= ch "+") (def b (conj b 1)) (if (= ch "0") (def b (conj b 0)) (if (= ch "-") (def b (conj b (- 0 1))) (throw (ex-info "return" {:v {"bt" [] "ok" false}}))))) (def i (- i 1)))) (throw (ex-info "return" {:v {"bt" b "ok" true}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn btToString [b]
  (try (do (when (= (count b) 0) (throw (ex-info "return" {:v "0"}))) (def r "") (def i (- (count b) 1)) (while (>= i 0) (do (def d (nth b i)) (if (= d (- 0 1)) (def r (str r "-")) (if (= d 0) (def r (str r "0")) (def r (str r "+")))) (def i (- i 1)))) (throw (ex-info "return" {:v r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn btInt [i]
  (try (do (when (= i 0) (throw (ex-info "return" {:v []}))) (def n i) (def b []) (while (not= n 0) (do (def m (mod n 3)) (def n (int (/ n 3))) (if (= m 2) (do (def m (- 0 1)) (def n (+ n 1))) (when (= m (- 0 2)) (do (def m 1) (def n (- n 1))))) (def b (conj b m)))) (throw (ex-info "return" {:v b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn btToInt [b]
  (try (do (def r 0) (def pt 1) (def i 0) (while (< i (count b)) (do (def r (+ r (* (nth b i) pt))) (def pt (* pt 3)) (def i (+ i 1)))) (throw (ex-info "return" {:v r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn btNeg [b]
  (try (do (def r []) (def i 0) (while (< i (count b)) (do (def r (conj r (- (nth b i)))) (def i (+ i 1)))) (throw (ex-info "return" {:v r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn btAdd [a b]
  (try (throw (ex-info "return" {:v (btInt (+ (btToInt a) (btToInt b)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn btMul [a b]
  (try (throw (ex-info "return" {:v (btInt (* (btToInt a) (btToInt b)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padLeft [s w]
  (try (do (def r s) (while (< (count r) w) (def r (str " " r))) (throw (ex-info "return" {:v r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn show [label b]
  (do (def l (padLeft label 7)) (def bs (padLeft (btToString b) 12)) (def is (padLeft (str (btToInt b)) 7)) (println (str (str (str (str l " ") bs) " ") is))))

(defn main []
  (do (def ares (btString "+-0++0+")) (def a (get ares "bt")) (def b (btInt (- 436))) (def cres (btString "+-++-")) (def c (get cres "bt")) (show "a:" a) (show "b:" b) (show "c:" c) (show "a(b-c):" (btMul a (btAdd b (btNeg c))))))

(defn -main []
  (main))

(-main)
