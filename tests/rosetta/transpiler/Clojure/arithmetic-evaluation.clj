(ns main (:refer-clojure :exclude [skipWS parseIntStr parseNumber parseFactor powInt parsePower parseTerm parseExpr evalExpr main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare skipWS parseIntStr parseNumber parseFactor powInt parsePower parseTerm parseExpr evalExpr main)

(defn skipWS [p]
  (try (do (def i (:pos p)) (while (and (< i (count (:expr p))) (= (subs (:expr p) i (+ i 1)) " ")) (def i (+ i 1))) (def p (assoc p :pos i)) (throw (ex-info "return" {:v p}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseIntStr [str]
  (try (do (def i 0) (def n 0) (while (< i (count str)) (do (def n (- (+ (* n 10) (Integer/parseInt (subs str i (+ i 1)))) 48)) (def i (+ i 1)))) (throw (ex-info "return" {:v n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseNumber [p]
  (try (do (def p (skipWS p)) (def start (:pos p)) (loop [while_flag_1 true] (when (and while_flag_1 (< (:pos p) (count (:expr p)))) (do (def ch (subs (:expr p) (:pos p) (+ (:pos p) 1))) (if (and (>= (compare ch "0") 0) (<= (compare ch "9") 0)) (def p (assoc p :pos (+ (:pos p) 1))) (set! while_flag_1 false)) (cond :else (recur while_flag_1))))) (def token (subs (:expr p) start (:pos p))) (throw (ex-info "return" {:v {:v (parseIntStr token) :p p}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseFactor [p]
  (try (do (def p (skipWS p)) (when (and (< (:pos p) (count (:expr p))) (= (subs (:expr p) (:pos p) (+ (:pos p) 1)) "(")) (do (def p (assoc p :pos (+ (:pos p) 1))) (def r (parseExpr p)) (def v (:v r)) (def p (:p r)) (def p (skipWS p)) (when (and (< (:pos p) (count (:expr p))) (= (subs (:expr p) (:pos p) (+ (:pos p) 1)) ")")) (def p (assoc p :pos (+ (:pos p) 1)))) (throw (ex-info "return" {:v {:v v :p p}})))) (when (and (< (:pos p) (count (:expr p))) (= (subs (:expr p) (:pos p) (+ (:pos p) 1)) "-")) (do (def p (assoc p :pos (+ (:pos p) 1))) (def r (parseFactor p)) (def v (:v r)) (def p (:p r)) (throw (ex-info "return" {:v {:v (- v) :p p}})))) (throw (ex-info "return" {:v (parseNumber p)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn powInt [base exp]
  (try (do (def r 1) (def b base) (def e exp) (while (> e 0) (do (when (= (mod e 2) 1) (def r (* r b))) (def b (* b b)) (def e (/ e (int 2))))) (throw (ex-info "return" {:v r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parsePower [p]
  (try (do (def r (parseFactor p)) (def v (:v r)) (def p (:p r)) (loop [while_flag_2 true] (when (and while_flag_2 true) (do (def p (skipWS p)) (if (and (< (:pos p) (count (:expr p))) (= (subs (:expr p) (:pos p) (+ (:pos p) 1)) "^")) (do (def p (assoc p :pos (+ (:pos p) 1))) (def r2 (parseFactor p)) (def rhs (:v r2)) (def p (:p r2)) (def v (powInt v rhs))) (set! while_flag_2 false)) (cond :else (recur while_flag_2))))) (throw (ex-info "return" {:v {:v v :p p}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseTerm [p]
  (try (do (def r (parsePower p)) (def v (:v r)) (def p (:p r)) (loop [while_flag_3 true] (when (and while_flag_3 true) (do (def p (skipWS p)) (cond (< (:pos p) (count (:expr p))) (do (def op (subs (:expr p) (:pos p) (+ (:pos p) 1))) (when (= op "*") (do (def p (assoc p :pos (+ (:pos p) 1))) (def r2 (parsePower p)) (def rhs (:v r2)) (def p (:p r2)) (def v (* v rhs)) (recur true))) (when (= op "/") (do (def p (assoc p :pos (+ (:pos p) 1))) (def r2 (parsePower p)) (def rhs (:v r2)) (def p (:p r2)) (def v (/ v (int rhs))) (recur true)))) :else (do (set! while_flag_3 false) (recur while_flag_3)))))) (throw (ex-info "return" {:v {:v v :p p}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseExpr [p]
  (try (do (def r (parseTerm p)) (def v (:v r)) (def p (:p r)) (loop [while_flag_4 true] (when (and while_flag_4 true) (do (def p (skipWS p)) (cond (< (:pos p) (count (:expr p))) (do (def op (subs (:expr p) (:pos p) (+ (:pos p) 1))) (when (= op "+") (do (def p (assoc p :pos (+ (:pos p) 1))) (def r2 (parseTerm p)) (def rhs (:v r2)) (def p (:p r2)) (def v (+ v rhs)) (recur true))) (when (= op "-") (do (def p (assoc p :pos (+ (:pos p) 1))) (def r2 (parseTerm p)) (def rhs (:v r2)) (def p (:p r2)) (def v (- v rhs)) (recur true)))) :else (do (set! while_flag_4 false) (recur while_flag_4)))))) (throw (ex-info "return" {:v {:v v :p p}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn evalExpr [expr]
  (try (do (def p {:expr expr :pos 0}) (def r (parseExpr p)) (throw (ex-info "return" {:v (:v r)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def expr "2*(3-1)+2*5") (println (str (str expr " = ") (str (evalExpr expr))))))

(defn -main []
  (main))

(-main)
