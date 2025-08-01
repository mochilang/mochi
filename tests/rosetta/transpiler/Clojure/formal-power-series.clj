(ns main (:refer-clojure :exclude [newFps extract one add sub mul div differentiate integrate sinCos floorf fmtF5 padFloat5 partialSeries main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare newFps extract one add sub mul div differentiate integrate sinCos floorf fmtF5 padFloat5 partialSeries main)

(declare b0 cos decs dot i idx k out p q s sin v y)

(defn newFps [fn]
  (try (throw (ex-info "return" {:v {:coeffs [] :compute fn}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn extract [f_p n]
  (try (do (def f f_p) (while (<= (count (:coeffs f)) n) (do (def idx (count (:coeffs f))) (def v ((:compute f) idx)) (def f (assoc f :coeffs (conj (:coeffs f) v))))) (throw (ex-info "return" {:v (get (:coeffs f) n)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn one []
  (try (throw (ex-info "return" {:v (newFps (fn [i] (do (when (= i 0) (throw (ex-info "return" {:v 1.0}))) (throw (ex-info "return" {:v 0.0})))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn add [a b]
  (try (throw (ex-info "return" {:v (newFps (fn [n] (throw (ex-info "return" {:v (+' (extract a n) (extract b n))}))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sub [a b]
  (try (throw (ex-info "return" {:v (newFps (fn [n] (throw (ex-info "return" {:v (- (extract a n) (extract b n))}))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mul [a b]
  (try (throw (ex-info "return" {:v (newFps (fn [n] (do (def s 0.0) (def k 0) (while (<= k n) (do (def s (+' s (* (extract a k) (extract b (- n k))))) (def k (+' k 1)))) (throw (ex-info "return" {:v s})))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn div [a b]
  (try (do (def q (newFps (fn [n] (throw (ex-info "return" {:v 0.0}))))) (def q (assoc q :compute (fn [n] (do (def b0 (extract b 0)) (when (= b0 0.0) (throw (ex-info "return" {:v (/ 0.0 0.0)}))) (def s (extract a n)) (def k 1) (while (<= k n) (do (def s (- s (* (extract b k) (extract q (- n k))))) (def k (+' k 1)))) (throw (ex-info "return" {:v (/ s b0)})))))) (throw (ex-info "return" {:v q}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn differentiate [a]
  (try (throw (ex-info "return" {:v (newFps (fn [n] (throw (ex-info "return" {:v (* (double (+' n 1)) (extract a (+' n 1)))}))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn integrate [a]
  (try (throw (ex-info "return" {:v (newFps (fn [n] (do (when (= n 0) (throw (ex-info "return" {:v 0.0}))) (throw (ex-info "return" {:v (/ (extract a (- n 1)) (double n))})))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sinCos []
  (try (do (def sin (newFps (fn [n] (throw (ex-info "return" {:v 0.0}))))) (def cos (sub (one) (integrate sin))) (def sin (assoc sin :compute (fn [n] (do (when (= n 0) (throw (ex-info "return" {:v 0.0}))) (throw (ex-info "return" {:v (/ (extract cos (- n 1)) (double n))})))))) (throw (ex-info "return" {:v {:sin sin :cos cos}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn floorf [x]
  (try (do (def y (int x)) (throw (ex-info "return" {:v (double y)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fmtF5 [x]
  (try (do (def y (/ (floorf (+' (* x 100000.0) 0.5)) 100000.0)) (def s (str y)) (def dot (indexOf s ".")) (if (= dot (- 0 1)) (def s (str s ".00000")) (do (def decs (- (- (count s) dot) 1)) (if (> decs 5) (def s (subs s 0 (+' dot 6))) (while (< decs 5) (do (def s (str s "0")) (def decs (+' decs 1))))))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padFloat5 [x width]
  (try (do (def s (fmtF5 x)) (while (< (count s) width) (def s (str " " s))) (throw (ex-info "return" {:v s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn partialSeries [f]
  (try (do (def out "") (def i 0) (while (< i 6) (do (def out (str (str (str out " ") (padFloat5 (extract f i) 8)) " ")) (def i (+' i 1)))) (throw (ex-info "return" {:v out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def p (sinCos)) (println (str "sin:" (partialSeries (:sin p)))) (println (str "cos:" (partialSeries (:cos p))))))

(defn -main []
  (main))

(-main)
