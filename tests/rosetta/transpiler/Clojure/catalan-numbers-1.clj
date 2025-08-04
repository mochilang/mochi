(ns main (:refer-clojure :exclude [binom catalan main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare binom catalan main)

(def ^:dynamic binom_i nil)

(def ^:dynamic binom_kk nil)

(def ^:dynamic binom_res nil)

(defn binom [binom_n binom_k]
  (binding [binom_i nil binom_kk nil binom_res nil] (try (do (when (or (< binom_k 0) (> binom_k binom_n)) (throw (ex-info "return" {:v 0}))) (set! binom_kk binom_k) (when (> binom_kk (- binom_n binom_kk)) (set! binom_kk (- binom_n binom_kk))) (set! binom_res 1) (set! binom_i 0) (while (< binom_i binom_kk) (do (set! binom_res (* binom_res (- binom_n binom_i))) (set! binom_i (+ binom_i 1)) (set! binom_res (long (/ binom_res binom_i))))) (throw (ex-info "return" {:v binom_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn catalan [catalan_n]
  (try (throw (ex-info "return" {:v (long (/ (binom (* 2 catalan_n) catalan_n) (+ catalan_n 1)))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (dotimes [i 15] (println (str (catalan i)))))

(defn -main []
  (main))

(-main)
