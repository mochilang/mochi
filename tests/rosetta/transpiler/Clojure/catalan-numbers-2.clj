(ns main (:refer-clojure :exclude [catalanRec main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare catalanRec main)

(def ^:dynamic catalanRec_t1 nil)

(def ^:dynamic catalanRec_t2 nil)

(def ^:dynamic catalanRec_t3 nil)

(def ^:dynamic catalanRec_t5 nil)

(defn catalanRec [catalanRec_n]
  (binding [catalanRec_t1 nil catalanRec_t2 nil catalanRec_t3 nil catalanRec_t5 nil] (try (do (when (= catalanRec_n 0) (throw (ex-info "return" {:v 1}))) (set! catalanRec_t1 (* 2 catalanRec_n)) (set! catalanRec_t2 (- catalanRec_t1 1)) (set! catalanRec_t3 (* 2 catalanRec_t2)) (set! catalanRec_t5 (* catalanRec_t3 (catalanRec (- catalanRec_n 1)))) (throw (ex-info "return" {:v (long (/ catalanRec_t5 (+ catalanRec_n 1)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (doseq [i (range 1 16)] (println (str (catalanRec i)))))

(defn -main []
  (main))

(-main)
