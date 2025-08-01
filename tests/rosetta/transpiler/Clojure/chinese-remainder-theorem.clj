(ns main (:refer-clojure :exclude [egcd modInv crt]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare egcd modInv crt)

(declare a crt_ai crt_i crt_inv crt_ni crt_p crt_prod crt_x egcd_g egcd_res egcd_x1 egcd_y1 modInv_r modInv_x n res)

(defn egcd [egcd_a egcd_b]
  (try (do (when (= egcd_a 0) (throw (ex-info "return" {:v [egcd_b 0 1]}))) (def egcd_res (egcd (mod egcd_b egcd_a) egcd_a)) (def egcd_g (nth egcd_res 0)) (def egcd_x1 (nth egcd_res 1)) (def egcd_y1 (nth egcd_res 2)) (throw (ex-info "return" {:v [egcd_g (- egcd_y1 (* (/ egcd_b egcd_a) egcd_x1)) egcd_x1]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn modInv [modInv_a modInv_m]
  (try (do (def modInv_r (egcd modInv_a modInv_m)) (when (not= (nth modInv_r 0) 1) (throw (ex-info "return" {:v 0}))) (def modInv_x (nth modInv_r 1)) (if (< modInv_x 0) (+ modInv_x modInv_m) modInv_x)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn crt [crt_a crt_n]
  (try (do (def crt_prod 1) (def crt_i 0) (while (< crt_i (count crt_n)) (do (def crt_prod (* crt_prod (nth crt_n crt_i))) (def crt_i (+ crt_i 1)))) (def crt_x 0) (def crt_i 0) (while (< crt_i (count crt_n)) (do (def crt_ni (nth crt_n crt_i)) (def crt_ai (nth crt_a crt_i)) (def crt_p (/ crt_prod crt_ni)) (def crt_inv (modInv (mod crt_p crt_ni) crt_ni)) (def crt_x (+ crt_x (* (* crt_ai crt_inv) crt_p))) (def crt_i (+ crt_i 1)))) (throw (ex-info "return" {:v (mod crt_x crt_prod)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def n [3 5 7])

(def a [2 3 2])

(def res (crt a n))

(defn -main []
  (println (str (str res) " <nil>")))

(-main)
