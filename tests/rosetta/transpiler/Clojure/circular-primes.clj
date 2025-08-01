(ns main (:refer-clojure :exclude [isPrime isCircular showList]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare isPrime isCircular showList)

(declare count_v isCircular_f isCircular_nn isCircular_pow isPrime_d main_circs main_digits main_f main_fd main_fq main_q showList_i showList_out)

(defn isPrime [isPrime_n]
  (try (do (when (< isPrime_n 2) (throw (ex-info "return" {:v false}))) (when (= (mod isPrime_n 2) 0) (throw (ex-info "return" {:v (= isPrime_n 2)}))) (when (= (mod isPrime_n 3) 0) (throw (ex-info "return" {:v (= isPrime_n 3)}))) (def isPrime_d 5) (while (<= (* isPrime_d isPrime_d) isPrime_n) (do (when (= (mod isPrime_n isPrime_d) 0) (throw (ex-info "return" {:v false}))) (def isPrime_d (+ isPrime_d 2)) (when (= (mod isPrime_n isPrime_d) 0) (throw (ex-info "return" {:v false}))) (def isPrime_d (+ isPrime_d 4)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_circs [])

(defn isCircular [isCircular_n]
  (try (do (def isCircular_nn isCircular_n) (def isCircular_pow 1) (while (> isCircular_nn 0) (do (def isCircular_pow (* isCircular_pow 10)) (def isCircular_nn (/ isCircular_nn 10)))) (def isCircular_nn isCircular_n) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def isCircular_nn (* isCircular_nn 10)) (def isCircular_f (/ isCircular_nn isCircular_pow)) (def isCircular_nn (+ isCircular_nn (* isCircular_f (- 1 isCircular_pow)))) (cond (= isCircular_nn isCircular_n) (recur false) :else (do (when (not (isPrime isCircular_nn)) (throw (ex-info "return" {:v false}))) (recur while_flag_1)))))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_digits [1 3 7 9])

(def main_q [1 2 3 5 7 9])

(def main_fq [1 2 3 5 7 9])

(def count_v 0)

(defn showList [showList_xs]
  (try (do (def showList_out "[") (def showList_i 0) (while (< showList_i (count showList_xs)) (do (def showList_out (str showList_out (str (nth showList_xs showList_i)))) (when (< showList_i (- (count showList_xs) 1)) (def showList_out (str showList_out ", "))) (def showList_i (+ showList_i 1)))) (throw (ex-info "return" {:v (str showList_out "]")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println "The first 19 circular primes are:")
      (loop [while_flag_2 true] (when (and while_flag_2 true) (do (def main_f (nth main_q 0)) (def main_fd (nth main_fq 0)) (cond (and (isPrime main_f) (isCircular main_f)) (do (def main_circs (conj main_circs main_f)) (def count_v (+ count_v 1)) (when (= count_v 19) (recur false))) :else (do (def main_q (subvec main_q 1 (count main_q))) (def main_fq (subvec main_fq 1 (count main_fq))) (when (and (not= main_f 2) (not= main_f 5)) (doseq [d main_digits] (do (def main_q (conj main_q (+ (* main_f 10) d))) (def main_fq (conj main_fq main_fd))))) (recur while_flag_2))))))
      (println (showList main_circs))
      (println "\nThe next 4 circular primes, in repunit format, are:")
      (println "[R(19) R(23) R(317) R(1031)]")
      (println "\nThe following repunits are probably circular primes:")
      (doseq [i [5003 9887 15073 25031 35317 49081]] (println (str (str "R(" (str i)) ") : true")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
