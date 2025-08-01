(ns main (:refer-clojure :exclude [isPrime bigMulSmall pow2 ccFactors ccNumbers]))

(require 'clojure.set)

(defn bigToString [a]
  (str a))

(defn bigTrim [a]
  a)

(defn bigFromInt [x]
  (bigint x))

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare isPrime bigMulSmall pow2 ccFactors ccNumbers)

(declare bigMulSmall_carry bigMulSmall_i bigMulSmall_prod bigMulSmall_res ccFactors_i ccFactors_p ccFactors_prod ccNumbers_m ccNumbers_n ccNumbers_num isPrime_d pow2_i pow2_r)

(defn isPrime [isPrime_n]
  (try (do (when (< isPrime_n 2) (throw (ex-info "return" {:v false}))) (when (= (mod isPrime_n 2) 0) (throw (ex-info "return" {:v (= isPrime_n 2)}))) (when (= (mod isPrime_n 3) 0) (throw (ex-info "return" {:v (= isPrime_n 3)}))) (def isPrime_d 5) (while (<= (* isPrime_d isPrime_d) isPrime_n) (do (when (= (mod isPrime_n isPrime_d) 0) (throw (ex-info "return" {:v false}))) (def isPrime_d (+ isPrime_d 2)) (when (= (mod isPrime_n isPrime_d) 0) (throw (ex-info "return" {:v false}))) (def isPrime_d (+ isPrime_d 4)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bigMulSmall [bigMulSmall_a bigMulSmall_m]
  (try (do (when (= bigMulSmall_m 0) (throw (ex-info "return" {:v [0]}))) (def bigMulSmall_res []) (def bigMulSmall_carry 0) (def bigMulSmall_i 0) (while (< bigMulSmall_i (count bigMulSmall_a)) (do (def bigMulSmall_prod (+ (* (nth bigMulSmall_a bigMulSmall_i) bigMulSmall_m) bigMulSmall_carry)) (def bigMulSmall_res (conj bigMulSmall_res (mod bigMulSmall_prod 10))) (def bigMulSmall_carry (/ bigMulSmall_prod 10)) (def bigMulSmall_i (+ bigMulSmall_i 1)))) (while (> bigMulSmall_carry 0) (do (def bigMulSmall_res (conj bigMulSmall_res (mod bigMulSmall_carry 10))) (def bigMulSmall_carry (/ bigMulSmall_carry 10)))) (throw (ex-info "return" {:v (bigTrim bigMulSmall_res)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pow2 [pow2_k]
  (try (do (def pow2_r 1) (def pow2_i 0) (while (< pow2_i pow2_k) (do (def pow2_r (* pow2_r 2)) (def pow2_i (+ pow2_i 1)))) (throw (ex-info "return" {:v pow2_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ccFactors [ccFactors_n ccFactors_m]
  (try (do (def ccFactors_p (+ (* 6 ccFactors_m) 1)) (when (not (isPrime ccFactors_p)) (throw (ex-info "return" {:v []}))) (def ccFactors_prod (bigFromInt ccFactors_p)) (def ccFactors_p (+ (* 12 ccFactors_m) 1)) (when (not (isPrime ccFactors_p)) (throw (ex-info "return" {:v []}))) (def ccFactors_prod (bigMulSmall ccFactors_prod ccFactors_p)) (def ccFactors_i 1) (while (<= ccFactors_i (- ccFactors_n 2)) (do (def ccFactors_p (+ (* (* (pow2 ccFactors_i) 9) ccFactors_m) 1)) (when (not (isPrime ccFactors_p)) (throw (ex-info "return" {:v []}))) (def ccFactors_prod (bigMulSmall ccFactors_prod ccFactors_p)) (def ccFactors_i (+ ccFactors_i 1)))) (throw (ex-info "return" {:v ccFactors_prod}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn ccNumbers [ccNumbers_start ccNumbers_end]
  (do (def ccNumbers_n ccNumbers_start) (while (<= ccNumbers_n ccNumbers_end) (do (def ccNumbers_m 1) (when (> ccNumbers_n 4) (def ccNumbers_m (pow2 (- ccNumbers_n 4)))) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def ccNumbers_num (ccFactors ccNumbers_n ccNumbers_m)) (cond (> (count ccNumbers_num) 0) (do (println (str (str (str "a(" (str ccNumbers_n)) ") = ") (bigToString ccNumbers_num))) (recur false)) :else (do (if (<= ccNumbers_n 4) (def ccNumbers_m (+ ccNumbers_m 1)) (def ccNumbers_m (+ ccNumbers_m (pow2 (- ccNumbers_n 4))))) (recur while_flag_1)))))) (def ccNumbers_n (+ ccNumbers_n 1))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (ccNumbers 3 9)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
