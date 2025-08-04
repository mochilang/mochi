(ns main (:refer-clojure :exclude [absf pow10 formatFloat]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare absf pow10 formatFloat)

(declare formatFloat_digits formatFloat_fracPart formatFloat_intPart formatFloat_n formatFloat_scale formatFloat_scaled main_e main_epsilon main_factval main_n main_term pow10_i pow10_r)

(def main_epsilon 0.000000000000001)

(defn absf [absf_x]
  (try (if (< absf_x 0.0) (- absf_x) absf_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pow10 [pow10_n]
  (try (do (def pow10_r 1.0) (def pow10_i 0) (while (< pow10_i pow10_n) (do (def pow10_r (* pow10_r 10.0)) (def pow10_i (+ pow10_i 1)))) (throw (ex-info "return" {:v pow10_r}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn formatFloat [formatFloat_f formatFloat_prec]
  (try (do (def formatFloat_scale (pow10 formatFloat_prec)) (def formatFloat_scaled (+ (* formatFloat_f formatFloat_scale) 0.5)) (def formatFloat_n (long formatFloat_scaled)) (def formatFloat_digits (str formatFloat_n)) (while (<= (count formatFloat_digits) formatFloat_prec) (def formatFloat_digits (str "0" formatFloat_digits))) (def formatFloat_intPart (subs formatFloat_digits 0 (- (count formatFloat_digits) formatFloat_prec))) (def formatFloat_fracPart (subs formatFloat_digits (- (count formatFloat_digits) formatFloat_prec) (count formatFloat_digits))) (throw (ex-info "return" {:v (str (str formatFloat_intPart ".") formatFloat_fracPart)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_factval 1)

(def main_e 2.0)

(def main_n 2)

(def main_term 1.0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def main_factval (* main_factval main_n)) (def main_n (+ main_n 1)) (def main_term (/ 1.0 (double main_factval))) (def main_e (+ main_e main_term)) (cond (< (absf main_term) main_epsilon) (recur false) :else (recur while_flag_1)))))
      (println (str "e = " (formatFloat main_e 15)))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
