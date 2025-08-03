(ns main (:refer-clojure :exclude [isPrime firstPrimeFactor indexOf padLeft formatFloat main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare isPrime firstPrimeFactor indexOf padLeft formatFloat main)

(declare firstPrimeFactor_i firstPrimeFactor_inc firstPrimeFactor_k formatFloat_idx formatFloat_need formatFloat_s indexOf_i isPrime_d main_bc main_blum main_counts main_d main_digits main_i main_idx main_j main_line main_p main_q padLeft_s)

(defn isPrime [isPrime_n]
  (try (do (when (< isPrime_n 2) (throw (ex-info "return" {:v false}))) (when (= (mod isPrime_n 2) 0) (throw (ex-info "return" {:v (= isPrime_n 2)}))) (when (= (mod isPrime_n 3) 0) (throw (ex-info "return" {:v (= isPrime_n 3)}))) (def isPrime_d 5) (while (<= (* isPrime_d isPrime_d) isPrime_n) (do (when (= (mod isPrime_n isPrime_d) 0) (throw (ex-info "return" {:v false}))) (def isPrime_d (+ isPrime_d 2)) (when (= (mod isPrime_n isPrime_d) 0) (throw (ex-info "return" {:v false}))) (def isPrime_d (+ isPrime_d 4)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn firstPrimeFactor [firstPrimeFactor_n]
  (try (do (when (= firstPrimeFactor_n 1) (throw (ex-info "return" {:v 1}))) (when (= (mod firstPrimeFactor_n 3) 0) (throw (ex-info "return" {:v 3}))) (when (= (mod firstPrimeFactor_n 5) 0) (throw (ex-info "return" {:v 5}))) (def firstPrimeFactor_inc [4 2 4 2 4 6 2 6]) (def firstPrimeFactor_k 7) (def firstPrimeFactor_i 0) (while (<= (* firstPrimeFactor_k firstPrimeFactor_k) firstPrimeFactor_n) (do (when (= (mod firstPrimeFactor_n firstPrimeFactor_k) 0) (throw (ex-info "return" {:v firstPrimeFactor_k}))) (def firstPrimeFactor_k (+ firstPrimeFactor_k (nth firstPrimeFactor_inc firstPrimeFactor_i))) (def firstPrimeFactor_i (mod (+ firstPrimeFactor_i 1) (count firstPrimeFactor_inc))))) (throw (ex-info "return" {:v firstPrimeFactor_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn indexOf [indexOf_s indexOf_ch]
  (try (do (def indexOf_i 0) (while (< indexOf_i (count indexOf_s)) (do (when (= (subs indexOf_s indexOf_i (+ indexOf_i 1)) indexOf_ch) (throw (ex-info "return" {:v indexOf_i}))) (def indexOf_i (+ indexOf_i 1)))) (throw (ex-info "return" {:v (- 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn padLeft [padLeft_n padLeft_width]
  (try (do (def padLeft_s (str padLeft_n)) (while (< (count padLeft_s) padLeft_width) (def padLeft_s (str " " padLeft_s))) (throw (ex-info "return" {:v padLeft_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn formatFloat [formatFloat_f formatFloat_prec]
  (try (do (def formatFloat_s (str formatFloat_f)) (def formatFloat_idx (indexOf formatFloat_s ".")) (when (< formatFloat_idx 0) (throw (ex-info "return" {:v formatFloat_s}))) (def formatFloat_need (+ (+ formatFloat_idx 1) formatFloat_prec)) (if (> (count formatFloat_s) formatFloat_need) (subs formatFloat_s 0 formatFloat_need) formatFloat_s)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def main_blum []) (def main_counts [0 0 0 0]) (def main_digits [1 3 7 9]) (def main_i 1) (def main_bc 0) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def main_p (firstPrimeFactor main_i)) (cond (= (mod main_p 4) 3) (do (def main_q (int (/ main_i main_p))) (when (and (and (not= main_q main_p) (= (mod main_q 4) 3)) (isPrime main_q)) (do (when (< main_bc 50) (def main_blum (conj main_blum main_i))) (def main_d (mod main_i 10)) (if (= main_d 1) (def main_counts (assoc main_counts 0 (+ (nth main_counts 0) 1))) (if (= main_d 3) (def main_counts (assoc main_counts 1 (+ (nth main_counts 1) 1))) (if (= main_d 7) (def main_counts (assoc main_counts 2 (+ (nth main_counts 2) 1))) (when (= main_d 9) (def main_counts (assoc main_counts 3 (+ (nth main_counts 3) 1))))))) (def main_bc (+ main_bc 1)) (when (= main_bc 50) (do (println "First 50 Blum integers:") (def main_idx 0) (while (< main_idx 50) (do (def main_line "") (def main_j 0) (while (< main_j 10) (do (def main_line (str (str main_line (padLeft (nth main_blum main_idx) 3)) " ")) (def main_idx (+ main_idx 1)) (def main_j (+ main_j 1)))) (println (subs main_line 0 (- (count main_line) 1))))) (recur false)))))) :else (do (if (= (mod main_i 5) 3) (def main_i (+ main_i 4)) (def main_i (+ main_i 2))) (recur while_flag_1))))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (main)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
