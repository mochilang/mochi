(ns main (:refer-clojure :exclude [bigrat calkinWilf toContinued termNumber commatize main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare bigrat calkinWilf toContinued termNumber commatize main)

(declare calkinWilf_a calkinWilf_b calkinWilf_f calkinWilf_i calkinWilf_prev calkinWilf_seq calkinWilf_t commatize_cnt commatize_i commatize_neg commatize_out commatize_s main_cf main_cw main_i main_r main_s main_tn termNumber_b termNumber_d toContinued_a toContinued_b toContinued_res toContinued_t)

(defn bigrat [bigrat_a bigrat_b]
  (try (throw (ex-info "return" {:v (/ bigrat_a bigrat_b)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn calkinWilf [calkinWilf_n]
  (try (do (def calkinWilf_seq []) (def calkinWilf_seq (conj calkinWilf_seq (bigrat 1 1))) (def calkinWilf_i 1) (while (< calkinWilf_i calkinWilf_n) (do (def calkinWilf_prev (nth calkinWilf_seq (- calkinWilf_i 1))) (def calkinWilf_a (if (instance? clojure.lang.Ratio calkinWilf_prev) (numerator calkinWilf_prev) calkinWilf_prev)) (def calkinWilf_b (if (instance? clojure.lang.Ratio calkinWilf_prev) (denominator calkinWilf_prev) 1)) (def calkinWilf_f (/ calkinWilf_a calkinWilf_b)) (def calkinWilf_t (bigrat calkinWilf_f 1)) (def calkinWilf_t (* calkinWilf_t 2)) (def calkinWilf_t (- calkinWilf_t calkinWilf_prev)) (def calkinWilf_t (+ calkinWilf_t 1)) (def calkinWilf_t (/ 1 calkinWilf_t)) (def calkinWilf_seq (conj calkinWilf_seq calkinWilf_t)) (def calkinWilf_i (+ calkinWilf_i 1)))) (throw (ex-info "return" {:v calkinWilf_seq}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn toContinued [toContinued_r]
  (try (do (def toContinued_a (if (instance? clojure.lang.Ratio toContinued_r) (numerator toContinued_r) toContinued_r)) (def toContinued_b (if (instance? clojure.lang.Ratio toContinued_r) (denominator toContinued_r) 1)) (def toContinued_res []) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (def toContinued_res (conj toContinued_res (long (/ toContinued_a toContinued_b)))) (def toContinued_t (mod toContinued_a toContinued_b)) (def toContinued_a toContinued_b) (def toContinued_b toContinued_t) (cond (= toContinued_a 1) (recur false) :else (recur while_flag_1))))) (when (= (mod (count toContinued_res) 2) 0) (do (def toContinued_res (assoc toContinued_res (- (count toContinued_res) 1) (- (nth toContinued_res (- (count toContinued_res) 1)) 1))) (def toContinued_res (conj toContinued_res 1)))) (throw (ex-info "return" {:v toContinued_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn termNumber [termNumber_cf]
  (try (do (def termNumber_b "") (def termNumber_d "1") (doseq [n termNumber_cf] (do (def termNumber_b (str (apply str (repeat n termNumber_d)) termNumber_b)) (if (= termNumber_d "1") (def termNumber_d "0") (def termNumber_d "1")))) (throw (ex-info "return" {:v (Integer/parseInt termNumber_b 2)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn commatize [commatize_n]
  (try (do (def commatize_s (str commatize_n)) (def commatize_out "") (def commatize_i 0) (def commatize_cnt 0) (def commatize_neg false) (when (= (subs commatize_s 0 1) "-") (do (def commatize_neg true) (def commatize_s (subs commatize_s 1 (count commatize_s))))) (def commatize_i (- (count commatize_s) 1)) (while (>= commatize_i 0) (do (def commatize_out (str (subs commatize_s commatize_i (+ commatize_i 1)) commatize_out)) (def commatize_cnt (+ commatize_cnt 1)) (when (and (= commatize_cnt 3) (not= commatize_i 0)) (do (def commatize_out (str "," commatize_out)) (def commatize_cnt 0))) (def commatize_i (- commatize_i 1)))) (when commatize_neg (def commatize_out (str "-" commatize_out))) (throw (ex-info "return" {:v commatize_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def main_cw (calkinWilf 20)) (println "The first 20 terms of the Calkin-Wilf sequnence are:") (def main_i 0) (while (< main_i 20) (do (def main_r (nth main_cw main_i)) (def main_s (str (if (instance? clojure.lang.Ratio main_r) (numerator main_r) main_r))) (when (not= (if (instance? clojure.lang.Ratio main_r) (denominator main_r) 1) 1) (def main_s (str (str main_s "/") (str (if (instance? clojure.lang.Ratio main_r) (denominator main_r) 1))))) (println (str (str (padStart (+ main_i (long 1)) 2 " ") ": ") main_s)) (def main_i (+ main_i 1)))) (def main_r (bigrat 83116 51639)) (def main_cf (toContinued main_r)) (def main_tn (termNumber main_cf)) (println (str (str (str (str (str (str "" (str (if (instance? clojure.lang.Ratio main_r) (numerator main_r) main_r))) "/") (str (if (instance? clojure.lang.Ratio main_r) (denominator main_r) 1))) " is the ") (commatize main_tn)) "th term of the sequence."))))

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
