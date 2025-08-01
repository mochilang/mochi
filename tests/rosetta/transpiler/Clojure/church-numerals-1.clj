(ns main (:refer-clojure :exclude [zero succ add mul pow incr toInt intToChurch]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare zero succ add mul pow incr toInt intToChurch)

(declare main_four main_three main_z pow_di pow_i pow_prod)

(defn zero [zero_f]
  (try (throw (ex-info "return" {:v (fn [x] (throw (ex-info "return" {:v x})))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn succ [succ_c]
  (try (throw (ex-info "return" {:v (fn [f] (throw (ex-info "return" {:v (fn [x] (throw (ex-info "return" {:v (f ((c f) x))})))})))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn add [add_c add_d]
  (try (throw (ex-info "return" {:v (fn [f] (throw (ex-info "return" {:v (fn [x] (throw (ex-info "return" {:v ((c f) ((d f) x))})))})))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mul [mul_c mul_d]
  (try (throw (ex-info "return" {:v (fn [f] (throw (ex-info "return" {:v (fn [x] (throw (ex-info "return" {:v ((c (d f)) x)})))})))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn pow [pow_c pow_d]
  (try (do (def pow_di (toInt pow_d)) (def pow_prod pow_c) (def pow_i 1) (while (< pow_i pow_di) (do (def pow_prod (mul pow_prod pow_c)) (def pow_i (+ pow_i 1)))) (throw (ex-info "return" {:v pow_prod}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn incr [incr_i]
  (try (throw (ex-info "return" {:v (+ (int incr_i) 1)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn toInt [toInt_c]
  (try (throw (ex-info "return" {:v (int ((c incr) 0))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn intToChurch [intToChurch_i]
  (try (if (= intToChurch_i 0) zero (succ (intToChurch (- intToChurch_i 1)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_z zero)

(def main_three (succ (succ (succ main_z))))

(def main_four (succ main_three))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str "three        -> " (str (toInt main_three))))
      (println (str "four         -> " (str (toInt main_four))))
      (println (str "three + four -> " (str (toInt (add main_three main_four)))))
      (println (str "three * four -> " (str (toInt (mul main_three main_four)))))
      (println (str "three ^ four -> " (str (toInt (pow main_three main_four)))))
      (println (str "four ^ three -> " (str (toInt (pow main_four main_three)))))
      (println (str "5 -> five    -> " (str (toInt (intToChurch 5)))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
