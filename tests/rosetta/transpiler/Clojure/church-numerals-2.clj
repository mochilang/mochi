(ns main (:refer-clojure :exclude [id compose zero one succ plus mult exp toInt toStr main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare id compose zero one succ plus mult exp toInt toStr main)

(declare main_eight main_four main_onev main_three main_two toInt_counter toStr_s)

(defn id [id_x]
  (try (throw (ex-info "return" {:v id_x})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn compose [compose_f compose_g]
  (try (throw (ex-info "return" {:v (fn [x] (throw (ex-info "return" {:v (f (g x))})))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn zero []
  (try (throw (ex-info "return" {:v (fn [f] (throw (ex-info "return" {:v id})))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn one []
  (try (throw (ex-info "return" {:v id})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn succ [succ_n]
  (try (throw (ex-info "return" {:v (fn [f] (throw (ex-info "return" {:v (compose f (n f))})))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn plus [plus_m plus_n]
  (try (throw (ex-info "return" {:v (fn [f] (throw (ex-info "return" {:v (compose (m f) (n f))})))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn mult [mult_m mult_n]
  (try (throw (ex-info "return" {:v (compose mult_m mult_n)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn exp [exp_m exp_n]
  (try (throw (ex-info "return" {:v (n exp_m)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fCounter [toInt_x fCounter_f]
  (try (do (def fCounter_counter (+ toInt_counter 1)) (throw (ex-info "return" {:v fCounter_f}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn toInt [toInt_x]
  (try (do (def toInt_counter 0) ((x fCounter) id) (throw (ex-info "return" {:v toInt_counter}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fCounter [toStr_x fCounter_f]
  (try (do (def fCounter_s (str toStr_s "|")) (throw (ex-info "return" {:v fCounter_f}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn toStr [toStr_x]
  (try (do (def toStr_s "") ((x fCounter) id) (throw (ex-info "return" {:v toStr_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (println (str "zero = " (str (toInt (zero))))) (def main_onev (one)) (println (str "one = " (str (toInt main_onev)))) (def main_two (succ (succ (zero)))) (println (str "two = " (str (toInt main_two)))) (def main_three (plus main_onev main_two)) (println (str "three = " (str (toInt main_three)))) (def main_four (mult main_two main_two)) (println (str "four = " (str (toInt main_four)))) (def main_eight (exp main_two main_three)) (println (str "eight = " (str (toInt main_eight)))) (println (str "toStr(four) = " (toStr main_four)))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]

      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
