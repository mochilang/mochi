(ns main (:refer-clojure :exclude [newFactory]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn Box_TellSecret [Box_TellSecret_self]
  (try (throw (ex-info "return" {:v (:secret Box_TellSecret_self)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare newFactory)

(declare New_b main_Count main_New main_funcs newFactory_sn)

(defn New []
  (try (do (def New_sn (+ newFactory_sn 1)) (def New_b {:secret New_sn}) (if (= New_sn 1) (def New_b (assoc New_b :Contents "rabbit")) (when (= New_sn 2) (def New_b (assoc New_b :Contents "rock")))) (throw (ex-info "return" {:v New_b}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn Count []
  (try (throw (ex-info "return" {:v newFactory_sn})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn newFactory []
  (try (do (def newFactory_sn 0) (throw (ex-info "return" {:v [New Count]}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_funcs (newFactory))

(def main_New (nth main_funcs 0))

(def main_Count (nth main_funcs 1))

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
