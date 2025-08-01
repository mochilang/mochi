(ns main (:refer-clojure :exclude [monthUnique dayUnique monthWithUniqueDay bstr]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare monthUnique dayUnique monthWithUniqueDay bstr)

(declare bstr_months dayUnique_c main_choices main_filtered main_filtered2 main_filtered3 main_filtered4 monthUnique_c)

(defn monthUnique [monthUnique_b monthUnique_list]
  (try (do (def monthUnique_c 0) (doseq [x monthUnique_list] (when (= (:month x) (:month monthUnique_b)) (def monthUnique_c (+ monthUnique_c 1)))) (throw (ex-info "return" {:v (= monthUnique_c 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dayUnique [dayUnique_b dayUnique_list]
  (try (do (def dayUnique_c 0) (doseq [x dayUnique_list] (when (= (:day x) (:day dayUnique_b)) (def dayUnique_c (+ dayUnique_c 1)))) (throw (ex-info "return" {:v (= dayUnique_c 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn monthWithUniqueDay [monthWithUniqueDay_b monthWithUniqueDay_list]
  (try (do (doseq [x monthWithUniqueDay_list] (when (and (= (:month x) (:month monthWithUniqueDay_b)) (dayUnique x monthWithUniqueDay_list)) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bstr [bstr_b]
  (try (do (def bstr_months ["" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"]) (throw (ex-info "return" {:v (str (str (nth bstr_months (:month bstr_b)) " ") (str (:day bstr_b)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_choices [{:month 5 :day 15} {:month 5 :day 16} {:month 5 :day 19} {:month 6 :day 17} {:month 6 :day 18} {:month 7 :day 14} {:month 7 :day 16} {:month 8 :day 14} {:month 8 :day 15} {:month 8 :day 17}])

(def main_filtered [])

(def main_filtered2 [])

(def main_filtered3 [])

(def main_filtered4 [])

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (doseq [bd main_choices] (when (not (monthUnique bd main_choices)) (def main_filtered (conj main_filtered bd))))
      (doseq [bd main_filtered] (when (not (monthWithUniqueDay bd main_filtered)) (def main_filtered2 (conj main_filtered2 bd))))
      (doseq [bd main_filtered2] (when (dayUnique bd main_filtered2) (def main_filtered3 (conj main_filtered3 bd))))
      (doseq [bd main_filtered3] (when (monthUnique bd main_filtered3) (def main_filtered4 (conj main_filtered4 bd))))
      (if (= (count main_filtered4) 1) (println (str "Cheryl's birthday is " (bstr (nth main_filtered4 0)))) (println "Something went wrong!"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
