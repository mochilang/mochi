(ns main (:refer-clojure :exclude [monthUnique dayUnique monthWithUniqueDay bstr]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare monthUnique dayUnique monthWithUniqueDay bstr)

(declare bstr_months choices dayUnique_c filtered filtered2 filtered3 filtered4 monthUnique_c)

(defn monthUnique [monthUnique_b monthUnique_list]
  (try (do (def monthUnique_c 0) (doseq [x monthUnique_list] (when (= (:month monthUnique_x) (:month monthUnique_b)) (def monthUnique_c (+ monthUnique_c 1)))) (throw (ex-info "return" {:v (= monthUnique_c 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn dayUnique [dayUnique_b dayUnique_list]
  (try (do (def dayUnique_c 0) (doseq [x dayUnique_list] (when (= (:day dayUnique_x) (:day dayUnique_b)) (def dayUnique_c (+ dayUnique_c 1)))) (throw (ex-info "return" {:v (= dayUnique_c 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn monthWithUniqueDay [monthWithUniqueDay_b monthWithUniqueDay_list]
  (try (do (doseq [x monthWithUniqueDay_list] (when (and (= (:month monthWithUniqueDay_x) (:month monthWithUniqueDay_b)) (dayUnique monthWithUniqueDay_x monthWithUniqueDay_list)) (throw (ex-info "return" {:v true})))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn bstr [bstr_b]
  (try (do (def bstr_months ["" "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"]) (throw (ex-info "return" {:v (str (str (nth bstr_months (:month bstr_b)) " ") (str (:day bstr_b)))}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def choices [{:month 5 :day 15} {:month 5 :day 16} {:month 5 :day 19} {:month 6 :day 17} {:month 6 :day 18} {:month 7 :day 14} {:month 7 :day 16} {:month 8 :day 14} {:month 8 :day 15} {:month 8 :day 17}])

(def filtered [])

(def filtered2 [])

(def filtered3 [])

(def filtered4 [])

(defn -main []
  (doseq [bd choices] (when (not (monthUnique bd choices)) (def filtered (conj filtered bd))))
  (doseq [bd filtered] (when (not (monthWithUniqueDay bd filtered)) (def filtered2 (conj filtered2 bd))))
  (doseq [bd filtered2] (when (dayUnique bd filtered2) (def filtered3 (conj filtered3 bd))))
  (doseq [bd filtered3] (when (monthUnique bd filtered3) (def filtered4 (conj filtered4 bd))))
  (if (= (count filtered4) 1) (println (str "Cheryl's birthday is " (bstr (nth filtered4 0)))) (println "Something went wrong!")))

(-main)
