(ns main (:refer-clojure :exclude [ord chr]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare ord chr)

(declare b r s)

(defn ord [ord_ch]
  (try (do (when (= ord_ch "a") (throw (ex-info "return" {:v 97}))) (when (= ord_ch "π") (throw (ex-info "return" {:v 960}))) (if (= ord_ch "A") 65 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn chr [chr_n]
  (try (do (when (= chr_n 97) (throw (ex-info "return" {:v "a"}))) (when (= chr_n 960) (throw (ex-info "return" {:v "π"}))) (if (= chr_n 65) "A" "?")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def b (ord "a"))

(def r (ord "π"))

(def s "aπ")

(defn -main []
  (println (str (str (str (str (str b) " ") (str r)) " ") s))
  (println (str (str (str (str "string cast to []rune: [" (str b)) " ") (str r)) "]"))
  (println (str (str (str "    string range loop: " (str b)) " ") (str r)))
  (println "         string bytes: 0x61 0xcf 0x80"))

(-main)
