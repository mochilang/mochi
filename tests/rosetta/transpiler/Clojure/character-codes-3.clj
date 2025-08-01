(ns main (:refer-clojure :exclude [ord chr]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare ord chr)

(declare main_b main_r main_s)

(defn ord [ord_ch]
  (try (do (when (= ord_ch "a") (throw (ex-info "return" {:v 97}))) (when (= ord_ch "π") (throw (ex-info "return" {:v 960}))) (if (= ord_ch "A") 65 0)) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn chr [chr_n]
  (try (do (when (= chr_n 97) (throw (ex-info "return" {:v "a"}))) (when (= chr_n 960) (throw (ex-info "return" {:v "π"}))) (if (= chr_n 65) "A" "?")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(def main_b (ord "a"))

(def main_r (ord "π"))

(def main_s "aπ")

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (str (str (str (str main_b) " ") (str main_r)) " ") main_s))
      (println (str (str (str (str "string cast to []rune: [" (str main_b)) " ") (str main_r)) "]"))
      (println (str (str (str "    string range loop: " (str main_b)) " ") (str main_r)))
      (println "         string bytes: 0x61 0xcf 0x80")
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
