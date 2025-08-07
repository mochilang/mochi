(ns main (:refer-clojure :exclude [index_of decrypt]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare index_of decrypt)

(def ^:dynamic decrypt_idx nil)

(def ^:dynamic decrypt_num nil)

(def ^:dynamic decrypt_symbol nil)

(def ^:dynamic decrypt_translated nil)

(def ^:dynamic index_of_i nil)

(def ^:dynamic main_LETTERS "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn index_of [index_of_s index_of_ch]
  (binding [index_of_i nil] (try (do (set! index_of_i 0) (while (< index_of_i (count index_of_s)) (do (when (= (subs index_of_s index_of_i (min (+ index_of_i 1) (count index_of_s))) index_of_ch) (throw (ex-info "return" {:v index_of_i}))) (set! index_of_i (+ index_of_i 1)))) (throw (ex-info "return" {:v (- 0 1)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn decrypt [decrypt_message]
  (binding [decrypt_idx nil decrypt_num nil decrypt_symbol nil decrypt_translated nil] (dotimes [key (count main_LETTERS)] (do (set! decrypt_translated "") (dotimes [i (count decrypt_message)] (do (set! decrypt_symbol (subs decrypt_message i (min (+ i 1) (count decrypt_message)))) (set! decrypt_idx (index_of main_LETTERS decrypt_symbol)) (if (not= decrypt_idx (- 0 1)) (do (set! decrypt_num (- decrypt_idx key)) (when (< decrypt_num 0) (set! decrypt_num (+ decrypt_num (count main_LETTERS)))) (set! decrypt_translated (str decrypt_translated (subs main_LETTERS decrypt_num (min (+ decrypt_num 1) (count main_LETTERS)))))) (set! decrypt_translated (str decrypt_translated decrypt_symbol))))) (println (str (str (str "Decryption using Key #" (str key)) ": ") decrypt_translated))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (decrypt "TMDETUX PMDVU")
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
