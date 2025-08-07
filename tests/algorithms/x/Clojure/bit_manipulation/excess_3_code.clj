(ns main (:refer-clojure :exclude [excess_3_code main]))

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

(declare excess_3_code main)

(def ^:dynamic excess_3_code_digit nil)

(def ^:dynamic excess_3_code_mapping nil)

(def ^:dynamic excess_3_code_n nil)

(def ^:dynamic excess_3_code_res nil)

(defn excess_3_code [excess_3_code_number]
  (binding [excess_3_code_digit nil excess_3_code_mapping nil excess_3_code_n nil excess_3_code_res nil] (try (do (set! excess_3_code_n excess_3_code_number) (when (< excess_3_code_n 0) (set! excess_3_code_n 0)) (set! excess_3_code_mapping ["0011" "0100" "0101" "0110" "0111" "1000" "1001" "1010" "1011" "1100"]) (set! excess_3_code_res "") (if (= excess_3_code_n 0) (set! excess_3_code_res (nth excess_3_code_mapping 0)) (while (> excess_3_code_n 0) (do (set! excess_3_code_digit (mod excess_3_code_n 10)) (set! excess_3_code_res (str (nth excess_3_code_mapping excess_3_code_digit) excess_3_code_res)) (set! excess_3_code_n (quot excess_3_code_n 10))))) (throw (ex-info "return" {:v (str "0b" excess_3_code_res)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (do (println (excess_3_code 0)) (println (excess_3_code 3)) (println (excess_3_code 2)) (println (excess_3_code 20)) (println (excess_3_code 120))))

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
