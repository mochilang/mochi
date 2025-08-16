(ns main (:refer-clojure :exclude [decimal_to_negative_base_2]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(defn indexOf [s sub]
  (let [idx (clojure.string/index-of s sub)] (if (nil? idx) -1 idx)))

(defn split [s sep]
  (clojure.string/split s (re-pattern sep)))

(defn toi [s]
  (Integer/parseInt (str s)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare decimal_to_negative_base_2)

(def ^:dynamic decimal_to_negative_base_2_ans nil)

(def ^:dynamic decimal_to_negative_base_2_n nil)

(def ^:dynamic decimal_to_negative_base_2_rem nil)

(defn decimal_to_negative_base_2 [decimal_to_negative_base_2_num]
  (binding [decimal_to_negative_base_2_ans nil decimal_to_negative_base_2_n nil decimal_to_negative_base_2_rem nil] (try (do (when (= decimal_to_negative_base_2_num 0) (throw (ex-info "return" {:v 0}))) (set! decimal_to_negative_base_2_n decimal_to_negative_base_2_num) (set! decimal_to_negative_base_2_ans "") (while (not= decimal_to_negative_base_2_n 0) (do (set! decimal_to_negative_base_2_rem (mod decimal_to_negative_base_2_n (- 2))) (set! decimal_to_negative_base_2_n (/ decimal_to_negative_base_2_n (- 2))) (when (< decimal_to_negative_base_2_rem 0) (do (set! decimal_to_negative_base_2_rem (+ decimal_to_negative_base_2_rem 2)) (set! decimal_to_negative_base_2_n (+ decimal_to_negative_base_2_n 1)))) (set! decimal_to_negative_base_2_ans (str (str decimal_to_negative_base_2_rem) decimal_to_negative_base_2_ans)))) (throw (ex-info "return" {:v (toi decimal_to_negative_base_2_ans)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (decimal_to_negative_base_2 0))
      (println (decimal_to_negative_base_2 (- 19)))
      (println (decimal_to_negative_base_2 4))
      (println (decimal_to_negative_base_2 7))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
