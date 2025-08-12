(ns main (:refer-clojure :exclude [get_avg guess_the_number]))

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

(declare get_avg guess_the_number)

(def ^:dynamic guess_the_number_last_highest nil)

(def ^:dynamic guess_the_number_last_lowest nil)

(def ^:dynamic guess_the_number_last_numbers nil)

(def ^:dynamic guess_the_number_number nil)

(def ^:dynamic guess_the_number_resp nil)

(defn get_avg [get_avg_number_1 get_avg_number_2]
  (try (throw (ex-info "return" {:v (/ (+ get_avg_number_1 get_avg_number_2) 2)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn answer [guess_the_number_lower guess_the_number_higher guess_the_number_to_guess answer_number]
  (try (if (> answer_number guess_the_number_to_guess) (throw (ex-info "return" {:v "high"})) (if (< answer_number guess_the_number_to_guess) (throw (ex-info "return" {:v "low"})) (throw (ex-info "return" {:v "same"})))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn guess_the_number [guess_the_number_lower guess_the_number_higher guess_the_number_to_guess]
  (binding [guess_the_number_last_highest nil guess_the_number_last_lowest nil guess_the_number_last_numbers nil guess_the_number_number nil guess_the_number_resp nil] (try (do (when (> guess_the_number_lower guess_the_number_higher) (throw (Exception. "argument value for lower and higher must be(lower > higher)"))) (when (not (and (< guess_the_number_lower guess_the_number_to_guess) (< guess_the_number_to_guess guess_the_number_higher))) (throw (Exception. "guess value must be within the range of lower and higher value"))) (println "started...") (set! guess_the_number_last_lowest guess_the_number_lower) (set! guess_the_number_last_highest guess_the_number_higher) (set! guess_the_number_last_numbers []) (loop [while_flag_1 true] (when (and while_flag_1 true) (do (set! guess_the_number_number (get_avg guess_the_number_last_lowest guess_the_number_last_highest)) (set! guess_the_number_last_numbers (conj guess_the_number_last_numbers guess_the_number_number)) (set! guess_the_number_resp (answer guess_the_number_lower guess_the_number_higher guess_the_number_to_guess guess_the_number_number)) (if (= guess_the_number_resp "low") (set! guess_the_number_last_lowest guess_the_number_number) (if (= guess_the_number_resp "high") (set! guess_the_number_last_highest guess_the_number_number) (recur false))) (cond :else (recur while_flag_1))))) (println (str "guess the number : " (str (nth guess_the_number_last_numbers (- (count guess_the_number_last_numbers) 1))))) (println (str "details : " (str guess_the_number_last_numbers))) (throw (ex-info "return" {:v guess_the_number_last_numbers}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (guess_the_number 10 1000 17)
      (guess_the_number (- 10000) 10000 7)
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
