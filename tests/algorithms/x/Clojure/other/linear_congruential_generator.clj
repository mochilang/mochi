(ns main (:refer-clojure :exclude [make_lcg next_number]))

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

(declare make_lcg next_number)

(def ^:dynamic main_i nil)

(def ^:dynamic next_number_lcg nil)

(defn make_lcg [make_lcg_multiplier make_lcg_increment make_lcg_modulo make_lcg_seed]
  (try (throw (ex-info "return" {:v {:increment make_lcg_increment :modulo make_lcg_modulo :multiplier make_lcg_multiplier :seed make_lcg_seed}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn next_number [next_number_lcg_p]
  (binding [next_number_lcg nil] (try (do (set! next_number_lcg next_number_lcg_p) (set! next_number_lcg (assoc next_number_lcg :seed (mod (+ (* (:multiplier next_number_lcg) (:seed next_number_lcg)) (:increment next_number_lcg)) (:modulo next_number_lcg)))) (throw (ex-info "return" {:v (:seed next_number_lcg)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_lcg (make_lcg 1664525 1013904223 4294967296 (swap! nowSeed (fn [s] (mod (+ (* s 1664525) 1013904223) 2147483647)))))

(def ^:dynamic main_i 0)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (while (< main_i 5) (do (println (str (next_number main_lcg))) (def main_i (+ main_i 1))))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
