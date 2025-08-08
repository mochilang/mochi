(ns main (:refer-clojure :exclude [empty_list push middle_element main]))

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

(declare empty_list push middle_element main)

(def ^:dynamic main_lst nil)

(def ^:dynamic middle_element_fast nil)

(def ^:dynamic middle_element_n nil)

(def ^:dynamic middle_element_slow nil)

(def ^:dynamic push_i nil)

(def ^:dynamic push_res nil)

(defn empty_list []
  (try (throw (ex-info "return" {:v {:data []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn push [push_lst push_value]
  (binding [push_i nil push_res nil] (try (do (set! push_res [push_value]) (set! push_i 0) (while (< push_i (count (:data push_lst))) (do (set! push_res (conj push_res (get (:data push_lst) push_i))) (set! push_i (+ push_i 1)))) (throw (ex-info "return" {:v {:data push_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn middle_element [middle_element_lst]
  (binding [middle_element_fast nil middle_element_n nil middle_element_slow nil] (try (do (set! middle_element_n (count (:data middle_element_lst))) (when (= middle_element_n 0) (do (println "No element found.") (throw (ex-info "return" {:v 0})))) (set! middle_element_slow 0) (set! middle_element_fast 0) (while (< (+ middle_element_fast 1) middle_element_n) (do (set! middle_element_fast (+ middle_element_fast 2)) (set! middle_element_slow (+ middle_element_slow 1)))) (throw (ex-info "return" {:v (get (:data middle_element_lst) middle_element_slow)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_lst nil] (do (set! main_lst (empty_list)) (middle_element main_lst) (set! main_lst (push main_lst 5)) (println 5) (set! main_lst (push main_lst 6)) (println 6) (set! main_lst (push main_lst 8)) (println 8) (set! main_lst (push main_lst 8)) (println 8) (set! main_lst (push main_lst 10)) (println 10) (set! main_lst (push main_lst 12)) (println 12) (set! main_lst (push main_lst 17)) (println 17) (set! main_lst (push main_lst 7)) (println 7) (set! main_lst (push main_lst 3)) (println 3) (set! main_lst (push main_lst 20)) (println 20) (set! main_lst (push main_lst (- 20))) (println (- 20)) (println (middle_element main_lst)))))

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
