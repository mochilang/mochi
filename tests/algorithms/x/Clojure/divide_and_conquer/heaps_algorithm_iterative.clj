(ns main (:refer-clojure :exclude [copy_list heaps]))

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

(declare copy_list heaps)

(def ^:dynamic copy_list_i nil)

(def ^:dynamic copy_list_result nil)

(def ^:dynamic heaps_arr nil)

(def ^:dynamic heaps_c nil)

(def ^:dynamic heaps_i nil)

(def ^:dynamic heaps_n nil)

(def ^:dynamic heaps_res nil)

(def ^:dynamic heaps_single nil)

(def ^:dynamic heaps_temp nil)

(defn copy_list [copy_list_arr]
  (binding [copy_list_i nil copy_list_result nil] (try (do (set! copy_list_result []) (set! copy_list_i 0) (while (< copy_list_i (count copy_list_arr)) (do (set! copy_list_result (conj copy_list_result (nth copy_list_arr copy_list_i))) (set! copy_list_i (+ copy_list_i 1)))) (throw (ex-info "return" {:v copy_list_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn heaps [heaps_arr_p]
  (binding [heaps_arr nil heaps_c nil heaps_i nil heaps_n nil heaps_res nil heaps_single nil heaps_temp nil] (try (do (set! heaps_arr heaps_arr_p) (when (<= (count heaps_arr) 1) (do (set! heaps_single []) (throw (ex-info "return" {:v (conj heaps_single (copy_list heaps_arr))})))) (set! heaps_n (count heaps_arr)) (set! heaps_c []) (set! heaps_i 0) (while (< heaps_i heaps_n) (do (set! heaps_c (conj heaps_c 0)) (set! heaps_i (+ heaps_i 1)))) (set! heaps_res []) (set! heaps_res (conj heaps_res (copy_list heaps_arr))) (set! heaps_i 0) (while (< heaps_i heaps_n) (if (< (nth heaps_c heaps_i) heaps_i) (do (if (= (mod heaps_i 2) 0) (do (set! heaps_temp (nth heaps_arr 0)) (set! heaps_arr (assoc heaps_arr 0 (nth heaps_arr heaps_i))) (set! heaps_arr (assoc heaps_arr heaps_i heaps_temp))) (do (set! heaps_temp (nth heaps_arr (nth heaps_c heaps_i))) (set! heaps_arr (assoc heaps_arr (nth heaps_c heaps_i) (nth heaps_arr heaps_i))) (set! heaps_arr (assoc heaps_arr heaps_i heaps_temp)))) (set! heaps_res (conj heaps_res (copy_list heaps_arr))) (set! heaps_c (assoc heaps_c heaps_i (+ (nth heaps_c heaps_i) 1))) (set! heaps_i 0)) (do (set! heaps_c (assoc heaps_c heaps_i 0)) (set! heaps_i (+ heaps_i 1))))) (throw (ex-info "return" {:v heaps_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (heaps [1 2 3])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
