(ns main (:refer-clojure :exclude [repeat_char vicsek print_pattern main]))

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

(declare repeat_char vicsek print_pattern main)

(def ^:dynamic main_depth nil)

(def ^:dynamic main_pattern nil)

(def ^:dynamic print_pattern_i nil)

(def ^:dynamic repeat_char_i nil)

(def ^:dynamic repeat_char_s nil)

(def ^:dynamic vicsek_blank nil)

(def ^:dynamic vicsek_i nil)

(def ^:dynamic vicsek_prev nil)

(def ^:dynamic vicsek_result nil)

(def ^:dynamic vicsek_size nil)

(defn repeat_char [repeat_char_c count_v]
  (binding [repeat_char_i nil repeat_char_s nil] (try (do (set! repeat_char_s "") (set! repeat_char_i 0) (while (< repeat_char_i count_v) (do (set! repeat_char_s (str repeat_char_s repeat_char_c)) (set! repeat_char_i (+ repeat_char_i 1)))) (throw (ex-info "return" {:v repeat_char_s}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn vicsek [vicsek_order]
  (binding [vicsek_blank nil vicsek_i nil vicsek_prev nil vicsek_result nil vicsek_size nil] (try (do (when (= vicsek_order 0) (throw (ex-info "return" {:v ["#"]}))) (set! vicsek_prev (vicsek (- vicsek_order 1))) (set! vicsek_size (count vicsek_prev)) (set! vicsek_blank (repeat_char " " vicsek_size)) (set! vicsek_result []) (set! vicsek_i 0) (while (< vicsek_i vicsek_size) (do (set! vicsek_result (conj vicsek_result (str (str vicsek_blank (nth vicsek_prev vicsek_i)) vicsek_blank))) (set! vicsek_i (+ vicsek_i 1)))) (set! vicsek_i 0) (while (< vicsek_i vicsek_size) (do (set! vicsek_result (conj vicsek_result (+ (+ (nth vicsek_prev vicsek_i) (nth vicsek_prev vicsek_i)) (nth vicsek_prev vicsek_i)))) (set! vicsek_i (+ vicsek_i 1)))) (set! vicsek_i 0) (while (< vicsek_i vicsek_size) (do (set! vicsek_result (conj vicsek_result (str (str vicsek_blank (nth vicsek_prev vicsek_i)) vicsek_blank))) (set! vicsek_i (+ vicsek_i 1)))) (throw (ex-info "return" {:v vicsek_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn print_pattern [print_pattern_pattern]
  (binding [print_pattern_i nil] (do (set! print_pattern_i 0) (while (< print_pattern_i (count print_pattern_pattern)) (do (println (nth print_pattern_pattern print_pattern_i)) (set! print_pattern_i (+ print_pattern_i 1)))))))

(defn main []
  (binding [main_depth nil main_pattern nil] (do (set! main_depth 3) (set! main_pattern (vicsek main_depth)) (print_pattern main_pattern))))

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
