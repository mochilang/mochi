(ns main (:refer-clojure :exclude [permute heaps main]))

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

(declare permute heaps main)

(def ^:dynamic heaps_res nil)

(def ^:dynamic main_perms nil)

(def ^:dynamic permute_arr nil)

(def ^:dynamic permute_copy nil)

(def ^:dynamic permute_i nil)

(def ^:dynamic permute_res nil)

(def ^:dynamic permute_temp nil)

(defn permute [permute_k permute_arr_p permute_res_p]
  (binding [permute_arr nil permute_copy nil permute_i nil permute_res nil permute_temp nil] (try (do (set! permute_arr permute_arr_p) (set! permute_res permute_res_p) (when (= permute_k 1) (do (set! permute_copy (subvec permute_arr 0 (count permute_arr))) (throw (ex-info "return" {:v (conj permute_res permute_copy)})))) (set! permute_res (permute (- permute_k 1) permute_arr permute_res)) (set! permute_i 0) (while (< permute_i (- permute_k 1)) (do (if (= (mod permute_k 2) 0) (do (set! permute_temp (nth permute_arr permute_i)) (set! permute_arr (assoc permute_arr permute_i (nth permute_arr (- permute_k 1)))) (set! permute_arr (assoc permute_arr (- permute_k 1) permute_temp))) (do (set! permute_temp (nth permute_arr 0)) (set! permute_arr (assoc permute_arr 0 (nth permute_arr (- permute_k 1)))) (set! permute_arr (assoc permute_arr (- permute_k 1) permute_temp)))) (set! permute_res (permute (- permute_k 1) permute_arr permute_res)) (set! permute_i (+ permute_i 1)))) (throw (ex-info "return" {:v permute_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn heaps [heaps_arr]
  (binding [heaps_res nil] (try (do (when (<= (count heaps_arr) 1) (throw (ex-info "return" {:v [(subvec heaps_arr 0 (count heaps_arr))]}))) (set! heaps_res []) (set! heaps_res (permute (count heaps_arr) heaps_arr heaps_res)) (throw (ex-info "return" {:v heaps_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_perms nil] (do (set! main_perms (heaps [1 2 3])) (println main_perms))))

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
