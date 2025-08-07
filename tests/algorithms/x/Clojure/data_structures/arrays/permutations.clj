(ns main (:refer-clojure :exclude [tail rotate_left permute_recursive swap permute_backtrack_helper permute_backtrack]))

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

(declare tail rotate_left permute_recursive swap permute_backtrack_helper permute_backtrack)

(def ^:dynamic count_v nil)

(def ^:dynamic permute_backtrack_helper_i nil)

(def ^:dynamic permute_backtrack_helper_res nil)

(def ^:dynamic permute_backtrack_helper_swapped nil)

(def ^:dynamic permute_backtrack_output nil)

(def ^:dynamic permute_recursive_base nil)

(def ^:dynamic permute_recursive_current nil)

(def ^:dynamic permute_recursive_j nil)

(def ^:dynamic permute_recursive_n nil)

(def ^:dynamic permute_recursive_perm nil)

(def ^:dynamic permute_recursive_perms nil)

(def ^:dynamic permute_recursive_result nil)

(def ^:dynamic rest_v nil)

(def ^:dynamic rotate_left_i nil)

(def ^:dynamic rotate_left_res nil)

(def ^:dynamic swap_k nil)

(def ^:dynamic swap_res nil)

(def ^:dynamic tail_i nil)

(def ^:dynamic tail_res nil)

(defn tail [tail_xs]
  (binding [tail_i nil tail_res nil] (try (do (set! tail_res []) (set! tail_i 1) (while (< tail_i (count tail_xs)) (do (set! tail_res (conj tail_res (nth tail_xs tail_i))) (set! tail_i (+ tail_i 1)))) (throw (ex-info "return" {:v tail_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rotate_left [rotate_left_xs]
  (binding [rotate_left_i nil rotate_left_res nil] (try (do (when (= (count rotate_left_xs) 0) (throw (ex-info "return" {:v rotate_left_xs}))) (set! rotate_left_res []) (set! rotate_left_i 1) (while (< rotate_left_i (count rotate_left_xs)) (do (set! rotate_left_res (conj rotate_left_res (nth rotate_left_xs rotate_left_i))) (set! rotate_left_i (+ rotate_left_i 1)))) (set! rotate_left_res (conj rotate_left_res (nth rotate_left_xs 0))) (throw (ex-info "return" {:v rotate_left_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn permute_recursive [permute_recursive_nums]
  (binding [count_v nil permute_recursive_base nil permute_recursive_current nil permute_recursive_j nil permute_recursive_n nil permute_recursive_perm nil permute_recursive_perms nil permute_recursive_result nil rest_v nil] (try (do (when (= (count permute_recursive_nums) 0) (do (set! permute_recursive_base []) (throw (ex-info "return" {:v (conj permute_recursive_base [])})))) (set! permute_recursive_result []) (set! permute_recursive_current permute_recursive_nums) (set! count_v 0) (while (< count_v (count permute_recursive_nums)) (do (set! permute_recursive_n (nth permute_recursive_current 0)) (set! rest_v (tail permute_recursive_current)) (set! permute_recursive_perms (permute_recursive rest_v)) (set! permute_recursive_j 0) (while (< permute_recursive_j (count permute_recursive_perms)) (do (set! permute_recursive_perm (conj (nth permute_recursive_perms permute_recursive_j) permute_recursive_n)) (set! permute_recursive_result (conj permute_recursive_result permute_recursive_perm)) (set! permute_recursive_j (+ permute_recursive_j 1)))) (set! permute_recursive_current (rotate_left permute_recursive_current)) (set! count_v (+ count_v 1)))) (throw (ex-info "return" {:v permute_recursive_result}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn swap [swap_xs swap_i swap_j]
  (binding [swap_k nil swap_res nil] (try (do (set! swap_res []) (set! swap_k 0) (while (< swap_k (count swap_xs)) (do (if (= swap_k swap_i) (set! swap_res (conj swap_res (nth swap_xs swap_j))) (if (= swap_k swap_j) (set! swap_res (conj swap_res (nth swap_xs swap_i))) (set! swap_res (conj swap_res (nth swap_xs swap_k))))) (set! swap_k (+ swap_k 1)))) (throw (ex-info "return" {:v swap_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn permute_backtrack_helper [permute_backtrack_helper_nums permute_backtrack_helper_start permute_backtrack_helper_output]
  (binding [permute_backtrack_helper_i nil permute_backtrack_helper_res nil permute_backtrack_helper_swapped nil] (try (do (when (= permute_backtrack_helper_start (- (count permute_backtrack_helper_nums) 1)) (throw (ex-info "return" {:v (conj permute_backtrack_helper_output permute_backtrack_helper_nums)}))) (set! permute_backtrack_helper_i permute_backtrack_helper_start) (set! permute_backtrack_helper_res permute_backtrack_helper_output) (while (< permute_backtrack_helper_i (count permute_backtrack_helper_nums)) (do (set! permute_backtrack_helper_swapped (swap permute_backtrack_helper_nums permute_backtrack_helper_start permute_backtrack_helper_i)) (set! permute_backtrack_helper_res (permute_backtrack_helper permute_backtrack_helper_swapped (+ permute_backtrack_helper_start 1) permute_backtrack_helper_res)) (set! permute_backtrack_helper_i (+ permute_backtrack_helper_i 1)))) (throw (ex-info "return" {:v permute_backtrack_helper_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn permute_backtrack [permute_backtrack_nums]
  (binding [permute_backtrack_output nil] (try (do (set! permute_backtrack_output []) (throw (ex-info "return" {:v (permute_backtrack_helper permute_backtrack_nums 0 permute_backtrack_output)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (str (permute_recursive [1 2 3])))
      (println (str (permute_backtrack [1 2 3])))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
