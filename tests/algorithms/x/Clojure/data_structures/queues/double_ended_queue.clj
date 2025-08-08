(ns main (:refer-clojure :exclude [empty_deque push_back push_front extend_back extend_front pop_back pop_front is_empty length to_string main]))

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

(declare empty_deque push_back push_front extend_back extend_front pop_back pop_front is_empty length to_string main)

(def ^:dynamic extend_back_i nil)

(def ^:dynamic extend_back_res nil)

(def ^:dynamic extend_front_i nil)

(def ^:dynamic extend_front_j nil)

(def ^:dynamic extend_front_res nil)

(def ^:dynamic main_dq nil)

(def ^:dynamic main_r nil)

(def ^:dynamic pop_back_i nil)

(def ^:dynamic pop_back_res nil)

(def ^:dynamic pop_front_i nil)

(def ^:dynamic pop_front_res nil)

(def ^:dynamic push_front_i nil)

(def ^:dynamic push_front_res nil)

(def ^:dynamic to_string_i nil)

(def ^:dynamic to_string_s nil)

(defn empty_deque []
  (try (throw (ex-info "return" {:v {:data []}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn push_back [push_back_dq push_back_value]
  (try (throw (ex-info "return" {:v {:data (conj (:data push_back_dq) push_back_value)}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn push_front [push_front_dq push_front_value]
  (binding [push_front_i nil push_front_res nil] (try (do (set! push_front_res [push_front_value]) (set! push_front_i 0) (while (< push_front_i (count (:data push_front_dq))) (do (set! push_front_res (conj push_front_res (get (:data push_front_dq) push_front_i))) (set! push_front_i (+ push_front_i 1)))) (throw (ex-info "return" {:v {:data push_front_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn extend_back [extend_back_dq extend_back_values]
  (binding [extend_back_i nil extend_back_res nil] (try (do (set! extend_back_res (:data extend_back_dq)) (set! extend_back_i 0) (while (< extend_back_i (count extend_back_values)) (do (set! extend_back_res (conj extend_back_res (nth extend_back_values extend_back_i))) (set! extend_back_i (+ extend_back_i 1)))) (throw (ex-info "return" {:v {:data extend_back_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn extend_front [extend_front_dq extend_front_values]
  (binding [extend_front_i nil extend_front_j nil extend_front_res nil] (try (do (set! extend_front_res []) (set! extend_front_i (- (count extend_front_values) 1)) (while (>= extend_front_i 0) (do (set! extend_front_res (conj extend_front_res (nth extend_front_values extend_front_i))) (set! extend_front_i (- extend_front_i 1)))) (set! extend_front_j 0) (while (< extend_front_j (count (:data extend_front_dq))) (do (set! extend_front_res (conj extend_front_res (get (:data extend_front_dq) extend_front_j))) (set! extend_front_j (+ extend_front_j 1)))) (throw (ex-info "return" {:v {:data extend_front_res}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pop_back [pop_back_dq]
  (binding [pop_back_i nil pop_back_res nil] (try (do (when (= (count (:data pop_back_dq)) 0) (throw (Exception. "pop from empty deque"))) (set! pop_back_res []) (set! pop_back_i 0) (while (< pop_back_i (- (count (:data pop_back_dq)) 1)) (do (set! pop_back_res (conj pop_back_res (get (:data pop_back_dq) pop_back_i))) (set! pop_back_i (+ pop_back_i 1)))) (throw (ex-info "return" {:v {:deque {:data pop_back_res} :value (get (:data pop_back_dq) (- (count (:data pop_back_dq)) 1))}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn pop_front [pop_front_dq]
  (binding [pop_front_i nil pop_front_res nil] (try (do (when (= (count (:data pop_front_dq)) 0) (throw (Exception. "popleft from empty deque"))) (set! pop_front_res []) (set! pop_front_i 1) (while (< pop_front_i (count (:data pop_front_dq))) (do (set! pop_front_res (conj pop_front_res (get (:data pop_front_dq) pop_front_i))) (set! pop_front_i (+ pop_front_i 1)))) (throw (ex-info "return" {:v {:deque {:data pop_front_res} :value (get (:data pop_front_dq) 0)}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn is_empty [is_empty_dq]
  (try (throw (ex-info "return" {:v (= (count (:data is_empty_dq)) 0)})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn length [length_dq]
  (try (throw (ex-info "return" {:v (count (:data length_dq))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn to_string [to_string_dq]
  (binding [to_string_i nil to_string_s nil] (try (do (when (= (count (:data to_string_dq)) 0) (throw (ex-info "return" {:v "[]"}))) (set! to_string_s (str "[" (str (get (:data to_string_dq) 0)))) (set! to_string_i 1) (while (< to_string_i (count (:data to_string_dq))) (do (set! to_string_s (str (str to_string_s ", ") (str (get (:data to_string_dq) to_string_i)))) (set! to_string_i (+ to_string_i 1)))) (throw (ex-info "return" {:v (str to_string_s "]")}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn main []
  (binding [main_dq nil main_r nil] (do (set! main_dq (empty_deque)) (set! main_dq (push_back main_dq 2)) (set! main_dq (push_front main_dq 1)) (set! main_dq (extend_back main_dq [3 4])) (set! main_dq (extend_front main_dq [0])) (println (to_string main_dq)) (set! main_r (pop_back main_dq)) (set! main_dq (:deque main_r)) (println (:value main_r)) (set! main_r (pop_front main_dq)) (set! main_dq (:deque main_r)) (println (:value main_r)) (println (to_string main_dq)) (println (is_empty (empty_deque))))))

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
