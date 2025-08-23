(ns main (:refer-clojure :exclude [is_prime search sort_int permutations_of_number abs_int contains_int solution]))

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
  (int (Double/valueOf (str s))))

(defn _ord [s]
  (int (first s)))

(defn mochi_str [v]
  (cond (float? v) (let [s (str v)] (if (clojure.string/ends-with? s ".0") (subs s 0 (- (count s) 2)) s)) :else (str v)))

(defn _fetch [url]
  {:data [{:from "" :intensity {:actual 0 :forecast 0 :index ""} :to ""}]})

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare is_prime search sort_int permutations_of_number abs_int contains_int solution)

(declare _read_file)

(def ^:dynamic contains_int_i nil)

(def ^:dynamic is_prime_i nil)

(def ^:dynamic permutations_of_number_a nil)

(def ^:dynamic permutations_of_number_b nil)

(def ^:dynamic permutations_of_number_c nil)

(def ^:dynamic permutations_of_number_d nil)

(def ^:dynamic permutations_of_number_e nil)

(def ^:dynamic permutations_of_number_i nil)

(def ^:dynamic permutations_of_number_res nil)

(def ^:dynamic permutations_of_number_s nil)

(def ^:dynamic permutations_of_number_val nil)

(def ^:dynamic search_left nil)

(def ^:dynamic search_middle nil)

(def ^:dynamic search_right nil)

(def ^:dynamic solution_a nil)

(def ^:dynamic solution_answer_nums nil)

(def ^:dynamic solution_b nil)

(def ^:dynamic solution_c nil)

(def ^:dynamic solution_candidate nil)

(def ^:dynamic solution_candidates nil)

(def ^:dynamic solution_found nil)

(def ^:dynamic solution_i nil)

(def ^:dynamic solution_j nil)

(def ^:dynamic solution_max_val nil)

(def ^:dynamic solution_n nil)

(def ^:dynamic solution_number nil)

(def ^:dynamic solution_passed nil)

(def ^:dynamic solution_perms nil)

(def ^:dynamic solution_prime nil)

(def ^:dynamic solution_prime_list nil)

(def ^:dynamic solution_seq nil)

(def ^:dynamic solution_tmp nil)

(def ^:dynamic solution_triple nil)

(def ^:dynamic solution_val nil)

(def ^:dynamic solution_x nil)

(def ^:dynamic solution_y nil)

(def ^:dynamic solution_z nil)

(def ^:dynamic sort_int_arr nil)

(def ^:dynamic sort_int_i nil)

(def ^:dynamic sort_int_j nil)

(def ^:dynamic sort_int_tmp nil)

(defn is_prime [is_prime_number]
  (binding [is_prime_i nil] (try (do (when (and (< 1 is_prime_number) (< is_prime_number 4)) (throw (ex-info "return" {:v true}))) (when (or (or (< is_prime_number 2) (= (mod is_prime_number 2) 0)) (= (mod is_prime_number 3) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i 5) (while (<= (* is_prime_i is_prime_i) is_prime_number) (do (when (or (= (mod is_prime_number is_prime_i) 0) (= (mod is_prime_number (+ is_prime_i 2)) 0)) (throw (ex-info "return" {:v false}))) (set! is_prime_i (+ is_prime_i 6)))) (throw (ex-info "return" {:v true}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn search [search_target search_arr]
  (binding [search_left nil search_middle nil search_right nil] (try (do (set! search_left 0) (set! search_right (- (count search_arr) 1)) (while (<= search_left search_right) (do (set! search_middle (quot (+ search_left search_right) 2)) (when (= (nth search_arr search_middle) search_target) (throw (ex-info "return" {:v true}))) (if (< (nth search_arr search_middle) search_target) (set! search_left (+ search_middle 1)) (set! search_right (- search_middle 1))))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn sort_int [sort_int_xs]
  (binding [sort_int_arr nil sort_int_i nil sort_int_j nil sort_int_tmp nil] (try (do (set! sort_int_arr sort_int_xs) (set! sort_int_i 0) (while (< sort_int_i (count sort_int_arr)) (do (set! sort_int_j (+ sort_int_i 1)) (while (< sort_int_j (count sort_int_arr)) (do (when (< (nth sort_int_arr sort_int_j) (nth sort_int_arr sort_int_i)) (do (set! sort_int_tmp (nth sort_int_arr sort_int_i)) (set! sort_int_arr (assoc sort_int_arr sort_int_i (nth sort_int_arr sort_int_j))) (set! sort_int_arr (assoc sort_int_arr sort_int_j sort_int_tmp)))) (set! sort_int_j (+ sort_int_j 1)))) (set! sort_int_i (+ sort_int_i 1)))) (throw (ex-info "return" {:v sort_int_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn permutations_of_number [permutations_of_number_n]
  (binding [permutations_of_number_a nil permutations_of_number_b nil permutations_of_number_c nil permutations_of_number_d nil permutations_of_number_e nil permutations_of_number_i nil permutations_of_number_res nil permutations_of_number_s nil permutations_of_number_val nil] (try (do (set! permutations_of_number_s (mochi_str permutations_of_number_n)) (set! permutations_of_number_d []) (set! permutations_of_number_i 0) (while (< permutations_of_number_i (count permutations_of_number_s)) (do (set! permutations_of_number_d (conj permutations_of_number_d (toi (nth permutations_of_number_s permutations_of_number_i)))) (set! permutations_of_number_i (+ permutations_of_number_i 1)))) (set! permutations_of_number_res []) (set! permutations_of_number_a 0) (while (< permutations_of_number_a (count permutations_of_number_d)) (do (set! permutations_of_number_b 0) (while (< permutations_of_number_b (count permutations_of_number_d)) (do (when (not= permutations_of_number_b permutations_of_number_a) (do (set! permutations_of_number_c 0) (while (< permutations_of_number_c (count permutations_of_number_d)) (do (when (and (not= permutations_of_number_c permutations_of_number_a) (not= permutations_of_number_c permutations_of_number_b)) (do (set! permutations_of_number_e 0) (while (< permutations_of_number_e (count permutations_of_number_d)) (do (when (and (and (not= permutations_of_number_e permutations_of_number_a) (not= permutations_of_number_e permutations_of_number_b)) (not= permutations_of_number_e permutations_of_number_c)) (do (set! permutations_of_number_val (+ (+ (+ (* (nth permutations_of_number_d permutations_of_number_a) 1000) (* (nth permutations_of_number_d permutations_of_number_b) 100)) (* (nth permutations_of_number_d permutations_of_number_c) 10)) (nth permutations_of_number_d permutations_of_number_e))) (set! permutations_of_number_res (conj permutations_of_number_res permutations_of_number_val)))) (set! permutations_of_number_e (+ permutations_of_number_e 1)))))) (set! permutations_of_number_c (+ permutations_of_number_c 1)))))) (set! permutations_of_number_b (+ permutations_of_number_b 1)))) (set! permutations_of_number_a (+ permutations_of_number_a 1)))) (throw (ex-info "return" {:v permutations_of_number_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn abs_int [abs_int_x]
  (try (if (< abs_int_x 0) (- abs_int_x) abs_int_x) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn contains_int [contains_int_xs contains_int_v]
  (binding [contains_int_i nil] (try (do (set! contains_int_i 0) (while (< contains_int_i (count contains_int_xs)) (do (when (= (nth contains_int_xs contains_int_i) contains_int_v) (throw (ex-info "return" {:v true}))) (set! contains_int_i (+ contains_int_i 1)))) (throw (ex-info "return" {:v false}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn solution []
  (binding [solution_a nil solution_answer_nums nil solution_b nil solution_c nil solution_candidate nil solution_candidates nil solution_found nil solution_i nil solution_j nil solution_max_val nil solution_n nil solution_number nil solution_passed nil solution_perms nil solution_prime nil solution_prime_list nil solution_seq nil solution_tmp nil solution_triple nil solution_val nil solution_x nil solution_y nil solution_z nil] (try (do (set! solution_prime_list []) (set! solution_n 1001) (while (< solution_n 10000) (do (when (is_prime solution_n) (set! solution_prime_list (conj solution_prime_list solution_n))) (set! solution_n (+ solution_n 2)))) (set! solution_candidates []) (set! solution_i 0) (while (< solution_i (count solution_prime_list)) (do (set! solution_number (nth solution_prime_list solution_i)) (set! solution_tmp []) (set! solution_perms (permutations_of_number solution_number)) (set! solution_j 0) (while (< solution_j (count solution_perms)) (do (set! solution_prime (nth solution_perms solution_j)) (when (and (not= (mod solution_prime 2) 0) (search solution_prime solution_prime_list)) (set! solution_tmp (conj solution_tmp solution_prime))) (set! solution_j (+ solution_j 1)))) (set! solution_tmp (sort_int solution_tmp)) (when (>= (count solution_tmp) 3) (set! solution_candidates (conj solution_candidates solution_tmp))) (set! solution_i (+ solution_i 1)))) (set! solution_passed []) (set! solution_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< solution_i (count solution_candidates))) (do (set! solution_candidate (nth solution_candidates solution_i)) (set! solution_found false) (set! solution_a 0) (loop [while_flag_2 true] (when (and while_flag_2 (< solution_a (count solution_candidate))) (do (set! solution_b (+ solution_a 1)) (loop [while_flag_3 true] (when (and while_flag_3 (< solution_b (count solution_candidate))) (do (set! solution_c (+ solution_b 1)) (loop [while_flag_4 true] (when (and while_flag_4 (< solution_c (count solution_candidate))) (do (set! solution_x (nth solution_candidate solution_a)) (set! solution_y (nth solution_candidate solution_b)) (set! solution_z (nth solution_candidate solution_c)) (if (and (and (and (= (abs_int (- solution_x solution_y)) (abs_int (- solution_y solution_z))) (not= solution_x solution_y)) (not= solution_x solution_z)) (not= solution_y solution_z)) (do (set! solution_triple (sort_int [solution_x solution_y solution_z])) (set! solution_passed (conj solution_passed solution_triple)) (set! solution_found true) (recur false)) (set! solution_c (+ solution_c 1))) (cond :else (recur while_flag_4))))) (if solution_found (recur false) (set! solution_b (+ solution_b 1))) (cond :else (recur while_flag_3))))) (if solution_found (recur false) (set! solution_a (+ solution_a 1))) (cond :else (recur while_flag_2))))) (set! solution_i (+ solution_i 1)) (cond :else (recur while_flag_1))))) (set! solution_answer_nums []) (set! solution_i 0) (while (< solution_i (count solution_passed)) (do (set! solution_seq (nth solution_passed solution_i)) (set! solution_val (toi (+ (+ (mochi_str (nth solution_seq 0)) (mochi_str (nth solution_seq 1))) (mochi_str (nth solution_seq 2))))) (when (not (contains_int solution_answer_nums solution_val)) (set! solution_answer_nums (conj solution_answer_nums solution_val))) (set! solution_i (+ solution_i 1)))) (set! solution_max_val (nth solution_answer_nums 0)) (set! solution_i 1) (while (< solution_i (count solution_answer_nums)) (do (when (> (nth solution_answer_nums solution_i) solution_max_val) (set! solution_max_val (nth solution_answer_nums solution_i))) (set! solution_i (+ solution_i 1)))) (throw (ex-info "return" {:v solution_max_val}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (solution))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
