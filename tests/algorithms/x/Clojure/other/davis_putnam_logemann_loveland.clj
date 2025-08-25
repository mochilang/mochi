(ns main (:refer-clojure :exclude [new_clause assign_clause evaluate_clause new_formula remove_symbol dpll_algorithm str_clause str_formula]))

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

(declare new_clause assign_clause evaluate_clause new_formula remove_symbol dpll_algorithm str_clause str_formula)

(declare _read_file)

(def ^:dynamic assign_clause_c nil)

(def ^:dynamic assign_clause_i nil)

(def ^:dynamic assign_clause_lit nil)

(def ^:dynamic assign_clause_lits nil)

(def ^:dynamic assign_clause_symbol nil)

(def ^:dynamic assign_clause_value nil)

(def ^:dynamic dpll_algorithm_all_true nil)

(def ^:dynamic dpll_algorithm_clauses nil)

(def ^:dynamic dpll_algorithm_ev nil)

(def ^:dynamic dpll_algorithm_i nil)

(def ^:dynamic dpll_algorithm_p nil)

(def ^:dynamic dpll_algorithm_res1 nil)

(def ^:dynamic dpll_algorithm_tmp1 nil)

(def ^:dynamic dpll_algorithm_tmp2 nil)

(def ^:dynamic evaluate_clause_any_true nil)

(def ^:dynamic evaluate_clause_c nil)

(def ^:dynamic evaluate_clause_i nil)

(def ^:dynamic evaluate_clause_lit nil)

(def ^:dynamic evaluate_clause_sym nil)

(def ^:dynamic evaluate_clause_value nil)

(def ^:dynamic first_v nil)

(def ^:dynamic new_clause_i nil)

(def ^:dynamic new_clause_lit nil)

(def ^:dynamic new_clause_m nil)

(def ^:dynamic new_clause_names nil)

(def ^:dynamic remove_symbol_i nil)

(def ^:dynamic remove_symbol_res nil)

(def ^:dynamic rest_v nil)

(def ^:dynamic str_clause_i nil)

(def ^:dynamic str_clause_line nil)

(def ^:dynamic str_clause_lit nil)

(def ^:dynamic str_formula_i nil)

(def ^:dynamic str_formula_line nil)

(defn new_clause [new_clause_lits]
  (binding [new_clause_i nil new_clause_lit nil new_clause_m nil new_clause_names nil] (try (do (set! new_clause_m {}) (set! new_clause_names []) (set! new_clause_i 0) (while (< new_clause_i (count new_clause_lits)) (do (set! new_clause_lit (nth new_clause_lits new_clause_i)) (set! new_clause_m (assoc new_clause_m new_clause_lit -1)) (set! new_clause_names (conj new_clause_names new_clause_lit)) (set! new_clause_i (+' new_clause_i 1)))) (throw (ex-info "return" {:v {:literals new_clause_m :names new_clause_names}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn assign_clause [assign_clause_c_p assign_clause_model]
  (binding [assign_clause_c assign_clause_c_p assign_clause_i nil assign_clause_lit nil assign_clause_lits nil assign_clause_symbol nil assign_clause_value nil] (try (do (set! assign_clause_lits (:literals assign_clause_c)) (set! assign_clause_i 0) (while (< assign_clause_i (count (:names assign_clause_c))) (do (set! assign_clause_lit (get (:names assign_clause_c) assign_clause_i)) (set! assign_clause_symbol (subs assign_clause_lit 0 (min 2 (count assign_clause_lit)))) (when (in assign_clause_symbol assign_clause_model) (do (set! assign_clause_value (get assign_clause_model assign_clause_symbol)) (when (and (= (subs assign_clause_lit (- (count assign_clause_lit) 1) (min (count assign_clause_lit) (count assign_clause_lit))) "'") (not= assign_clause_value -1)) (set! assign_clause_value (- 1 assign_clause_value))) (set! assign_clause_lits (assoc assign_clause_lits assign_clause_lit assign_clause_value)))) (set! assign_clause_i (+' assign_clause_i 1)))) (set! assign_clause_c (assoc assign_clause_c :literals assign_clause_lits)) (throw (ex-info "return" {:v assign_clause_c}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var assign_clause_c) (constantly assign_clause_c))))))

(defn evaluate_clause [evaluate_clause_c_p evaluate_clause_model]
  (binding [evaluate_clause_c evaluate_clause_c_p evaluate_clause_any_true nil evaluate_clause_i nil evaluate_clause_lit nil evaluate_clause_sym nil evaluate_clause_value nil] (try (do (set! evaluate_clause_i 0) (while (< evaluate_clause_i (count (:names evaluate_clause_c))) (do (set! evaluate_clause_lit (get (:names evaluate_clause_c) evaluate_clause_i)) (set! evaluate_clause_sym (if (= (subs evaluate_clause_lit (- (count evaluate_clause_lit) 1) (min (count evaluate_clause_lit) (count evaluate_clause_lit))) "'") (subs evaluate_clause_lit 0 (min 2 (count evaluate_clause_lit))) (str evaluate_clause_lit "'"))) (when (in evaluate_clause_sym (:literals evaluate_clause_c)) (throw (ex-info "return" {:v {:clause evaluate_clause_c :value 1}}))) (set! evaluate_clause_i (+' evaluate_clause_i 1)))) (set! evaluate_clause_c (let [__res (assign_clause evaluate_clause_c evaluate_clause_model)] (do (set! evaluate_clause_c assign_clause_c) __res))) (set! evaluate_clause_i 0) (while (< evaluate_clause_i (count (:names evaluate_clause_c))) (do (set! evaluate_clause_lit (get (:names evaluate_clause_c) evaluate_clause_i)) (set! evaluate_clause_value (get (:literals evaluate_clause_c) evaluate_clause_lit)) (when (= evaluate_clause_value 1) (throw (ex-info "return" {:v {:clause evaluate_clause_c :value 1}}))) (when (= evaluate_clause_value -1) (throw (ex-info "return" {:v {:clause evaluate_clause_c :value -1}}))) (set! evaluate_clause_i (+' evaluate_clause_i 1)))) (set! evaluate_clause_any_true 0) (set! evaluate_clause_i 0) (while (< evaluate_clause_i (count (:names evaluate_clause_c))) (do (set! evaluate_clause_lit (get (:names evaluate_clause_c) evaluate_clause_i)) (when (= (get (:literals evaluate_clause_c) evaluate_clause_lit) 1) (set! evaluate_clause_any_true 1)) (set! evaluate_clause_i (+' evaluate_clause_i 1)))) (throw (ex-info "return" {:v {:clause evaluate_clause_c :value evaluate_clause_any_true}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var evaluate_clause_c) (constantly evaluate_clause_c))))))

(defn new_formula [new_formula_cs]
  (try (throw (ex-info "return" {:v {:clauses new_formula_cs}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn remove_symbol [remove_symbol_symbols remove_symbol_s]
  (binding [remove_symbol_i nil remove_symbol_res nil] (try (do (set! remove_symbol_res []) (set! remove_symbol_i 0) (while (< remove_symbol_i (count remove_symbol_symbols)) (do (when (not= (nth remove_symbol_symbols remove_symbol_i) remove_symbol_s) (set! remove_symbol_res (conj remove_symbol_res (nth remove_symbol_symbols remove_symbol_i)))) (set! remove_symbol_i (+' remove_symbol_i 1)))) (throw (ex-info "return" {:v remove_symbol_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn dpll_algorithm [dpll_algorithm_clauses_p dpll_algorithm_symbols dpll_algorithm_model]
  (binding [dpll_algorithm_clauses dpll_algorithm_clauses_p dpll_algorithm_all_true nil dpll_algorithm_ev nil dpll_algorithm_i nil dpll_algorithm_p nil dpll_algorithm_res1 nil dpll_algorithm_tmp1 nil dpll_algorithm_tmp2 nil rest_v nil] (try (do (set! dpll_algorithm_all_true true) (set! dpll_algorithm_i 0) (while (< dpll_algorithm_i (count dpll_algorithm_clauses)) (do (set! dpll_algorithm_ev (let [__res (evaluate_clause (nth dpll_algorithm_clauses dpll_algorithm_i) dpll_algorithm_model)] (do __res))) (set! dpll_algorithm_clauses (assoc dpll_algorithm_clauses dpll_algorithm_i (:clause dpll_algorithm_ev))) (if (= (:value dpll_algorithm_ev) 0) (throw (ex-info "return" {:v {:model {} :sat false}})) (when (= (:value dpll_algorithm_ev) -1) (set! dpll_algorithm_all_true false))) (set! dpll_algorithm_i (+' dpll_algorithm_i 1)))) (when dpll_algorithm_all_true (throw (ex-info "return" {:v {:model dpll_algorithm_model :sat true}}))) (set! dpll_algorithm_p (nth dpll_algorithm_symbols 0)) (set! rest_v (remove_symbol dpll_algorithm_symbols dpll_algorithm_p)) (set! dpll_algorithm_tmp1 dpll_algorithm_model) (set! dpll_algorithm_tmp2 dpll_algorithm_model) (set! dpll_algorithm_tmp1 (assoc dpll_algorithm_tmp1 dpll_algorithm_p 1)) (set! dpll_algorithm_tmp2 (assoc dpll_algorithm_tmp2 dpll_algorithm_p 0)) (set! dpll_algorithm_res1 (let [__res (dpll_algorithm dpll_algorithm_clauses rest_v dpll_algorithm_tmp1)] (do (set! dpll_algorithm_clauses dpll_algorithm_clauses) __res))) (if (:sat dpll_algorithm_res1) dpll_algorithm_res1 (let [__res (dpll_algorithm dpll_algorithm_clauses rest_v dpll_algorithm_tmp2)] (do (set! dpll_algorithm_clauses dpll_algorithm_clauses) __res)))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))) (finally (alter-var-root (var dpll_algorithm_clauses) (constantly dpll_algorithm_clauses))))))

(defn str_clause [str_clause_c]
  (binding [first_v nil str_clause_i nil str_clause_line nil str_clause_lit nil] (try (do (set! str_clause_line "{") (set! first_v true) (set! str_clause_i 0) (while (< str_clause_i (count (:names str_clause_c))) (do (set! str_clause_lit (get (:names str_clause_c) str_clause_i)) (if first_v (set! first_v false) (set! str_clause_line (str str_clause_line " , "))) (set! str_clause_line (str str_clause_line str_clause_lit)) (set! str_clause_i (+' str_clause_i 1)))) (set! str_clause_line (str str_clause_line "}")) (throw (ex-info "return" {:v str_clause_line}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn str_formula [str_formula_f]
  (binding [str_formula_i nil str_formula_line nil] (try (do (set! str_formula_line "{") (set! str_formula_i 0) (while (< str_formula_i (count (:clauses str_formula_f))) (do (set! str_formula_line (str str_formula_line (str_clause (get (:clauses str_formula_f) str_formula_i)))) (when (< str_formula_i (- (count (:clauses str_formula_f)) 1)) (set! str_formula_line (str str_formula_line " , "))) (set! str_formula_i (+' str_formula_i 1)))) (set! str_formula_line (str str_formula_line "}")) (throw (ex-info "return" {:v str_formula_line}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(def ^:dynamic main_clause1 nil)

(def ^:dynamic main_clause2 nil)

(def ^:dynamic main_formula nil)

(def ^:dynamic main_formula_str nil)

(def ^:dynamic main_clauses nil)

(def ^:dynamic main_symbols nil)

(def ^:dynamic main_model nil)

(def ^:dynamic main_result nil)

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (alter-var-root (var main_clause1) (constantly (new_clause ["A4" "A3" "A5'" "A1" "A3'"])))
      (alter-var-root (var main_clause2) (constantly (new_clause ["A4"])))
      (alter-var-root (var main_formula) (constantly (new_formula [main_clause1 main_clause2])))
      (alter-var-root (var main_formula_str) (constantly (str_formula main_formula)))
      (alter-var-root (var main_clauses) (constantly [main_clause1 main_clause2]))
      (alter-var-root (var main_symbols) (constantly ["A4" "A3" "A5" "A1"]))
      (alter-var-root (var main_model) (constantly {}))
      (alter-var-root (var main_result) (constantly (let [__res (dpll_algorithm main_clauses main_symbols main_model)] (do (alter-var-root (var main_clauses) (constantly dpll_algorithm_clauses)) __res))))
      (if (:sat main_result) (println (str (str "The formula " main_formula_str) " is satisfiable.")) (println (str (str "The formula " main_formula_str) " is not satisfiable.")))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
