(ns main (:refer-clojure :exclude [get_freq sort_nodes rest count_freq build_tree concat_pairs collect_codes find_code huffman_encode]))

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

(declare get_freq sort_nodes rest count_freq build_tree concat_pairs collect_codes find_code huffman_encode)

(def ^:dynamic build_tree_arr nil)

(def ^:dynamic build_tree_left nil)

(def ^:dynamic build_tree_node nil)

(def ^:dynamic build_tree_right nil)

(def ^:dynamic concat_pairs_i nil)

(def ^:dynamic concat_pairs_res nil)

(def ^:dynamic count_freq_c nil)

(def ^:dynamic count_freq_chars nil)

(def ^:dynamic count_freq_found nil)

(def ^:dynamic count_freq_freqs nil)

(def ^:dynamic count_freq_i nil)

(def ^:dynamic count_freq_j nil)

(def ^:dynamic count_freq_k nil)

(def ^:dynamic count_freq_leaves nil)

(def ^:dynamic find_code_i nil)

(def ^:dynamic huffman_encode_c nil)

(def ^:dynamic huffman_encode_codes nil)

(def ^:dynamic huffman_encode_encoded nil)

(def ^:dynamic huffman_encode_i nil)

(def ^:dynamic huffman_encode_leaves nil)

(def ^:dynamic huffman_encode_tree nil)

(def ^:dynamic rest_i nil)

(def ^:dynamic rest_res nil)

(def ^:dynamic sort_nodes_arr nil)

(def ^:dynamic sort_nodes_i nil)

(def ^:dynamic sort_nodes_j nil)

(def ^:dynamic sort_nodes_key nil)

(defn get_freq [get_freq_n]
  (try (throw (ex-info "return" {:v (cond (and (map? get_freq_n) (= (:__tag get_freq_n) "Leaf") (contains? get_freq_n :symbol) (contains? get_freq_n :freq)) (let [get_freq__ (:symbol get_freq_n) get_freq_f (:freq get_freq_n)] get_freq_f) (and (map? get_freq_n) (= (:__tag get_freq_n) "Node") (contains? get_freq_n :freq) (contains? get_freq_n :left) (contains? get_freq_n :right)) (let [get_freq_f (:freq get_freq_n) get_freq__ (:left get_freq_n) get_freq__ (:right get_freq_n)] get_freq_f))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn sort_nodes [sort_nodes_nodes]
  (binding [sort_nodes_arr nil sort_nodes_i nil sort_nodes_j nil sort_nodes_key nil] (try (do (set! sort_nodes_arr sort_nodes_nodes) (set! sort_nodes_i 1) (while (< sort_nodes_i (count sort_nodes_arr)) (do (set! sort_nodes_key (nth sort_nodes_arr sort_nodes_i)) (set! sort_nodes_j (- sort_nodes_i 1)) (while (and (>= sort_nodes_j 0) (> (get_freq (nth sort_nodes_arr sort_nodes_j)) (get_freq sort_nodes_key))) (do (set! sort_nodes_arr (assoc sort_nodes_arr (+ sort_nodes_j 1) (nth sort_nodes_arr sort_nodes_j))) (set! sort_nodes_j (- sort_nodes_j 1)))) (set! sort_nodes_arr (assoc sort_nodes_arr (+ sort_nodes_j 1) sort_nodes_key)) (set! sort_nodes_i (+ sort_nodes_i 1)))) (throw (ex-info "return" {:v sort_nodes_arr}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn rest [rest_nodes]
  (binding [rest_i nil rest_res nil] (try (do (set! rest_res []) (set! rest_i 1) (while (< rest_i (count rest_nodes)) (do (set! rest_res (conj rest_res (nth rest_nodes rest_i))) (set! rest_i (+ rest_i 1)))) (throw (ex-info "return" {:v rest_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn count_freq [count_freq_text]
  (binding [count_freq_c nil count_freq_chars nil count_freq_found nil count_freq_freqs nil count_freq_i nil count_freq_j nil count_freq_k nil count_freq_leaves nil] (try (do (set! count_freq_chars []) (set! count_freq_freqs []) (set! count_freq_i 0) (while (< count_freq_i (count count_freq_text)) (do (set! count_freq_c (subs count_freq_text count_freq_i (min (+ count_freq_i 1) (count count_freq_text)))) (set! count_freq_j 0) (set! count_freq_found false) (loop [while_flag_1 true] (when (and while_flag_1 (< count_freq_j (count count_freq_chars))) (cond (= (nth count_freq_chars count_freq_j) count_freq_c) (do (set! count_freq_freqs (assoc count_freq_freqs count_freq_j (+ (nth count_freq_freqs count_freq_j) 1))) (set! count_freq_found true) (recur false)) :else (do (set! count_freq_j (+ count_freq_j 1)) (recur while_flag_1))))) (when (not count_freq_found) (do (set! count_freq_chars (conj count_freq_chars count_freq_c)) (set! count_freq_freqs (conj count_freq_freqs 1)))) (set! count_freq_i (+ count_freq_i 1)))) (set! count_freq_leaves []) (set! count_freq_k 0) (while (< count_freq_k (count count_freq_chars)) (do (set! count_freq_leaves (conj count_freq_leaves {:__tag "Leaf" :symbol (nth count_freq_chars count_freq_k) :freq (nth count_freq_freqs count_freq_k)})) (set! count_freq_k (+ count_freq_k 1)))) (throw (ex-info "return" {:v (sort_nodes count_freq_leaves)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn build_tree [build_tree_nodes]
  (binding [build_tree_arr nil build_tree_left nil build_tree_node nil build_tree_right nil] (try (do (set! build_tree_arr build_tree_nodes) (while (> (count build_tree_arr) 1) (do (set! build_tree_left (nth build_tree_arr 0)) (set! build_tree_arr (rest build_tree_arr)) (set! build_tree_right (nth build_tree_arr 0)) (set! build_tree_arr (rest build_tree_arr)) (set! build_tree_node {:__tag "Node" :freq (+ (get_freq build_tree_left) (get_freq build_tree_right)) :left build_tree_left :right build_tree_right}) (set! build_tree_arr (conj build_tree_arr build_tree_node)) (set! build_tree_arr (sort_nodes build_tree_arr)))) (throw (ex-info "return" {:v (nth build_tree_arr 0)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn concat_pairs [concat_pairs_a concat_pairs_b]
  (binding [concat_pairs_i nil concat_pairs_res nil] (try (do (set! concat_pairs_res concat_pairs_a) (set! concat_pairs_i 0) (while (< concat_pairs_i (count concat_pairs_b)) (do (set! concat_pairs_res (conj concat_pairs_res (nth concat_pairs_b concat_pairs_i))) (set! concat_pairs_i (+ concat_pairs_i 1)))) (throw (ex-info "return" {:v concat_pairs_res}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn collect_codes [collect_codes_tree collect_codes_prefix]
  (try (throw (ex-info "return" {:v (cond (and (map? collect_codes_tree) (= (:__tag collect_codes_tree) "Leaf") (contains? collect_codes_tree :symbol) (contains? collect_codes_tree :freq)) (let [collect_codes_s (:symbol collect_codes_tree) collect_codes__ (:freq collect_codes_tree)] [[collect_codes_s collect_codes_prefix]]) (and (map? collect_codes_tree) (= (:__tag collect_codes_tree) "Node") (contains? collect_codes_tree :freq) (contains? collect_codes_tree :left) (contains? collect_codes_tree :right)) (let [collect_codes__ (:freq collect_codes_tree) collect_codes_l (:left collect_codes_tree) collect_codes_r (:right collect_codes_tree)] (concat_pairs (collect_codes collect_codes_l (str collect_codes_prefix "0")) (collect_codes collect_codes_r (str collect_codes_prefix "1")))))})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn find_code [find_code_pairs find_code_ch]
  (binding [find_code_i nil] (try (do (set! find_code_i 0) (while (< find_code_i (count find_code_pairs)) (do (when (= (nth (nth find_code_pairs find_code_i) 0) find_code_ch) (throw (ex-info "return" {:v (nth (nth find_code_pairs find_code_i) 1)}))) (set! find_code_i (+ find_code_i 1)))) (throw (ex-info "return" {:v ""}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn huffman_encode [huffman_encode_text]
  (binding [huffman_encode_c nil huffman_encode_codes nil huffman_encode_encoded nil huffman_encode_i nil huffman_encode_leaves nil huffman_encode_tree nil] (try (do (when (= huffman_encode_text "") (throw (ex-info "return" {:v ""}))) (set! huffman_encode_leaves (count_freq huffman_encode_text)) (set! huffman_encode_tree (build_tree huffman_encode_leaves)) (set! huffman_encode_codes (collect_codes huffman_encode_tree "")) (set! huffman_encode_encoded "") (set! huffman_encode_i 0) (while (< huffman_encode_i (count huffman_encode_text)) (do (set! huffman_encode_c (subs huffman_encode_text huffman_encode_i (min (+ huffman_encode_i 1) (count huffman_encode_text)))) (set! huffman_encode_encoded (str (str huffman_encode_encoded (find_code huffman_encode_codes huffman_encode_c)) " ")) (set! huffman_encode_i (+ huffman_encode_i 1)))) (throw (ex-info "return" {:v huffman_encode_encoded}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (huffman_encode "beep boop beer!"))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
