(ns main (:refer-clojure :exclude [parseIntStr fields unescape parseProgram runVM trim split main]))

(require 'clojure.set)

(defn in [x coll]
  (cond (string? coll) (clojure.string/includes? coll x) (map? coll) (contains? coll x) (sequential? coll) (some (fn [e] (= e x)) coll) :else false))

(defn padStart [s w p]
  (loop [out (str s)] (if (< (count out) w) (recur (str p out)) out)))

(def nowSeed (atom (let [s (System/getenv "MOCHI_NOW_SEED")] (if (and s (not (= s ""))) (Integer/parseInt s) 0))))

(declare parseIntStr fields unescape parseProgram runVM trim split main)

(declare fields_ch fields_cur fields_i fields_words main_prog main_programText parseIntStr_digits parseIntStr_i parseIntStr_n parseIntStr_neg parseProgram_addr parseProgram_addrMap parseProgram_arg parseProgram_code parseProgram_dataSize parseProgram_header parseProgram_i parseProgram_line parseProgram_lines parseProgram_nStrings parseProgram_op parseProgram_parts parseProgram_s parseProgram_stringPool runVM_addrMap runVM_arg runVM_code runVM_data runVM_i runVM_inst runVM_line runVM_op runVM_pc runVM_pool runVM_s runVM_stack runVM_v split_cur split_i split_parts trim_end trim_start unescape_c unescape_i unescape_out)

(defn parseIntStr [parseIntStr_str]
  (try (do (def parseIntStr_i 0) (def parseIntStr_neg false) (when (and (> (count parseIntStr_str) 0) (= (subs parseIntStr_str 0 1) "-")) (do (def parseIntStr_neg true) (def parseIntStr_i 1))) (def parseIntStr_n 0) (def parseIntStr_digits {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9}) (while (< parseIntStr_i (count parseIntStr_str)) (do (def parseIntStr_n (+ (* parseIntStr_n 10) (get parseIntStr_digits (subs parseIntStr_str parseIntStr_i (+ parseIntStr_i 1))))) (def parseIntStr_i (+ parseIntStr_i 1)))) (when parseIntStr_neg (def parseIntStr_n (- parseIntStr_n))) (throw (ex-info "return" {:v parseIntStr_n}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn fields [fields_s]
  (try (do (def fields_words []) (def fields_cur "") (def fields_i 0) (while (< fields_i (count fields_s)) (do (def fields_ch (subs fields_s fields_i (+ fields_i 1))) (if (or (or (= fields_ch " ") (= fields_ch "\t")) (= fields_ch "\n")) (when (> (count fields_cur) 0) (do (def fields_words (conj fields_words fields_cur)) (def fields_cur ""))) (def fields_cur (+ fields_cur fields_ch))) (def fields_i (+ fields_i 1)))) (when (> (count fields_cur) 0) (def fields_words (conj fields_words fields_cur))) (throw (ex-info "return" {:v fields_words}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn unescape [unescape_s]
  (try (do (def unescape_out "") (def unescape_i 0) (loop [while_flag_1 true] (when (and while_flag_1 (< unescape_i (count unescape_s))) (do (when (and (= (subs unescape_s unescape_i (+ unescape_i 1)) "\\") (< (+ unescape_i 1) (count unescape_s))) (do (def unescape_c (subs unescape_s (+ unescape_i 1) (+ unescape_i 2))) (if (= unescape_c "n") (do (def unescape_out (str unescape_out "\n")) (def unescape_i (+ unescape_i 2)) (recur true)) (when (= unescape_c "\\") (do (def unescape_out (str unescape_out "\\")) (def unescape_i (+ unescape_i 2)) (recur true)))))) (def unescape_out (str unescape_out (subs unescape_s unescape_i (+ unescape_i 1)))) (def unescape_i (+ unescape_i 1)) (cond :else (recur while_flag_1))))) (throw (ex-info "return" {:v unescape_out}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn parseProgram [parseProgram_src]
  (try (do (def parseProgram_lines (split parseProgram_src "\n")) (def parseProgram_header (fields (nth parseProgram_lines 0))) (def parseProgram_dataSize (parseIntStr (nth parseProgram_header 1))) (def parseProgram_nStrings (parseIntStr (nth parseProgram_header 3))) (def parseProgram_stringPool []) (def parseProgram_i 1) (while (<= parseProgram_i parseProgram_nStrings) (do (def parseProgram_s (nth parseProgram_lines parseProgram_i)) (when (> (count parseProgram_s) 0) (def parseProgram_stringPool (conj parseProgram_stringPool (unescape (subvec parseProgram_s 1 (- (count parseProgram_s) 1)))))) (def parseProgram_i (+ parseProgram_i 1)))) (def parseProgram_code []) (def parseProgram_addrMap {}) (loop [while_flag_2 true] (when (and while_flag_2 (< parseProgram_i (count parseProgram_lines))) (do (def parseProgram_line (trim (nth parseProgram_lines parseProgram_i))) (cond (= (count parseProgram_line) 0) (recur false) :else (do (def parseProgram_parts (fields parseProgram_line)) (def parseProgram_addr (parseIntStr (nth parseProgram_parts 0))) (def parseProgram_op (nth parseProgram_parts 1)) (def parseProgram_arg 0) (if (= parseProgram_op "push") (def parseProgram_arg (parseIntStr (nth parseProgram_parts 2))) (if (or (= parseProgram_op "fetch") (= parseProgram_op "store")) (def parseProgram_arg (parseIntStr (subvec (nth parseProgram_parts 2) 1 (- (count (nth parseProgram_parts 2)) 1)))) (when (or (= parseProgram_op "jmp") (= parseProgram_op "jz")) (def parseProgram_arg (parseIntStr (nth parseProgram_parts 3)))))) (def parseProgram_code (conj parseProgram_code {"addr" parseProgram_addr "op" parseProgram_op "arg" parseProgram_arg})) (def parseProgram_addrMap (assoc parseProgram_addrMap parseProgram_addr (- (count parseProgram_code) 1))) (def parseProgram_i (+ parseProgram_i 1)) (recur while_flag_2)))))) (throw (ex-info "return" {:v {"dataSize" parseProgram_dataSize "strings" parseProgram_stringPool "code" parseProgram_code "addrMap" parseProgram_addrMap}}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn runVM [runVM_prog]
  (do (def runVM_data []) (def runVM_i 0) (while (< runVM_i (get runVM_prog "dataSize")) (do (def runVM_data (conj runVM_data 0)) (def runVM_i (+ runVM_i 1)))) (def runVM_stack []) (def runVM_pc 0) (def runVM_code (get runVM_prog "code")) (def runVM_addrMap (get runVM_prog "addrMap")) (def runVM_pool (get runVM_prog "strings")) (def runVM_line "") (loop [while_flag_3 true] (when (and while_flag_3 (< runVM_pc (count runVM_code))) (do (def runVM_inst (nth runVM_code runVM_pc)) (def runVM_op (get runVM_inst "op")) (def runVM_arg (get runVM_inst "arg")) (cond (= runVM_op "push") (do (def runVM_stack (conj runVM_stack runVM_arg)) (def runVM_pc (+ runVM_pc 1)) (recur true)) :else (do (when (= runVM_op "store") (do (def runVM_data (assoc runVM_data runVM_arg (nth runVM_stack (- (count runVM_stack) 1)))) (def runVM_stack (slice runVM_stack 0 (- (count runVM_stack) 1))) (def runVM_pc (+ runVM_pc 1)) (recur true))) (when (= runVM_op "fetch") (do (def runVM_stack (conj runVM_stack (nth runVM_data runVM_arg))) (def runVM_pc (+ runVM_pc 1)) (recur true))) (when (= runVM_op "add") (do (def runVM_stack (assoc runVM_stack (- (count runVM_stack) 2) (+ (nth runVM_stack (- (count runVM_stack) 2)) (nth runVM_stack (- (count runVM_stack) 1))))) (def runVM_stack (slice runVM_stack 0 (- (count runVM_stack) 1))) (def runVM_pc (+ runVM_pc 1)) (recur true))) (when (= runVM_op "lt") (do (def runVM_v 0) (when (< (nth runVM_stack (- (count runVM_stack) 2)) (nth runVM_stack (- (count runVM_stack) 1))) (def runVM_v 1)) (def runVM_stack (assoc runVM_stack (- (count runVM_stack) 2) runVM_v)) (def runVM_stack (slice runVM_stack 0 (- (count runVM_stack) 1))) (def runVM_pc (+ runVM_pc 1)) (recur true))) (when (= runVM_op "jz") (do (def runVM_v (nth runVM_stack (- (count runVM_stack) 1))) (def runVM_stack (slice runVM_stack 0 (- (count runVM_stack) 1))) (if (= runVM_v 0) (def runVM_pc (nth runVM_addrMap runVM_arg)) (def runVM_pc (+ runVM_pc 1))) (recur true))) (when (= runVM_op "jmp") (do (def runVM_pc (nth runVM_addrMap runVM_arg)) (recur true))) (when (= runVM_op "prts") (do (def runVM_s (nth runVM_pool (nth runVM_stack (- (count runVM_stack) 1)))) (def runVM_stack (slice runVM_stack 0 (- (count runVM_stack) 1))) (when (not= runVM_s "\n") (def runVM_line (+ runVM_line runVM_s))) (def runVM_pc (+ runVM_pc 1)) (recur true))) (when (= runVM_op "prti") (do (def runVM_line (str runVM_line (str (nth runVM_stack (- (count runVM_stack) 1))))) (println runVM_line) (def runVM_line "") (def runVM_stack (slice runVM_stack 0 (- (count runVM_stack) 1))) (def runVM_pc (+ runVM_pc 1)) (recur true))) (when (= runVM_op "halt") (recur false)) (def runVM_pc (+ runVM_pc 1)) (recur while_flag_3))))))))

(defn trim [trim_s]
  (try (do (def trim_start 0) (while (and (< trim_start (count trim_s)) (or (= (subs trim_s trim_start (+ trim_start 1)) " ") (= (subs trim_s trim_start (+ trim_start 1)) "\t"))) (def trim_start (+ trim_start 1))) (def trim_end (count trim_s)) (while (and (> trim_end trim_start) (or (= (subs trim_s (- trim_end 1) trim_end) " ") (= (subs trim_s (- trim_end 1) trim_end) "\t"))) (def trim_end (- trim_end 1))) (throw (ex-info "return" {:v (subs trim_s trim_start trim_end)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn split [split_s split_sep]
  (try (do (def split_parts []) (def split_cur "") (def split_i 0) (while (< split_i (count split_s)) (if (and (and (> (count split_sep) 0) (<= (+ split_i (count split_sep)) (count split_s))) (= (subs split_s split_i (+ split_i (count split_sep))) split_sep)) (do (def split_parts (conj split_parts split_cur)) (def split_cur "") (def split_i (+ split_i (count split_sep)))) (do (def split_cur (str split_cur (subs split_s split_i (+ split_i 1)))) (def split_i (+ split_i 1))))) (def split_parts (conj split_parts split_cur)) (throw (ex-info "return" {:v split_parts}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn main []
  (do (def main_programText (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str (str "Datasize: 1 Strings: 2\n" "\"count is: \"\n") "\"\\n\"\n") "    0 push  1\n") "    5 store [0]\n") "   10 fetch [0]\n") "   15 push  10\n") "   20 lt\n") "   21 jz     (43) 65\n") "   26 push  0\n") "   31 prts\n") "   32 fetch [0]\n") "   37 prti\n") "   38 push  1\n") "   43 prts\n") "   44 fetch [0]\n") "   49 push  1\n") "   54 add\n") "   55 store [0]\n") "   60 jmp    (-51) 10\n") "   65 halt\n")) (def main_prog (parseProgram main_programText)) (runVM main_prog)))

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
