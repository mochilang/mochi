(ns main (:refer-clojure :exclude [make_conn_mock conn_recv conn_send conn_close make_socket_mock socket_bind socket_listen socket_accept socket_shutdown socket_close make_file_mock file_read file_open send_file test_send_file_running_as_expected]))

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

(declare make_conn_mock conn_recv conn_send conn_close make_socket_mock socket_bind socket_listen socket_accept socket_shutdown socket_close make_file_mock file_read file_open send_file test_send_file_running_as_expected)

(def ^:dynamic conn_close_conn nil)

(def ^:dynamic conn_recv_conn nil)

(def ^:dynamic conn_send_conn nil)

(def ^:dynamic file_read_f nil)

(def ^:dynamic file_read_value nil)

(def ^:dynamic send_file__ nil)

(def ^:dynamic send_file_conn nil)

(def ^:dynamic send_file_data nil)

(def ^:dynamic socket_accept_sock nil)

(def ^:dynamic socket_bind_sock nil)

(def ^:dynamic socket_close_sock nil)

(def ^:dynamic socket_listen_sock nil)

(def ^:dynamic socket_shutdown_sock nil)

(def ^:dynamic test_send_file_running_as_expected_conn nil)

(def ^:dynamic test_send_file_running_as_expected_f nil)

(def ^:dynamic test_send_file_running_as_expected_sock nil)

(defn make_conn_mock []
  (try (throw (ex-info "return" {:v {:close_called 0 :recv_called 0 :send_called 0}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn conn_recv [conn_recv_conn_p conn_recv_size]
  (binding [conn_recv_conn nil] (try (do (set! conn_recv_conn conn_recv_conn_p) (set! conn_recv_conn (assoc conn_recv_conn :recv_called (+ (:recv_called conn_recv_conn) 1))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn conn_send [conn_send_conn_p conn_send_data]
  (binding [conn_send_conn nil] (do (set! conn_send_conn conn_send_conn_p) (set! conn_send_conn (assoc conn_send_conn :send_called (+ (:send_called conn_send_conn) 1))))))

(defn conn_close [conn_close_conn_p]
  (binding [conn_close_conn nil] (do (set! conn_close_conn conn_close_conn_p) (set! conn_close_conn (assoc conn_close_conn :close_called (+ (:close_called conn_close_conn) 1))))))

(defn make_socket_mock [make_socket_mock_conn]
  (try (throw (ex-info "return" {:v {:accept_called 0 :bind_called 0 :close_called 0 :conn make_socket_mock_conn :listen_called 0 :shutdown_called 0}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn socket_bind [socket_bind_sock_p]
  (binding [socket_bind_sock nil] (do (set! socket_bind_sock socket_bind_sock_p) (set! socket_bind_sock (assoc socket_bind_sock :bind_called (+ (:bind_called socket_bind_sock) 1))))))

(defn socket_listen [socket_listen_sock_p]
  (binding [socket_listen_sock nil] (do (set! socket_listen_sock socket_listen_sock_p) (set! socket_listen_sock (assoc socket_listen_sock :listen_called (+ (:listen_called socket_listen_sock) 1))))))

(defn socket_accept [socket_accept_sock_p]
  (binding [socket_accept_sock nil] (try (do (set! socket_accept_sock socket_accept_sock_p) (set! socket_accept_sock (assoc socket_accept_sock :accept_called (+ (:accept_called socket_accept_sock) 1))) (throw (ex-info "return" {:v (:conn socket_accept_sock)}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn socket_shutdown [socket_shutdown_sock_p]
  (binding [socket_shutdown_sock nil] (do (set! socket_shutdown_sock socket_shutdown_sock_p) (set! socket_shutdown_sock (assoc socket_shutdown_sock :shutdown_called (+ (:shutdown_called socket_shutdown_sock) 1))))))

(defn socket_close [socket_close_sock_p]
  (binding [socket_close_sock nil] (do (set! socket_close_sock socket_close_sock_p) (set! socket_close_sock (assoc socket_close_sock :close_called (+ (:close_called socket_close_sock) 1))))))

(defn make_file_mock [make_file_mock_values]
  (try (throw (ex-info "return" {:v {:data make_file_mock_values :read_called 0}})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn file_read [file_read_f_p file_read_size]
  (binding [file_read_f nil file_read_value nil] (try (do (set! file_read_f file_read_f_p) (when (< (:read_called file_read_f) (count (:data file_read_f))) (do (set! file_read_value (get (:data file_read_f) (:read_called file_read_f))) (set! file_read_f (assoc file_read_f :read_called (+ (:read_called file_read_f) 1))) (throw (ex-info "return" {:v file_read_value})))) (set! file_read_f (assoc file_read_f :read_called (+ (:read_called file_read_f) 1))) (throw (ex-info "return" {:v 0}))) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn file_open []
  (try (throw (ex-info "return" {:v (make_file_mock [1 0])})) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e)))))

(defn send_file [send_file_sock send_file_f]
  (binding [send_file__ nil send_file_conn nil send_file_data nil] (do (socket_bind send_file_sock) (socket_listen send_file_sock) (set! send_file_conn (socket_accept send_file_sock)) (set! send_file__ (conn_recv send_file_conn 1024)) (set! send_file_data (file_read send_file_f 1024)) (while (not= send_file_data 0) (do (conn_send send_file_conn send_file_data) (set! send_file_data (file_read send_file_f 1024)))) (conn_close send_file_conn) (socket_shutdown send_file_sock) (socket_close send_file_sock))))

(defn test_send_file_running_as_expected []
  (binding [test_send_file_running_as_expected_conn nil test_send_file_running_as_expected_f nil test_send_file_running_as_expected_sock nil] (try (do (set! test_send_file_running_as_expected_conn (make_conn_mock)) (set! test_send_file_running_as_expected_sock (make_socket_mock test_send_file_running_as_expected_conn)) (set! test_send_file_running_as_expected_f (file_open)) (send_file test_send_file_running_as_expected_sock test_send_file_running_as_expected_f) (if (and (and (and (and (and (and (and (and (= (:bind_called test_send_file_running_as_expected_sock) 1) (= (:listen_called test_send_file_running_as_expected_sock) 1)) (= (:accept_called test_send_file_running_as_expected_sock) 1)) (= (:recv_called test_send_file_running_as_expected_conn) 1)) (>= (:read_called test_send_file_running_as_expected_f) 1)) (= (:send_called test_send_file_running_as_expected_conn) 1)) (= (:close_called test_send_file_running_as_expected_conn) 1)) (= (:shutdown_called test_send_file_running_as_expected_sock) 1)) (= (:close_called test_send_file_running_as_expected_sock) 1)) "pass" "fail")) (catch clojure.lang.ExceptionInfo e (if (= (ex-message e) "return") (get (ex-data e) :v) (throw e))))))

(defn -main []
  (let [rt (Runtime/getRuntime)
    start-mem (- (.totalMemory rt) (.freeMemory rt))
    start (System/nanoTime)]
      (println (test_send_file_running_as_expected))
      (System/gc)
      (let [end (System/nanoTime)
        end-mem (- (.totalMemory rt) (.freeMemory rt))
        duration-us (quot (- end start) 1000)
        memory-bytes (Math/abs ^long (- end-mem start-mem))]
        (println (str "{\n  \"duration_us\": " duration-us ",\n  \"memory_bytes\": " memory-bytes ",\n  \"name\": \"main\"\n}"))
      )
    ))

(-main)
