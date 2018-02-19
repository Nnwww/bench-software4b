(ns bench-software4b.core
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.pprint :as pp]
            [clojure.string :as cstr]
            [clojure.spec.alpha :as s]
            [jansi-clj.core])
  (:import [java.nio.file Files]
           [java.nio.file Paths]
           [java.io File]
           [s4.t000002 TestCase2]
           [java.lang ReflectiveOperationException])
  (:gen-class))

(defmacro with-timeout [millis & body]
  `(let [future# (future ~@body)]
     (try
       (.get future# ~millis java.util.concurrent.TimeUnit/MILLISECONDS)
       (catch java.util.concurrent.TimeoutException x#
         (do
           (future-cancel future#)
           nil)))))

(defn slurp-bytes
  "Slurp the bytes from a slurpable thing"
  [x]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))

(assert (-> (shell/sh "type" "git") :exit (= 0))  "Pre check error: not found git")
(assert (-> (shell/sh "type" "javac") :exit (= 0)) "Pre check error: not found javac")
(assert (-> (shell/sh "type" "java") :exit (= 0)) "Pre check error: not found java")

(def user-cd-path (str (System/getProperty "user.dir") "/"))

(defn get-canonical-path [^String path]
  {:pre [nil? path]}
  (cond-> path (.startsWith path "~") (.replaceFirst "~" (System/getProperty "user.home"))))

;; classpathは末尾バックスラッシュ必要
;; Fileオブジェクトは削ってしまうので注意
(def stab-compilable-user-path (io/file (get-canonical-path "~/WorkSpace/software_correct/tut2017informationQuantity/s4/t000002")))
(def stab-compilable-classpath (get-canonical-path "~/WorkSpace/software_correct/tut2017informationQuantity/"))

(def compile-names ["Frequencer" "InformationEstimator"])
(def test-names ["Minimal" "FullSpec"])
(def test-functions [[#(TestCase2/frequencerBasicTest %) #(TestCase2/frequencerSpecTest %)]
                     [#(TestCase2/informationEstimatorBasicTest %) #(TestCase2/informationEstimatorSpecTest %)]])
(def test-suite (reduce #(->> (map vector test-names (nth test-functions %2))
                              (assoc %1 (nth compile-names %2)))
                        {} (range (count compile-names))))

(def bench-names
  [["space_100b.txt" "target_10b.txt"]
   ["space_1k.txt" "target_100b.txt"]
   ;;   ["space_1m.txt" "target_10k.txt"]
   ])

(def bench-suite (reduce #(assoc %1 %2 (slurp-bytes (io/resource %2)))
                         {} (flatten bench-names)))

(defrecord Env [names classpath build test bench])
(def stab-env (Env. compile-names stab-compilable-classpath true true true))

(defn ret-assert [x message ret]
  (do (assert x message) ret))

(s/def ::shell-opt? #{:in :in-enc :out-enc :env :dir})
(s/def ::args-type-cmd
  (s/cat :cmd-arg (s/+ string?)
         :opt (s/* (s/cat :opt-key ::shell-opt?
                          :opt-val string?))))
(s/fdef type-cmd :args ::args-type-cmd)

(defn type-cmd [& args]
  (let [{[cmd & evaluted-str] :cmd-arg opts :opt} (s/conform ::args-type-cmd args)]
    (do (assert (-> (shell/sh "type" cmd) :exit (= 0))  "Error: not found commnad")
        (let [result-cmd (apply shell/sh cmd evaluted-str)]
          (ret-assert (-> result-cmd :exit (= 0)) (str "Error: " (:err result-cmd)) (:out result-cmd))))))

(defn git-root-path [exec-dir]
  (-> (if (nil? exec-dir)
        (shell/sh "git" "rev-parse" "--show-toplevel")
        (shell/sh "git" "rev-parse" "--show-toplevel" :dir exec-dir))
      (update :out cstr/trim-newline)))

(defn cmd-javac [user-path-obj compile-name classpath]
  (let [user-path (.getAbsolutePath user-path-obj)
        compile-target (str user-path "/" compile-name ".java")]
    (shell/sh "javac" compile-target "-classpath" classpath :dir user-path)))

(defn cmd-java [compile-path]
  (shell/sh "java" compile-path "-classpath" "."))

(defn extract-students-folder [files]
  (filter #(and (.isDirectory %)
                (->> % .getName (re-find #"b([1-9])+"))) files))

(defn diff-relative-paths [target-file decrease-file]
  (-> target-file
      .toURI
      (.relativize (.toURI decrease-file))
      .toString
      io/file))

(defn show-build-result [compile-name sh-result]
  (if (= 0 (:exit sh-result))
    (jansi-clj.core/green compile-name)
    (jansi-clj.core/red compile-name)))

(defn pairing-name-and-result [env user-path-obj compile-name]
  [compile-name
   (cmd-javac user-path-obj compile-name (:classpath env))])

(defn build-user [env user-path-obj]
  (let [compile-results (map #(pairing-name-and-result env user-path-obj %) (:names env))
        compile-print-strings (map #(apply show-build-result %) compile-results)]
    compile-print-strings))

(defn split-class-name-from-fqcn [fqcn] (last (cstr/split fqcn #"\.")))

(defn reflect-class-or-err-str [fqcn]
  (try (resolve (symbol fqcn))
       (catch ClassNotFoundException e
         (jansi-clj.core/yellow
          (str (split-class-name-from-fqcn fqcn) "(Class not found)")))))

(defn reflect-instance-or-err-str [class]
  (try (.newInstance class)
       (catch java.lang.ReflectiveOperationException e
         (jansi-clj.core/yellow
          (str (split-class-name-from-fqcn (str class)) "(Can't instantiate)")))))

(defn instantiate-for-fqcn [fqcn]
  (let [class (reflect-class-or-err-str fqcn)]
    (if (string? class) class
        (reflect-instance-or-err-str class))))

(defn test-a-set [class test-kind test-set]
  (try (do (test-set class)
           (jansi-clj.core/green test-kind))
       (catch AssertionError e ;; テストケースに失敗がある場合
         (jansi-clj.core/red test-kind))
       (catch NullPointerException e ;; インターフェースを満たせてない場合
         (jansi-clj.core/red test-kind))))

(defn test-class [name class]
  (if (string? class) (cons class nil)
      (print-str (map #(apply (partial test-a-set class) %) (test-suite name)))))

(defn pairing-name-and-class [name fqcn]
  [name (reflect-class-or-err-str fqcn)])

(defn make-package-prefix [user-path-obj] (str "s4." (.getName user-path-obj) "."))

(defn test-user [env user-path-obj]
  (let [package-name-prefix (make-package-prefix user-path-obj)
        fqcns (map #(str package-name-prefix %) (:names env))]
    (->> (:names env)
         (#(map vector % fqcns))
         (map #(apply pairing-name-and-class %))
         (map #(vector (first %) (apply test-class %))))))

;;手前の関数をテストしてここから再開、TestSoftware的なjavaファイルを作り投げ込むコードをかく。
;; このためにはテストスイート公開用のjavaファイルの位置とか、来週までの隠し場所とかを奥本君と相談する必要がある
;; テストスイートは失敗時に例外を投げて貰うと良さそう

(defn unsnoc [str]
  (if (empty? str) str
      (.substring str 0 (dec (.length str)))))

(defn last-char [str]
  (if (empty? str) str
      (.charAt str (dec (.length str)))))

(defn init-work-dir [path]
  (if (not (nil? path))
    (if (= \/ (last-char path)) path
        (str path "/"))
    (let [result-git-root (git-root-path user-cd-path)]
      (if (zero? (:exit result-git-root))
        (str (:out result-git-root) "/")
        (throw (Exception. (str "Illegal Work Directory Error: This program works in git directories. "
                                "Please cd or specify a git project by args.")))))))

(defn bench-time-sandbox [fqcn space-file target-file]
  (with-timeout 600000
    (let [start (System/currentTimeMillis)
          bench-obj (instantiate-for-fqcn fqcn)]
      (do (.setSpace bench-obj (bench-suite space-file))
          (.setTarget bench-obj (bench-suite target-file))
          (if (< 0 (.estimation bench-obj))
            (- (System/currentTimeMillis) start))))))

(defn bench-set [fqcn space-file target-file]
  (let [result (try (bench-time-sandbox fqcn space-file target-file)
                    (catch Exception e nil))]
    (if (nil? result)
      (map jansi-clj.core/red (vector space-file target-file "Time out or execution failure ..."))
      (map jansi-clj.core/green (vector space-file target-file (str result "(msec)"))))))

(defn bench-user [env user-path-obj]
  (let [package-name-prefix (make-package-prefix user-path-obj)
        fqcn (str package-name-prefix ((:names env) 1))
        pre-bench-obj (instantiate-for-fqcn fqcn)]
    (if (string? pre-bench-obj) pre-bench-obj
        (map #(bench-set fqcn (% 0) (% 1)) bench-names))))

(defn evaluate-user [env user-path-obj]
  (do (println (.getName user-path-obj))
      (when (:build env) (println (str "build: " (apply print-str (build-user env user-path-obj)))))
      (when (:test  env) (println (str "test : " (apply print-str (test-user env user-path-obj)))))
      (when (:bench env) (println (str "bench: \n"
                                       (reduce #(str %1 "\n" %2)
                                               (map #(apply print-str %) (bench-user env user-path-obj))))))
      (print "\n")))

(def cl (ClassLoader/getSystemClassLoader))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println user-cd-path)
  (println (reduce #(str %1 "\n" %2) (.getURLs cl)))
  (println (jansi-clj.core/green  "The green string indicates success."))
  (println (jansi-clj.core/red    "The red string indicates failure."))
  (println (jansi-clj.core/yellow "The yellow string indicates the absence of class files. Please consult with TA."))
  (print "\n")
  (let [classpath (init-work-dir (first args))
        flag-build (= -1 (.indexOf args "build"))
        flag-test (= -1 (.indexOf args "test"))
        flag-bench (= -1 (.indexOf args "bench"))
        workflow-env (Env. compile-names classpath flag-build flag-test flag-bench)]
    (doall
     (->> (str classpath "s4")
          io/file
          file-seq
          extract-students-folder
          (map #(evaluate-user workflow-env %))))))
