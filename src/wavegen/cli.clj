(ns wavegen.cli
  (:import [java.io FileOutputStream BufferedOutputStream PushbackReader FileReader])
  (:use [wavegen.core])
  (:use [wavegen.html]))

(defn -main [& args]
  "Main entry point - takes a source clj file, evaluates to create a wave and then renders to html and saves to the 
   file system"
  (let [[wavesrc title filename & more] args]
    (println "Generating wave file" filename "from input" wavesrc)
    (with-open [bos (BufferedOutputStream. (FileOutputStream. filename))]
        (.write bos
          (.getBytes
            (gen-html
              (load-file wavesrc))))
        (.flush bos))))
