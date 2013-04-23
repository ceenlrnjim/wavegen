(ns wavegen.cli
  (:import [java.io FileOutputStream BufferedOutputStream PushbackReader FileReader])
  (:require [wavegen.core :as core])
  (:require [wavegen.excel :as xl])
  (:require [wavegen.html :as html]))

(defn -main [& args]
  "Main entry point - takes a source clj file, evaluates to create a wave and then renders to html and saves to the 
   file system"
  (let [[wavesrc title filename fmt & more] args]
    (println "Generating wave file" filename "from input" wavesrc "with format" fmt)
    (with-open [bos (BufferedOutputStream. (FileOutputStream. filename))]
      (if 
        (= fmt "xls") (.write (xl/gen-xlsx (load-file wavesrc)) bos)
        (.write bos
          (.getBytes
            (html/gen-html
              (load-file wavesrc)))))
        (.flush bos))))
