(ns wavegen.cli
  (:import [java.io FileOutputStream BufferedOutputStream])
  (:use [wavegen.core])
  (:use [wavegen.html]))

(defmacro generate-wave
  "Macro to generate the code to handle the file writing - to keep the dsl feel for end users"
  [title filename & body]
  `(with-open [bos (BufferedOutputStream. (FileOutputStream. ~filename))]
    (.write bos 
      (.getBytes 
        (gen-html
          (with-wave ~title
            ~@body))))
    (.flush bos)))


(defn -main [& args]
  (let [[wavesrc title filename & more] args]
    ; TODO: read the contents of wavesrc and then evaluate them to spit out a file
    nil)
