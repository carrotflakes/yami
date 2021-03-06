(defpackage yami.server
  (:use :cl)
  (:import-from :ningle
                :<app>
                :route
                :*request*)
  (:import-from :yami
                :setup
                :run-query)
  (:export))
(in-package :yami.server)

(setup #p"change.log" #p"query.log")

(defparameter +static-path+
  (merge-pathnames "server/"
                   (asdf:system-source-directory 'yami-server)))

(defvar *app* (make-instance '<app>))

(setf (route *app* "/")
      (lambda (env)
        (setf (getf env :path-info) "/index.html")
        (funcall (lack.app.file:make-app :root +static-path+) env)))

(setf (route *app* "/api/v1/query" :method :POST)
      (lambda (params)
        (let ((query (caar params)))
          ;'(200 (:content-type "application/json") ("{}"))
          (handler-case
              `(200
                (:content-type "text/plain"
                 :access-control-allow-origin "*")
                (,(with-output-to-string (*standard-output*)
                            (run-query query))))
            (error (c)
               `(500
                   (:access-control-allow-origin "*")
                   ,(princ-to-string c)))))))

(clack:clackup
 (lack.builder:builder
  (:backtrace
   :result-on-error `(500 (:content-type "text/plain") ("Internal Server Error")))
  *app*)
 :port (parse-integer (or (uiop:getenv "PORT") "3000"))
 :server :hunchentoot
 :use-default-middlewares nil)
