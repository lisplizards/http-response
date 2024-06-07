;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.http-response"
  :version "1.0.0"
  :author "John Newton"
  :license "Apache-2.0"
  :homepage "https://github.com/lisplizards/http-response"
  :bug-tracker "https://github.com/lisplizards/http-response/issues"
  :source-control (:git "https://github.com/lisplizards/http-response.git")
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "package"))))
  :description "HTTP response utilities"
  :in-order-to ((test-op (test-op "foo.lisp.http-response/tests"))))

(defsystem "foo.lisp.http-response/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("foo.lisp.http-response"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for foo.lisp.http-response"
  :perform (test-op (op c) (symbol-call :rove :run c)))
