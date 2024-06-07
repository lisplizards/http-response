;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:foo.lisp.http-response
  (:use #:cl)
  (:export #:status-code
           #:status-code-informational
           #:status-code-success
           #:status-code-redirect
           #:status-code-error
           #:status-code-client-error
           #:status-code-server-error)
  (:export #:unknown-status-error
           #:http-error
           #:server-error
           #:client-error)
  (:export #:status-code-to-keyword
           #:status-code-to-text
           #:status-keyword-to-code
           #:status-keyword-to-text
           #:status-text
           #:status-text-clack-response)
  (:documentation "HTTP response related utilities."))
