# http-response

> HTTP response utilities

## Usage

Example:

```common-lisp
(assert (typep 200 'foo.lisp.http-response:status-code))

(assert (typep 100 'foo.lisp.http-response:status-code-informational))

(assert (typep 200 'foo.lisp.http-response:status-code-success))

(assert (typep 300 'foo.lisp.http-response:status-code-redirect))

(assert (typep 400 'foo.lisp.http-response:status-code-client-error))

(assert (typep 500 'foo.lisp.http-response:status-code-server-error))

(assert (eq :service-unavailable
            (foo.lisp.http-response:status-code-to-keyword 503)))

(assert (string= "Bad Request"
                  (foo.lisp.http-response:status-code-to-text 400)))

(assert (= 200
           (foo.lisp.http-response:status-keyword-to-code :ok)))

(assert (string= "Not Implemented"
                  (foo.lisp.http-response:status-keyword-to-text :not-implemented)))

(assert (string= "Unauthorized"
                 (foo.lisp.http-response:status-text :unauthorized)))

(assert (string= "Unauthorized"
                 (foo.lisp.http-response:status-text 401)))

(assert (equal `(418
                  (:content-type "text/plain"
                   :content-length 12)
                  ("I'm a teapot"))
                (foo.lisp.http-response:status-text-clack-response 418)))

(assert (equal `(418
                  (:content-type "text/plain"
                   :content-length 12)
                  ("I'm a teapot"))
                (foo.lisp.http-response:status-text-clack-response :im-a-teapot)))
```

See the tests for further examples.

## Installation

Not in Quicklisp, so clone to "local-projects/".

## Development

Run tests:

```common-lisp
(asdf:test-system :foo.lisp.http-response)
```

## Author

* John Newton (<a href="mailto:jnewton@lisplizards.dev">jnewton@lisplizards.dev</a>)

## Copyright

Copyright (c) 2024 John Newton

## License

Apache-2.0
