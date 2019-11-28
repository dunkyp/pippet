;;;; pippet.asd

(defsystem "pippet"
    :class :package-inferred-system
    :description "Describe cl-pippet here"
    :author "Duncan Paterson"
    :license  "MIT"
    :version "0.0.1"
    :depends-on ("cl-utilities"
                 "pippet/cpu")
    :in-order-to ((test-op (test-op "pippet/tests"))))

(defsystem "pippet/tests"
    :class :package-inferred-system
    :depends-on ("rove"
                 "pippet/tests/cpu")
     :perform (test-op (o c) (symbol-call :rove '#:run c)))
