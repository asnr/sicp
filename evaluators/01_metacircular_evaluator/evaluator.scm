(load "driver.scm")
(load "eval.scm")

(define the-global-environment (setup-environment))

(driver-loop)
