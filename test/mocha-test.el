;;; -*- lexical-binding: t; -*-
(require 'ert-expectations)
(require 'cl-lib)

(expectations
  (desc "mocha-project-root-path")
  (desc "it retruns project root path")
  (lexical-let ((file (f-join (f-dirname (f-this-file)) "env" "test" "mocha-test.js")))
    (expect (f-expand "env")
      (mocha-project-root-path file)))

  (desc "it raise user-error when project root could not found.")
  (lexical-let ((file (f-this-file)))
    (expect (error)
      (mocha-project-root-path file))))

(expectations
  (desc "mocha-executable-path")
  (desc "it returns path where mocha.js placed")
  (lexical-let* ((file (f-join (f-dirname (f-this-file)) "env" "test" "mocha-test.js"))
                 (project-root (mocha-project-root-path file)))
    (expect (f-join "~/" ".nodebrew" "current" "bin" "mocha")
      (mocha-executable-path file project-root))))

(expectations
  (desc "mocha-make-minimum-command")
  (desc "it returns minimum command which does't include target filename")
  (lexical-let* ((exec-path "mocha"))
    (expect '("mocha" "--reporter spec")
      (mocha-make-minimum-command exec-path))
    (expect '("mocha" "--reporter spec" "-g" "FizzBuzz")
      (mocha-make-minimum-command exec-path '("-g" "FizzBuzz")))))

(expectations
  (desc "mocha-make-command")
  (desc "it returns appropriate command which include only 1 target filename")
  (lexical-let* ((file "foo-test.js")
                 (exec-path "mocha"))
    (expect "mocha --reporter spec foo-test.js"
      (mocha-make-command file exec-path))
    (expect "mocha --reporter spec "
      (mocha-make-command "" exec-path))
    (expect "mocha --reporter spec -g 'fizzbuzz' foo-test.js"
      (mocha-make-command file exec-path (mocha-grep-option "fizzbuzz")))))

