;;; -*- lexical-binding: t; -*-
(require 'ert-expectations)
(require 'cl-lib)
(require 'mocha)

(expectations
  (desc "mocha-project-root-path")
  (desc "it retruns project root path.")
  (lexical-let ((file "~/Programming/tmp/mocha-test/test/mocha-test.js"))
    (expect (f-expand "~/Programming/tmp/mocha-test")
      (mocha-project-root-path file)))

  (desc "it raise user-error when project root could not found.")
  (lexical-let ((file "~/Programming/"))
    (expect (error)
      (mocha-project-root-path file))))

(expectations
  (desc "mocha-executable-path")
  (desc "it returns path where mocha.js placed.")
  (lexical-let* ((file "~/Programming/tmp/mocha-test/test/mocha-test.js")
                 (project-root (mocha-project-root-path file)))
    (expect (f-join "~/" ".nodebrew" "current" "bin" "mocha")
      (mocha-executable-path file project-root))))

(expectations
  (desc "mocha-make-minimum-command")
  (desc "it returns minimum command")
  (lexical-let* ((exec-path "mocha"))
    (expect '("mocha" "--reporter spec")
      (mocha-make-minimum-command exec-path))
    (expect '("mocha" "--reporter spec" "-g" "FizzBuzz")
      (mocha-make-minimum-command exec-path '("-g" "FizzBuzz")))))

(expectations
  (desc "mocha-run-this-file-command")
  (desc "it returns appropriate command")
  (lexical-let* ((file "foo-test.js")
                 (exec-path "mocha"))
    (expect "mocha --reporter spec foo-test.js"
      (mocha-run-this-file-command file exec-path))
    (expect "mocha --reporter spec -g fizzbuzz foo-test.js"
      (mocha-run-this-file-command file
                                   exec-path
                                   (mocha-grep-option "fizzbuzz")))))

(expectations
  (desc "mocha-test-file?")
  (desc "it returns true if filename contains spec or test.")
  (expect t (mocha-test-file? "~/project-root/test/foo-spec.js"))
  (expect t (mocha-test-file? "~/project-root/test/test-foo.js"))
  (expect nil (mocha-test-file? "~/project-root/src/foo.js"))
  (expect nil (mocha-test-file? "~/project-root/test/foo.js")))
