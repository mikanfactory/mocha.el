;;; -*- lexical-binding: t; -*-
(require 'ert-expectations)
(require 'cl-lib)
;; (require 'mocha)

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

(expectations
  (desc "mocha-test-file?")
  (desc "it returns true if filename contains spec or test")
  (expect t (mocha-test-file? "project-root/test/foo-spec.js"))
  (expect t (mocha-test-file? "project-root/test/test-foo.js"))
  (expect nil (mocha-test-file? "project-root/src/foo.js"))
  (expect t (mocha-test-file? "project-root/test/foo.js")))

(expectations
  (desc "mocha-minimize-path")
  (desc "it cuts path in order to change depth of path to 2")
  (lexical-let ((file "project-root/app/models/user.js"))
    (expect "models/user.js"
      (mocha-minimize-path file))
    (expect (error)
      (mocha-minimize-path "foo.js"))))

(expectations
  (desc "mocha-add-suffix")
  (desc "it returns list which contains src-file added suffixs")
  (lexical-let ((file "models/user.js"))
    (expect '("models/user.js" "models/user-test.js" "models/user_test.js"
              "models/user-spec.js" "models/user_spec.js")
      (mocha-add-suffix file))))

(expectations
  (desc "mocha-remove-suffix")
  (desc "it returns filename which removed 'test' and 'spec' suffix")
  (expect "project-root/src/foo.js"
    (mocha-remove-suffix "project-root/src/foo-test.js"))
  (expect "project-root/src/foo.js"
    (mocha-remove-suffix "project-root/src/foo_test.js"))
  (expect "project-root/src/foo.js"
    (mocha-remove-suffix "project-root/src/foo-spec.js"))
  (expect "project-root/src/foo.js"
    (mocha-remove-suffix "project-root/src/foo_spec.js")))

(expectations
  (desc "mocha--collect-entries")
  (desc "it collect directory and file under the path of argument")
  (lexical-let ((path (f-join (f-dirname (f-this-file)) "env" "test")))
    (expect (list (f-join path "spec")
                  (f-join path "mocha-test.js")
                  (f-join path "mocha-test-join.js")
                  (f-join path "spec" "mocha-mocha-spec.js"))
      (mocha--collect-entries path t))))

(expectations
  (desc "mocha-find-test-file-under-test-dir")
  (desc "it check whether candidates is realy exist or not")
  (lexical-let* ((project-root (f-join (f-dirname (f-this-file)) "env"))
                 (candidates (mocha-add-suffix "mocha.js")))
    (expect (f-join project-root "test" "mocha-test.js")
      (mocha-find-test-file-under-test-dir project-root candidates)))
  (lexical-let* ((project-root (f-join (f-dirname (f-this-file)) "env"))
                 (candidates (mocha-add-suffix "mocha.js")))
      )
  )

(lexical-let* ((project-root (f-join (f-dirname (f-this-file)) "env"))
               (candidates (mocha-add-suffix "mocha.js")))
  (mocha-find-test-file-under-test-dir project-root candidates))

