;;; -*- Mode: Scheme -*-
;;;
;;; Authors: Stas Boukarev <stassats@gmail.com>
;;;
;;; License: Public Domain

(define nil #f)
(define t #t)

(define-module swank
  (export start-server)
  (use gauche.interactive)
  (use gauche.net)
  (use gauche.selector)
  (use gauche.vport)
  (use gauche.uvector)
  (use file.util)
  (use srfi-1)
  (use util.match)
  (use util.queue))

(define (main args)
  (define (parse-integer string)
    (or (string->number string)
        string))
  (if (= (length args) 2)
      ((with-module swank start-server)
       (parse-integer (cadr args)))
      (print "The only argument should be either a port number,
 or a file name to which randomly assigned port number will be written.")))

(select-module swank)

(define-class <swank-connection> ()
  ((socket :init-keyword :socket
           :getter swank-socket)
   (output :init-keyword :output
           :getter swank-output)
   (input :init-keyword :input
          :getter swank-input)
   (out-port :init-keyword :out-port
             :getter swank-out-port)
   (in-port :init-keyword :in-port
            :getter swank-in-port)
   (tag :init-value 0
        :accessor tag)
   (event-queue :init-value (make-queue)
                :accessor event-queue)))

(define nil #f)
(define t #t)

(define *connection* #f)
(define *module* (current-module))

(define (make-connection socket)
  (make <swank-connection>
    :socket socket
    :output (socket-output-port socket :buffering :none)
    :input  (socket-input-port socket :buffering :full)
    :out-port (make <buffered-output-port> :flush output-for-emacs)
    :in-port (make <buffered-input-port> :fill input-from-emacs)))

(define (make-tag! connection)
  (inc! (tag connection))
  (tag connection))

;;;

(define (swank-require . rest))
(define (buffer-first-change filename))

(define (describe-symbol symbol-name)
  (with-output-to-string
   (lambda ()
     (describe (eval-in-module (read-from-string symbol-name)
                               *module*)))))

(define (list-all-package-names . nicknames)
  (map module-name (all-modules)))

(define (set-package name)
  (let ((module (find-module (read-from-string name))))
    (unless module
      (error "package" name "doesn't exist"))
    (list (symbol->string (module-name module))
          (symbol->string (module-name module)))))

(define default-directory current-directory)

(define (set-default-directory directory)
  (current-directory directory)
  (current-directory))

(define (get-procedure form)
  (match form
    (("with-module" (? string? module) (? string? symbol) . _)
     (let ((module (find-module (string->symbol module))))
       (when module
         (global-variable-ref module (string->symbol symbol) #f))))
    (((? string? symbol) . _)
     (global-variable-ref *module* (string->symbol symbol) #f))
    (_ #f)))

(define (procedure-name procedure)
  (cond ((is-a? procedure <procedure>)
         (slot-ref procedure 'info))
        ((is-a? procedure <generic>)
         (slot-ref procedure 'name))))

(define (procedure-arglist form)
  (let ((procedure (get-procedure form)))
    (cond ((procedure? procedure)
           (let* ((arity (arity procedure))
                  (at-least (cond ((arity-at-least? arity)
                                   (arity-at-least-value arity))
                                  ((pair? arity)
                                   (if (number? (car arity))
                                       (car arity)
                                       (arity-at-least-value (car arity))))
                                  (else arity))))
             (format #f "~a"
                     (append (list (procedure-name procedure))
                             (make-list at-least 'arg)
                             (if (arity-at-least? arity)
                                 "rest"
                                 ())))))
          ((or (is-a? procedure <syntax>)
               (is-a? procedure <macro>))
           (write-to-string procedure))
          (else
           :not-available))))

(define (find-subform-for-arglist form)
  (define (target? forms) (memq 'swank::%cursor-marker% forms))
  (cond ((not (pair? form))
         #f)
        ((target? form)
         form)
        (else (or (find-subform-for-arglist (car form))
                  (find-subform-for-arglist (cdr form))))))

(define (autodoc forms . rest)
  (procedure-arglist (find-subform-for-arglist forms)))

(define (send-results values)
  (if (null? values)
      (write-packet `(:write-string "; No value" :repl-result) *connection*)
      (dolist (value values)
              (write-packet `(:write-string ,(string-append (write-to-string value)
                                                            "\n")
                                            :repl-result)

                            *connection*))))

(define (variable-desc-for-echo-area symbol)
  (let ((symbol (read-from-string symbol)))
    (when (symbol-bound? symbol)
      (write-to-string
       (eval-in-module symbol *module*)))))

(define (eval-form-for-emacs form)
  (call-with-values (lambda ()
                      (eval-in-module form *module*))
    list))

(define (interactive-eval form)
  (let ((values (eval-form-for-emacs (read-from-string form))))
    (if (null? values)
        "; No value"
        (with-output-to-string
         (lambda ()
           (display "=>")
           (map (lambda (x)
                  (display " ")
                  (display x))
                values))))))

(define (listener-eval form)
  (send-results
   (eval-form-for-emacs (read-from-string form)))
  'nil)

(define (changelog-date)
  "2010-02-07")

(define (connection-info)
  (list :pid (sys-getpid)
        :lisp-implementation (list
                              :type "Gauche Scheme"
                              :name "gosh"
                              :version (gauche-version))
        :machine (list
                  :instance (sys-gethostname)
                  :type (gauche-architecture)
                  :version "")
        :package (symbol->string (module-name (current-module)))
        :version (changelog-date)))

(define (create-repl env)
  (list "user" "user"))

(define (quit-lisp)
  (uninstall-signal-handler)
  (socket-close (swank-socket *connection*)))

(define (swank-macroexpand-1 form)
  (write-to-string (macroexpand-1 (read-from-string form))))

(define (load-file file)
  (load file))

;;;;

(define (output-for-emacs vector flag)
  (write-packet
   (list :write-string
         (u8vector->string vector))
   *connection*)
  (u8vector-length vector))

(define (wait-for-event matcher)
  (let ((result #f))
    (until result
      (let* ((request (read-from-string (read-packet *connection*)))
             (match (matcher request)))
        (set! result match)
        (unless match
          (enqueue! (event-queue *connection*) request))))
    result))

(define (read-string-from-emacs)
  (let ((tag (make-tag! *connection*)))
    (write-packet
     (list :read-string 't tag)
     *connection*)
    (wait-for-event
     (match-lambda ((:emacs-return-string _ returned-tag value)
                    (if (= tag returned-tag)
                        value
                        #f))
                   (_ #f)))))

(define (input-from-emacs uvector)
  (let ((input (string->u8vector (read-string-from-emacs))))
    (do ((index 0 (+ index 1)))
        ((or (>= index (u8vector-length uvector))
             (>= index (u8vector-length input)))
         index)
      (u8vector-set! uvector index
                     (u8vector-ref input index)))))

(define (emacsify value)
  (cond ((or (not value) (is-a? value <undefined-object>))
         ())
        ((eq? value #t) 't)
        (else value)))

(define eval-continuation #f)
(define sigint? #f)

(define (install-signal-handler)
  (set-signal-handler! SIGINT
                       (lambda (x)
                         (set! sigint? #t)
                         (if eval-continuation
                             (eval-continuation)
                             (warn "Caught SIGINT")))))

(define (uninstall-signal-handler)
  (set-signal-handler! SIGINT %default-signal-handler))

(define (de-clfy-symbol symbol)
  (rxmatch-if (rxmatch #/^(.+?):{1,2}(.+)$/ (symbol->string symbol))
              (_ module name)
              `(with-module ,(string->symbol module)
                            ,(string->symbol name))
              symbol))

(define (transform-package form)
  (if (and (pair? form)
           (symbol? (car form)))
      (cons (de-clfy-symbol (car form))
            (cdr form))
      form))

(define (find-package package-name)
  (let* ((name (cond ((or (string-ci=? package-name "common-lisp-user")
                          (string-ci=? package-name "cl-user"))
                      'user)
                     ((or (string-ci=? package-name "common-lisp")
                          (string-ci=? package-name "cl"))
                      'scheme)
                     (else (read-from-string package-name))))
         (module (find-module name)))
    (or module (find-module 'user))))

(define (eval-in-module form module)
  (eval `(with-module ,(module-name module)
                      ,form)
        (interaction-environment)))

(define (eval-in-package form package)
  (let ((old-module *module*))
   (unwind-protect
    (begin
      (set! *module* (find-package package))
      (eval-in-module (transform-package form) *module*))
    (set! *module* old-module))))

(define (eval-for-emacs connection form package thread id)
  (set! sigint? #f)
  (unwind-protect
   (write-packet
    (list :return
          (guard (exc
                  ((<message-condition> exc)
                   (format #t "EXCEPTION: ~a~%" (slot-ref exc 'message))
                   '(:abort))
                  (else
                   (format #t "EXCEPTION: ~a~%" exc)
                   '(:abort)))
                 (call/cc (lambda (cc)
                            (set! eval-continuation cc)))
                 (begin0
                     (if sigint?
                         '(:abort)
                         `(:ok ,(emacsify (eval-in-package form package))))
                   (set! eval-continuation #f)))
          id)
    connection)
   (flush (swank-out-port connection))))

(define (process-event event connection)
  (case (car event)
    ((:emacs-rex)
     (apply eval-for-emacs connection (cdr event)))))

(define (handle-requests connection)
  (while #t
    (until (queue-empty? (event-queue connection))
      (process-event (dequeue! (event-queue connection)) connection))
    (let ((event (read-from-string (read-packet connection))))
      (process-event event connection)
      ;; (write request (standard-output-port)) (newline (standard-output-port))
      
      )))

(define (read-length input)
  (string->number (read-block 6 input) 16))

(define (read-packet connection)
  (let ((input (swank-input connection)))
    (read-block (read-length input) input)))

(define (encode-length string)
  (format #f "~6,'0x" (string-length string)))

(define (write-packet form connection)
  (let ((string (write-to-string form)))
    (display (string-append (encode-length string) string)
             (swank-output connection))))

(define (setup-environment socket)
  (set! *connection* (make-connection socket))
  (set! (port-buffering (swank-out-port *connection*)) :line))



(define (write-port-number-to-file file-name socket)
  (with-output-to-file file-name
    (lambda ()
      (display (sockaddr-port (socket-getsockname socket))))))

(define listen-address "127.0.0.1")

(define (start-server port)
  "Port should be either an integer or a file name
to which a randomly assigned port numbe will be written"
  (let* ((file (if (string? port) port #f))
         (port (if file 0 port))
         (server (make-server-socket (make <sockaddr-in> listen-address port)
                                     :reuse-addr? #t))
         (selector (make <selector>)))
    (define (accept-handler sock flag)
      (setup-environment (socket-accept server))
      (with-input-from-port (swank-in-port *connection*)
        (lambda ()
          (with-error-to-port (swank-out-port *connection*)
                              (lambda ()
                                (with-output-to-port (swank-out-port *connection*)
                                  (lambda ()
                                    (handle-requests *connection*))))))))
    (when file
      (write-port-number-to-file file server))
    (selector-add! selector (socket-fd server)
                   accept-handler '(r))
    (unwind-protect (selector-select selector)
                    (begin (socket-close server)
                           (uninstall-signal-handler)))))

;;; slime-c-p-c completion

(define (completions string package)
  (let* ((names (map symbol->string
                     (list-all-symbols
                      (find-package package))))
         (completions (filter (make-completion-regex string) names)))
    completions
    (list completions (if (null? completions)
                          string
                          (common-prefix completions)))))

(define (make-completion-regex string)
  (string->regexp (string-append "^"
                                 (regexp-replace-all #/-/ (regexp-quote string) ".*-"))))

(define (list-symbols module)
  (hash-table-keys (module-table module)))

(define (list-all-symbols module)
  (apply append!
         (delete-duplicates!
          (map list-symbols
               (append (module-precedence-list module)
                       (module-imports module))))))

(define (common-prefix strings)
  (define (nth-char? n char string)
    (char=? (string-ref string n) char))
  (define (not-every-with index char)
    (not (every (lambda (string) (and (< index (string-length string))
                                      (nth-char? index char string)))
                (cdr strings))))

  (let ((first (car strings)))
    (if (= (length strings) 1)
        first
        (substring first 0 (do ((i 0 (+ i 1)))
                               ((or (>= i (string-length first))
                                    (not-every-with i (string-ref first i))) i))))))
