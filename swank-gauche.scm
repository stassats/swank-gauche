;;; -*- Mode: Scheme -*-

;; (define-module swank
  

;;   (export start-server))
(use gauche.net)
(use gauche.selector)
(use gauche.vport)
(use gauche.uvector)
(use srfi-1)
(use util.match)
(use util.queue)

;;(select-module swank)

(define nil #f)
(define t #t)

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

(define *connection* #f)

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

(define (swank:swank-require . rest))
(define (swank:buffer-first-change filename))

(define (swank:describe-symbol symbol-name)
  (with-output-to-string
   (lambda ()
     (describe (eval (read-from-string symbol-name)
                     (interaction-environment))))))

(define (get-procedure form)
  (match form
    (("with-module" module symbol . _)
     (let ((module (find-module (string->symbol module))))
       (when module
         (global-variable-ref module (string->symbol symbol) #f))))
    (((? string? symbol) . _)
     (global-variable-ref (current-module) (string->symbol symbol) #f))
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
                                  ((pair? arity) (car arity))
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

(define (swank:autodoc forms . rest)
  (procedure-arglist (find-subform-for-arglist forms)))

(define (send-results values)
  (if (null? values)
      (write-packet `(:write-string "; No value" :repl-result) *connection*)
      (dolist (value values)
              (write-packet `(:write-string ,(string-append (write-to-string value)
                                                            "\n")
                                            :repl-result)

                            *connection*))))

(define (swank:variable-desc-for-echo-area symbol)
  (let ((symbol (read-from-string symbol)))
    (when (symbol-bound? symbol)
      (write-to-string
       (eval symbol (interaction-environment))))))

(define (eval-form-for-emacs form)
  (call-with-values (lambda () (eval form (interaction-environment)))
    list))

(define (swank:interactive-eval form)
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

(define (swank:listener-eval form)
  (send-results
   (eval-form-for-emacs (read-from-string form)))
  'nil)

(define (changelog-date)
  "2010-01-28")

(define (swank:connection-info)
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

(define (swank:create-repl env)
  (list "user" "user"))

(define (swank:quit-lisp)
  (uninstall-signal-handler)
  (socket-close (swank-socket *connection*)))

(define (swank:swank-macroexpand-1 form)
  (write-to-string (macroexpand-1 (read-from-string form))))

(define (swank:load-file file)
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

(define (eval-for-emacs connection form package thread id)
  (set! sigint? #f)
  (unwind-protect
   (write-packet
    (list :return
          (guard (exc
                  ((<error> exc)
                   (format #t "ERROR: ~a~%" (slot-ref exc 'message))
                   '(:abort))
                  (else
                   (format #t "EXCEPTION: ~a~%" exc)
                   '(:abort)))
                 (call/cc (lambda (cc)
                            (set! eval-continuation cc)))
                 (begin0
                     (if sigint?
                         '(:abort)
                         `(:ok ,(emacsify (eval form (interaction-environment)))))
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
  (install-signal-handler)
  (set! *connection* (make-connection socket))
  (set! (port-buffering (swank-out-port *connection*)) :line))

(define (start-server port)
  (let ((server (make-server-socket port :reuse-addr? #t))
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

    (selector-add! selector (socket-fd server)
                   accept-handler '(r))
    (unwind-protect (selector-select selector)
                    (socket-close server))))

;;; slime-c-p-c completion

(define (swank:completions string package)
  (let* ((names (map symbol->string (list-all-symbols)))
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

(define (list-all-symbols)
  (apply append!
         (delete-duplicates!
          (map list-symbols
               (append (module-precedence-list (current-module))
                       (module-imports (current-module)))))))

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