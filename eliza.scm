#!/usr/local/bin/gosh

;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File eliza.lisp: Advanced version of Eliza.
;;; Has more rules, and accepts input without parens.

;;;; File eliza1.lisp: Basic version of the Eliza program

;;; The basic are in auxfns.lisp; look for "PATTERN MATCHING FACILITY"

;; New version of pat-match with segment variables

;; converted to Gauche Scheme

(use gauche.parseopt) ; command line args
(use srfi-27) ; random-real

(define (main args)
 (eliza))

(define variable-p
 (lambda (x)
  (and (symbol? x)
   (equal (string-ref (symbol->string x) 0) #\?))))

(define punctuation-p
 (lambda (char)
  (find char ".,;:`!?#-()\\\"")))

(define pat-match
 (lambda (pattern input :optional (bindings '()))
  (cond ((eq? bindings 'fail) fail)
   ((variable-p pattern)
    (match-variable pattern input bindings))
   ((equal? pattern input) bindings)
   ((segment-pattern-p pattern)                ; ***
    (segment-match pattern input bindings))    ; ***
   ((and (pair? pattern) (pair? input))
    (pat-match (cdir pattern) (rest input)
     (pat-match (car pattern) (car input)
      bindings)))
   (else 'fail))))

(define segment-pattern-p
 (lambda (pattern)
  (and (pair? pattern)
   (eq? (car (car pattern)) '?*))))

(define segment-match
 (lambda (pattern input bindings :optional (start 0))
  (let ((var (cadr (car pattern)))
        (pat (cdr pattern)))
   (if (null? pat)
    (match-variable var input bindings)
    ;; We assume that pat starts with a constant
    ;; In other words, a pattern can't have 2 consecutive vars
    (let ((pos (position (car pat) input
                :start start :test equal?)))
     (if (null? pos)
      'fail
      (let ((b2 (pat-match
                 pat (subseq input pos)
                 (match-variable var (subseq input 0 pos)
                  bindings))))
       ;; If this match failed, try another longer one
       (if (eq b2 'fail)
        (segment-match pattern input bindings (+ pos 1))
        b2))))))))

(define rule-pattern
 (lambda (rule)
  (car rule)))

(define rule-responses
 (lambda (rule)
  (cdr rule)))

(define use-eliza-rules
 (lambda (input)
  (filter-map
   (lambda (rule)
    (let ((result (pat-match (rule-pattern rule) input)))
     (if (not (eq? result fail))
      (sublis (switch-viewpoint result)
       (random-elt (rule-responses rule))
       #f))))
   *eliza-rules*)))

(define switch-viewpoint
 (lambda (words)
  (sublis '((I . you) (you . I) (me . you) (am . are))
   words)))

(define flatten
 (lambda (the-list)
  (map-append mklist the-list)))

(define mklist
 (lambda (x)
  (if (list? x)
   x
   (list x))))

(define random-elt
 (lambda (choices)
  (elt choices (random (length choices)))))

;;;> (eliza)
;;;ELIZA> (hello there)
;;;(HOW DO YOU DO. PLEASE STATE YOUR PROBLEM.)
;;;ELIZA> (i want to test this program)
;;;(WHAT WOULD IT MEAN IF YOU GOT TO TEST THIS PROGRAM)
;;;ELIZA> (i could see if it works)
;;;(DO YOU REALLY THINK ITS LIKELY THAT IT WORKS)
;;;ELIZA> (no not really)
;;;(ARE YOU SAYING "NO" JUST TO BE NEGATIVE?)
;;;ELIZA> (no)
;;;(ARE YOU SAYING "NO" JUST TO BE NEGATIVE?)
;;;ELIZA> (forget it-- i was wondering how general the program is)
;;;(WHY DO YOU TELL ME YOU WERE WONDERING HOW GENERAL THE PROGRAM IS NOW?)
;;;ELIZA> (i felt like it)
;;;(WHAT OTHER FEELINGS DO YOU HAVE?)
;;;ELIZA> (i feel this is enough)
;;;(DO YOU OFTEN FEEL THIS IS ENOUGH ?)
;;;ELIZA> [Abort]

(define read-line-no-punct
 (lambda ()
  (string-split (read-line) #\ )))

(define eliza
 (lambda ()
  (print 'eliza>)
  (let* ((input (read-line-no-punct))
         (response (flatten (use-eliza-rules input))))
   (print-with-spaces response)
   (if (not (equal response '(good bye)))
    (eliza)))))

(define print-with-spaces
 (lambda (list)
  (write (string-join list " "))))

(define *eliza-rules*
 '((((?* ?x) hello (?* ?y))
    (How do you do.  Please state your problem.))
   (((?* ?x) computer (?* ?y))
    (Do computers worry you?) (What do you think about machines?)
    (Why do you mention computers?)
    (What do you think machines have to do with your problem?))
   (((?* ?x) name (?* ?y))
    (I am not interested in names))
   (((?* ?x) sorry (?* ?y))
    (Please don't apologize) (Apologies are not necessary)
    (What feelings do you have when you apologize))
   (((?* ?x) I remember (?* ?y))
    (Do you often think of ?y)
    (Does thinking of ?y bring anything else to mind?)
    (What else do you remember) (Why do you recall ?y right now?)
    (What in the present situation reminds you of ?y)
    (What is the connection between me and ?y))
   (((?* ?x) do you remember (?* ?y))
    (Did you think I would forget ?y ?)
    (Why do you think I should recall ?y now)
    (What about ?y) (You mentioned ?y))
   (((?* ?x) if (?* ?y))
    (Do you really think its likely that ?y) (Do you wish that ?y)
    (What do you think about ?y) (Really-- if ?y))

   (((?* ?x) I dreamt (?* ?y))
    (Really-- ?y) (Have you ever fantasized ?y while you were awake?)
    (Have you dreamt ?y before?))
   (((?* ?x) dream about (?* ?y))
    (How do you feel about ?y in reality?))
   (((?* ?x) dream (?* ?y))
    (What does this dream suggest to you?) (Do you dream often?)
    (What persons appear in your dreams?)
    (Don't you believe that dream has to do with your problem?))
   (((?* ?x) my mother (?* ?y))
    (Who else in your family ?y) (Tell me more about your family))
   (((?* ?x) my father (?* ?y))
    (Your father) (Does he influence you strongly?)
    (What else comes to mind when you think of your father?))

   (((?* ?x) I want (?* ?y))
    (What would it mean if you got ?y)
    (Why do you want ?y) (Suppose you got ?y soon))
   (((?* ?x) I am glad (?* ?y))
    (How have I helped you to be ?y) (What makes you happy just now)
    (Can you explain why you are suddenly ?y))
   (((?* ?x) I am sad (?* ?y))
    (I am sorry to hear you are depressed)
    (I'm sure its not pleasant to be sad))
   (((?* ?x) are like (?* ?y))
    (What resemblance do you see between ?x and ?y))
   (((?* ?x) is like (?* ?y))
    (In what way is it that ?x is like ?y)
    (What resemblance do you see?)
    (Could there really be some connection?) (How?))
   (((?* ?x) alike (?* ?y))
    (In what way?) (What similarities are there?))
   (((?* ?x) same (?* ?y))
    (What other connections do you see?))

   (((?* ?x) I was (?* ?y))
    (Were you really?) (Perhaps I already knew you were ?y)
    (Why do you tell me you were ?y now?))
   (((?* ?x) was I (?* ?y))
    (What if you were ?y ?) (Do you thin you were ?y)
    (What would it mean if you were ?y))
   (((?* ?x) I am (?* ?y))
    (In what way are you ?y) (Do you want to be ?y ?))
   (((?* ?x) am I (?* ?y))
    (Do you believe you are ?y) (Would you want to be ?y)
    (You wish I would tell you you are ?y)
    (What would it mean if you were ?y))
   (((?* ?x) am (?* ?y))
    (Why do you say "AM?") (I don't understand that))
   (((?* ?x) are you (?* ?y))
    (Why are you interested in whether I am ?y or not?)
    (Would you prefer if I weren't ?y)
    (Perhaps I am ?y in your fantasies))
   (((?* ?x) you are (?* ?y))
    (What makes you think I am ?y ?))

   (((?* ?x) because (?* ?y))
    (Is that the real reason?) (What other reasons might there be?)
    (Does that reason seem to explain anything else?))
   (((?* ?x) were you (?* ?y))
    (Perhaps I was ?y) (What do you think?) (What if I had been ?y))
   (((?* ?x) I can't (?* ?y))
    (Maybe you could ?y now) (What if you could ?y ?))
   (((?* ?x) I feel (?* ?y))
    (Do you often feel ?y ?))
   (((?* ?x) I felt (?* ?y))
    (What other feelings do you have?))
   (((?* ?x) I (?* ?y) you (?* ?z))
    (Perhaps in your fantasy we ?y each other))
   (((?* ?x) why don't you (?* ?y))
    (Should you ?y yourself?)
    (Do you believe I don't ?y) (Perhaps I will ?y in good time))
   (((?* ?x) yes (?* ?y))
    (You seem quite positive) (You are sure) (I understand))
   (((?* ?x) no (?* ?y))
    (Why not?) (You are being a bit negative)
    (Are you saying "NO" just to be negative?))

   (((?* ?x) someone (?* ?y))
    (Can you be more specific?))
   (((?* ?x) everyone (?* ?y))
    (surely not everyone) (Can you think of anyone in particular?)
    (Who for example?) (You are thinking of a special person))
   (((?* ?x) always (?* ?y))
    (Can you think of a specific example) (When?)
    (What incident are you thinking of?) (Really-- always))
   (((?* ?x) what (?* ?y))
    (Why do you ask?) (Does that question interest you?)
    (What is it you really want to know?) (What do you think?)
    (What comes to your mind when you ask that?))
   (((?* ?x) perhaps (?* ?y))
    (You do not seem quite certain))
   (((?* ?x) are (?* ?y))
    (Did you think they might not be ?y)
    (Possibly they are ?y))
   (((?* ?x))
    (Very interesting) (I am not sure I understand you fully)
    (What does that suggest to you?) (Please continue) (Go on)
    (Do you feel strongly about discussing such things?))))


