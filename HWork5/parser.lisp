;;; =====================================================================
;;; CSCI-330: Software Systems
;;; Homework 5 - Recursive Descent Parser
;;; Mohammad Aryan Khan
;;; =====================================================================


(defun Ifn (str)
  "Starting rule: I -> i E S X
  This function initiates the parsing process. It expects the first token to be 'i'.
  If 'i' is not found, it returns an error. Otherwise, it proceeds to parse E, S, and X."
  (print '(in Ifn))
  (print str)
  (if (or (null str) (not (eql (car str) 'i)))
      (list 'err)  ; Return an error if the first token is not 'i'
      (let ((e (Efn (cdr str))))  ; Move forward and parse E
        (if (and (consp e) (eq (car e) 'err))
            e  ; If parsing E fails, return the error
            (let ((s (Sfn e)))  ; Otherwise, parse S
              (if (and (consp s) (eq (car s) 'err))
                  s  ; If parsing S fails, return the error
                  (Xfn s)))))))  ; Proceed to parse X

(defun Xfn (str)
  "X -> ε | e S
  Parses optional 'e' followed by S. If 'e' is present, S must follow.
  If 'e' is missing, it assumes an empty production (epsilon)."
  (print '(in Xfn))
  (print str)
  (if (and str (eql (car str) 'e))
      (Sfn (cdr str))  ; Consume 'e' and then parse S
      str))  ; If no 'e', return input as-is (epsilon production)

(defun Efn (str)
  "E -> G E'
  Parses G first, then E'. If G fails, parsing fails."
  (print '(in Efn))
  (print str)
  (let ((g (Gfn str)))  ; Parse G
    (if (and (consp g) (eq (car g) 'err))
        g  ; If G fails, return error
        (Eprimefn g))))  ; Otherwise, parse E'

(defun Eprimefn (str)
  "E' -> o G E' | ε
  Parses a sequence of 'o' followed by G and optionally more E'.
  If 'o' is missing, it assumes an empty production (epsilon)."
  (print '(in Eprimefn))
  (print str)
  (if (and str (eql (car str) 'o))
      (let ((g (Gfn (cdr str))))  ; Consume 'o' and parse G
        (if (and (consp g) (eq (car g) 'err))
            g  ; If G fails, return error
            (Eprimefn g)))  ; Otherwise, continue parsing E'
      str))  ; If no 'o', return input as-is (epsilon production)

(defun Ofn (str)
  "O -> o G O | ε
  Similar to E', but for a different grammar rule.
  Parses sequences of 'o' followed by G, with optional repetition.
  If 'o' is missing, it assumes an empty production (epsilon)."
  (print '(in Ofn))
  (print str)
  (if (and str (eql (car str) 'o))
      (let ((g (Gfn (cdr str))))  ; Consume 'o' and parse G
        (if (and (consp g) (eq (car g) 'err))
            g  ; If G fails, return error
            (Ofn g)))  ; Otherwise, continue parsing O recursively
      str))  ; If no 'o', return input as-is (epsilon production)

(defun Gfn (str)
  "G -> x | y | z | w
  Parses a single terminal symbol ('x', 'y', 'z', or 'w').
  If the first token is not one of these, it returns an error."
  (print '(in Gfn))
  (print str)
  (if (null str)
      (list 'err)  ; Return an error if input is empty
      (case (car str)
        ((x y z w) (cdr str))  ; If the first token matches, consume it and return the rest
        (otherwise (list 'err)))))  ; If no match, return error

(defun Sfn (str)
  "S -> s | d L b
  Parses either 's' (single character case) or 'd L b' (complex case).
  If 's' is found, it is consumed. If 'd' is found, it must be followed by L and 'b'."
  (print '(in Sfn))
  (print str)
  (cond 
    ((and str (eql (car str) 's))
     (cdr str))  ; Consume 's' and return the rest of the input
    ((and str (eql (car str) 'd))
     (let ((l (Lfn (cdr str))))  ; Parse L after consuming 'd'
       (if (and l (not (and (consp l) (eq (car l) 'err)))  ; Ensure L parsed correctly
                (and l (eql (car l) 'b)))  ; Ensure 'b' follows
           (cdr l)  ; Consume 'b' and return remaining input
           (list 'err))))  ; If conditions not met, return error
    (t (list 'err))))  ; If neither case matches, return error

(defun Lfn (str)
  "L -> s L'
  Parses 's' followed by L'. If 's' is missing, returns an error."
  (print '(in Lfn))
  (print str)
  (if (and str (eql (car str) 's))
      (Lprimefn (cdr str))  ; Consume 's' and parse L'
      (list 'err)))  ; If no 's', return error

(defun Lprimefn (str)
  "L' -> s L' | ε
  Parses sequences of 's' or assumes an empty production (epsilon)."
  (print '(in Lprimefn))
  (print str)
  (if (and str (eql (car str) 's))
      (Lprimefn (cdr str))  ; Consume 's' and continue parsing L'
      str))  ; If no 's', return input as-is (epsilon production)

(defun parse-input (tokens)
  "Wrapper function that initiates parsing from the start symbol 'I'.
  If the entire input is consumed without errors, it prints success.
  Otherwise, it indicates a parsing failure."
  (let ((result (Ifn tokens)))
    (if (or (null result)  ; If input is fully consumed
            (and (consp result) (null result))) ; or if result is an empty list
        (format t "Parse successful!~%")
        (if (and (consp result) (eq (car result) 'err))
            (format t "Parse error. Parse Failed.")
            (format t "Parse error. Parse Failed.")))))
