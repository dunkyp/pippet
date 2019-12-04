(in-package :cl-user)

(defpackage #:pippet
  (:nicknames #:pippet/cpu)
  (:use #:cl
        #:pippet/instruction)
  (:import-from :cl-utilities :rotate-byte)
  (:export
   #:gameboy
   #:make-mmu
   #:get-value
   #:set-value
   #:A #:B #:C #:D #:E
   #:F #:H #:SP #:PC
   #:AF #:BC #:DE #:HL
   #:get-value #:set-value
   #:step-cpu #:run-cpu
   #:zero-flag))

(in-package :pippet)

(defmacro defcpu (name slots)
  (flet ((make-register (name type)
                       `(,name
                            :initarg ,(find-symbol (symbol-name name) "KEYWORD")
                            :initform 0
                            :accessor ,name
                            :type ,type)))
    `(defclass ,name ()
       ,(loop for (slot-name form) in slots
           collect (make-register slot-name form)))))

(defcpu gameboy
    ((A '(unsigned-byte 8))
     (B '(unsigned-byte 8))
     (C '(unsigned-byte 8))
     (D '(unsigned-byte 8))
     (E '(unsigned-byte 8))
     (F '(unsigned-byte 8))
     (H '(unsigned-byte 8))
     (L '(unsigned-byte 8))
     (SP u16)
     (PC '(unsigned-byte 16))))

(defun make-mmu (length)
  (make-array length :element-type '(unsigned-byte 8)))

(deftype u16 () '(unsigned-byte 16))

(defmacro defregister-pair (hi lo)
  (let ((name (find-symbol (concatenate 'string (symbol-name hi) (symbol-name lo)))))
    `(progn
      (defun ,name (cpu)
         (let ((high-byte (,hi cpu))
               (low-byte (,lo cpu)))
           (dpb high-byte (byte 16 8) low-byte)))
      (defun (setf ,name) (value cpu)
         (setf (,lo cpu) (ldb (byte 8 0) value))
         (setf (,hi cpu) (ldb (byte 8 8) value))
         value))))

(defregister-pair A F)
(defregister-pair B C)
(defregister-pair D E)
(defregister-pair H L)

(defun set-flag-bit (bit set cpu)
  (if set
      (setf (F cpu) (logior (ash 1 bit) (F cpu)))
      (setf (F cpu) (logandc1 (ash 1 bit) (F cpu)))))

(defun zero-flag (cpu) (logbitp 7 (F cpu)))
(defun (setf zero-flag) (set cpu)
  (set-flag-bit 7 set cpu))
(defun subtraction-flag (cpu) (logbitp 6 (F cpu)))
(defun (setf subtraction-flag) (set cpu)
  (set-flag-bit 6 set cpu))
(defun half-carry-flag (cpu) (logbitp 5 (F cpu)))
(defun (setf half-carry-flag) (set cpu)
  (set-flag-bit 5 set cpu))
(defun carry-flag (cpu) (logbitp 4 (F cpu)))
(defun (setf carry-flag) (set cpu)
  (set-flag-bit 4 set cpu))

(defun write-byte-mmu (byte offset mmu)
  (setf (aref mmu offset) byte))

(defun write-word-mmu (word offset mmu)
  (setf (aref mmu offset) (ldb (byte 8 8) word))
  (setf (aref mmu (1+ offset)) (ldb (byte 8 0) word)))

(defun read-byte-mmu (offset mmu)
  (the (unsigned-byte 8)
       (aref mmu offset)))

(defun read-word-mmu (offset mmu)
  (the (unsigned-byte 16)
       (dpb (aref mmu (+ offset 1)) (byte 16 8) (aref mmu offset))))

(defun op-push (location cpu mmu)
  (decf (SP cpu) 2)
  (write-word-mmu (funcall (symbol-function location) cpu)
                  (SP cpu) mmu))

(defun op-call (cpu mmu)
  (let ((address (read-word-mmu (+ 1 (PC cpu)) mmu)))
    (op-push (PC cpu) cpu mmu)
    (setf (PC mmu) address)))

(defun op-ret (cpu mmu)
  (let ((address (read-word-mmu (SP cpu) mmu)))
    (setf (PC cpu) address)
    (incf (SP cpu)  2)))

(defun reset (cpu)
  (setf (PC cpu) #x0100)
  (setf (SP cpu) #xFFFE)
  (setf (AF cpu) #xB001)
  (setf (BC cpu) #x1300)
  (setf (DE cpu) #xD800)
  (setf (HL cpu) #x4D01)
  cpu)

(defun get-value (operand offset cpu mmu)
  (let* ((name (operand-name operand))
         (immediate-value (cond
                            ((eq name 'a8) (read-byte-mmu (+ offset (PC cpu)) mmu))
                            ((eq name 'r8) (read-byte-mmu (+ offset (PC cpu)) mmu))
                            ((eq name 'a16) (read-word-mmu (+ offset (PC cpu)) mmu))
                            ((eq name 'd8) (read-byte-mmu (+ offset (PC cpu)) mmu))
                            ((eq name 'd16) (read-word-mmu (+ offset (PC cpu)) mmu))
                            (t (funcall (symbol-function name) cpu)))))
    (cond
      ((operand-immediate operand) immediate-value)
      (t (read-word-mmu immediate-value mmu)))))

(defun set-value (value operand offset cpu mmu)
  (let ((name (operand-name operand)))
    (cond
      ((eq name 'a8) (write-byte-mmu value (read-byte-mmu (+ (PC cpu) offset) mmu) mmu))
      ((eq name 'a16) (write-word-mmu value (read-byte-mmu (+ (PC cpu) offset) mmu) mmu))
      ((eq (operand-immediate operand) nil) (write-byte-mmu value (funcall (symbol-function (operand-name operand)) cpu) mmu))
      (t (funcall (fdefinition `(setf ,(operand-name operand))) value cpu)))))

(defun fetch-instruction (cpu mmu &optional (prefixed nil))
  (decode-instruction (if prefixed
                          (+ 256 (aref mmu (PC cpu)))
                          (aref mmu (PC cpu)))))

(defun next-instruction (instruction cpu)
  (incf (PC cpu) (instruction-bytes instruction)))

(defun handle-ld-instruction (instruction cpu mmu)
  (let* ((operands (instruction-operands instruction))
         (source-operand (aref operands 1))
         (destination-operand (aref operands 0))
         (source-value (get-value source-operand (+ 1 (operand-bytes destination-operand)) cpu mmu)))
    (set-value source-value destination-operand 1 cpu mmu)
    (if (operand-decrement destination-operand)
        (funcall (fdefinition `(setf ,(operand-name destination-operand)))
                 (funcall (symbol-function (operand-name destination-operand)) cpu)
                 cpu))))

(defun handle-jp-instruction (instruction cpu mmu)
  (let* ((operands (instruction-operands instruction))
         (destination-operand (if (aref operands 1)
                                  (aref operands 1)
                                  (aref operands 0)))
         (condition (if (aref operands 1)
                        (aref operands 0)
                        nil)))
    (if
     (or
      (not condition)
      (and (eq condition 'NOT-ZERO) (not (zero-flag cpu)))
      (and (eq condition 'ZERO) (zero-flag cpu))
      (and (eq condition 'NOT-CARRY) (not (carry-flag cpu)))
      (and (eq condition 'CARRY) (carry-flag cpu))
      (and (eq condition 'NOT-HALF-CARRY) (not (half-carry-flag cpu)))
      (and (eq condition 'HALF-CARRY) (half-carry-flag cpu))
      (and (eq condition 'NOT-SUBTRACTION) (not (subtraction-flag cpu)))
      (and (eq condition 'SUBTRACTION) (subtraction-flag cpu)))
     (setf (PC cpu) (get-value destination-operand 1 cpu mmu)))))

(defun chop-8 (value)
  (ldb (byte 8 0) value))

(defun unsigned-to-signed-8 (i)
  (if (logbitp 7 i)
      (- (chop-8 (1+ (lognot i))))
      i))

(defun handle-jr-instruction (instruction cpu mmu)
  (let* ((operands (instruction-operands instruction))
         (destination-operand (if (aref operands 1)
                                  (aref operands 1)
                                  (aref operands 0)))
         (condition (if (aref operands 1)
                        (operand-name (aref operands 0))
                        nil)))
    (if
     (or
      (not condition)
      (and (eq condition 'NOT-ZERO) (not (zero-flag cpu)))
      (and (eq condition 'ZERO) (zero-flag cpu))
      (and (eq condition 'NOT-CARRY) (not (carry-flag cpu)))
      (and (eq condition 'CARRY) (carry-flag cpu))
      (and (eq condition 'NOT-HALF-CARRY) (not (half-carry-flag cpu)))
      (and (eq condition 'HALF-CARRY) (half-carry-flag cpu))
      (and (eq condition 'NOT-SUBTRACTION) (not (subtraction-flag cpu)))
      (and (eq condition 'SUBTRACTION) (subtraction-flag cpu)))
     (incf (PC cpu) (+ (instruction-bytes instruction) (unsigned-to-signed-8 (get-value destination-operand 1 cpu mmu))))
     (next-instruction instruction cpu))))

(defun handle-inc-instruction (instruction cpu mmu)
  (let* ((operand (aref (instruction-operands instruction) 0))
         (value (get-value operand 1 cpu mmu)))
    (set-value (+ 1 value) operand 1 cpu mmu)))

(defun handle-add-instruction (instruction cpu mmu)
  (let* ((operand (aref (instruction-operands instruction) 1))
         (value (get-value operand 1 cpu mmu)))
    (setf (A cpu) (logand (+ (A cpu) value) #xFF))
    (if (= (A cpu) 0)
        (setf (zero-flag cpu) 1))))

(defun handle-sub-instruction (instruction cpu mmu)
  (let* ((operand (aref (instruction-operands instruction) 0))
         (value (get-value operand 1 cpu mmu)))
    (setf (A cpu) (logand (- (A cpu) value) #xFF))
    (if (= (A cpu) 0)
        (setf (zero-flag cpu) 1))))

(defun handle-dec-instruction (instruction cpu mmu)
  (let* ((operand (aref (instruction-operands instruction) 0))
         (value (get-value operand 1 cpu mmu)))
    (set-value (logand (- value 1) #xFF) operand 1 cpu mmu) ;; needs to check for 16 bit values here
    (if (= (funcall (symbol-function (operand-name operand)) cpu) 0)
        (setf (zero-flag cpu) 1))))


(defun rotate-left (integer amount)
  (rotate-byte amount (byte 8 0) integer))

(defun rotate-right (integer amount)
  (rotate-left integer (- amount)))

(defun handle-rla-instruction (cpu)
  (setf (A cpu) (rotate-left (A cpu) 1)))

(defun handle-rra-instruction (cpu)
  (setf (A cpu) (rotate-right (A cpu) 1)))

(defun handle-and-instruction (instruction cpu mmu)
  (let ((value (get-value (aref (instruction-operands instruction) 0) 1 cpu mmu)))
    (setf (A cpu) (logand (A cpu) value))))

(defun handle-or-instruction (instruction cpu mmu)
  (let ((value (get-value (aref (instruction-operands instruction) 0) 1 cpu mmu)))
    (setf (A cpu) (logior (A cpu) value))))

(defun handle-xor-instruction (instruction cpu mmu)
  (let ((value (get-value (aref (instruction-operands instruction) 0) 1 cpu mmu)))
    (setf (A cpu) (logxor (A cpu) value))))

(defun read-immediate (immediate)
  (parse-integer (subseq (string immediate) 1)))

(defun handle-set-instruction (instruction cpu mmu)
  (let ((position (read-immediate
                   (operand-name (aref (instruction-operands instruction) 0))))
        (operand (aref (instruction-operands instruction) 1)))
    (set-value (dpb 1 (byte 1 position) (get-value operand 1 cpu mmu)) operand 1 cpu mmu)))

(defun handle-res-instruction (instruction cpu mmu)
  (let ((position (read-immediate
                   (operand-name (aref (instruction-operands instruction) 0))))
        (operand (aref (instruction-operands instruction) 1)))
    (set-value (dpb 0 (byte 1 position) (get-value operand 1 cpu mmu)) operand 1 cpu mmu)))


(defun handle-bit-instruction (insutrction cpu mmu))

(defun handle-swap-instruction (instruction cpu mmu)
  (let* ((operand (aref (instruction-operands instruction) 0))
         (value (get-value operand 1 cpu mmu))
         (new-value value))
    (setf new-value (dpb (ldb (byte 4 0) value) (byte 4 4) new-value))
    (setf new-value (dpb (ldb (byte 4 4) value) (byte 4 0) new-value))
    (set-value new-value operand 1 cpu mmu)))

(defun handle-push-instruction (instruction cpu mmu)
  (let ((operand (aref (instruction-operands instruction) 0)))
    (op-push (operand-name operand) cpu mmu)))

(defun handle-pop-instruction (instruction cpu mmu)
  (let* ((operand (aref (instruction-operands instruction) 0))
         (value (read-word-mmu (SP cpu) mmu)))
    (incf (SP cpu) 2)
    (set-value value operand 1 cpu mmu)))


(defun execute-instruction (instruction cpu mmu)
  (print (describe cpu))
  (print (describe instruction))
  (cond
    ((eq (instruction-mnemonic instruction) 'NOP)
     ;; NOP
     (next-instruction instruction cpu))
    ((eq (instruction-mnemonic instruction) 'LD)
     ;; LD
     (handle-ld-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
     ;; JP
    ((eq (instruction-mnemonic instruction) 'JP)
     (handle-jp-instruction instruction cpu mmu))
    ((eq (instruction-mnemonic instruction) 'JR)
     (handle-jr-instruction instruction cpu mmu))
    ((eq (instruction-mnemonic instruction) 'INC)
     ;; INC
     (handle-inc-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
    ((eq (instruction-mnemonic instruction) 'DEC)
     ;; DEC
     (handle-dec-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
    ((eq (instruction-mnemonic instruction) 'ADD)
     ;; ADD
     (handle-add-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
    ((eq (instruction-mnemonic instruction) 'SUB)
     ;; SUB
     (handle-sub-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
    ((or (eq (instruction-mnemonic instruction) 'RLCA)
         (eq (instruction-mnemonic instruction) 'RLA))
     ;; RL
     (handle-rla-instruction cpu)
     (next-instruction instruction cpu))
    ((or (eq (instruction-mnemonic instruction) 'RRCA)
         (eq (instruction-mnemonic instruction) 'RRA))
     ;; RR
     (handle-rra-instruction cpu)
     (next-instruction instruction cpu))
    ((eq (instruction-mnemonic instruction) 'CPL)
     ;; CPL
     (setf (A cpu) (logxor #xFF (A cpu)))
     (next-instruction instruction cpu))
    ((eq (instruction-mnemonic instruction) 'AND)
     ;; AND
     (handle-and-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
    ((eq (instruction-mnemonic instruction) 'OR)
     ;; OR
     (handle-or-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
    ((eq (instruction-mnemonic instruction) 'XOR)
     ;; XOR
     (handle-xor-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
    ((eq (instruction-mnemonic instruction) 'PREFIX)
     ;; PREFIX
     (next-instruction instruction cpu)
     (execute-instruction (fetch-instruction cpu mmu t) cpu mmu))
    ((eq (instruction-mnemonic instruction) 'SET)
     ;; SET
     (handle-set-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
    ((eq (instruction-mnemonic instruction) 'RES)
     ;; RES
     (handle-res-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
    ((eq (instruction-mnemonic instruction) 'BIT)
     ;; BIT
     (handle-bit-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
    ((eq (instruction-mnemonic instruction) 'SWAP)
     ;; SWAP
     (handle-swap-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
    ((eq (instruction-mnemonic instruction) 'PUSH)
     ;; PUSH
     (handle-push-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
    ((eq (instruction-mnemonic instruction) 'POP)
     (handle-pop-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
    (t (break) (error "Unhandled opcode")))
  ;; Handle (un)set flags
  (if (eq (instruction-zero instruction) 'SET)
      (setf (zero-flag cpu) 1))
  (if (eq (instruction-zero instruction) 'UNSET)
      (setf (zero-flag cpu) 0))
  (if (eq (instruction-subtraction instruction) 'SET)
      (setf (subtraction-flag cpu) 1))
  (if (eq (instruction-subtraction instruction) 'UNSET)
      (setf (subtraction-flag cpu) 0))
  (if (eq (instruction-half-carry instruction) 'SET)
      (setf (half-carry-flag cpu) 1))
  (if (eq (instruction-half-carry instruction) 'UNSET)
      (setf (half-carry-flag cpu) 0))
  (if (eq (instruction-carry instruction) 'SET)
      (setf (zero-flag cpu) 1))
  (if (eq (instruction-carry instruction) 'UNSET)
      (setf (zero-flag cpu) 0)))



(defun step-cpu (cpu mmu)
  (execute-instruction (fetch-instruction cpu mmu) cpu mmu))

(defun run-cpu (cpu mmu )
  (reset cpu)
  (loop  for i from 0
     do (step-cpu cpu mmu)
     while (not (= (PC cpu) #x021b))))
