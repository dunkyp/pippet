(in-package :cl-user)

(defpackage #:pippet
  (:nicknames #:pippet/cpu)
  (:use #:cl
        #:pippet/instruction)
  (:export
   #:gameboy
   #:make-mmu
   #:get-value
   #:set-value
   #:A #:B #:C #:D #:E
   #:F #:H #:SP #:PC
   #:AF #:BC #:DE #:HL
   #:get-value #:set-value
   #:step-cpu #:run-cpu))

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
     (SP '(unsigned-byte 16))
     (PC '(unsigned-byte 16))))

(defun make-mmu (length)
  (make-array length :element-type '(unsigned-byte 8)))

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

(defun set-bit (bit set cpu)
  (if set
      (setf (F cpu) (logior (ash 1 bit) (F cpu)))
      (setf (F cpu) (logandc1 (ash 1 bit) (F cpu)))))

(defun zero (cpu) (logbitp 7 (F cpu)))
(defun (setf zero) (set cpu)
  (set-bit 7 set cpu))
(defun subtraction (cpu) (logbitp 6 (F cpu)))
(defun (setf subtraction) (set cpu)
  (set-bit 6 set cpu))
(defun half-carry (cpu) (logbitp 5 (F cpu)))
(defun (setf half-carry) (set cpu)
  (set-bit 5 set cpu))
(defun carry (cpu) (logbitp 4 (F cpu)))
(defun (setf carry) (set cpu)
  (set-bit 4 set cpu))

(defun write-byte-mmu (byte offset mmu)
  (setf (aref mmu offset) byte))

(defun write-word-mmu (word offset mmu)
  (setf (aref mmu offset) (ldb (byte 8 8) word))
  (setf (aref mmu (1+ offset)) (ldb (byte 8 0) word)))

(defun read-byte-mmu (offset mmu)
  (aref mmu offset))

(defun read-word-mmu (offset mmu)
  (dpb (aref mmu offset) (byte 16 8) (aref mmu (+ offset 1))))

(defun op-push (location cpu mmu)
  (setf (SP cpu) (- (SP cpu) 2))
  (write-word-mmu (funcall (symbol-function location) cpu)
                  (SP cpu) mmu))

(defun op-pop (location cpu mmu)
  (let ((value (read-word-mmu (SP cpu) mmu)))
    (incf (SP cpu) 2)
    `(setf (,(symbol-function location) cpu) ,value)))

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
      (t (setf (slot-value cpu (operand-name operand)) value)))))

(defun fetch-instruction (cpu mmu)
  (decode-instruction (aref mmu (PC cpu))))

(defun next-instruction (instruction cpu)
  (incf (PC cpu) (instruction-bytes instruction)))

(defun handle-ld-instruction (instruction cpu mmu)
  (let* ((operands (instruction-operands instruction))
         (source-operand (aref operands 1))
         (destination-operand (aref operands 0))
         (source-value (get-value source-operand (+ 1 (operand-bytes destination-operand)) cpu mmu)))
    (set-value source-value destination-operand 1 cpu mmu)))

(defun handle-jp-instruction (instruction cpu mmu)
  (setf (PC cpu) (get-value (aref (instruction-operands instruction) 0) 1 cpu mmu)))

(defun handle-jr-instruction (instruction cpu mmu)
  (incf (PC cpu) (get-value (aref (instruction-operands instruction) 0) 1 cpu mmu)))

(defun handle-inc-instruction (instruction cpu mmu)
  (let* ((operand (aref (instruction-operands instruction) 0))
         (value (get-value operand 1 cpu mmu)))
    (set-value (+ 1 value) operand 1 cpu mmu)))

(defun handle-add-instruction (instruction cpu mmu)
  (let* ((operand (aref (instruction-operands instruction) 1))
         (value (get-value operand 1 cpu mmu)))
    (incf (A cpu) value)))

(defun handle-sub-instruction (instruction cpu mmu)
  (let* ((operand (aref (instruction-operands instruction) 0))
         (value (get-value operand 1 cpu mmu)))
    (decf (A cpu) value)))

(defun handle-dec-instruction (instruction cpu mmu)
  (let* ((operand (aref (instruction-operands instruction) 0))
         (value (get-value operand 1 cpu mmu)))
    (set-value (- value 1) operand 1 cpu mmu)))

(defun execute-instruction (instruction cpu mmu)
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
     (handle-sub-instruction instruction cpu mmu)
     (next-instruction instruction cpu))
    (t (error "Unhandled opcode"))))

(defun step-cpu (cpu mmu)
  (execute-instruction (fetch-instruction cpu mmu) cpu mmu))

(defun run-cpu (cpu mmu)
  (reset cpu)
  (loop do (step-cpu cpu mmu)))

(defun load-rom (filename mmu)
  (let ((stream (open filename :element-type '(unsigned-byte 8))))
    (read-sequence mmu stream)))
