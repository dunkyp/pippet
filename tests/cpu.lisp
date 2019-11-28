(in-package #:cl-user)
(defpackage #:pippet/tests/cpu
  (:use #:cl
        #:rove
        #:pippet/cpu
        #:pippet/instruction))
(in-package #:pippet/tests/cpu)

(defvar *CPU* (make-instance 'gameboy))
(defvar *MMU* (make-mmu 20))

(setup
  (setf (A *CPU*) 10)
  (setf (B *CPU*) 0)
  (setf (C *CPU*) 20)
  (setf (F *CPU*) 10)
  (setf (aref *MMU* 10) 20)
  (setf (aref *MMU* 11) 10))

(deftest get-value-direct
    (testing "get 8 bit value from register"
             (ok (= 10 (A *CPU*))))
    (testing "get 16 bit value from register"
             (ok (= 20 (BC *CPU*)))
      (ok (= 2570 (AF *CPU*))))
    (testing "get byte from mmu"
             (ok (= 20 (aref *MMU* 10)))))

(deftest nop-instruction
  (setf (aref *MMU* 0) 0)
  (setf (PC *CPU*) 0)
  (step-cpu *CPU* *MMU*)
  (ok (= (PC *CPU*) 1)))

(deftest ld-instruction
  (setf (PC *CPU*) 0)
  (setf (A *CPU*) 0)
  (setf (B *CPU*) 1)
  (step-cpu *CPU* (assemble "LD A B"))
  (ok (= (B *CPU*) 1))
  (ok (= (A *CPU*) (B *CPU*)))
  (ok (= (A *CPU*) 1))
  (ok (= (PC *CPU*) 1))
  (let ((mmu (assemble "LD (BC) A")))
    (setf (PC *CPU*) 0)
    (setf (BC *CPU*) 1)
    (ok (= (aref mmu (BC *CPU*)) 0))
    (step-cpu *CPU* mmu)
    (ok (= (aref mmu (BC *CPU*)) 1))))

(deftest inc-instruction
  (setf (PC *CPU*) 0)
  (setf (C *CPU*) 1)
  (step-cpu *CPU* (assemble "INC C"))
  (ok (= (PC *CPU*) 1))
  (ok (= (C *CPU*) 2)))

(deftest dec-instruction
  (setf (PC *CPU*) 0)
  (setf (D *CPU*) 1)
  (step-cpu *CPU* (assemble "DEC D"))
  (ok (= (PC *CPU*) 1))
  (ok (= (D *CPU*) 0)))

(deftest jp-instruction
  (setf (PC *CPU*) 0)
  (setf (HL *CPU*) 2)
  (step-cpu *CPU* (assemble "JP HL"))
  (ok (= (PC *CPU*) 2)))

(deftest add-instruction
  (setf (PC *CPU*) 0)
  (setf (A *CPU*) 1)
  (setf (B *CPU*) 5)
  (step-cpu *CPU* (assemble "ADD A B"))
  (ok (= (PC *CPU*) 1))
  (ok (= (A *CPU*) 6)))

(deftest sub-instruction
  (setf (PC *CPU*) 0)
  (setf (A *CPU*) 124)
  (setf (B *CPU*) 17)
  (step-cpu *CPU* (assemble "SUB B"))
  (ok (= (PC *CPU*) 1))
  (ok (= (A *CPU*) 107)))

(deftest rla-instruction
  (setf (PC *CPU*) 0)
  (setf (A *CPU*) 2)
  (step-cpu *CPU* (assemble "RLA"))
  (ok (= (PC *CPU*) 1))
  (ok (= (A *CPU*) 4)))
