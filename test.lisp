(defpackage :cl-gimei-test
  (:use :cl :cl-gimei :prove))
(in-package :cl-gimei-test)

(plan nil)

(subtest "male is male"
  (loop :repeat 100 :do
        (let ((name (make-male)))
          (if (and (malep name)
                   (not (femalep name)))
              (pass (kanji name))
              (fail (kanji name))))))

(subtest "female is female"
  (loop :repeat 100 :do
        (let ((name (make-female)))
          (if (and (femalep name)
                   (not (malep name)))
              (pass (kanji name))
              (fail (kanji name))))))

(subtest "name is male or female"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (if (or (malep name)
                  (femalep name))
              (pass (kanji name))
              (fail (kanji name))))))

(defun hiragana-p (c)
  (<= #x3042 (char-code c) #x3093))

(defun katakana-p (c)
  (<= #x30A2 (char-code c) #x30F3))

(defun kanji-p (c)
  (<= #x4E00 (char-code c) #x9FFF))

(defun test-name (name test-fn &optional (map-test-fn #'every))
  (let ((space-pos (position #\space name)))
    (let ((last (subseq name 0 space-pos))
          (first (subseq name (1+ space-pos))))
      (and (funcall map-test-fn test-fn last)
           (funcall map-test-fn test-fn first)))))

(subtest "name.kanji"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (if (test-name (kanji name) #'kanji-p #'some)
              (pass (kanji name))
              (fail (kanji name))))))

(subtest "name.hiragana"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (if (test-name (hiragana name) #'hiragana-p)
              (pass (hiragana name))
              (fail (hiragana name))))))

(subtest "name.katakana"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (if (test-name (katakana name) #'katakana-p)
              (pass (katakana name))
              (fail (katakana name))))))

(subtest "name.first.kanji"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (if (some #'kanji-p (kanji (first-name name)))
              (pass (kanji name))
              (fail (kanji name))))))

(subtest "name.first.hiragana"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (if (every #'hiragana-p (hiragana (first-name name)))
              (pass (kanji name))
              (fail (kanji name))))))

(subtest "name.first.katakana"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (if (every #'katakana-p (katakana (first-name name)))
              (pass (kanji name))
              (fail (kanji name))))))

(subtest "name.last.kanji"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (if (some #'kanji-p (kanji (last-name name)))
              (pass (kanji name))
              (fail (kanji name))))))

(subtest "name.last.hiragana"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (if (every #'hiragana-p (hiragana (last-name name)))
              (pass (kanji name))
              (fail (kanji name))))))

(subtest "name.last.katakana"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (if (every #'katakana-p (katakana (last-name name)))
              (pass (kanji name))
              (fail (kanji name))))))

(subtest "address.kanji"
  (loop :repeat 100 :do
        (let ((address (make-address)))
          (if (some #'kanji-p (kanji address))
              (pass (kanji address))
              (fail (kanji address))))))

(subtest "address.hiragana"
  (loop :repeat 100 :do
        (let ((address (make-address)))
          (if (every #'hiragana-p (hiragana address))
              (pass (hiragana address))
              (fail (hiragana address))))))

(subtest "address.katakana"
  (loop :repeat 100 :do
        (let ((address (make-address)))
          (if (every #'katakana-p (katakana address))
              (pass (katakana address))
              (fail (katakana address))))))

(finalize)
