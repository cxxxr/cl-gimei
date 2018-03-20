(defpackage :cl-gimei/test
  (:use :cl :cl-gimei :prove))
(in-package :cl-gimei/test)

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

(defun test (str test-fn)
  (if (funcall test-fn str)
      (pass str)
      (fail str)))

(defun katakana-p (c)
  (<= #x30A2 (char-code c) #x30F3))

(defun hiragana-p (c)
  (<= #x3042 (char-code c) #x3093))

(defun kanji-p (c)
  ;(<= #x4E00 (char-code c) #x9FFF)
  t)

(defun kanji-str-p (str)
  (every #'kanji-p str))

(defun hiragana-str-p (str)
  (every #'hiragana-p str))

(defun katakana-str-p (str)
  (every #'katakana-p str))

(defun test-name (name test-fn &optional (map-test-fn #'every))
  (let ((space-pos (position #\space name)))
    (let ((last (subseq name 0 space-pos))
          (first (subseq name (1+ space-pos))))
      (and (funcall map-test-fn test-fn last)
           (funcall map-test-fn test-fn first)))))

(defun kanji-name-p (name)
  (test-name name #'kanji-p #'every))

(defun hiragana-name-p (name)
  (test-name name #'hiragana-p))

(defun katakana-name-p (name)
  (test-name name #'katakana-p))

(subtest "name.kanji"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (test (kanji name) #'kanji-name-p))))

(subtest "name.hiragana"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (test (hiragana name) #'hiragana-name-p))))

(subtest "name.katakana"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (test (katakana name) #'katakana-name-p))))

(subtest "name.first.kanji"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (test (kanji (first-name name)) #'kanji-str-p))))

(subtest "name.first.hiragana"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (test (hiragana (first-name name)) #'hiragana-str-p))))

(subtest "name.first.katakana"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (test (katakana (first-name name)) #'katakana-str-p))))

(subtest "name.last.kanji"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (test (kanji (last-name name)) #'kanji-str-p))))

(subtest "name.last.hiragana"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (test (hiragana (last-name name)) #'hiragana-str-p))))

(subtest "name.last.katakana"
  (loop :repeat 100 :do
        (let ((name (make-name)))
          (test (katakana (last-name name)) #'katakana-str-p))))

(subtest "address.kanji"
  (loop :repeat 100 :do
        (let ((address (make-address)))
          (test (kanji address) #'kanji-str-p))))

(subtest "address.hiragana"
  (loop :repeat 100 :do
        (let ((address (make-address)))
          (test (hiragana address) #'hiragana-str-p))))

(subtest "address.katakana"
  (loop :repeat 100 :do
        (let ((address (make-address)))
          (test (katakana address) #'katakana-str-p))))

(finalize)
