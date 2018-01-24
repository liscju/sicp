#lang sicp

; serialize exchange gets into critical section
; and tries to do operation that tries to get
; into the same critical section - this causes
; deadlock (assuming it is not reentrant lock)