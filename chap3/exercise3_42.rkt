#lang sicp

; Quote from http://community.schemewiki.org/?sicp-ex-3.42
;
;  It's safe to do that change. There is nothing different about
; concurrency in these two version. The only difference is new one
; serialize the procedures before call the functions, but the original
; one do it when call withdraw or deposit.  