#lang sicp

; N = 5
; pairs = {(E;16), (D;8), (C;4), (B;2), (A;1)}
; pairs = {(E;16), (D;8), (C;4), (tree (B;2) (A;1))}
; pairs = {(E;16), (D;8), (tree (C;4) (tree (B;2) (A;1)))}
; pairs = {(E;16), (tree (D;8) (tree (C;4) (tree (B;2) (A;1))))}
; tree  = (tree (E;16) (tree (D;8) (tree (C;4) (tree (B;2) (A;1)))))

; Smallest symbol encoding => 1
; Largest symbol encoding => 5

; In General:
; Smallest symbol encoding in N = > 1
; Highest symbol encoding in N => N
