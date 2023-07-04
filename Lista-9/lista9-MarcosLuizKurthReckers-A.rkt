;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lista9-MarcosLuizKurthReckers-A) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #t)))
;; Nome: Marcos Luiz Kurth Reckers 00315653

;; ==============================================================
;; 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
;; ==============================================================

;; Um elemento do conjunto Data é um elemento do cjto. NumerosInteiros

;; d1<=d2? : Data Data -> Boolean
;; Objetivos : dadas 2 datas (apenas o ano), verifica se a primeira é menor ou igual a segunda

;; Exemplos :
;: (d1<=d2? 2014 2014) = true
;; (d1<=d2? 2014 2013) = false

;; Corpo da função :
(define (d1<=d2? d1 d2)
         (cond
           [(<= d1 d2) true]
           [else false]
          )
 )

;; Testes :
   (check-expect(d1<=d2? 2014 2014)true)
   (check-expect(d1<=d2? 2014 2015)true)
   (check-expect(d1<=d2? 2014 2013)false)

;; ==============================================================
;; 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
;; ==============================================================

(define-struct filho (pai mãe nome data olhos))
; Um elemento nó de um conjunto Nó (de uma árvore genealógica) é:
; empty, representando a falta de informação ou
; (make-filho p m n d o), onde:
; p: Filho, representa o pai.
; m: Filho, representa a mãe.
; n: String, representa o nome desse filho.
; d: Número, representa o ano de nascimento.
; o: String, representa a cor dos olhos desse filho.

(define Althea (make-filho empty empty "Althea" 1915 "brown"))
(define Jack (make-filho empty empty "Jack" 1948 "brown"))
(define Judy (make-filho empty Althea "Judy" 1945 "green"))
(define Monica (make-filho Jack Judy "Monica" 1968 "blue"))
(define Ross (make-filho Jack Judy "Ross" 1966 "brown"))
(define Sandra (make-filho empty empty "Sandra" 1947 "brown"))
(define Leonard (make-filho empty empty "Leonard" 1947 "brown"))
(define Rachel (make-filho Leonard Sandra "Rachel" 1969 "blue"))
(define Nora (make-filho empty empty "Nora" 1948 "blue"))
(define Charles (make-filho empty empty "Charles" 1948 "blue"))
(define Chandler (make-filho Charles Nora "Chandler" 1966 "blue"))
(define GloriaTribbiani (make-filho empty empty "GloriaTribbiani" 1950 "brown"))
(define MrTribbiani (make-filho empty empty "MrTribbiani" 1949 "brown"))
(define Joey (make-filho MrTribbiani GloriaTribbiani "Joey" 1969 "brown"))
(define Frank (make-filho empty empty "Frank" 1940 "brown"))
(define LilyBuffay (make-filho empty empty "LilyBuffay" 1940 "blue"))
(define Phoebe (make-filho Frank LilyBuffay "Phoebe" 1965 "blue"))
(define Carol (make-filho empty empty "Carol" 1965 "blue"))
(define Ben (make-filho Ross Carol "Ben" 1994 "blue"))
(define Emma (make-filho Ross Rachel "Emma" 2002 "blue"))

;; ==============================================================
;; 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 
;; ==============================================================

;; maisIdoso : Filho -> Filho
;; Objetivo : Dado um nó de uma árvore genealógica, devolve o ancestral mais idoso.

;; Exemplos :
;;  (maisIdoso Emma) = Althea
;;  (maisIdoso Ben) = Althea

;; Corpo da função :
(define(maisIdoso f)
         (cond 

           [(and
              (empty? (filho-pai f))
              (empty? (filho-mãe f))
             )
             f
            ]     

           [(and
              (empty? (filho-pai f))
              (not(empty? (filho-mãe f)))
             )
             (filho-mãe f)
            ]

           [(and
              (empty? (filho-mãe f))
              (not(empty? (filho-pai f)))
             )
             (filho-pai f)
            ] 

           [else
            (cond

              [(d1<=d2?
                 (filho-data (maisIdoso(filho-pai f)))
                 (filho-data (maisIdoso(filho-mãe f)))
                )
                (maisIdoso (filho-pai f))
               ]
            
              [(d1<=d2?
                 (filho-data (maisIdoso(filho-mãe f)))
                 (filho-data (maisIdoso(filho-pai f)))
                )
                (maisIdoso (filho-mãe f))
               ]
             )
            ]
          )
 )
 
;; Testes :
(maisIdoso Ben)
(maisIdoso Emma)
(maisIdoso Chandler)
(maisIdoso Althea)

;; ==============================================================
;; 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 
;; ==============================================================

(define-struct nó (id conteúdo esq dir))
;; Um elemento do conjunto ABP (Árvore Binária de Pesquisa) pode ser
;; 1. empty , representando a falta de informação, ou
;; 2. (make-nó id c e d)
;; onde:
;; id : Número, representa o identificador do nó
;; c : String, representa o conteúdo do nó
;; e : ABP, representa a sub-árvore da esquerda e contém apenas nós com identificadores menores que id
;; d : ABP, representa a sub-árvore da direita contém apenas nós com identificadores maiores que id






;; é-abp? ; 

