;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lista1-MarcosLuizKurthReckers-A) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;; Marcos Luiz Kurth Reckers ;;;;;;;;;;;
;;;;;;;;;;;; Lista de exerćıcios 1 ;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 1 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Item a

(/(+ 5 3)4)

;; Item b

(-(sqrt 9)1)

;; Item c

(+(sin(/ pi 2))1)

;; Item d

(* (/ 1 12)(-(sqrt(expt 5 4))(log e)))

;; Item e

(/(+ -4 (sqrt(-(sqr 4)(* 4 2 -16)))) (* 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 2 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(define (area-circulo raio)
;;  (* pi (raio * raio)))

;;(define (volume-cilindro r a)
;;  (* (area-Circulo raio) a))

(define (area-Circulo raio)
  (* pi (* raio raio))
)

;; "EXEMPLO TESTE" (area-Circulo 2)

(define (volume-Cilindro raio a)
  (* (area-Circulo raio) a)
)

;; "EXEMPLO TESTE" (volume-Cilindro 2 2)

;; 1º erro: A função "area-Circulo" estava escrita
;; com "c" maiusculo em uma e com "C" minusculo na
;; outra.

;; 2º erro: Ora chamou a variavel do raio de "raio"
;; ora de "r".

;; 3º erro: Na libguagem Racket o chamado da função
;; sempre vem antes das variáveis na segunda linha
;; do programa (raio * raio) deve-se escrever
;; (* raio raio)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 3 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bhaskara a b c)
  (/(+ (- b) (sqrt(-(sqr b)(* 4 a c)))) (* 2 a))
)

;; "EXEMPLO TESTE" (bhaskara 2 4 -16)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; Exercício 4 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (calcula-TMB p a i f)
  (*(-(+ 66.5 (* 13.75 p)(* 5 a))(* 6.8 i)) f)
)

;; "EXEMPLO TESTE"  (calcula-TMB 70 170 18 1.3)