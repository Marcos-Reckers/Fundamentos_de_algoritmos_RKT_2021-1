;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lista7-MarcosLuizKurthReckers-A |) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; ========================================================================
;;                        DEFINIÇÕES DE DADOS
;; ========================================================================  

;; CONSTANTES:
(define PULA_VEZ -1)
(define COMPRA2 -2)
(define INVERTE -3)
(define CURINGA -5)
(define CURINGA_COMPRA4 -4)

;; -----------------
;; TIPO CARTA:
;; -----------------
(define-struct carta (cor valor))
;; Um elemento do conjunto Carta é
;;   (make-carta c v)     onde
;;   c : String, é a cor da carta, que pode ser "azul", "verde", "amarelo", "vermelho" ou "preto" ou "livre"
;;   v : Número, é o valor da carta, que pode ser qualquer inteiro entre 0 e 9,
;;               ou um número negativo -1 (PulaVez), -2 (Compra2), -3 (Inverte),-4 (Compra4) ou -5 (Curinga)

;; --------------------
;; TIPO LISTA DE CARTAS:
;; --------------------
;; Uma ListaDeCartas é
;; 1. vazia (empty), ou
;; 2. (cons c lc), onde 
;;        c: Carta;
;;       lc: ListaDeCartas

;; --------------------
;; TIPO LISTA DE NUMEROS:
;; --------------------
;; Uma ListaDeNumeros é
;; 1. vazia (empty), ou
;; 2. (cons n ln), onde 
;;        n: Numero;
;;       ln: ListaDeNumeros

(define lista1 (cons(make-carta "amarelo" 12)(cons(make-carta "amarelo" 11)(cons(make-carta "amarelo" 10)(cons(make-carta "amarelo" 9)(cons(make-carta "amarelo" 8)(cons(make-carta "amarelo" 7)(cons(make-carta "amarelo" 6)(cons(make-carta "amarelo" 5)(cons(make-carta "amarelo" 4) (cons(make-carta "amarelo" 3)(cons(make-carta "amarelo" 2)(cons(make-carta "amarelo" 1)empty)))))))))))))
 

;; =========================================================================
;;                                 QUESTÃO 1
;; =========================================================================
;; cria-cartas : String ListaDeNumeros -> ListaDeCartas
;; Objetivo : Dada uma cor e uma lista de numeros, retorna uma lista de cartas de UNO.

;; Exemplos :
;; (cria-cartas "vermelho" (cons 0
;;                          (cons 1
;;                           (cons 3
;;                            (cons 5
;;                             (cons 7
;;                              (cons 9
;;                                empty)))))))-> (cons
;;                                                (make-carta "vermelho" 0)
;;                                                 (cons
;;                                                  (make-carta "vermelho" 1)
;;                                                  (cons
;;                                                   (make-carta "vermelho" 3)
;;                                                   (cons
;;                                                    (make-carta "vermelho" 5)
;;                                                    (cons
;;                                                     (make-carta "vermelho" 7)
;;                                                      (cons (make-carta "vermelho" 9) '()))))))

;; (cria-cartas "amarelo" (cons 0
;;                         (cons 2
;;                          (cons 4
;;                           (cons 6
;;                            (cons 8
;;                             (cons COMPRA2
;;                              empty))))))) -> (cons
;;                                               (make-carta "amarelo" 0)
;;                                               (cons
;;                                                (make-carta "amarelo" 2)
;;                                                (cons
;;                                                 (make-carta "amarelo" 4)
;;                                                 (cons
;;                                                  (make-carta "amarelo" 6)
;;                                                  (cons
;;                                                   (make-carta "amarelo" 8)
;;                                                   (cons (make-carta "amarelo" -2) '()))))))

;; (cria-cartas "azul" (cons INVERTE
;;                      (cons COMPRA2
;;                       (cons PULA_VEZ
;;                        (cons 5
;;                         (cons 7
;;                          (cons 0
;;                           empty))))))) -> (cons
;;                                            (make-carta "azul" -3)
;;                                            (cons
;;                                             (make-carta "azul" -2)
;;                                             (cons
;;                                              (make-carta "azul" -1)
;;                                              (cons
;;                                               (make-carta "azul" 5)
;;                                               (cons
;;                                                (make-carta "azul" 7)
;;                                                (cons (make-carta "azul" 0) '()))))))


;; Corpo da função :
  (define (cria-cartas c ln)
           (cond
             ;; Se a lista estiver vazia, retorna uma lista vazia.
             [(empty? ln) empty]

             ;; Se não constrói uma carta com o primeiro número da lista e reinicia a função
             ;; para o resto da lista de numeros
             [else (cons (make-carta c (first ln)) (cria-cartas c (rest ln)))]
            )
   )

;; Testes :
   (check-expect(cria-cartas "vermelho"(cons 0(cons 1(cons 3(cons 5(cons 7(cons 9 empty)))))))
                 (cons(make-carta "vermelho" 0)(cons(make-carta "vermelho" 1)(cons(make-carta "vermelho" 3)(cons(make-carta "vermelho" 5)(cons(make-carta "vermelho" 7)(cons (make-carta "vermelho" 9) '()))))))
    )
                 
   (check-expect(cria-cartas "amarelo" (cons 0 (cons 2 (cons 4 (cons 6 (cons 8 (cons COMPRA2 empty)))))))
                (cons(make-carta "amarelo" 0)(cons(make-carta "amarelo" 2)(cons(make-carta "amarelo" 4)(cons(make-carta "amarelo" 6)(cons(make-carta "amarelo" 8)(cons (make-carta "amarelo" -2) '()))))))
    )

   (check-expect(cria-cartas "azul" (cons INVERTE (cons COMPRA2 (cons PULA_VEZ (cons 5 (cons 7 (cons 0 empty)))))))
                (cons(make-carta "azul" -3)(cons(make-carta "azul" -2)(cons(make-carta "azul" -1)(cons(make-carta "azul" 5)(cons(make-carta "azul" 7)(cons (make-carta "azul" 0) '()))))))
    )
        
;; =========================================================================
;;                                 QUESTÃO 2
;; =========================================================================
;; seleciona-cartas : ListaDeCartas String -> ListaDeCartas
;; Objetivo : Dada uma lista de cartas e uma string representando uma cor, retorna uma lista de
;;            cartas contendo as cartas da lista de entrada que possuem a cor especificada.

;; Exemplos :
;;   (seleciona-cartas (cons(make-carta "azul" -3)(cons(make-carta "azul" -2)(cons(make-carta "azul" -1)(cons(make-carta "azul" 5)(cons(make-carta "azul" 7)(cons (make-carta "azul" 0) empty)))))) "vermelho")
;;    '()

;; (seleciona-cartas (cons(make-carta "azul" -3)(cons(make-carta "verde" -2)(cons(make-carta "azul" -1)(cons(make-carta "azul" 5)(cons(make-carta "azul" 7)(cons (make-carta "verde" 0) empty)))))) "verde")
;;  (cons(make-carta "verde" -2)(cons(make-carta "verde" 0)'()))

;; (seleciona-cartas (cons(make-carta "azul" -3)(cons(make-carta "azul" -2)(cons(make-carta "azul" -1)(cons(make-carta "azul" 5)(cons(make-carta "azul" 7)(cons (make-carta "azul" 0) empty)))))) "azul")
;;  (cons(make-carta "azul" -3)(cons(make-carta "azul" -2)(cons(make-carta "azul" -1)(cons(make-carta "azul" 5)(cons(make-carta "azul" 7)(cons(make-carta "azul" 0) '()))))))

;; Corpo da função :
  (define (seleciona-cartas lc c)
    (cond
      ;; Se a lista estiver vazia, retorna uma lista vazia.
      [(empty? lc) empty]

      ;; Se a primeira carta for da cor solicitada começa a montar uma lista com ela e segue a função para o resto da lista
      [(string=? (carta-cor (first lc)) c) (cons (first lc) (seleciona-cartas (rest lc) c))]

      ;; Se a primeira nao for da cor solicitada repete a função para o resto da lista descartando a primeira carta.
      [else (seleciona-cartas (rest lc) c)]
     )
   )

;; Testes :
   (check-expect(seleciona-cartas (cons(make-carta "azul" -3)(cons(make-carta "azul" -2)(cons(make-carta "azul" -1)(cons(make-carta "azul" 5)(cons(make-carta "azul" 7)(cons (make-carta "azul" 0) empty)))))) "vermelho")
                 '()
    )

   (check-expect(seleciona-cartas (cons(make-carta "azul" -3)(cons(make-carta "verde" -2)(cons(make-carta "azul" -1)(cons(make-carta "azul" 5)(cons(make-carta "azul" 7)(cons (make-carta "verde" 0) empty)))))) "verde")
                (cons(make-carta "verde" -2)(cons(make-carta "verde" 0)'()))
    )

   (check-expect(seleciona-cartas (cons(make-carta "azul" -3)(cons(make-carta "azul" -2)(cons(make-carta "azul" -1)(cons(make-carta "azul" 5)(cons(make-carta "azul" 7)(cons (make-carta "azul" 0) empty)))))) "azul")
                (cons(make-carta "azul" -3)(cons(make-carta "azul" -2)(cons(make-carta "azul" -1)(cons(make-carta "azul" 5)(cons(make-carta "azul" 7)(cons(make-carta "azul" 0) '()))))))
    )

;; =========================================================================
;;                                 QUESTÃO 3
;; =========================================================================
;; carta=? : Carta Carta -> Booleano
;; Objetivo : Dadas duas cartas e compara cada um dos atributos das cartas dadas, retornando #true se forem
;;            iguais e #false caso contrário.

;; Exemplos :
;; (carta=?(make-carta "azul" -3)(make-carta "azul" -2)) -> #false
;; (carta=?(make-carta "azul" -3)(make-carta "azul" -3)) -> #true
;; (carta=?(make-carta "amarelo" -3)(make-carta "azul" -3)) -> #false

;; Corpo da função :
   (define (carta=? carta1 carta2)
            (cond

              ;; Se a cor e o valor das duas cartas forem iguais retorna true.
              [(and
                 (string=?(carta-cor carta1)(carta-cor carta2))
                 (=(carta-valor carta1)(carta-valor carta2))
                ) true
               ]

              ;; Se não retorna false.
              [else false]
             )
    )

;; Testes : 
   (check-expect(carta=?(make-carta "azul" -3)(make-carta "azul" -2)) #false)
   (check-expect(carta=?(make-carta "azul" -3)(make-carta "azul" -3)) #true)
   (check-expect(carta=?(make-carta "amarelo" -3)(make-carta "azul" -3)) #false)

;------------------------------------------------------------------------------------
;; carta-repetida? : Carta ListaDeCartas -> booleano
;; Objetivo : Dada uma carta e uma lista de cartas, verifica se a carta está repetida na lista,
;;            retornando #true caso a carta esteja repetida ou #false se for a única.

;; Exemplos :
;; (carta-repetida? (make-carta "verde" 2)(cons(make-carta "verde" 5)(cons(make-carta "verde" 2)(cons (make-carta "verde" -2)(cons(make-carta "vermelho" 2) empty))))) -> #true
;; (carta-repetida? (make-carta "verde" 2)(cons(make-carta "verde" 5)(cons(make-carta "azul" 2)(cons(make-carta "verde" -2)(cons(make-carta "vermelho" 2) empty))))) -> #false

;; Corpo da função :
   (define (carta-repetida? c lc)
            (cond
              
              ;;Se a lista for vazia, retorna false.
              [(empty? lc) false]

              ;; Se a primeira carta da lista for igual a carta em questão, retorna true.
              [(carta=? (first lc)c)true]

              ;; Se não roda a função para o resto da lista.
              [else (carta-repetida? c (rest lc))]
              )
    )

;; Testes :
   (check-expect(carta-repetida? (make-carta "verde" 2)(cons(make-carta "verde" 5)(cons(make-carta "verde" 2)(cons (make-carta "verde" -2)(cons(make-carta "vermelho" 2) empty))))) true)
   (check-expect(carta-repetida? (make-carta "verde" 2)(cons(make-carta "verde" 5)(cons(make-carta "azul" 2)(cons(make-carta "verde" -2)(cons(make-carta "vermelho" 2) empty))))) false)
 
;------------------------------------------------------------------------------------
;; remove-repetidas : ListaDeCartas -> ListaDeCartas
;; Objetivo : Dada uma lista de cartas, retorna uma lista com as cartas da lista de entrada que n~ao se repetem.

;; Exemplos :
;; (remove-repetidas(cons(make-carta "verde" 2)(cons(make-carta "verde" 1)(cons(make-carta "verde" 2)empty)))) -> (cons(make-carta "verde" 1)(cons(make-carta "verde" 2)'()))
;; (remove-repetidas(cons(make-carta "verde" 2)(cons(make-carta "verde" 1)(cons(make-carta "verde" 4)empty)))) -> (cons(make-carta "verde" 2)(cons(make-carta "verde" 1)(cons(make-carta "verde" 4)'())))

;; Corpo da função :
    (define (remove-repetidas lc)
            (cond
              
              ;;Se a lista for vazia, retorna uma lista vazia.
              [(empty? lc) empty]

              ;; Se a primeira carta da lista for repetida, roda a função para o resto da lista excluindo então a repetição.
              [(carta-repetida? (first lc)(rest lc))(remove-repetidas (rest lc))]

              ;; Se não constrói uma lista com a primeira carta da lista e roda a função para o resto da lista.
              [else
               (cons(first lc)(remove-repetidas (rest lc)))
               ]
              )
    )

;; Testes :
   (check-expect(remove-repetidas(cons(make-carta "verde" 2)(cons(make-carta "verde" 1)(cons(make-carta "verde" 2)empty))))(cons(make-carta "verde" 1)(cons(make-carta "verde" 2)'())))
   (check-expect(remove-repetidas(cons(make-carta "verde" 2)(cons(make-carta "verde" 1)(cons(make-carta "verde" 4)empty))))(cons(make-carta "verde" 2)(cons(make-carta "verde" 1)(cons(make-carta "verde" 4)'()))))

;; =========================================================================
;;                                 QUESTÃO 4
;; =========================================================================

;; Documentação da função random, que já existe no Racket, que deve ser usada
;; na resolução do exercício:

;; random: Número -> Número
;; obj: Dado um número inteiro k, retorna um número inteiro aleatório, 
;; no intervalo [0, k-1]
;; ex:
;;   (random 1) -> 0
;;   (random 2) -> 0
;;   (random 2) -> 1
;;   (random 3) -> 2
;;   (random 1000) -> 42

;------------------------------------------------------------------------------------
;; quantas-cartas : ListaDeCartas -> Numero
;; Objetivo : dada uma lista de cartas, retorna quantas cartas existe nesta lista.

;; Exemplos :
;;   (quantas-cartas (cons(make-carta "verde" 2)(cons(make-carta "verde" 1)(cons(make-carta "verde" 2)empty)))) -> 3
;;   (quantas-cartas (cons(make-carta "verde" 2)(cons(make-carta "verde" 1)empty))) -> 2
;;   (quantas-cartas (cons(make-carta "amarelo" 6) (cons(make-carta "verde" 2)(cons(make-carta "verde" 1)(cons(make-carta "verde" 2)empty))))) -> 4

;; Corpo da função :
  (define (quantas-cartas lc)
           (cond
             ;; Se a lista for vazia, retorna 0
             [(empty? lc) 0]

             ;; Se não, soma 1 para cada carta da lista.
             [else (+ 1 (quantas-cartas (rest lc)))]
            )
    )

;; Testes : 
   (check-expect(quantas-cartas (cons(make-carta "verde" 2)(cons(make-carta "verde" 1)(cons(make-carta "verde" 2)empty)))) 3)        
   (check-expect(quantas-cartas (cons(make-carta "verde" 2)(cons(make-carta "verde" 1)empty))) 2)
   (check-expect(quantas-cartas (cons(make-carta "amarelo" 6) (cons(make-carta "verde" 2)(cons(make-carta "verde" 1)(cons(make-carta "verde" 2)empty))))) 4)

;------------------------------------------------------------------------------------
;; insere-carta : ListaDeCartas Carta Numero -> ListaDeCartas
;; Objetivo : Dada uma lista de cartas, uma carta e uma posição (p), insere a carta dada na lista dada na posição indicada.

;; Exemplos :

;; Corpo da função :
   (define (insere-carta lc c p)
            (cond
              ;; Se a lista for vazia então devolve uma lista somente com a carta inserida.
               [(empty? lc) (cons c empty)]
               ;; Se a posição escolhida for maior q o numero de cartas disponiveis, forma uma lista inserindo a carta na ultima posição.
               [(<(quantas-cartas lc)p)(cons c lc)]
               ;; Enquanto a quantidade de cartas não for uma posição a menos do que a posição (p) forma uma lista com as cartas em sequencia.
               [(not(=(- p 1) (quantas-cartas lc)))(cons (first lc)(insere-carta (rest lc) c p))]
              
             )
    )
            
;; Testes : 
   (check-expect(insere-carta (cons(make-carta "amarelo" 6) empty) (make-carta "verde" 5) 3)(cons(make-carta "verde" 5)(cons (make-carta "amarelo" 6) '())))
   (check-expect(insere-carta  empty (make-carta "verde" 5) 3)(cons (make-carta "verde" 5) '()))
   (check-expect(insere-carta (cons(make-carta "amarelo" 6)(cons(make-carta "verde" 2)(cons(make-carta "verde" 1)(cons(make-carta "azul" 2)empty))))(make-carta "verde" 5) 1)(cons(make-carta "amarelo" 6)(cons(make-carta "verde" 2)(cons(make-carta "verde" 1)(cons(make-carta "azul" 2)(cons (make-carta "verde" 5) '()))))))

;------------------------------------------------------------------------------------
;; embaralha : ListaDeCartas -> ListaDecartas
;; Objetivo : dada uma lista de cartas, retorna uma lista com as mesmas cartas, mas com suas posições embaralhadas.

;; Exemplos:
;(embaralha (cons(make-carta "amarelo" 12)(cons(make-carta "amarelo" 11)(cons(make-carta "amarelo" 10)(cons(make-carta "amarelo" 9)(cons(make-carta "amarelo" 8)(cons(make-carta "amarelo" 7)(cons(make-carta "amarelo" 6)(cons(make-carta "amarelo" 5)(cons(make-carta "amarelo" 4) (cons(make-carta "amarelo" 3)(cons(make-carta "amarelo" 2)(cons(make-carta "amarelo" 1)empty)))))))))))))
;(cons
; (make-carta "amarelo" 9)
; (cons
;  (make-carta "amarelo" 2)
;  (cons
;   (make-carta "amarelo" 1)
;   (cons
;    (make-carta "amarelo" 7)
;    (cons
;     (make-carta "amarelo" 5)
;     (cons
;      (make-carta "amarelo" 6)
;      (cons
;       (make-carta "amarelo" 3)
;       (cons
;        (make-carta "amarelo" 4)
;        (cons
;         (make-carta "amarelo" 12)
;         (cons
;          (make-carta "amarelo" 11)
;          (cons
;           (make-carta "amarelo" 8)
;           (cons
;            (make-carta "amarelo" 10) '()))))))))))))


;; Corpo da função :
   (define (embaralha lc)
            (cond
               ;;Se a lista for vazia, retorna uma lista vazia.
              [(empty? lc) empty]
               ;;Se o resto da lista for vazio, retorna a própria lista, já que chegou ao fim da lista.
              [(empty? (rest lc))lc]
              ;; Se não... insere a primeira carta em uma posição aleatória na lista de cartas ja embaralhada
              [else
               (insere-carta ;;insere a primeira carta em uma posição aleatória no resto da lista de cartas ja embaralhada
                (embaralha(insere-carta (rest(rest lc)) (first (rest lc))(random(quantas-cartas lc))));; embaralha o resto da lista
                (first lc)
                (random(quantas-cartas lc)))]
             )
    )
                
             
(embaralha lista1)































