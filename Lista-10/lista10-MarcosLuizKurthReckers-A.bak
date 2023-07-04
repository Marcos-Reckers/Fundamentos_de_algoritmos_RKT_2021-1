;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lista10-MarcosLuizKurthReckers-A) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #t)))
;; Nome: Marcos Luiz Kurth Reckers 00315653

;; ============================================
;; DEFINIÇÕES DE TIPOS DE DADOS
;; ============================================

;; ------------------
;; TIPO ARQUIVO:
;; ------------------
(define-struct arquivo (nome tamanho)) 
;; Um elemento do conjunto Arquivo tem o formato
;;  (make-arquivo n t), onde
;;    n: String, é o nome do arquivo
;;    t: Número, é o tamanho do arquivo, em Kb.

;; ------------------
;; TIPO CONTEUDO:
;; ------------------
;; Um Conteudo é
;; 1. empty,
;; 2. (cons a lc), onde a: Arquivo e lc: Conteudo
;; 3. (cons d lc), onde d: Diretorio e lc: Conteudo

;; ------------------
;; TIPO DIRETORIO:
;; ------------------
(define-struct diretorio (nome conteudo))
;; Um elemento do conjunto Diretorio tem o formato
;;  (make-diretorio n c), onde
;;    n: String, é o nome do diretório
;;    c: Conteudo, é o conteúdo do diretório

;; =========================================================================
;;                                 QUESTÃO 1
;; =========================================================================

(define PASTA-JOÂO
  (make-diretorio "João"(list
     (make-diretorio "Documentos" (list
                                    (make-arquivo "CPF.png" 100)
                                    (make-arquivo "RG.png" 100)
                                   )
      )
     (make-diretorio "Faculdade" (list
                                    (make-diretorio "Fundamentos" (list
                                                                    (make-arquivo "lista1-JoãoMarcosFlach-x.rkt" 50)
                                                                    (make-arquivo "lista2-JoãoMarcosFlach-x.rkt" 50)
                                                                    (make-arquivo "lista3-JoãoMarcosFlach-x.rkt" 50)
                                                                   )
                                    )
                                    (make-diretorio "Programação" empty)
                                    (make-diretorio "TCC" (list
                                                            (make-diretorio "Imagens" (list
                                                                                         (make-arquivo "algoritmo.png" 100)
                                                                                         (make-arquivo "resultados.png" 100)
                                                                                         (make-arquivo "testes.png" 100)
                                                                                        )
                                                             )
                                                            (make-arquivo "tcc.txt" 50)
                                                           )
                                    )
                                   )
      )
     (make-diretorio "Imagens" (list
                                 (make-arquivo "profile.png" 100)                                 
                                )
      )
     (make-diretorio "Músicas" (list
                                 (make-arquivo "unknow-artist-1.mp3" 2000)
                                 (make-arquivo "unknow-artist-2.mp3" 2000)
                                 (make-arquivo "unknow-artist-3.mp3" 2000)
                                 (make-arquivo "unknow-artist-4.mp3" 2000)
                                 (make-arquivo "unknow-artist-5.mp3" 2000)
                                )
      )
    )
  )
 )
;; =========================================================================
;;                                 QUESTÃO 2
;; =========================================================================
;; arquivo-no-dir? : Conteudo String -> Booleano
;; Objetivo : Dados o Conteúdo de um diretório e um nome de arquivo, verifica
;;            se existe um arquivo com este nome neste conteúdo, sem
;;            considerar subdiretórios. Retornando true caso tenha esse arquivo
;;            no diretótio, ou false caso não tenha.
;; Exemplos :



;; Corpo da função :
   (define (arquivo-no-dir? c n)
            (cond
              [(empty? c) false]
              [(and
                 (arquivo?(first c))
                 (string=?(arquivo-nome (first c)) n)                 
                )
                true
               ]
              [(not(empty? (rest c)))(arquivo-no-dir?(rest c) n)]
              [else false]
             )
     )
;; Testes :


;; =========================================================================
;;                                 QUESTÃO 3
;; =========================================================================
;; arquivo-encontrado? : Conteudo String -> Booleano
;; Objetivo : Dados o Conteúdo de um diretório e um nome de arquivo, verifica
;;            se existe um arquivo com este nome neste conteúdo, considerando
;;            subdiretórios. Retornando true caso tenha esse arquivo no
;;            diretótio, ou false caso não tenha.
;; Exemplos :
;;  
;;  

;; Corpo da função :
   (define (arquivo-encontrado? c n)
            (cond
              [(empty? c) false]
              [(arquivo-no-dir? c n)true]
              [else (arquivo-encontrado-sub? c n)]
             )
     )
;; Testes :
  

;;--------------------------------------------------------------------------
;; arquivo-encontrado-sub? : Conteudo String -> Booleano
;; Objetivo : Dados o Conteúdo de um diretório e um nome de arquivo, verifica
;;            se existe um arquivo com este nome neste conteúdo, considerando
;;            subdiretórios. Retornando true caso tenha esse arquivo no
;;            diretótio, ou false caso não tenha.
;; Exemplos :
;;  
;;  

;; Corpo da função :
   (define (arquivo-encontrado-sub? lc n)
            (cond
              [(empty? lc) false]
              [(diretorio? (first lc))
                    (or
                      (arquivo-encontrado? (diretorio-conteudo (first lc)) n)
                      (arquivo-encontrado-sub?(rest lc) n)
                     )
               ]
              [else (arquivo-encontrado-sub?(rest lc) n)]
             )
     )
;; Testes :

  

;; =========================================================================
;;                                 QUESTÃO 4
;; =========================================================================
;; calcula-tamanho : Diretorio -> Numero
;; Objetivo : Dado um Diretório, calcula o tamanho necessário em disco para
;;            armazenar este diretório. O tamanho é calculado da seguinte
;;            forma: para cada arquivo que faz parte do diretório, soma-se o
;;            tamanho do arquivo, e para cada diretório soma-se 10 (para
;;            guardar as informações sobre o diretório) ao tamanho de seu conteúdo.

;; Exemplos :
;;  
;;  

;; Corpo da função :
   (define (calcula-tamanho d)
            (+
              (foldl + 10 (map arquivo-tamanho (filter arquivo?(diretorio-conteudo d))))
              (calcula-tamanho-sub(filter diretorio? (diretorio-conteudo d)))
             )
    )
;; Testes :

  

;;--------------------------------------------------------------------------
;; calcula-tamanho-sub : Conteudo -> Numero
;; Objetivo : Dado uma lista de subdiretórios de um diretório, devolve o tamanho
;;            necessário em disco para armazenar estes subdiretórios. 
;; Exemplos :
;;  
;;  

;; Corpo da função :
   (define (calcula-tamanho-sub ld)
            (cond
              [(empty? ld) 0]
              [(diretorio? (first ld))
                    (+
                      (calcula-tamanho (first ld))
                      (calcula-tamanho-sub (rest ld))
                     )
               ]
              [else (calcula-tamanho-sub (rest ld))]
             )
     )
;; Testes :





;; =========================================================================
;;                                 QUESTÃO 5
;; =========================================================================
;; mostra-caminho : String Diterorio -> String
;; Objetivo : Dado um nome de arquivo e um diretório, mostra o caminho para
;;            encontrar esse arquivo, ou uma mensagem dizendo que o arquivo
;;            não esta nesse diretório.
;; Exemplos :
;;  
;;  

;; Corpo da função :
   (define (mostra-caminho n d)
             (cond
               [(not(arquivo-encontrado? (diretorio-conteudo d) n))"Arquivo não encontrado"]
               [else(string-append
                     (mostra-caminho n (rest(diretorio-conteudo d)))
                     (arquivo-no-dir? (diretorio-conteudo d) n)(string-append(diretorio-nome d)"->" n))]))
               
                                                                 
                                                        
          
;; Testes :



