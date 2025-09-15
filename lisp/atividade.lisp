; Instituto de Tecnologia e Liderança
; Paradigmas de Programação - M11
; Prof. Rafael Will M. de Araujo


(defun media-ponderada (a b c p1 p2 p3)
    ; Função que calcula a média ponderada de a, b e c.
    ; Deve retornar o resultado de: (a*p1 + b*p2 + c*p3) / (p1 + p2 + p3)

    (let ((soma1 (* a p1))
        (soma2 (* b p2))
        (soma3 (* c p3))
        (pesos (+ p1 (+ p2 p3))))
    (/ (+ soma1 (+ soma2 soma3)) pesos))
)


(defun distancia-3d (x1 y1 z1 x2 y2 z2)
    ; Função que calcula a distância euclidiana dos pontos: p1=(x1, y1, z1) e p2=(x2, y2, z2).
    ; Dica: você deve usar as funções sqrt (raiz quadrada) e expt (exponenciação).

    (let ((dx (- x2 x1))
        (dy (- y2 y1))
        (dz (- z2 z1)))
    (let ((q1 (expt dx 2))
          (q2 (expt dy 2))
          (q3 (expt dz 2)))
      (sqrt (+ q1 (+ q2 q3)))))
)


(defun fatorial (n)
    ; Função que calcula n!, onde n é um inteiro.
    ; Esta função não será avaliada, mas é necessária para a próxima função: e-elevado-x.
    ; Você pode usar a versão mostrada em sala de aula.

    (if (<= n 1)
    1
    (* n (fatorial (- n 1))))
)


(defun e-elevado-x (x n)
    ; Função que calcula a aproximação de e^x usando a série de Taylor até o termo n, onde n é inteiro.
    ; Isto é, sua função deve calcular: e^x = \sum_{n=0}^{\infty} \frac{x^n}{n!}
    ; mas limitando o último termo até n (ao invés de infinito).
    ; Dica: utilize a macro dotimes para iterar n vezes. Pode ser necessário criar
    ; variáveis locais utilizando let ou let*. Você pode utilizar as funções expt e definida (criada acima).

    (let ((soma 0.0))
    (dotimes (i n soma) ; agora roda de 0 até n-1
      (let ((num (expt x i))
            (den (fatorial i)))
        (setf soma (+ soma (/ num den))))))
)


(defun ultimo-elemento (lista)
    ; Função que retorna o último elemento de uma lista. Ou seja, se a lista for '(10 20 30), retorna 30.
    ; Note que a lista pode ter qualquer átomo em Lisp.
    ; Dica: implemente usando recursão. Utilize as funções:
    ; - eq, para comparar se a lista é vazia (nil);
    ; - car e cdr para obter o primeiro elemento da lista e o restante da lista.
    
    (if (null lista)
      nil
      (if (null (cdr lista))
          (car lista)
          (ultimo-elemento (cdr lista))))
)


(defun busca-binaria (vetor alvo inicio fim)
    ; Função que recebe um vetor de inteiros, o alvo (valor a ser buscado), o índice de inicio e fim do vetor, e
    ; realiza uma busca binária recursiva sobre este vetor. Deve retornar o índice onde o alvo foi encontrado,
    ; ou -1 caso o alvo não exista no vetor.
    ; Dica: implemente utilizando recursão. Será necessário criar variáveis locais com let ou let*, além das funções:
    ; - floor, que retorna a divisão inteira entre 2 valores;
    ; - aref que retorna o valor do i-ésimo elemento do vetor;
    ; Você pode usar cond para facilitar as várias comparações.
  
    (if (> inicio fim)
      -1
      (let* ((meio (floor (/ (+ inicio fim) 2)))
             (valor (aref vetor meio)))
        (cond
          ((= valor alvo) meio)
          ((< alvo valor) (busca-binaria vetor alvo inicio (- meio 1)))
          ((> alvo valor) (busca-binaria vetor alvo (+ meio 1) fim)))))
)


; ========================================================================================
; Testes: a partir deste ponto, NÃO EDITE O CÓDIGO ABAIXO!
;
; As suas funções serão chamadas e o resultado será impresso na tela.
; ========================================================================================


(defun teste-media-ponderada (qtd-acertos qtd-erros a b c p1 p2 p3 resultado tolerancia)
    (format t "  media-ponderada: (~D*~1,1F + ~D*~1,1F + ~D*~1,1F)/(~1,1F + ~1,1F + ~1,1F)," a p1 b p2 c p3 p1 p2 p3)
    (let* ((resultado-obtido (media-ponderada a b c p1 p2 p3)))   ; obtém o retorno da função
        (format t " retorno=~F, esperado=~F ..... " resultado-obtido resultado)   ; imprime se o retorno e o valor esperado
        (if (<= (abs (- resultado-obtido resultado)) tolerancia)   ; testa se: |resultado-obtido - resultado| <= tolerancia
            (progn (format t "PASS~C" #\linefeed) (values (+ qtd-acertos 1) qtd-erros))  ; executa o format e, em seguida, retorna 2 valores
            (progn (format t "FAIL~C" #\linefeed) (values qtd-acertos (+ qtd-erros 1)))
        )
    )
)

(defun teste-distancia-3d (qtd-acertos qtd-erros x1 y1 z1 x2 y2 z2 resultado tolerancia)
    (format t "  teste-distancia-3d: p1=(~D,~D,~D), p2=(~D,~D,~D)," x1 y1 z1 x2 y2 z2)
    (let* ((resultado-obtido (distancia-3d x1 y1 z1 x2 y2 z2)))   ; obtém o retorno da função
        (format t " retorno=~F, esperado=~F ..... " resultado-obtido resultado)   ; imprime se o retorno e o valor esperado
        (if (<= (abs (- resultado-obtido resultado)) tolerancia)   ; testa se: |resultado-obtido - resultado| <= tolerancia
            (progn (format t "PASS~C" #\linefeed) (values (+ qtd-acertos 1) qtd-erros))  ; executa o format e, em seguida, retorna 2 valores
            (progn (format t "FAIL~C" #\linefeed) (values qtd-acertos (+ qtd-erros 1)))
        )
    )
)

(defun teste-e-elevado-x (qtd-acertos qtd-erros x n resultado tolerancia)
    (format t "  teste-e-elevado-x: x=~F, n=~D," x n)
    (let* ((resultado-obtido (e-elevado-x x n)))   ; obtém o retorno da função
        (format t " retorno=~F, esperado=~F ..... " resultado-obtido resultado)   ; imprime se o retorno e o valor esperado
        (if (<= (abs (- resultado-obtido resultado)) tolerancia)   ; testa se: |e^x - resultado| <= tolerancia
            (progn (format t "PASS~C" #\linefeed) (values (+ qtd-acertos 1) qtd-erros))
            (progn (format t "FAIL~C" #\linefeed) (values qtd-acertos (+ qtd-erros 1)))
        )
    )
)


(defun teste-ultimo-elemento (qtd-acertos qtd-erros lista resultado)
    (format t "  teste-ultimo-elemento: lista='(~:(~{~A~^, ~}~))," lista)
    (let* ((resultado-obtido (ultimo-elemento lista)))
        (format t " retorno=~A, esperado=~A ..... " resultado-obtido resultado)
        (if (eq (ultimo-elemento lista) resultado)
            (progn (format t "PASS~C" #\linefeed) (values (+ qtd-acertos 1) qtd-erros))
            (progn (format t "FAIL~C" #\linefeed) (values qtd-acertos (+ qtd-erros 1)))
        )
    )
)


(defun teste-busca-binaria (qtd-acertos qtd-erros vetor alvo resultado)
    (format t "  teste-busca-binaria: vetor=~:(~{~A~^, ~}~), alvo=~D ..... " (list vetor) alvo)
    (if (= (busca-binaria vetor alvo 0 (- (length vetor) 1)) resultado)
        (progn (format t "PASS~C" #\linefeed) (values (+ qtd-acertos 1) qtd-erros))
        (progn (format t "FAIL~C" #\linefeed) (values qtd-acertos (+ qtd-erros 1)))
    )
)

(defvar *acertos* 0)
(defvar *erros* 0)
(defvar *nota* 0.0)
(defvar *total-testes* 20)

(format t "Testando a função media-ponderada:~%")
(setf (values *acertos* *erros*) (teste-media-ponderada *acertos* *erros* 2 1 3 1 4 1 1.5 0.001)) ; recebe os 2 valores retornados pela função de teste
(setf (values *acertos* *erros*) (teste-media-ponderada *acertos* *erros* 2 0.5 3 1.0 4 1.5 1.3076923 0.001))
(setf (values *acertos* *erros*) (teste-media-ponderada *acertos* *erros* 10 .25 100 .5 1000 .75 0.32958803 0.001))

(format t "Testando a função distancia-3d:~%")
(setf (values *acertos* *erros*) (teste-distancia-3d *acertos* *erros* 2 2 2 2 2 2 0.0 0.001))
(setf (values *acertos* *erros*) (teste-distancia-3d *acertos* *erros* 1 0 0 0 1 0 1.4142135 0.001))
(setf (values *acertos* *erros*) (teste-distancia-3d *acertos* *erros* 1 1 1 2 2 2 1.7320508 0.001))


(format t "Testando a função e-elevado-x:~%")
(setf (values *acertos* *erros*) (teste-e-elevado-x *acertos* *erros* 2 1 1.0 0.001))
(setf (values *acertos* *erros*) (teste-e-elevado-x *acertos* *erros* 2 3 5.0 0.001))
(setf (values *acertos* *erros*) (teste-e-elevado-x *acertos* *erros* 2 8 7.380953 0.01))
(setf (values *acertos* *erros*) (teste-e-elevado-x *acertos* *erros* 10 10 10086.573 0.1))
(setf (values *acertos* *erros*) (teste-e-elevado-x *acertos* *erros* 10 100 22026.467 0.1))


(format t "Testando a função ultimo-elemento:~%")
(setf (values *acertos* *erros*) (teste-ultimo-elemento *acertos* *erros* nil nil))
(setf (values *acertos* *erros*) (teste-ultimo-elemento *acertos* *erros* '(10) 10))
(setf (values *acertos* *erros*) (teste-ultimo-elemento *acertos* *erros* '(10 20 30) 30))
(setf (values *acertos* *erros*) (teste-ultimo-elemento *acertos* *erros* '(Programar em Lisp) 'Lisp))


(format t "Testando a função teste-busca-binaria:~%")
(setf (values *acertos* *erros*) (teste-busca-binaria *acertos* *erros* #(10 20 30 40 50) 30 2))
(setf (values *acertos* *erros*) (teste-busca-binaria *acertos* *erros* #(1 10 20 30 40 50 60) 10 1))
(setf (values *acertos* *erros*) (teste-busca-binaria *acertos* *erros* #(3 20 66 89 300 301 303 500 502 999) 303 6))
(setf (values *acertos* *erros*) (teste-busca-binaria *acertos* *erros* #(10 20 30 40 50) 15 -1))
(setf (values *acertos* *erros*) (teste-busca-binaria *acertos* *erros* #(11 22 33 44 55) 56 -1))

(terpri)
(format t "Quantidade de acertos: ~D~%" *acertos*)
(format t "Quantidade de erros: ~D~%" *erros*)
(setf *nota* (* 10 (/ *acertos* *total-testes*)))
(format t "Nota final: ~1,1F~%" *nota*)
(format t "ATENÇÃO: a nota final é preliminar e pode sofrer decréscimo após a correção, dependendo da aderência do seu código ao que foi solicitado.~%")
(terpri)  ; quebra linha no final
