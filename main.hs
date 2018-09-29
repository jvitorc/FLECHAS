-- -*- coding: utf-8 -*-
------------------------------------------------------
------------------------------------------------------
-- DEFINIÇÕES

type Elemento = Int
type Valor = Int
type Tamanho = Int
type IndiceColuna = Int
type IndiceLinha = Int
type Coordenada = (IndiceLinha,IndiceColuna)
type Linha = [Elemento]
type Matriz = [Linha]
type Direcao = Int -- 1 Diagonal Cima - 2 Lado - 3 Diagonal Baixo
------------------------------------------------------
------------------------------------------------------
-- MATRIZES TESTE
matriz:: Int -> Matriz
matriz 0 = [[1,2,3], [4,5,6], [7,8,9]]
matriz 1 = [[1,2,1], [1,0,0], [1,0,0]]
matriz 2 = [[1,1,1], [0,0,0], [2,1,1]]
matriz 3 = [[0,0,0], [0,0,0], [0,0,0]]
matriz n = []
------------------------------------------------------
------------------------------------------------------
-- OPERACOES BASICAS

retorna_elemento_matriz:: Matriz -> Coordenada -> Elemento
retorna_elemento_matriz (a:_) (0,j) = retorna_elemento_linha a j 
retorna_elemento_matriz (a:b) (i,j) = retorna_elemento_matriz b ((i-1), j)

retorna_elemento_linha:: Linha -> IndiceColuna -> Elemento
retorna_elemento_linha (a:b) 0 = a
retorna_elemento_linha (a:b) i = retorna_elemento_linha b (i-1)

tamanho::[t] -> Tamanho
tamanho [] = 0
tamanho (a:b) = 1 + (tamanho b)

alterar_matriz:: Matriz -> Coordenada -> Elemento -> Matriz
alterar_matriz (a:b) (0,j) elemento = [(alterar_linha a j elemento)] ++ b
alterar_matriz (a:b) (i,j) elemento = [a] ++ (alterar_matriz b (i-1,j) elemento)

alterar_linha:: Linha -> IndiceColuna -> Elemento -> Linha
alterar_linha (a:b) 0 elemento = [elemento] ++ b
alterar_linha (a:b) n elemento = [a] ++ (alterar_linha b (n-1) elemento)

------------------------------------------------------
------------------------------------------------------
-- DECREMENTAR

decrementar:: Matriz -> IndiceLinha -> Direcao -> Matriz
decrementar m i 0 = m
decrementar m i 1 = decrementar_diagonal m (i,0) 1
decrementar m i 2 = decrementar_linha m i
decrementar m i 3 = decrementar_diagonal m (i,0) 0

decrementar_elemento:: Matriz -> Coordenada -> Matriz
decrementar_elemento (a:b) (0,j) = [(f a j)] ++ b
    where
        f (a:b) 0 = [a-1] ++ b
        f (a:b) j = [a] ++ (f b (j-1))
decrementar_elemento (a:b) (i,j) = [a] ++ (decrementar_elemento b (i-1,j))

decrementar_linha::  Matriz -> IndiceLinha -> Matriz
decrementar_linha (a:b) 0 = [[w | x <- a, let w = x -1]] ++ b
decrement'ar_linha (a:b) n = [a] ++ (decrementar_linha b (n-1))

decrementar_diagonal:: Matriz -> Coordenada -> Direcao -> Matriz
decrementar_diagonal [] _ _ = []
decrementar_diagonal m cord 0 = f m cord
    where
        f m (0,j) = decrementar_elemento m (0,j)
        f m (i,j) = f (decrementar_elemento m (i,j)) (i-1,j+1)
decrementar_diagonal m cord n = f m cord ((tamanho m)-1)
    where
        f m (i,j) n | i == n =  decrementar_elemento m (i,j)
                    | otherwise = f (decrementar_elemento m (i,j)) (i+1,j+1) n

------------------------------------------------------
------------------------------------------------------
-- ROTACIONAR MATRIZ
rotacionar_matriz:: Matriz -> Matriz
rotacionar_matriz []     = []
rotacionar_matriz ([]:_) = []
rotacionar_matriz (h:t)  = (map last (h:t)):(rotacionar_matriz (map init (h:t)))

------------------------------------------------------
------------------------------------------------------
-- VERIFICACOES

numero_negativo:: Matriz -> Bool
numero_negativo [] = True
numero_negativo (a:b) | [w | w <- a, w < 0]  == [] = numero_negativo b
                      | otherwise = False

matriz_zeros:: Matriz -> Bool
matriz_zeros [] = True
matriz_zeros (a:b) | [w | w <- a, w /= 0]  == [] = matriz_zeros b
                   | otherwise = False

------------------------------------------------------
------------------------------------------------------
-- RESOLVER


------------------------------------------------------
------------------------------------------------------
-- EXECUTACAO

main = do
    print(matriz 0)
    print(rotacionar_matriz (matriz 0))
    print(rotacionar_matriz (rotacionar_matriz (matriz 0)))
    print(rotacionar_matriz (rotacionar_matriz (rotacionar_matriz (matriz 0))))
    print(rotacionar_matriz (rotacionar_matriz (rotacionar_matriz (rotacionar_matriz (matriz 0)))))
    print("")
    print(decrementar (matriz 3) 0 0)
    print(decrementar (matriz 3) 0 1)
    print(decrementar (matriz 3) 0 2)
    print(decrementar (matriz 3) 0 3)
