-- -*- coding: utf-8 -*-
------------------------------------------------------
-------------------------------------------------------
-- DEFINIÇÕES

type Elemento = Int
type Valor = Int
type Tamanho = Int
type IndiceColuna = Int
type IndiceLinha = Int
type Coordenada = (IndiceLinha,IndiceColuna)
type Linha = [Elemento]
type Matriz = [Linha]

------------------------------------------------------
-------------------------------------------------------
-- MATRIZES TESTE
matriz:: Int -> Matriz
matriz 0 = [[1,2,3], [4,5,6], [7,8,9]]
matriz 1 = [[9,8,7], [6,5,4], [3,2,1]]
matriz n = []

------------------------------------------------------
-------------------------------------------------------
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

decrementar_linha::  Matriz -> IndiceLinha -> Matriz
decrementar_linha (a:b) 0 = [[w | x <- a, let w = x -1]] ++ b
decrementar_linha (a:b) n = [a] ++ (decrementar_linha b (n-1))

------------------------------------------------------
-------------------------------------------------------
-- EXECUÇÃO

main = do
    print(decrementar_linha (matriz 0) 3)