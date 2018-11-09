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
type Direcao = Int 
type Lado = Int

{- DIREÇÕES
LESTE:
NE      = 1 [1-n]
LESTE   = 2 [0-n]
SE      = 3 [0-(n-1)] 
n = tamanho da matriz - 1
LADO:
DIREITA = 0
CIMA    = 1
ESQUERDA= 2
BAIXO   = 3
-}

------------------------------------------------------
------------------------------------------------------
-- MATRIZES TESTE
matriz:: Int -> Matriz
matriz 0 = [[0,0,0], [0,0,0], [0,0,0]]
matriz 1 = [[5,2,5], [3,0,1], [4,3,4]]
matriz 2 = [[0,0], [0,0]]
matriz 3 = [[4,3,3,0], [7,3,3,2], [5,3,3,2], [3,1,3,0]]
matriz n = []
------------------------------------------------------
------------------------------------------------------
-- OPERACOES BASICAS

-- Retorna a ordem da matriz quadrada

tamanho::[t] -> Tamanho
tamanho [] = 0
tamanho (a:b) = 1 + (tamanho b)

-- Altera o elemento da posição (i,j) para o elemento passado como parametro

alterar_matriz:: Matriz -> Coordenada -> Elemento -> Matriz
alterar_matriz (a:b) (0,j) elemento = [(alterar_linha a j elemento)] ++ b
alterar_matriz (a:b) (i,j) elemento = [a] ++ (alterar_matriz b (i-1,j) elemento)

-- Altera o elemento de uma linha I de uma coluna J, para um elemento E passado como parametro

alterar_linha:: Linha -> IndiceColuna -> Elemento -> Linha
alterar_linha (a:b) 0 elemento = [elemento] ++ b
alterar_linha (a:b) n elemento = [a] ++ (alterar_linha b (n-1) elemento)

------------------------------------------------------
------------------------------------------------------
-- DECREMENTAR

-- Subtrai em 1 todos os valores de uma linha ou diagonal passada como parametro

decrementar:: Matriz -> IndiceLinha -> Direcao -> Matriz
decrementar m i 0 = m
decrementar m i 1 = decrementar_diagonal m (i-1,0) 0
decrementar m i 2 = decrementar_linha m i
decrementar m i 3 = decrementar_diagonal m (i+1,0) 1

-- Subtrai 1 de uma coordenada da matriz

decrementar_elemento:: Matriz -> Coordenada -> Matriz
decrementar_elemento (a:b) (0,j) = [(f a j)] ++ b
    where
        f (a:b) 0 = [a-1] ++ b
        f (a:b) j = [a] ++ (f b (j-1))
decrementar_elemento (a:b) (i,j) = [a] ++ (decrementar_elemento b (i-1,j))

-- Subtrai 1 de todos os elementos de uma linha da matriz

decrementar_linha::  Matriz -> IndiceLinha -> Matriz
decrementar_linha (a:b) 0 = [[w | x <- a, let w = x -1]] ++ b
decrementar_linha (a:b) n = [a] ++ (decrementar_linha b (n-1))

-- Subtrai 1 de todos os elementos da diagonal com direção sudeste ou nordeste

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

-- Faz a rotação da matriz para esquerda

rotacionar_matriz:: Matriz -> Matriz
rotacionar_matriz []     = []
rotacionar_matriz ([]:_) = []
rotacionar_matriz (h:t)  = (map last (h:t)):(rotacionar_matriz (map init (h:t)))

------------------------------------------------------
------------------------------------------------------
-- VERIFICACOES

-- Retorna se a matriz possui todos os elementos ZEROs

matriz_zeros:: Matriz -> Bool
matriz_zeros [] = True
matriz_zeros (a:b) | [w | w <- a, w /= 0]  == [] = matriz_zeros b
                   | otherwise = False


-- Rotorna se a matriz possui um número negativo

numero_negativo:: Matriz -> Bool
numero_negativo [] = False
numero_negativo (a:b) | [w | w <- a, w < 0]  == [] = numero_negativo b
                      | otherwise = True

------------------------------------------------------
------------------------------------------------------
-- RESOLVER


-- Retorna o conjunto de coordenadas respectivo aos lados da matriz.

resolver:: Matriz -> [[[Char]]]
resolver [] = []
resolver m = traducao (inicializacao m (tamanho m)) 0
    where
        -- Cria um vetor de flechas para o lado esquerdo da matriz com a primeira flecha na direção leste

        inicializacao m n = primeiro m ([2] ++ [w | x <- [1..(n-1)], let w = 0]) 0 2 n 0 []
    
        -- Preenche o vetor com direções das flechas, e decrementando os valores que a flecha aponta na matriz
        -- Se matriz tiver um valor negativo, discarta essa direcao e tenta a proxima
        -- Se conseguir preencher um lado inteiro com flechas, rotaciona a matriz e tenta o proximo
        -- Se conseguir preencher os 3 primeiros lados da matriz chama a função para resolver o ultimo lado

        primeiro m vetor indice direcao n seq resposta
            | m == [] = []
            | (numero_negativo m) = []
            | (indice == n) && (seq > 1) = ultimo (rotacionar_matriz m) ([2]++[w | x <- [1..(n-1)],
                                                             let w = 0]) 0 2 n (resposta ++ [vetor])
            | (indice == n) = primeiro (rotacionar_matriz m) ([2]++[w | x <- [1..(n-1)], let w = 0])
                                                                 0 2 n (seq+1) (resposta ++ [vetor])
            | (indice == (n-1)) && (direcao > 2) = []
            | direcao > 3 = []
            | (primeiro (decrementar m indice direcao) vetor (indice+1) 1 n seq resposta)
                                         /= [] = primeiro (decrementar m indice direcao) 
                                         (alterar_linha vetor indice direcao) (indice+1) 1 n seq resposta
            | otherwise = primeiro m (alterar_linha vetor indice (direcao+1)) indice (direcao+1) n seq resposta

        -- Preenche o vetor da flechas para o ultimo lado da matriz
        -- E verifica se ganhou quando preencher a ultima posição do vetor

        ultimo m vetor indice direcao n resposta
            | m == [] = []
            | (numero_negativo m) = []
	        | indice == n = verificar m (resposta ++ [vetor])
            | (indice == (n-1)) && (direcao > 2) = []
            | direcao > 3 = []
            | ultimo (decrementar m indice direcao) vetor (indice+1) 1 n resposta /= [] = ultimo 
                            (decrementar m indice direcao) (alterar_linha vetor indice direcao)
                                                                        (indice+1) 1 n resposta
            | otherwise = ultimo m (alterar_linha vetor indice (direcao+1)) indice (direcao+1) n resposta

        -- Verifica se só possui zeros na matriz
        -- Se sim é porque ganhou retornando o vetor com as direções
        -- Se não retorna o vetor vazio
        
        verificar m vetor
            | matriz_zeros m  = vetor
            | otherwise = []


-- Inverte uma linha

inverter::Linha -> Linha
inverter [] = []
inverter (a:b) = (inverter b) ++ [a]

-- Faz a tradução dos números 1, 2 e 3 para as respectivas direções

traducao:: Matriz -> Lado -> [[[Char]]]
traducao [] _ = []
traducao (a:b) 0 = [(f a)] ++ (traducao b 1)
        where
            f [] = []
            f (1:b) = ["NE"] ++ (f b)
            f (2:b) = ["E"] ++ (f b)
            f (3:b) = ["SE"] ++ (f b)

traducao (a:b) 1 = [(f (inverter a))] ++ (traducao b 2)
    where
        f [] = []
        f (1:b) = ["SE"] ++ (f b)
        f (2:b) = ["S"] ++ (f b)
        f (3:b) = ["SW"] ++ (f b)

traducao (a:b) 2 = [(f (inverter a))] ++ (traducao b 3)
    where
        f [] = []
        f (1:b) = ["SW"] ++ (f b)
        f (2:b) = ["W"] ++ (f b)
        f (3:b) = ["NW"] ++ (f b)

traducao (a:b) 3 = [(f a)]
    where
        f [] = []
        f (1:b) = ["NW"] ++ (f b)
        f (2:b) = ["N"] ++ (f b)
        f (3:b) = ["NE"] ++ (f b)

------------------------------------------------------
------------------------------------------------------
-- EXECUTACAO

main = do
    print("RESOLVER - Exemplo entrada 1,2,3: https://br.spoj.com/problems/FLECHAS/")
    print("")
    print(matriz 1)
    print(resolver(matriz 1))
    print("")
    print(matriz 2)
    print(resolver(matriz 2))
    print("")
    print(matriz 3)
    print(resolver(matriz 3))