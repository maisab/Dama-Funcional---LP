import Data.List
import Data.Function
import Data.Char

tabuleiroInicial = [['p', '0', 'p', '0', 'p', '0', 'p', '0'],
                             ['0', 'p', '0', 'p', '0', 'p', '0', 'p'],
                             ['p', '0', 'p', '0', 'p', '0', 'p', '0'],
                             ['0', '1', '0', '1', '0', '1', '0', '1'],
                             ['1', '0', '1', '0', '1', '0', '1', '0'],
                             ['0', 'c', '0', 'c', '0', 'c', '0', 'c'],
                             ['c', '0', 'c', '0', 'c', '0', 'c', '0'],
                             ['0', 'c', '0', 'c', '0', 'c', '0', 'c']]

tabuleiro          = [['p', '0', 'p', '0', 'p', '0', 'p', '0'],
                             ['0', 'p', '0', 'p', '0', 'p', '0', 'p'],
                             ['p', '0', 'p', '0', 'p', '0', 'p', '0'],
                             ['0', '1', '0', '1', '0', '1', '0', '1'],
                             ['1', '0', '1', '0', '1', '0', '1', '0'],
                             ['0', 'c', '0', 'c', '0', 'c', '0', 'c'],
                             ['c', '0', 'c', '0', 'c', '0', 'c', '0'],
                             ['0', 'c', '0', 'c', '0', 'c', '0', 'c']]

--pecas p sao do jogador
--pecas c sao do computador

-- -------encontrar posição --------------------
--encontraPosicao :: [[Char]] -> Int -> Int
encontraPosicao tabuleiro linha coluna =
    encontraColuna coluna 0 (encontraLinha tabuleiro linha 0)

encontraLinha tabuleiro linha pos  = do -- percorre as linhas até achar a linha certa
    if (linha == pos)
        then head tabuleiro
    else
        encontraLinha (tail tabuleiro) linha (pos + 1)

encontraColuna coluna pos linha = do -- percorre até achar a coluna certa na linha
    if (coluna == pos)
        --then ('1' : tail linha)
        then  head linha
    else
        encontraColuna coluna (pos + 1) (tail linha)

-- -------troca posição --------------------
--trocaPosicao :: ([[Char]] -> [[]] -> Int -> Int -> Char)-> [[Char]]
trocaPosicao tabuleiro novoTabuleiro linhaAtual colunaAtual caracter =
   trocaLinha tabuleiro [[ ]] linhaAtual colunaAtual caracter

trocaLinha tabuleiro novoTabuleiro linhaAtual colunaAtual contLinha caracter =
    if (linhaAtual == contLinha)
        then  (novoTabuleiro ++ (trocaColuna [(head tabuleiro)] [ ] colunaAtual 0 caracter) ++ (tail tabuleiro))
    else
        trocaLinha (tail tabuleiro) (novoTabuleiro ++ [(head tabuleiro)]) linhaAtual colunaAtual (contLinha + 1) caracter

trocaColuna listaLinha listaNova colunaAtual contColuna caracter =
    if (colunaAtual == contColuna)
        then (listaNova ++ caracter ++ tail listaLinha)
    else trocaColuna (tail listaLinha) (listaNova ++ [(head listaLinha)]) colunaAtual (contColuna + 1) caracter


-- printar matriz ----------------------------------------

--printaMatriz tabuleiro =
--    printaColuna 0 (printaLinha tabuleiro 0)

--printaLinha tabuleiro posL  = do -- percorre as linhas até achar a linha certa
--    if ( (length tabuleiro) /= posL)
--        then printaLinha (tail tabuleiro) (posL + 1)
--    else
--        putStrLn "Acabou Tabuleiro"

--printaColuna posC linhaLista = do -- percorre até achar a coluna certa na linha
--    if ((length linhaLista) /= posC)
--        --then ('1' : tail linha)
--        then
--            putStrLn (linhaLista !! posC)
--            --putStrLn (head linha)
--            printaColuna (posC + 1)  (tail linhaLista)
--    else
--        putStrLn "Acabou linha  "

-- -------verificar posição ---------------------
verificaPosicaoPeca board linhaAtual colunaAtual linhaDestino colunaDestino turno nPecas= do

    --verificar o numero de pecas
    --ir incrementando o turno

    if (turno == 0) then --se for a vez do jogador
        if ( (encontraPosicao board linhaAtual colunaAtual) == 'p')  then -- se existe uma peca na posicao atual
            if ( (linhaDestino == (linhaAtual + 1)) && (colunaDestino  ==  (colunaAtual + 1)) ) then --se for uma casa possivel para direita
                case (encontraPosicao board linhaDestino colunaDestino) of
                    '1' ->
                        putStrLn "-- trocando peca, casa vazia --" --mover  peca para direita e colocar 1 na posicao antiga
                        --verificaPosicaoPeca (trocaPosicao board [[ ]] linhaAtual colunaAtual caracter)

                    'b' -> putStrLn "yay!" --comer peca a direita
                    'B' -> putStrLn "yay!"--comer peca a direita
                    _ -> putStrLn "Nao e uma casa valida!"--comer peca a direita

            else if ( (linhaDestino == (linhaAtual + 1)) && (colunaDestino  ==  (colunaAtual - 1))) then  --se for uma casa possivel para esquerda
                case (encontraPosicao board linhaDestino colunaDestino) of
                    '1' -> putStrLn "yay!" --mover  peca para direita e colocar 1 na posicao antiga
                    'b' -> putStrLn "yay!" --comer peca a direita
                    'B' -> putStrLn "yay!"--comer peca a direita
                    _ -> putStrLn "Nao e uma casa valida!"--comer peca a direita

            else
                   putStrLn "Nao e uma casa valida!"
                      --nao é uma jogada possivel
        else
            putStrLn "Nao e uma peca valida!"

    else
         putStrLn "Nao e sua vez!"

-- ------- main ---------------------
main = do
            --print
            --printaMatriz (trocaPosicao tabuleiro [[ ]] 7 7 's') 0
            print(encontraPosicao tabuleiro 7 7)
            --print(trocaPosicao tabuleiro [[ ]] 6 5 't')
