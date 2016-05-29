import Data.List
import Data.Function

tabuleiroInicial = [['p', '0', 'p', '0', 'p', '0', 'p', '0'],
                             ['0', 'p', '0', 'p', '0', 'p', '0', 'p'],
                             ['p', '0', 'p', '0', 'p', '0', 'p', '0'],
                             ['0', '1', '0', '1', '0', '1', '0', '1'],
                             ['1', '0', '1', '0', '1', '0', '1', '0'],
                             ['0', 'c', '0', 'c', '0', 'c', '0', 'c'],
                             ['c', '0', 'c', '0', 'c', '0', 'c', '0'],
                             ['0', 'c', '0', 'c', '0', 'c', '0', 'c']]

tabuleiro = [['p', '0', 'p', '0', 'p', '0', 'p', '0'],
                     ['0', 'p', '0', 'p', '0', 'p', '0', 'p'],
                     ['p', '0', 'p', '0', 'p', '0', 'p', '0'],
                     ['0', '1', '0', '1', '0', '1', '0', '1'],
                     ['1', '0', '1', '0', '1', '0', '1', '0'],
                     ['0', 'c', '0', 'c', '0', 'c', '0', 'c'],
                     ['c', '0', 'c', '0', 'c', '0', 'c', '0'],
                     ['0', 'c', '0', 'c', '0', 'c', '0', 'c']]

--pecas p sao do jogador
--pecas c sao do computador

encontraPosicao :: [[Char]] -> Int -> Int -> Char
encontraPosicao tabuleiro linha coluna =
    encontraColuna coluna 0 (encontraLinha tabuleiro linha 0) -- primeiro pega linha que é o parametro para achar a coluna


encontraLinha tabuleiro linha pos  = do -- percorre as linhas até achar a linha certa
    if (linha == pos)
        then head tabuleiro
    else
        encontraLinha (tail tabuleiro) linha (pos + 1)

encontraColuna coluna pos linha = do -- percorre até achar a coluna certa na linha
    if (coluna == pos)
        then head linha
    else
        encontraColuna coluna (pos + 1) (tail linha)

verificaPosicaoPeca tabuleiro linhaAtual colunaAtual linhaDestino colunaDestino turno = do

    if (turno == 0) then --se for a vez do jogador

        if ( (encontraPosicao tabuleiro linhaAtual colunaAtual) == 'p')  then -- se existe uma peca na posicao atual
            if ( (linhaDestino == (linhaAtual + 1)) && (colunaDestino  ==  (colunaDestino + 1)) ) then --se for uma casa possivel para direita

                case (encontraPosicao tabuleiro linhaDestino colunaDestino) of
                    '1' -> putStrLn "yay!" --mover  peca para direita e colocar 1 na posicao antiga
                    'b' -> putStrLn "yay!" --comer peca a direita
                    'B' -> putStrLn "yay!"--comer peca a direita
                    _ -> putStrLn "Nao e uma casa valida!"--comer peca a direita

                        --if( (encontraPosicao tabuleiro linhaDestino colunaDestino) == '1' ) then --se a casa esta livre
                        --        --precisa mover direita e colocar 1 na posição antiga

                        --else if( ((encontraPosicao tabuleiro linhaDestino colunaDestino) == 'b') || ((encontraPosicao tabuleiro linhaDestino colunaDestino) == 'B') ) then --se for uma peca do adversario a direita
                        --        --precisa comer a direita

                        --else
                        --        --nao é uma jogada possivel

            else if ( (linhaDestino == (linhaAtual + 1)) && (colunaDestino  ==  (colunaDestino - 1))) then  --se for uma casa possivel para esquerda

                case (encontraPosicao tabuleiro linhaDestino colunaDestino) of
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

main = print(encontraPosicao tabuleiro 7 7)
