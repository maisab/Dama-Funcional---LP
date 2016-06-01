import Data.List
import Data.Function
import Data.Char
import Control.Monad
import System.Random

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
encontraPosicao :: [[Char]] -> Int -> Int -> Char
encontraPosicao board linha coluna =
    encontraColuna coluna 0 (encontraLinha board linha 0)

encontraLinha board linha pos  = do -- percorre as linhas até achar a linha certa
    if (linha == pos)
        then head board
    else
        encontraLinha (tail board) linha (pos + 1)

encontraColuna coluna pos linha = do -- percorre até achar a coluna certa na linha
    if (coluna == pos)
        then  head linha
    else
        encontraColuna coluna (pos + 1) (tail linha)

-- -------troca posição-----------------------------------------------------------------------------
trocaPosicao (hd : ht) linha coluna contLinha contColuna char =
    if( linha == contLinha) then
        [(trocaColuna hd coluna contColuna char)] ++ ht --passa a lista da linha atual
    else
        hd : (trocaPosicao ht linha coluna (contLinha + 1) contColuna char) --salva a cabeça atual

trocaColuna (hd1 : ht1) coluna posColuna char =
    if(coluna == posColuna) then
        [char] ++ ht1 --tira a cabeça e concatena o caracter com a calda

    else
        hd1 : (trocaColuna ht1 coluna (posColuna + 1) char) --salva a cabeça atual

-- -------verificar posição ---------------------
realizaJogadas :: [[Char]] -> Int -> Int -> Int -> Int -> Int -> Int-> Int-> IO ()
realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino turno pecasJogador pecasComputador  = do

    putStrLn "Printar tabuleiro"
    print(board)

    if (pecasComputador == 0) then
        --print("Parabén você venceu !")
        return()

    else if (pecasJogador == 0) then
        --putStrLn "Você perdeu :("
        return()

    else
        if (turno == 0) then --se for a vez do jogador

            -- para direita
            if ( (encontraPosicao board linhaAtual colunaAtual) == 'p')  then -- se existe uma peca na posicao atual
                if ( (linhaDestino == (linhaAtual + 1)) && (colunaDestino  ==  (colunaAtual + 1))  && (linhaAtual < 7)  && (colunaAtual < 7) ) then --se for uma casa possivel para direita

                    if ((encontraPosicao board linhaDestino colunaDestino) == '1') then
                        if (colunaDestino == 7) then --vira dama
                            realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 'P') linhaAtual colunaAtual 0 0 '1') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador pecasComputador

                        else
                            realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 'p') linhaAtual colunaAtual 0 0 '1') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador pecasComputador

                    else if ((encontraPosicao board linhaDestino colunaDestino) == 'c') then
                        if ( (linhaDestino == 7) || ( colunaDestino == 7) ) then
                                --putStrLn ("Jogada Impossível")
                                realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino 0 pecasJogador pecasComputador

                        else --come a peca
                                realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 '1') (linhaDestino + 1) (colunaDestino + 1) 0 0 'p') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador (pecasComputador - 1)

                    else --posição inválida
                                realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino 0 pecasJogador pecasComputador

            -- para esquerda
                else if ( (linhaDestino == (linhaAtual + 1)) && (colunaDestino  ==  (colunaAtual - 1))  && (linhaAtual < 7)  && (colunaAtual > 0) ) then  --se for uma casa possivel para esquerda

                    if ((encontraPosicao board linhaDestino colunaDestino) == '1') then
                        if (colunaDestino == 7) then --vira dama
                            realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 'P') linhaAtual colunaAtual 0 0 '1') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador pecasComputador

                        else
                            realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 'p') linhaAtual colunaAtual 0 0 '1') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador pecasComputador

                    else if ((encontraPosicao board linhaDestino colunaDestino) == 'c') then
                       if ( (linhaDestino == 7) || ( colunaDestino == 0) ) then
                                --putStrLn ("Jogada Impossível")
                                realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino 0 pecasJogador pecasComputador

                        else --come a peca
                                realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 '1') (linhaDestino - 1) (colunaDestino - 1) 0 0 'p') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador (pecasComputador - 1)

                    else
                                --borda tabuleiro
                                realizaJogadas board linhaAtual colunaAtual linhaAtual colunaDestino 0 pecasJogador pecasComputador --jogada inválida

                -- não é a proxima linha
                else
                       putStrLn "Nao e uma casa valida!"
                          --nao é uma jogada possivel

            else  if ( (linhaDestino == (linhaAtual + 1)) && (colunaDestino  ==  (colunaAtual + 1))  && (linhaAtual < 7)  && (colunaAtual < 7) ) then --se for uma casa possivel para direita

                    if ((encontraPosicao board linhaDestino colunaDestino) == '1') then
                        if (colunaDestino == 7) then --vira dama
                            realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 'P') linhaAtual colunaAtual 0 0 '1') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador pecasComputador

                        else
                            realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 'p') linhaAtual colunaAtual 0 0 '1') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador pecasComputador

                    else if ((encontraPosicao board linhaDestino colunaDestino) == 'c') then
                        if ( (linhaDestino == 7) || ( colunaDestino == 7) ) then
                                --putStrLn ("Jogada Impossível")
                                realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino 0 pecasJogador pecasComputador

                        else --come a peca
                                realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 '1') (linhaDestino + 1) (colunaDestino + 1) 0 0 'p') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador (pecasComputador - 1)

                    else --posição inválida
                                realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino 0 pecasJogador pecasComputador

             ----------------------------------------Dama---------------------------
            else if ( (encontraPosicao board linhaAtual colunaAtual) == 'P')  then -- se existe uma peca na posicao atual
                --direita
                if ( (linhaDestino == (linhaAtual + 1)) && (colunaDestino  ==  (colunaAtual + 1))  && (linhaAtual < 7)  && (colunaAtual < 7) ) then --se for uma casa possivel para direita

                    if ((encontraPosicao board linhaDestino colunaDestino) == '1') then
                              realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 'P') linhaAtual colunaAtual 0 0 '1') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador pecasComputador

                    else if ((encontraPosicao board linhaDestino colunaDestino) == 'c') then
                        if ( (linhaDestino == 7) || ( colunaDestino == 7) ) then
                                --putStrLn ("Jogada Impossível")
                                realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino 0 pecasJogador pecasComputador

                        else --come a peca
                                realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 '1') (linhaDestino + 1) (colunaDestino + 1) 0 0 'P') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador (pecasComputador - 1)

                    else --posição inválida
                                realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino 0 pecasJogador pecasComputador

            -- para esquerda
                else if ( (linhaDestino == (linhaAtual + 1)) && (colunaDestino  ==  (colunaAtual - 1))  && (linhaAtual < 7)  && (colunaAtual > 0) ) then  --se for uma casa possivel para esquerda

                    if ((encontraPosicao board linhaDestino colunaDestino) == '1') then
                            realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 'P') linhaAtual colunaAtual 0 0 '1') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador pecasComputador

                    else if ((encontraPosicao board linhaDestino colunaDestino) == 'c') then
                       if ( (linhaDestino == 7) || ( colunaDestino == 0) ) then
                                --putStrLn ("Jogada Impossível")
                                realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino 0 pecasJogador pecasComputador

                        else --come a peca
                                realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 '1') (linhaDestino + 1) (colunaDestino - 1) 0 0 'P') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador (pecasComputador - 1)

                    else
                                --borda tabuleiro
                                realizaJogadas board linhaAtual colunaAtual linhaAtual colunaDestino 0 pecasJogador pecasComputador --jogada inválida

                --para trás direita
                else if ( (linhaDestino == (linhaAtual - 1)) && (colunaDestino  ==  (colunaAtual - 1))  && (linhaAtual > 0)  && (colunaAtual < 7) ) then --se for uma casa possivel para direita

                    if ((encontraPosicao board linhaDestino colunaDestino) == '1') then
                              realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 'P') linhaAtual colunaAtual 0 0 '1') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador pecasComputador

                    else if ((encontraPosicao board linhaDestino colunaDestino) == 'c') then
                        if ( (linhaDestino == 0) || ( colunaDestino == 0) ) then
                                --putStrLn ("Jogada Impossível")
                                realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino 0 pecasJogador pecasComputador

                        else --come a peca
                                realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 '1') (linhaDestino - 1) (colunaDestino + 1) 0 0 'P') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador (pecasComputador - 1)

                    else --posição inválida
                                realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino 0 pecasJogador pecasComputador

                --para trás esquerda
               else if ( (linhaDestino == (linhaAtual - 1)) && (colunaDestino  ==  (colunaAtual - 1))  && (linhaAtual > 0)  && (colunaAtual > 0) ) then --se for uma casa possivel para direita

                    if ((encontraPosicao board linhaDestino colunaDestino) == '1') then
                              realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 'P') linhaAtual colunaAtual 0 0 '1') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador pecasComputador

                    else if ((encontraPosicao board linhaDestino colunaDestino) == 'c') then
                        if ( (linhaDestino == 0) || ( colunaDestino == 0) ) then
                                --putStrLn ("Jogada Impossível")
                                realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino 0 pecasJogador pecasComputador

                        else --come a peca
                                realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 '1') (linhaDestino - 1) (colunaDestino - 1) 0 0 'P') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador (pecasComputador - 1)

                    else --posição inválida
                                realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino 0 pecasJogador pecasComputador

                -- não é a proxima linha
                else
                       putStrLn "Nao e uma casa valida!"
                          --nao é uma jogada possivel

            else  if ( (linhaDestino == (linhaAtual + 1)) && (colunaDestino  ==  (colunaAtual + 1))  && (linhaAtual < 7)  && (colunaAtual < 7) ) then --se for uma casa possivel para direita

                    if ((encontraPosicao board linhaDestino colunaDestino) == '1') then
                        if (colunaDestino == 7) then --vira dama
                            realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 'P') linhaAtual colunaAtual 0 0 '1') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador pecasComputador

                        else
                            realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 'p') linhaAtual colunaAtual 0 0 '1') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador pecasComputador

                    else if ((encontraPosicao board linhaDestino colunaDestino) == 'c') then
                        if ( (linhaDestino == 7) || ( colunaDestino == 7) ) then
                                --putStrLn ("Jogada Impossível")
                                realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino 0 pecasJogador pecasComputador

                        else --come a peca
                                realizaJogadas ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 '1') (linhaDestino + 1) (colunaDestino + 1) 0 0 'p') linhaAtual colunaAtual linhaDestino colunaDestino 1 pecasJogador (pecasComputador - 1)

                    else --posição inválida
                                realizaJogadas board linhaAtual colunaAtual linhaDestino colunaDestino 0 pecasJogador pecasComputador

            else -- else peça selecionada
                putStrLn "Nao e uma peca valida!"

        else -- else turno
             return()

-- ------- main ---------------------
main = do

            realizaJogadas tabuleiro 2 0 3 0 0 12 12

