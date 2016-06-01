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
verificaPosicaoPeca :: [[Char]] -> Int -> Int -> Int -> Int -> Int -> Int-> IO ()
verificaPosicaoPeca board linhaAtual colunaAtual linhaDestino colunaDestino turno nPecas= do

    putStrLn "Printar tabuleiro"
    print(board)

    if (turno == 0) then --se for a vez do jogador

        --putStrLn ("Digite a linha da peca atual")
        --linhaAtual <- getChar

        --putStrLn ("Digite a coluna da peca atual")
        --colunaAtual <- getChar

        --putStrLn ("Digite a linha da casa de destino")
        --linhaDestino <- getChar

        --putStrLn ("Digite a coluna da casa de destino")
        --colunaDestino <- getChar

        if ( (encontraPosicao board linhaAtual colunaAtual) == 'p')  then -- se existe uma peca na posicao atual
            if ( (linhaDestino == (linhaAtual + 1)) && (colunaDestino  ==  (colunaAtual + 1)) ) then --se for uma casa possivel para direita

                if ((encontraPosicao board linhaDestino colunaDestino) == '1') then
                    --putStrLn "Trocando linha"
                    verificaPosicaoPeca ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 'p') linhaAtual colunaAtual 0 0 '1') linhaAtual colunaAtual linhaDestino colunaDestino 1 0

                else if ((encontraPosicao board linhaDestino colunaDestino) == 'c') then
                    if ( (linhaDestino == 7) || ( colunaDestino == 7) ) then
                            --putStrLn ("Jogada Impossível")
                            verificaPosicaoPeca board linhaAtual colunaAtual linhaDestino colunaDestino 0 0

                    else --come a peca
                            verificaPosicaoPeca ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 '1') (linhaDestino + 1) (colunaDestino + 1) 0 0 'p') 0 0 0 0 0 0

                else --posição inválida
                            verificaPosicaoPeca board linhaAtual colunaAtual linhaDestino colunaDestino 0 0

            else if ( (linhaDestino == (linhaAtual + 1)) && (colunaDestino  ==  (colunaAtual - 1))) then  --se for uma casa possivel para esquerda

                if ((encontraPosicao board linhaDestino colunaDestino) == '1') then
                    verificaPosicaoPeca ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 'p') linhaAtual colunaAtual 0 0 '1') 0 0 0 0 1 0

                else if ((encontraPosicao board linhaDestino colunaDestino) == 'c') then
                   if ( (linhaDestino == 7) || ( colunaDestino == 0) ) then
                            --putStrLn ("Jogada Impossível")
                            verificaPosicaoPeca board 0 0 0 0 0 0

                    else --come a peca
                            verificaPosicaoPeca ( trocaPosicao (trocaPosicao board linhaDestino colunaDestino 0 0 '1') (linhaDestino - 1) (colunaDestino - 1) 0 0 'p') 0 0 0 0 0 0

                else --else comer peça
                            verificaPosicaoPeca board 0 0 0 0 0 0 --jogada inválida

            else -- não é a proxima linha
                   putStrLn "Nao e uma casa valida!"
                      --nao é uma jogada possivel

        else -- else peça selecionada
            putStrLn "Nao e uma peca valida!"

    else -- else turno
         --putStrLn "Nao e sua vez!"
         return()

-- ------- main ---------------------
main = do
            verificaPosicaoPeca tabuleiro 2 0 4 1 0 0
            --print
            --printaMatriz (trocaPosicao tabuleiro [[ ]] 7 7 's') 0
            --print(encontraPosicao tabuleiro 7 7)
            --print(trocaPosicao tabuleiro [[ ]] 6 5 't')
