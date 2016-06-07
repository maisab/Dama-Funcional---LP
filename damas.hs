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

posicaoPecasComputador = [ [5,1],[5,3],[5,5],[5,7],
                                                [6,0],[6,2],[6,4],[6,6],
                                                [7,1],[7,3],[7,5],[7,7]]

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

---------------Int -------------------
getInt :: Char -> Int
getInt x = digitToInt x

---------------Char -------------------
charToInt :: Int -> Char
charToInt x = intToDigit x

--------mostra tabuleiro --------------
mostraTabuleiro :: [[Char]] -> Int -> IO ()
mostraTabuleiro board pos = do
    if (pos <= 7) then do
          print ( ([charToInt pos]) ++ " | " ++ (head board))
          mostraTabuleiro (tail board) (pos + 1)

     else
          print("  |012345678")

 -------verificar posição ---------------------
realizaJogadas :: [[Char]] -> Int -> Int-> Int-> [[Int]] -> IO ()
realizaJogadas board turno npecasJogador npecasComputador listaPecasComp = do

    mostraTabuleiro board 0

    if (npecasComputador == 0) then do
        putStrLn("Parabén você venceu !")
        return()

    else if (npecasJogador == 0) then do
        putStrLn "Você perdeu :("
        return()

    else do
        if (turno == 0) then do --se for a vez do jogador

            putStrLn("\n")
            putStrLn("Linha da peça atual : ")
            linhaAtual <- getChar
            getChar

            putStrLn("Coluna da peça atual : ")
            colunaAtual <- getChar
            getChar

            putStrLn("Linha da casa de destino : ")
            linhaDestino <- getChar
            getChar

            putStrLn("Coluna da casa de destino : ")
            colunaDestino <- getChar
            getChar

            if( ((getInt linhaAtual) < 0) || ((getInt colunaAtual) < 0) ||  ((getInt linhaDestino) < 0) || ((getInt colunaDestino) < 0) ||
                ((getInt linhaAtual) > 7) || ((getInt colunaAtual) > 7) ||  ((getInt linhaDestino) > 7) || ((getInt colunaDestino) > 7)) then do

                putStrLn("Posições inválidas\n")
                realizaJogadas board 0 npecasJogador npecasComputador listaPecasComp

            else do
                if ( (encontraPosicao board (getInt linhaAtual) (getInt colunaAtual)) == 'p') then do-- se existe uma peca na posicao atual

                    if ( ((getInt linhaDestino) == ((getInt linhaAtual) + 1)) &&
                        ( ((getInt colunaDestino)  ==  ( (getInt colunaAtual) + 1)) || ( (getInt colunaDestino)  ==  (( getInt colunaAtual) - 1) ))  &&
                        ( ((getInt linhaAtual) <= 7)  &&  (( (getInt colunaAtual) <= 7)  || ((getInt colunaAtual) >= 0)) ) ) then do

                        if ((encontraPosicao board (getInt linhaDestino) (getInt colunaDestino)) == '1') then do
                            if ((getInt linhaDestino) == 7) then do --vira dama
                                putStrLn(" --Virou Dama!-- ")
                                realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 'P') (getInt linhaAtual) (getInt colunaAtual) 0 0 '1') 1 npecasJogador npecasComputador listaPecasComp

                            else do
                                realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 'p') (getInt linhaAtual) (getInt colunaAtual) 0 0 '1') 1 npecasJogador npecasComputador listaPecasComp

                        else if ( ((encontraPosicao board (getInt linhaDestino) (getInt colunaDestino)) == 'c') &&      --se for peça do adversario e puder comer
                                    ( ((encontraPosicao board ((getInt linhaDestino) + 1) ((getInt colunaDestino) + 1)) == '1' ) ||
                                    ((encontraPosicao board ((getInt linhaDestino) + 1) ((getInt colunaDestino) - 1)) == '1') ) ) then do

                            if (  (((getInt linhaDestino) == 7) && ((getInt colunaDestino) == 0))  || -- inferior esquerdo
                                  (((getInt linhaDestino) == 7) && ((getInt colunaDestino) == 7)) ) then do --inferior direito

                                    putStrLn (" --Jogada Impossível-- ")
                                    realizaJogadas board 0 npecasJogador npecasComputador listaPecasComp

                            else do --come a peca
                                if( (getInt colunaDestino) == ( (getInt colunaAtual) + 1)) then do --come para direita
                                    realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 '1') ( (getInt linhaDestino) + 1) ( ( getInt colunaDestino) + 1) 0 0 'p') 1 npecasJogador (npecasComputador - 1) listaPecasComp

                                else do --come para esquerda
                                    realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 '1') ( (getInt linhaDestino) + 1) ( (getInt colunaDestino) - 1) 0 0 'p') 1 npecasJogador (npecasComputador - 1) listaPecasComp

                        else do--posição inválida ou não é possivel comer
                            putStrLn (" --Jogada Impossível-- ")
                            realizaJogadas board 0 npecasJogador npecasComputador listaPecasComp

                    else do  -- não é a proxima linha
                        putStrLn (" -- Jogada Impossível : Não é uma casa alcançavel -- ")
                        realizaJogadas board 0 npecasJogador npecasComputador listaPecasComp
                              --nao é uma jogada possivel

                 ----------------------------------------Dama---------------------------

                else if ( (encontraPosicao board (getInt linhaAtual) (getInt colunaAtual)) == 'P')  then do  -- se existe uma peca na posicao atual

                    if ( ((getInt linhaDestino) == ((getInt linhaAtual) + 1) || ((getInt linhaDestino) == ((getInt linhaAtual) - 1)) ) &&
                        ( ((getInt colunaDestino)  ==  ( (getInt colunaAtual) + 1)) || ( (getInt colunaDestino)  ==  ( ( getInt colunaAtual) - 1)) )  &&
                        ( (((getInt linhaAtual) <= 7) || ((getInt linhaAtual) >= 0) )  &&  ( ((getInt colunaAtual) <= 7)  || ((getInt colunaAtual) >= 0)) )) then do --se for uma casa possivel para direita
                           --putStrLn "Nao e uma casa valida!"

                        if ((encontraPosicao board (getInt linhaDestino) (getInt colunaDestino)) == '1') then do
                                realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 'P') (getInt linhaAtual) (getInt colunaAtual) 0 0 '1') 1 npecasJogador npecasComputador listaPecasComp

                        else if ( (encontraPosicao board (getInt linhaDestino) (getInt colunaDestino)) == 'c' &&  --se for peça do adversario e puder comer
                                    ( ((encontraPosicao board ((getInt linhaDestino) + 1) ((getInt colunaDestino) + 1)) == '1' ) ||
                                    ((encontraPosicao board ((getInt linhaDestino) + 1) ((getInt colunaDestino) - 1)) == '1' ) ||
                                    ((encontraPosicao board ((getInt linhaDestino) - 1) ((getInt colunaDestino) + 1)) == '1' ) ||
                                    ((encontraPosicao board ((getInt linhaDestino) - 1) ((getInt colunaDestino) - 1)) == '1' ) ) ) then do

                            if (  (((getInt linhaDestino) == 7) && ((getInt colunaDestino) == 0)) || --inferior esquerdo
                                  (((getInt linhaDestino) == 7) && ((getInt colunaDestino) == 7)) || --inferior direito
                                  (((getInt linhaDestino) == 0) && ((getInt colunaDestino) == 7)) || --superior direito
                                  (((getInt linhaDestino) == 0) && ((getInt colunaDestino) == 0)) ) then do --superior esquerdo

                                    putStrLn ("Jogada Impossível")
                                    realizaJogadas board 0 npecasJogador npecasComputador listaPecasComp

                            else do --come a peca
                                    if( (((getInt linhaDestino) == ( (getInt linhaAtual) + 1)) && ((getInt colunaDestino) == ( (getInt colunaAtual) + 1)) ) ) then do --come para direita
                                        realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 '1') ( (getInt linhaDestino) + 1) ( ( getInt colunaDestino) + 1) 0 0 'p') 1 npecasJogador (npecasComputador - 1) listaPecasComp

                                    else if ( ((getInt linhaDestino) == ( (getInt linhaAtual) + 1)) && ((getInt colunaDestino) == ( (getInt colunaAtual) - 1)) ) then do -- come para esquerda
                                        realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 '1') ( (getInt linhaDestino) + 1) ( (getInt colunaDestino) - 1) 0 0 'p') 1 npecasJogador (npecasComputador - 1) listaPecasComp

                                    else if ( ((getInt linhaDestino) == ( (getInt linhaAtual) - 1)) && ((getInt colunaDestino) == ( (getInt colunaAtual) + 1)) ) then do -- come para trás e direita
                                        realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 '1') ( (getInt linhaDestino) - 1) ( (getInt colunaDestino) + 1) 0 0 'p') 1 npecasJogador (npecasComputador - 1) listaPecasComp

                                    else do --come para trás e esquerda
                                        realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 '1') ( (getInt linhaDestino) - 1) ( (getInt colunaDestino) - 1) 0 0 'p') 1 npecasJogador (npecasComputador - 1) listaPecasComp

                        else do --posição inválida ou não é possivel comer
                                    putStrLn (" --Jogada Impossível-- ")
                                    realizaJogadas board 0 npecasJogador npecasComputador listaPecasComp

                    -- não é a proxima linha
                    else do
                        putStrLn (" -- Jogada Impossível : Não é uma casa alcançavel -- ")
                        realizaJogadas board 0 npecasJogador npecasComputador listaPecasComp --nao é uma jogada possivel

                else do -- else peça selecionada
                    putStrLn (" -- Jogada Impossível : Não é uma peça válida -- ")
                    realizaJogadas board 0 npecasJogador npecasComputador listaPecasComp --não é uma peça válida

            else do -- else turno computador

                putStrLn " -- Turno computador  -- \n"
                -- se existe uma peca na posicao atual
                if ( (encontraPosicao board (head (head listaPecasComp)) (head (tail (head listaPecasComp))) ) == 'c')  then do
                    -- comer peca a direita
                    if( ((encontraPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) + 1) ) == 'p')  &&
                        ((encontraPosicao board ((head (head listaPecasComp)) - 2) ((head (tail (head listaPecasComp))) + 2) ) == '1') ) then do

                        realizaJogadas (trocaPosicao (trocaPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) + 1) 0 0 '1')
                            ((head (head listaPecasComp)) - 2 ) ((head (tail (head listaPecasComp))) + 2 ) 0 0 'c')
                                0 (npecasJogador - 1) npecasComputador ([((head (head listaPecasComp)) - 2),((head (tail (head listaPecasComp))) + 2)] : (tail listaPecasComp))

                    -- comer peca a esquerda
                    else if( ((encontraPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) - 1) ) == 'p')  &&
                        ((encontraPosicao board ((head (head listaPecasComp)) - 2) ((head (tail (head listaPecasComp))) - 2) ) == '1') ) then do

                        realizaJogadas (trocaPosicao (trocaPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) + 1) 0 0 '1')
                            ((head (head listaPecasComp)) - 2 ) ((head (tail (head listaPecasComp))) - 2 ) 0 0 'c')
                                0 (npecasJogador - 1) npecasComputador ([((head (head listaPecasComp)) - 2),((head (tail (head listaPecasComp))) - 2)] : (tail listaPecasComp))

                    -- casa livre a direita
                    else if( (encontraPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) + 1) ) == '1' ) then

                        realizaJogadas (trocaPosicao (trocaPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) + 1) 0 0 'c')
                            (head (head listaPecasComp)) (head (tail (head listaPecasComp))) 0 0 '1')
                                0 npecasJogador npecasComputador ([((head (head listaPecasComp)) - 1),((head (tail (head listaPecasComp))) + 1)] : (tail listaPecasComp))

                    -- casa livre a esquerda
                    else if( (encontraPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) - 1) ) == '1' ) then

                        realizaJogadas (trocaPosicao (trocaPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) - 1) 0 0 'c')
                            (head (head listaPecasComp)) (head (tail (head listaPecasComp))) 0 0 '1')
                                0 npecasJogador npecasComputador ([((head (head listaPecasComp)) - 1),((head (tail (head listaPecasComp))) - 1)] : (tail listaPecasComp))

                    --nenhuma jogada possível com a peça atual
                    else
                        realizaJogadas board 1 npecasJogador npecasComputador  (reverse ([((head (head listaPecasComp)) - 1),((head (tail (head listaPecasComp))) - 1)] : (reverse (tail listaPecasComp))))

                -----------Dama Computador ---------------------

                else if ( (encontraPosicao board (head (head listaPecasComp)) (head (tail (head listaPecasComp))) ) == 'C')  then do
                    -- comer peca a direita
                    if( ((encontraPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) + 1) ) == 'p')  &&
                        ((encontraPosicao board ((head (head listaPecasComp)) - 2) ((head (tail (head listaPecasComp))) + 2) ) == '1') ) then do

                        realizaJogadas (trocaPosicao (trocaPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) + 1) 0 0 '1')
                            ((head (head listaPecasComp)) - 2 ) ((head (tail (head listaPecasComp))) + 2 ) 0 0 'C')
                                0 (npecasJogador - 1) npecasComputador ([((head (head listaPecasComp)) - 2),((head (tail (head listaPecasComp))) + 2)] : (tail listaPecasComp))

                    -- comer peca a esquerda
                    else if( ((encontraPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) - 1) ) == 'p')  &&
                        ((encontraPosicao board ((head (head listaPecasComp)) - 2) ((head (tail (head listaPecasComp))) - 2) ) == '1') ) then do

                        realizaJogadas (trocaPosicao (trocaPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) + 1) 0 0 '1')
                            ((head (head listaPecasComp)) - 2 ) ((head (tail (head listaPecasComp))) - 2 ) 0 0 'C')
                                0 (npecasJogador - 1) npecasComputador ([((head (head listaPecasComp)) - 2),((head (tail (head listaPecasComp))) - 2)] : (tail listaPecasComp))

                    -- casa livre a direita
                    else if( (encontraPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) + 1) ) == '1' ) then

                        realizaJogadas (trocaPosicao (trocaPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) + 1) 0 0 'C')
                            (head (head listaPecasComp)) (head (tail (head listaPecasComp))) 0 0 '1')
                                0 npecasJogador npecasComputador ([((head (head listaPecasComp)) - 1),((head (tail (head listaPecasComp))) + 1)] : (tail listaPecasComp))

                    -- casa livre a direita para trás
                    else if( (encontraPosicao board ((head (head listaPecasComp)) + 1) ((head (tail (head listaPecasComp))) + 1) ) == '1' ) then

                        realizaJogadas (trocaPosicao (trocaPosicao board ((head (head listaPecasComp)) + 1) ((head (tail (head listaPecasComp))) + 1) 0 0 'C')
                            (head (head listaPecasComp)) (head (tail (head listaPecasComp))) 0 0 '1')
                                0 npecasJogador npecasComputador ([((head (head listaPecasComp)) + 1),((head (tail (head listaPecasComp))) + 1)] : (tail listaPecasComp))


                    -- casa livre a esquerda
                    else if( (encontraPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) - 1) ) == '1' ) then

                        realizaJogadas (trocaPosicao (trocaPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) - 1) 0 0 'C')
                            (head (head listaPecasComp)) (head (tail (head listaPecasComp))) 0 0 '1')
                                0 npecasJogador npecasComputador ([((head (head listaPecasComp)) - 1),((head (tail (head listaPecasComp))) - 1)] : (tail listaPecasComp))

                    -- casa livre a esquerda para trás
                    else if( (encontraPosicao board ((head (head listaPecasComp)) + 1) ((head (tail (head listaPecasComp))) - 1) ) == '1' ) then

                        realizaJogadas (trocaPosicao (trocaPosicao board ((head (head listaPecasComp)) + 1) ((head (tail (head listaPecasComp))) - 1) 0 0 'C')
                            (head (head listaPecasComp)) (head (tail (head listaPecasComp))) 0 0 '1')
                                0 npecasJogador npecasComputador ([((head (head listaPecasComp)) + 1),((head (tail (head listaPecasComp))) - 1)] : (tail listaPecasComp))


                    --nenhuma jogada possível com a peça atual
                    else
                        realizaJogadas board 1 npecasJogador npecasComputador  (reverse ([((head (head listaPecasComp)) - 1),((head (tail (head listaPecasComp))) - 1)] : (reverse (tail listaPecasComp))))


                else -- não é uma peça
                        realizaJogadas board 1 npecasJogador npecasComputador (tail listaPecasComp)


-- ------- main ---------------------
main = do

            putStrLn "Digite as coordenadas na seguinte ordem e sem espaços e virgulas : "
            putStrLn "Linha peça atual, Coluna peça atual, Linha casa de destino, Coluna casa de Destino"
            realizaJogadas tabuleiro 0 12 12 posicaoPecasComputador

-- -----------------------deu certo

            --print( [((head (head posicaoPecasComputador)) - 1),((head (tail (head posicaoPecasComputador))) + 1)])
            --print([((head (head posicaoPecasComputador)) - 1),((head (tail (head posicaoPecasComputador))) + 1)] : (tail posicaoPecasComputador) )
            --print( reverse( [((head (head posicaoPecasComputador)) - 1),((head (tail (head posicaoPecasComputador))) + 1)] : (reverse (tail posicaoPecasComputador)) ))
            --print( tail (tail posicaoPecasComputador) )
            --print( tail (tail (tail posicaoPecasComputador) ))
            --print( tail (tail (tail (tail posicaoPecasComputador) )))
 ----------------------------------------------




