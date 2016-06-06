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

posicaoPecasComputador = [[5,1],[5,3],[5,5],[5,7],
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

---------encontra peça computador -------------
--encontraPecaComputador :: [[Char]] -> [Int] -> IO ()
--encontraPecaComputador board [] = do
--    x <- randomRIO (0, 7 :: Int)
--    y <- randomRIO (0, 7 :: Int)
--    if ( (encontraPosicao board x y == 'c')) then
--        [x,y]
--    else
--        encontraPecaComputador board

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
charToString :: Char -> String
charToString = (:[])

-----------------Concatena Posição -------------------
setNum (x:xs) lin col linA colA novoValor1 novoValor2
     | (lin == linA) = [(setColuna x col colA novoValor1 novoValor2)]++xs -- altera o valor na coluna e concatena a linha modificada com a tail
     | (lin /= linA) = x:(setNum xs lin col (linA+1) colA novoValor1 novoValor2)--se nao é a linha verifica a próxma e sala a head

setColuna (x1:xs1) col colA novoValor1 novoValor2
     | (col == colA) = [novoValor1,novoValor2]++(tail xs1) -- concatena o novo valor com a cauda
     | (col /= colA) = x1:(setColuna xs1 col (colA+1) novoValor1 novoValor2)-- salva a head e ve o proximo elemento da lista

-----fazer um encontra posicao das posicoes da lista, onde nao tive peça, remove a posicao

-----------------Remove Posição -------------------
removePosicao (x:xs) lin col linA colA
     | (lin == linA) = [(removeColuna x col colA)]++xs -- altera o valor na coluna e concatena a linha modificada com a tail
     | (lin /= linA) = x:(removePosicao xs lin col (linA+1) colA)--se nao é a linha verifica a próxma e sala a head

removeColuna (x1:xs1) col colA
     | (col == colA) = (tail xs1) -- concatena o novo valor com a cauda
     | (col /= colA) = x1:(removeColuna xs1 col (colA+1) )-- salva a head e ve o proximo elemento da lista


--concatenaPosicao :: [[Int]] -> Int -> Int -> [[Int]]
--concatenaPosicao (hd : ht) linhaDestino colunaDestino = do
--    ht ++ [(adicionaLinha ([colunaDestino]) linhaDestino)]

--adicionaLinha lista linhaDestino = do
--    linhaDestino : lista

-------------------Remove Posição -------------------
--removePosicao :: [[Int]] -> [[Int]]
--removePosicao lista = do
--    (tail lista)

 -------verificar posição ---------------------
realizaJogadas :: [[Char]] -> Int -> Int-> Int-> [[Int]] -> IO ()
realizaJogadas board turno npecasJogador npecasComputador listaPecasComp = do

    putStrLn "Tabuleiro"
    print(board)

    linhaAtual <- getChar
    colunaAtual <- getChar
    linhaDestino <- getChar
    colunaDestino <- getChar

    if (npecasComputador == 0) then
        print("Parabén você venceu !")
        --return()

    else if (npecasJogador == 0) then
        putStrLn "Você perdeu :("
        --return()

    else
        if (turno == 0) then --se for a vez do jogador
            if ( (encontraPosicao board (getInt linhaAtual) (getInt colunaAtual)) == 'p')  then -- se existe uma peca na posicao atual
                if ( ((getInt linhaDestino) == ((getInt linhaAtual) + 1)) &&
                    ( ((getInt colunaDestino)  ==  ( (getInt colunaAtual) + 1)) || ( (getInt colunaDestino)  ==  (( getInt colunaAtual) - 1) ))  &&
                    ( ((getInt linhaAtual) <= 7)  &&  (( (getInt colunaAtual) <= 7)  || ((getInt colunaAtual) >= 0)) ) ) then --se for uma casa possivel para direita
                       --putStrLn "Nao e uma casa valida!"

                    if ((encontraPosicao board (getInt linhaDestino) (getInt colunaDestino)) == '1') then
                        if ((getInt linhaDestino) == 7) then --vira dama
                            realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 'P') (getInt linhaAtual) (getInt colunaAtual) 0 0 '1') 1 npecasJogador npecasComputador listaPecasComp

                        else
                            realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 'p') (getInt linhaAtual) (getInt colunaAtual) 0 0 '1') 1 npecasJogador npecasComputador listaPecasComp

                    else if ( ((encontraPosicao board (getInt linhaDestino) (getInt colunaDestino)) == 'c') &&      --se for peça do adversario e puder comer
                                ( ((encontraPosicao board ((getInt linhaDestino) + 1) ((getInt colunaDestino) + 1)) == '1' ) ||
                                ((encontraPosicao board ((getInt linhaDestino) + 1) ((getInt colunaDestino) - 1)) == '1') ) ) then

                        if (  (((getInt linhaDestino) == 7) && ((getInt colunaDestino) == 0))  || -- inferior esquerdo
                              (((getInt linhaDestino) == 7) && ((getInt colunaDestino) == 7)) ) then --inferior direito
                                --putStrLn ("Jogada Impossível")
                                realizaJogadas board 0 npecasJogador npecasComputador listaPecasComp

                        else --come a peca
                                if( (getInt colunaDestino) == ( (getInt colunaAtual) + 1)) then --come para direita
                                    realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 '1') ( (getInt linhaDestino) + 1) ( ( getInt colunaDestino) + 1) 0 0 'p') 1 npecasJogador (npecasComputador - 1) listaPecasComp
                                else --come para esquerda
                                    realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 '1') ( (getInt linhaDestino) + 1) ( (getInt colunaDestino) - 1) 0 0 'p') 1 npecasJogador (npecasComputador - 1) listaPecasComp

                    else --posição inválida ou não é possivel comer
                                realizaJogadas board 0 npecasJogador npecasComputador listaPecasComp

                -- não é a proxima linha
                else
                       putStrLn "Nao e uma casa valida!"
                          --nao é uma jogada possivel

             ----------------------------------------Dama---------------------------

            else if ( (encontraPosicao board (getInt linhaAtual) (getInt colunaAtual)) == 'P')  then  -- se existe uma peca na posicao atual
                if ( ((getInt linhaDestino) == ((getInt linhaAtual) + 1) || ((getInt linhaDestino) == ((getInt linhaAtual) - 1)) ) &&
                    ( ((getInt colunaDestino)  ==  ( (getInt colunaAtual) + 1)) || ( (getInt colunaDestino)  ==  ( ( getInt colunaAtual) - 1)) )  &&
                    ( (((getInt linhaAtual) <= 7) || ((getInt linhaAtual) >= 0) )  &&  ( ((getInt colunaAtual) <= 7)  || ((getInt colunaAtual) >= 0)) )) then --se for uma casa possivel para direita
                       --putStrLn "Nao e uma casa valida!"

                    if ((encontraPosicao board (getInt linhaDestino) (getInt colunaDestino)) == '1') then
                            realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 'P') (getInt linhaAtual) (getInt colunaAtual) 0 0 '1') 1 npecasJogador npecasComputador listaPecasComp

                    else if ( (encontraPosicao board (getInt linhaDestino) (getInt colunaDestino)) == 'c' &&  --se for peça do adversario e puder comer
                                ( ((encontraPosicao board ((getInt linhaDestino) + 1) ((getInt colunaDestino) + 1)) == '1' ) ||
                                ((encontraPosicao board ((getInt linhaDestino) + 1) ((getInt colunaDestino) - 1)) == '1' ) ||
                                ((encontraPosicao board ((getInt linhaDestino) - 1) ((getInt colunaDestino) + 1)) == '1' ) ||
                                ((encontraPosicao board ((getInt linhaDestino) - 1) ((getInt colunaDestino) - 1)) == '1' ) ) ) then

                        if (  (((getInt linhaDestino) == 7) && ((getInt colunaDestino) == 0)) || --inferior esquerdo
                              (((getInt linhaDestino) == 7) && ((getInt colunaDestino) == 7)) || --inferior direito
                              (((getInt linhaDestino) == 0) && ((getInt colunaDestino) == 7)) || --superior direito
                              (((getInt linhaDestino) == 0) && ((getInt colunaDestino) == 0)) ) then --superior esquerdo
                                --putStrLn ("Jogada Impossível")
                                realizaJogadas board 0 npecasJogador npecasComputador listaPecasComp

                        else --come a peca
                                if( (((getInt linhaDestino) == ( (getInt linhaAtual) + 1)) && ((getInt colunaDestino) == ( (getInt colunaAtual) + 1)) ) ) then --come para direita
                                    realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 '1') ( (getInt linhaDestino) + 1) ( ( getInt colunaDestino) + 1) 0 0 'p') 1 npecasJogador (npecasComputador - 1) listaPecasComp

                                else if ( ((getInt linhaDestino) == ( (getInt linhaAtual) + 1)) && ((getInt colunaDestino) == ( (getInt colunaAtual) - 1)) ) then -- come para esquerda
                                    realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 '1') ( (getInt linhaDestino) + 1) ( (getInt colunaDestino) - 1) 0 0 'p') 1 npecasJogador (npecasComputador - 1) listaPecasComp

                                else if ( ((getInt linhaDestino) == ( (getInt linhaAtual) - 1)) && ((getInt colunaDestino) == ( (getInt colunaAtual) + 1)) ) then -- come para trás e direita
                                    realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 '1') ( (getInt linhaDestino) - 1) ( (getInt colunaDestino) + 1) 0 0 'p') 1 npecasJogador (npecasComputador - 1) listaPecasComp

                                else --come para trás e esquerda
                                    realizaJogadas ( trocaPosicao (trocaPosicao board (getInt linhaDestino) (getInt colunaDestino) 0 0 '1') ( (getInt linhaDestino) - 1) ( (getInt colunaDestino) - 1) 0 0 'p') 1 npecasJogador (npecasComputador - 1) listaPecasComp

                    else --posição inválida ou não é possivel comer
                                realizaJogadas board 0 npecasJogador npecasComputador listaPecasComp

                -- não é a proxima linha
                else
                       putStrLn "Nao e uma casa valida!"
                          --nao é uma jogada possivel

               ---------- Computador ------------------


               --------------------------------------------------

            else -- else peça selecionada
                putStrLn "Nao e uma peca valida!"

        else -- else turno

            if ( (encontraPosicao board (head (head listaPecasComp)) (head (tail (head listaPecasComp))) ) == 'c')  then -- se existe uma peca na posicao atual

                if( (encontraPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) + 1) ) == '1' ) then -- casa livre a direita
                    realizaJogadas (trocaPosicao (trocaPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) + 1) 0 0 'c')
                        (head (head listaPecasComp)) (head (tail (head listaPecasComp))) 0 0 '1')
                            0 npecasJogador npecasComputador ([((head (head listaPecasComp)) - 1),((head (tail (head listaPecasComp))) + 1)] : (tail listaPecasComp))

                else if( (encontraPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) - 1) ) == '1' ) then -- casa livre a esquerda
                    realizaJogadas (trocaPosicao (trocaPosicao board ((head (head listaPecasComp)) - 1) ((head (tail (head listaPecasComp))) - 1) 0 0 'c')
                        (head (head listaPecasComp)) (head (tail (head listaPecasComp))) 0 0 '1')
                            0 npecasJogador npecasComputador ([((head (head listaPecasComp)) - 1),((head (tail (head listaPecasComp))) - 1)] : (tail listaPecasComp))

                --print(posicaoPecasComputador)
                    --putStrLn "Nao e uma peca valida!"
                else
                    putStrLn "Nao e uma peca valida!"
            else -- não é uma peça
                    putStrLn "Nao e uma peca "


             --return()

-- ------- main ---------------------
main = do

            print(posicaoPecasComputador)
            print((head (head posicaoPecasComputador)) - 1)


            --print( ((head (tail (head posicaoPecasComputador))) + 1) )((head (head posicaoPecasComputador)) - 1)

-- -----------------------deu certo

            --print( [((head (head posicaoPecasComputador)) - 1),((head (tail (head posicaoPecasComputador))) + 1)])
            --print([((head (head posicaoPecasComputador)) - 1),((head (tail (head posicaoPecasComputador))) + 1)] : (tail posicaoPecasComputador) )
            --print( tail (tail posicaoPecasComputador) )
            --print( tail (tail (tail posicaoPecasComputador) ))
            --print( tail (tail (tail (tail posicaoPecasComputador) )))
 ----------------------------------------------

            --print(setNum posicaoPecasComputador 0 0 0 0 4 2)
            --print(setNum posicaoPecasComputador 0 0 0 0 6 2)
            --print(removePosicao (removePosicao posicaoPecasComputador 0 0 0 0) 0 0 0 0)
            --print(removePosicao posicaoPecasComputador 0 0 0 0)

            --print (encontraPosicao tabuleiro (head (head posnpecasComputador)) (head (tail (head posnpecasComputador))))
            --print(posicaoPecasComputador)
            --print(concatenaPosicao posicaoPecasComputador 4 2)
            --print(removePosicao posicaoPecasComputador)
            --print(concatenaPosicao posicaoPecasComputador 3 1)
            --print(removePosicao posicaoPecasComputador)

            --putStrLn "Digite as coordenadas na seguinte ordem e sem espaços e virgulas : "
            --putStrLn "Linha peça atual, Coluna peça atual, Linha casa de destino, Coluna casa de Destino"
            realizaJogadas tabuleiro 0 12 12 posicaoPecasComputador

