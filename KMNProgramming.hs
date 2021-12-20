{-# language ViewPatterns #-}
{-# language RecursiveDo #-}
{-# language FlexibleInstances #-}
{-# language TypeSynonymInstances #-}
module KMNProgramming where

import Control.Monad hiding (unless)
import Control.Exception (evaluate)
import Foreign

import CodeGen.X86

{- register usage
eax:    maximum value
rbx:    level pointer
rcx:    (aux) loop counter
rdx:    (aux) loop pointer
rsp:    (unused) stack pointer
rbp:    (aux) loop pointer
rsi:    (static) -16
rdi:    (static) vector pointer & start level
r8:     (static) trace level
r9:     (static) stop level
r10:    (static) debug flag
r11:    (static) level length
r12:    (static) loop length
r13:    result bits (high)
r14:    result bits (low)
r15:    (aux) vector norm
-}

foreign import ccall "static stdio.h &fflush" fflush :: FunPtr a

type T = Ptr Int32 -> Int32
foreign import ccall "dynamic" callPBII :: FunPtr T -> T
instance Callable T where dynCCall = callPBII

foreign import ccall "dynamic" callIO :: FunPtr (IO ()) -> IO ()
instance Callable (IO ()) where dynCCall = callIO

umes_ :: Int -> Bool -> Int -> Int -> Int -> Int -> Int -> [Int32] -> [[Int32]] -> IO (Int32, [Int32])
umes_ guess trr ali ali' tr uroll lev levs m = umes guess trr ali ali' tr uroll lev levs (length m) (length $ head m) m

{-# NOINLINE umes #-}
umes :: Int -> Bool -> Int -> Int -> Int -> Int -> Int -> [Int32] -> Int -> Int -> [[Int32]] -> IO (Int32, [Int32])
umes guess trr ali ali' tr uroll lev levs rows_ d_ = fun
  where
    fun (align' (4*uroll) -> a: as)
        = withArrayAligned (concat $ a: zipWith (++) ((: [0,0,0]) <$> (replicate (rows-length levs) 0 ++ levs)) as) (2^8) $ \r -> do
            x <- evaluate $ code r
            xs <- forM [d+d1*lev, d+d1*(lev+1)..d+d1*(rows-1)] $ peekElemOff r
            return (x, xs)

    rows = rows_ - 1
    d = d_ `modd` (4*uroll)
    d1 = d+4

    modd a b = b * ((a - 1) `div` b + 1)

    code :: Ptr Int32 -> Int32
    code = compile $ saveNonVolatile $ mdo
        mov rdx $ fromIntegral $ 4*d1*(rows-length levs)
        xor_ rax rax
        mloop <- label
        cmp rdx $ fromIntegral $ 4*d1*lev
        unless Z $ do
            save [rdi, rdx] $ do
                xor_ r10 r10
                call $ ipRelValue main
            mov (addr $ rdi + rdx - 16) eax
            sub rdx $ fromIntegral $ 4*d1
            jmp mloop

        xor_ rdx rdx
        mov r10 $ fromIntegral $ fromEnum trr
        mov rax $ fromIntegral guess
        call $ ipRelValue main
        jmp end

        main <- label
        lea r9  $ addr8 $ rdi + fromIntegral (4 * (rows + 1) * d1)
        lea r8  $ addr8 $ rdi + fromIntegral (4 * tr * d1)
        mov r11 $ fromIntegral $ 4*d1
        mov r12 $ fromIntegral $ d `div` (4*uroll)
        mov rsi (-16)
        xor_ r13 r13
        xor_ r14 r14
        lea rdi $ addr8 $ rdi + rdx
        mov rbx rdi
        jmp chk

        trace <- label
        save [rax, rcx, rdx, rbx, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15] $ do
            leaData arg1 $ CString "%ld.\n"
            mov arg2 r14
            printfflush
        ret

        trace2 <- label
        save [rax, rcx, rdx, rbx, rbp, rsi, rdi, r8, r9, r10, r11, r12, r13, r14, r15] $ do
            leaData arg1 $ CString "%016lx%016lx    %d\n"
            mov arg2 r13
            mov arg3 r14
            mov (resizeOperand arg4) eax
            printfflush
        ret

        align ali
        chk <- label
        add rbx r11
        cmp rbx r9
        unless E $ mdo
            mov r15d $ addr $ rbx + rsi
            test r15d r15d
            if_ NZ (mdo
                mes
                cmp r15d eax
                j LE up) $ mdo
                cmp rbx r8
                unless NE $ call $ ipRelValue trace
            stc
            rcl r14 1
            when (rows > 64) $ rcl r13 1
            copyAdd
            jmp chk
        xor_ r15d r15d
        mes
        cmp r15d eax
        unless NA $ do
            mov eax r15d
            test r10 r10
            unless Z $ call $ ipRelValue trace2

        align ali
        up <- label
        sub rbx r11
        cmp rbx rdi
        unless E $ mdo
            test r14 1
            j Z zero
            xor_ r14 1
            copySub
            jmp chk
            zero <- label
            if rows > 64 then do
                shr r13 1
                rcr r14 1
              else do
                shr r14 1
            copyAdd
            jmp up
        ret

        end <- label
        return ()

    mes = mdo
        mov rcx r12
        mov rdx rdi
        pxor xmm2 xmm2
        align ali'
        doWhile NZ $ mdo
            replicateM_ uroll $ mdo
                movdqa xmm0 $ addr rdx
                sub rdx rsi
                movdqa xmm1 xmm0
                psrad  xmm0 31
                pxor  xmm1 xmm0
                psubd xmm2 xmm0
                paddd xmm2 xmm1
            sub rcx 1
        movdqa xmm1 xmm2
        psrldq xmm1 8
        paddd xmm2 xmm1
        movd ecx xmm2
        add r15d ecx
        psrldq xmm2 4
        movd ecx xmm2
        add r15d ecx

    copyAdd = copy $ mdo
        movdqa xmm0 $ addr rbp
        sub rbp rsi
        paddd  xmm0 $ addr rdx
        movdqa (addr rdx) xmm0
        sub rdx rsi

    copySub = copy $ mdo
        movdqa xmm0 $ addr rbp
        sub rbp rsi
        movdqa xmm1 $ addr rdx
        paddd xmm0 xmm0
        psubd xmm1 xmm0
        movdqa (addr rdx) xmm1
        sub rdx rsi

    copy body = mdo
        mov rcx r12
        mov rbp rbx
        mov rdx rdi
        align ali'
        doWhile NZ $ mdo
            replicateM_ uroll body
            sub rcx 1

    printfflush = do
        xor_ rax rax
        callFun rbp printf
        xor_ arg1 arg1
        callFun rbp fflush

align' n xs = (++ pad) <$> xs
  where
    pad = replicate ((-length (head xs)) `mod` n) 0

withArrayAligned bs al cont = allocaBytesAligned (sizeOf (head bs) * length bs) al $ \r -> do
    forM_ (zip [0..] bs) $ uncurry $ pokeElemOff r
    cont r

save rs c = do
    sequence_ [push r | r <- rs]
    c
    sequence_ [pop r | r <- reverse rs]

