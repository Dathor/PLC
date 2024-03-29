{-# OPTIONS_GHC -w #-}
module Grammar where
import Tokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,85) ([0,18,32768,0,0,0,512,0,0,512,0,0,16384,2,1024,0,512,0,64,4130,7,0,64768,16,8260,8206,28930,0,256,16520,28,0,0,0,0,0,0,512,0,0,0,510,34816,7232,1088,226,4130,4103,14465,2176,452,8260,8206,28930,0,0,0,0,64,0,2,4096,0,240,32768,7,0,0,0,34816,7232,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_lang","FuncList","FuncDecl","VarList","Exp","\"=\"","\"(\"","\";\"","\")\"","\"+\"","\"-\"","\"*\"","\"/\"","\"%\"","\"^\"","\"stdout\"","\"->\"","\"let\"","\"{\"","\"}\"","\",\"","\"print\"","name","num","%eof"]
        bit_start = st * 27
        bit_end = (st + 1) * 27
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..26]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (18) = happyShift action_6
action_0 (21) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_5
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (21) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyFail (happyExpListPerState 2)

action_3 (25) = happyShift action_9
action_3 (6) = happyGoto action_8
action_3 _ = happyReduce_7

action_4 (27) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (19) = happyShift action_7
action_5 _ = happyReduce_1

action_6 _ = happyReduce_3

action_7 (18) = happyShift action_6
action_7 (21) = happyShift action_3
action_7 (4) = happyGoto action_12
action_7 (5) = happyGoto action_5
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (19) = happyShift action_11
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (23) = happyShift action_10
action_9 _ = happyReduce_5

action_10 (25) = happyShift action_9
action_10 (6) = happyGoto action_20
action_10 _ = happyReduce_7

action_11 (9) = happyShift action_14
action_11 (13) = happyShift action_15
action_11 (20) = happyShift action_16
action_11 (24) = happyShift action_17
action_11 (25) = happyShift action_18
action_11 (26) = happyShift action_19
action_11 (7) = happyGoto action_13
action_11 _ = happyFail (happyExpListPerState 11)

action_12 _ = happyReduce_2

action_13 (10) = happyShift action_25
action_13 (12) = happyShift action_26
action_13 (13) = happyShift action_27
action_13 (14) = happyShift action_28
action_13 (15) = happyShift action_29
action_13 (16) = happyShift action_30
action_13 (17) = happyShift action_31
action_13 (22) = happyShift action_32
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (9) = happyShift action_14
action_14 (13) = happyShift action_15
action_14 (20) = happyShift action_16
action_14 (24) = happyShift action_17
action_14 (25) = happyShift action_18
action_14 (26) = happyShift action_19
action_14 (7) = happyGoto action_24
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (9) = happyShift action_14
action_15 (13) = happyShift action_15
action_15 (20) = happyShift action_16
action_15 (24) = happyShift action_17
action_15 (25) = happyShift action_18
action_15 (26) = happyShift action_19
action_15 (7) = happyGoto action_23
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (25) = happyShift action_22
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (9) = happyShift action_14
action_17 (13) = happyShift action_15
action_17 (20) = happyShift action_16
action_17 (24) = happyShift action_17
action_17 (25) = happyShift action_18
action_17 (26) = happyShift action_19
action_17 (7) = happyGoto action_21
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_18

action_19 _ = happyReduce_19

action_20 _ = happyReduce_6

action_21 (10) = happyShift action_25
action_21 (12) = happyShift action_26
action_21 (13) = happyShift action_27
action_21 (14) = happyShift action_28
action_21 (15) = happyShift action_29
action_21 (16) = happyShift action_30
action_21 (17) = happyShift action_31
action_21 _ = happyReduce_17

action_22 (8) = happyShift action_41
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (10) = happyShift action_25
action_23 _ = happyReduce_16

action_24 (10) = happyShift action_25
action_24 (11) = happyShift action_40
action_24 (12) = happyShift action_26
action_24 (13) = happyShift action_27
action_24 (14) = happyShift action_28
action_24 (15) = happyShift action_29
action_24 (16) = happyShift action_30
action_24 (17) = happyShift action_31
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (9) = happyShift action_14
action_25 (13) = happyShift action_15
action_25 (20) = happyShift action_16
action_25 (24) = happyShift action_17
action_25 (25) = happyShift action_18
action_25 (26) = happyShift action_19
action_25 (7) = happyGoto action_39
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (9) = happyShift action_14
action_26 (13) = happyShift action_15
action_26 (20) = happyShift action_16
action_26 (24) = happyShift action_17
action_26 (25) = happyShift action_18
action_26 (26) = happyShift action_19
action_26 (7) = happyGoto action_38
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (9) = happyShift action_14
action_27 (13) = happyShift action_15
action_27 (20) = happyShift action_16
action_27 (24) = happyShift action_17
action_27 (25) = happyShift action_18
action_27 (26) = happyShift action_19
action_27 (7) = happyGoto action_37
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (9) = happyShift action_14
action_28 (13) = happyShift action_15
action_28 (20) = happyShift action_16
action_28 (24) = happyShift action_17
action_28 (25) = happyShift action_18
action_28 (26) = happyShift action_19
action_28 (7) = happyGoto action_36
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (9) = happyShift action_14
action_29 (13) = happyShift action_15
action_29 (20) = happyShift action_16
action_29 (24) = happyShift action_17
action_29 (25) = happyShift action_18
action_29 (26) = happyShift action_19
action_29 (7) = happyGoto action_35
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (9) = happyShift action_14
action_30 (13) = happyShift action_15
action_30 (20) = happyShift action_16
action_30 (24) = happyShift action_17
action_30 (25) = happyShift action_18
action_30 (26) = happyShift action_19
action_30 (7) = happyGoto action_34
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (9) = happyShift action_14
action_31 (13) = happyShift action_15
action_31 (20) = happyShift action_16
action_31 (24) = happyShift action_17
action_31 (25) = happyShift action_18
action_31 (26) = happyShift action_19
action_31 (7) = happyGoto action_33
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_4

action_33 (10) = happyShift action_25
action_33 _ = happyReduce_14

action_34 (10) = happyShift action_25
action_34 (17) = happyShift action_31
action_34 _ = happyReduce_13

action_35 (10) = happyShift action_25
action_35 (17) = happyShift action_31
action_35 _ = happyReduce_10

action_36 (10) = happyShift action_25
action_36 (17) = happyShift action_31
action_36 _ = happyReduce_9

action_37 (10) = happyShift action_25
action_37 (14) = happyShift action_28
action_37 (15) = happyShift action_29
action_37 (16) = happyShift action_30
action_37 (17) = happyShift action_31
action_37 _ = happyReduce_12

action_38 (10) = happyShift action_25
action_38 (14) = happyShift action_28
action_38 (15) = happyShift action_29
action_38 (16) = happyShift action_30
action_38 (17) = happyShift action_31
action_38 _ = happyReduce_11

action_39 (10) = happyShift action_25
action_39 (12) = happyShift action_26
action_39 (13) = happyShift action_27
action_39 (14) = happyShift action_28
action_39 (15) = happyShift action_29
action_39 (16) = happyShift action_30
action_39 (17) = happyShift action_31
action_39 _ = happyReduce_20

action_40 _ = happyReduce_15

action_41 (9) = happyShift action_14
action_41 (13) = happyShift action_15
action_41 (20) = happyShift action_16
action_41 (24) = happyShift action_17
action_41 (25) = happyShift action_18
action_41 (26) = happyShift action_19
action_41 (7) = happyGoto action_42
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (10) = happyShift action_25
action_42 (12) = happyShift action_26
action_42 (13) = happyShift action_27
action_42 (14) = happyShift action_28
action_42 (15) = happyShift action_29
action_42 (16) = happyShift action_30
action_42 (17) = happyShift action_31
action_42 _ = happyReduce_8

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (FuncList happy_var_1 FEmpty
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (FuncList happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn4
		 (FEmpty
	)

happyReduce_4 = happyReduce 5 5 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (FuncDecl happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyTerminal (TokAlphaNum happy_var_1))
	 =  HappyAbsSyn6
		 (VarList happy_var_1 VEmpty
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (TokAlphaNum happy_var_1))
	 =  HappyAbsSyn6
		 (VarList happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  6 happyReduction_7
happyReduction_7  =  HappyAbsSyn6
		 (VEmpty
	)

happyReduce_8 = happyReduce 4 7 happyReduction_8
happyReduction_8 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokAlphaNum happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (ELet happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (EProd happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (EDiv happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (EPlus happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  7 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (EMinus happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (EMod happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  7 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (EPow happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  7 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  7 happyReduction_16
happyReduction_16 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ENeg happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  7 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (EPrint happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  7 happyReduction_18
happyReduction_18 (HappyTerminal (TokAlphaNum happy_var_1))
	 =  HappyAbsSyn7
		 (Var happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  7 happyReduction_19
happyReduction_19 (HappyTerminal (TokDigit happy_var_1))
	 =  HappyAbsSyn7
		 (Val happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  7 happyReduction_20
happyReduction_20 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (ENext happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 27 27 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokEquals -> cont 8;
	TokLeftPar -> cont 9;
	TokSemiCol -> cont 10;
	TokRightPar -> cont 11;
	TokPlus -> cont 12;
	TokMinus -> cont 13;
	TokProd -> cont 14;
	TokDiv -> cont 15;
	TokMod -> cont 16;
	TokPow -> cont 17;
	TokStdout -> cont 18;
	TokArrow -> cont 19;
	TokLet -> cont 20;
	TokLeftBrace -> cont 21;
	TokRightBrace -> cont 22;
	TokComma -> cont 23;
	TokPrint -> cont 24;
	TokAlphaNum happy_dollar_dollar -> cont 25;
	TokDigit happy_dollar_dollar -> cont 26;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 27 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
lang tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"
    
data FuncList = FuncList FuncDecl FuncList | FEmpty
                deriving (Show, Eq)

data FuncDecl = FuncDecl VarList Exp
                deriving (Show, Eq)

data VarList = VarList String VarList | VEmpty
               deriving (Show, Eq)

            
data Exp = ELet String Exp
           | EProd Exp Exp
           | EDiv Exp Exp
           | EPlus Exp Exp
           | EMinus Exp Exp
           | EMod Exp Exp
           | EPow Exp Exp
           | ENeg Exp
           | EPrint Exp
           | Var String
           | Val Int
           | ENext Exp Exp
           deriving (Show, Eq)
{-# LINE 1 "templates/GenericTemplate.hs" #-}





































































































































































































-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 











data Happy_IntList = HappyCons Int Happy_IntList




















infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action




indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.

