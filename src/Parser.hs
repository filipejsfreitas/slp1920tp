{-# OPTIONS_GHC -w #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Parser where

import Data.Char

import qualified State
import qualified Aexp
import qualified Bexp
import qualified Stm

import State (State (State))
import Aexp (Aexp)
import Bexp (Bexp)
import Stm (Stm)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t7 t8 t9 t10 t11 t12 t13 t14 t15
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,168) ([32768,25,0,38912,77,0,0,32,0,16,5312,0,1,0,0,0,0,0,512,0,0,0,0,0,0,0,0,64,0,0,0,19864,0,32768,1241,0,4096,49152,20,0,0,0,0,2,0,1,0,57344,2096,0,0,24,0,0,0,32768,25,0,38912,77,0,0,0,0,0,0,0,16,0,57344,0,0,6528,0,0,736,0,32768,25,0,38912,1,0,6528,0,0,19864,0,0,33582,0,0,386,0,0,0,0,32768,0,0,4096,0,0,16,0,6528,1,0,4504,0,0,16384,1,0,0,0,2048,0,0,16,0,0,0,514,0,384,8,0,4120,0,4096,0,0,1,332,0,0,0,6528,0,0,16,5312,0,1,332,0,0,0,0,64,0,0,8,0,0,0,0,0,0,256,0,0,224,0,32768,25,0,57344,0,0,6528,0,0,408,0,32768,1241,0,38912,77,0,0,0,0,0,0,0,6176,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,224,0,0,14,0,57344,0,0,0,0,0,136,0,0,0,0,0,0,0,0,0,0,224,0,0,1,332,0,0,0,128,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_readAexp","%start_readBexp","%start_readState","%start_readStm","Aexp","Bexp","State","StateVarList","StateList","StateVar","StmList","StmBlock","Stm","int","var","'+'","'*'","'-'","'('","')'","\"true\"","\"false\"","'='","'<'","'!'","'&'","'|'","'['","']'","'>'","','","';'","\"skip\"","\"if\"","\"then\"","\"else\"","\"while\"","\"do\"","'{'","'}'","':'","%eof"]
        bit_start = st * 44
        bit_end = (st + 1) * 44
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..43]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (16) = happyShift action_19
action_0 (17) = happyShift action_5
action_0 (20) = happyShift action_20
action_0 (21) = happyShift action_26
action_0 (7) = happyGoto action_25
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (16) = happyShift action_19
action_1 (17) = happyShift action_5
action_1 (20) = happyShift action_20
action_1 (21) = happyShift action_21
action_1 (23) = happyShift action_22
action_1 (24) = happyShift action_23
action_1 (27) = happyShift action_24
action_1 (7) = happyGoto action_17
action_1 (8) = happyGoto action_18
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (30) = happyShift action_16
action_2 (9) = happyGoto action_14
action_2 (11) = happyGoto action_15
action_2 _ = happyReduce_24

action_3 (17) = happyShift action_9
action_3 (35) = happyShift action_10
action_3 (36) = happyShift action_11
action_3 (39) = happyShift action_12
action_3 (41) = happyShift action_13
action_3 (13) = happyGoto action_6
action_3 (14) = happyGoto action_7
action_3 (15) = happyGoto action_8
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (17) = happyShift action_5
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_4

action_6 (34) = happyShift action_48
action_6 (44) = happyAccept
action_6 _ = happyFail (happyExpListPerState 6)

action_7 _ = happyReduce_31

action_8 _ = happyReduce_33

action_9 (43) = happyShift action_47
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_35

action_11 (16) = happyShift action_19
action_11 (17) = happyShift action_5
action_11 (20) = happyShift action_20
action_11 (21) = happyShift action_21
action_11 (23) = happyShift action_22
action_11 (24) = happyShift action_23
action_11 (27) = happyShift action_24
action_11 (7) = happyGoto action_17
action_11 (8) = happyGoto action_46
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (16) = happyShift action_19
action_12 (17) = happyShift action_5
action_12 (20) = happyShift action_20
action_12 (21) = happyShift action_21
action_12 (23) = happyShift action_22
action_12 (24) = happyShift action_23
action_12 (27) = happyShift action_24
action_12 (7) = happyGoto action_17
action_12 (8) = happyGoto action_45
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (17) = happyShift action_9
action_13 (35) = happyShift action_10
action_13 (36) = happyShift action_11
action_13 (39) = happyShift action_12
action_13 (41) = happyShift action_13
action_13 (13) = happyGoto action_44
action_13 (14) = happyGoto action_7
action_13 (15) = happyGoto action_8
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (44) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (30) = happyShift action_43
action_15 _ = happyReduce_23

action_16 (17) = happyShift action_42
action_16 (10) = happyGoto action_40
action_16 (12) = happyGoto action_41
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (18) = happyShift action_28
action_17 (19) = happyShift action_29
action_17 (20) = happyShift action_30
action_17 (25) = happyShift action_37
action_17 (26) = happyShift action_38
action_17 (32) = happyShift action_39
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (28) = happyShift action_35
action_18 (29) = happyShift action_36
action_18 (44) = happyAccept
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_5

action_20 (16) = happyShift action_19
action_20 (17) = happyShift action_5
action_20 (20) = happyShift action_20
action_20 (21) = happyShift action_26
action_20 (7) = happyGoto action_34
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (16) = happyShift action_19
action_21 (17) = happyShift action_5
action_21 (20) = happyShift action_20
action_21 (21) = happyShift action_21
action_21 (23) = happyShift action_22
action_21 (24) = happyShift action_23
action_21 (27) = happyShift action_24
action_21 (7) = happyGoto action_32
action_21 (8) = happyGoto action_33
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_11

action_23 _ = happyReduce_12

action_24 (21) = happyShift action_31
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (18) = happyShift action_28
action_25 (19) = happyShift action_29
action_25 (20) = happyShift action_30
action_25 (44) = happyAccept
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (16) = happyShift action_19
action_26 (17) = happyShift action_5
action_26 (20) = happyShift action_20
action_26 (21) = happyShift action_26
action_26 (7) = happyGoto action_27
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (18) = happyShift action_28
action_27 (19) = happyShift action_29
action_27 (20) = happyShift action_30
action_27 (22) = happyShift action_67
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (16) = happyShift action_19
action_28 (17) = happyShift action_5
action_28 (20) = happyShift action_20
action_28 (21) = happyShift action_26
action_28 (7) = happyGoto action_71
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (16) = happyShift action_19
action_29 (17) = happyShift action_5
action_29 (20) = happyShift action_20
action_29 (21) = happyShift action_26
action_29 (7) = happyGoto action_70
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (16) = happyShift action_19
action_30 (17) = happyShift action_5
action_30 (20) = happyShift action_20
action_30 (21) = happyShift action_26
action_30 (7) = happyGoto action_69
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (16) = happyShift action_19
action_31 (17) = happyShift action_5
action_31 (20) = happyShift action_20
action_31 (21) = happyShift action_21
action_31 (23) = happyShift action_22
action_31 (24) = happyShift action_23
action_31 (27) = happyShift action_24
action_31 (7) = happyGoto action_17
action_31 (8) = happyGoto action_68
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (18) = happyShift action_28
action_32 (19) = happyShift action_29
action_32 (20) = happyShift action_30
action_32 (22) = happyShift action_67
action_32 (25) = happyShift action_37
action_32 (26) = happyShift action_38
action_32 (32) = happyShift action_39
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (22) = happyShift action_66
action_33 (28) = happyShift action_35
action_33 (29) = happyShift action_36
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (18) = happyShift action_28
action_34 (19) = happyShift action_29
action_34 (20) = happyShift action_30
action_34 _ = happyReduce_7

action_35 (28) = happyShift action_65
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (29) = happyShift action_64
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (25) = happyShift action_63
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (16) = happyShift action_19
action_38 (17) = happyShift action_5
action_38 (20) = happyShift action_20
action_38 (21) = happyShift action_26
action_38 (25) = happyShift action_62
action_38 (7) = happyGoto action_61
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (16) = happyShift action_19
action_39 (17) = happyShift action_5
action_39 (20) = happyShift action_20
action_39 (21) = happyShift action_26
action_39 (25) = happyShift action_60
action_39 (7) = happyGoto action_59
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (31) = happyShift action_57
action_40 (33) = happyShift action_58
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (31) = happyShift action_56
action_41 _ = happyReduce_25

action_42 (20) = happyShift action_55
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (17) = happyShift action_42
action_43 (12) = happyGoto action_54
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (34) = happyShift action_48
action_44 (42) = happyShift action_53
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (28) = happyShift action_35
action_45 (29) = happyShift action_36
action_45 (40) = happyShift action_52
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (28) = happyShift action_35
action_46 (29) = happyShift action_36
action_46 (37) = happyShift action_51
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (25) = happyShift action_50
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (17) = happyShift action_9
action_48 (35) = happyShift action_10
action_48 (36) = happyShift action_11
action_48 (39) = happyShift action_12
action_48 (41) = happyShift action_13
action_48 (14) = happyGoto action_49
action_48 (15) = happyGoto action_8
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_32

action_50 (16) = happyShift action_19
action_50 (17) = happyShift action_5
action_50 (20) = happyShift action_20
action_50 (21) = happyShift action_26
action_50 (7) = happyGoto action_83
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (17) = happyShift action_9
action_51 (35) = happyShift action_10
action_51 (36) = happyShift action_11
action_51 (39) = happyShift action_12
action_51 (41) = happyShift action_13
action_51 (14) = happyGoto action_82
action_51 (15) = happyGoto action_8
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (17) = happyShift action_9
action_52 (35) = happyShift action_10
action_52 (36) = happyShift action_11
action_52 (39) = happyShift action_12
action_52 (41) = happyShift action_13
action_52 (14) = happyGoto action_81
action_52 (15) = happyGoto action_8
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_34

action_54 (31) = happyShift action_80
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (32) = happyShift action_79
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_27

action_57 _ = happyReduce_22

action_58 (17) = happyShift action_42
action_58 (12) = happyGoto action_78
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (18) = happyShift action_28
action_59 (19) = happyShift action_29
action_59 (20) = happyShift action_30
action_59 _ = happyReduce_15

action_60 (16) = happyShift action_19
action_60 (17) = happyShift action_5
action_60 (20) = happyShift action_20
action_60 (21) = happyShift action_26
action_60 (7) = happyGoto action_77
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (18) = happyShift action_28
action_61 (19) = happyShift action_29
action_61 (20) = happyShift action_30
action_61 _ = happyReduce_17

action_62 (16) = happyShift action_19
action_62 (17) = happyShift action_5
action_62 (20) = happyShift action_20
action_62 (21) = happyShift action_26
action_62 (7) = happyGoto action_76
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (16) = happyShift action_19
action_63 (17) = happyShift action_5
action_63 (20) = happyShift action_20
action_63 (21) = happyShift action_26
action_63 (7) = happyGoto action_75
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (16) = happyShift action_19
action_64 (17) = happyShift action_5
action_64 (20) = happyShift action_20
action_64 (21) = happyShift action_21
action_64 (23) = happyShift action_22
action_64 (24) = happyShift action_23
action_64 (27) = happyShift action_24
action_64 (7) = happyGoto action_17
action_64 (8) = happyGoto action_74
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (16) = happyShift action_19
action_65 (17) = happyShift action_5
action_65 (20) = happyShift action_20
action_65 (21) = happyShift action_21
action_65 (23) = happyShift action_22
action_65 (24) = happyShift action_23
action_65 (27) = happyShift action_24
action_65 (7) = happyGoto action_17
action_65 (8) = happyGoto action_73
action_65 _ = happyFail (happyExpListPerState 65)

action_66 _ = happyReduce_13

action_67 _ = happyReduce_6

action_68 (22) = happyShift action_72
action_68 (28) = happyShift action_35
action_68 (29) = happyShift action_36
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (18) = happyShift action_28
action_69 (19) = happyShift action_29
action_69 (20) = happyShift action_30
action_69 _ = happyReduce_10

action_70 (18) = happyShift action_28
action_70 (19) = happyShift action_29
action_70 (20) = happyShift action_30
action_70 _ = happyReduce_9

action_71 (18) = happyShift action_28
action_71 (19) = happyShift action_29
action_71 (20) = happyShift action_30
action_71 _ = happyReduce_8

action_72 _ = happyReduce_19

action_73 (28) = happyShift action_35
action_73 (29) = happyShift action_36
action_73 _ = happyReduce_20

action_74 (28) = happyShift action_35
action_74 (29) = happyShift action_36
action_74 _ = happyReduce_21

action_75 (18) = happyShift action_28
action_75 (19) = happyShift action_29
action_75 (20) = happyShift action_30
action_75 _ = happyReduce_14

action_76 (18) = happyShift action_28
action_76 (19) = happyShift action_29
action_76 (20) = happyShift action_30
action_76 _ = happyReduce_18

action_77 (18) = happyShift action_28
action_77 (19) = happyShift action_29
action_77 (20) = happyShift action_30
action_77 _ = happyReduce_16

action_78 _ = happyReduce_26

action_79 (16) = happyShift action_85
action_79 (20) = happyShift action_86
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_28

action_81 _ = happyReduce_39

action_82 (38) = happyShift action_84
action_82 _ = happyReduce_38

action_83 (18) = happyShift action_28
action_83 (19) = happyShift action_29
action_83 (20) = happyShift action_30
action_83 _ = happyReduce_36

action_84 (17) = happyShift action_9
action_84 (35) = happyShift action_10
action_84 (36) = happyShift action_11
action_84 (39) = happyShift action_12
action_84 (41) = happyShift action_13
action_84 (14) = happyGoto action_88
action_84 (15) = happyGoto action_8
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_29

action_86 (16) = happyShift action_87
action_86 _ = happyFail (happyExpListPerState 86)

action_87 _ = happyReduce_30

action_88 _ = happyReduce_37

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn7
		 (Aexp.Var happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn7
		 (Aexp.Num happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (Aexp.Sim happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Aexp.Add happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Aexp.Mult happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Aexp.Sub happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn8
		 (Bexp.True
	)

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn8
		 (Bexp.False
	)

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 8 happyReduction_14
happyReduction_14 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Bexp.Equals happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_3  8 happyReduction_15
happyReduction_15 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 (Bexp.GT happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 8 happyReduction_16
happyReduction_16 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Bexp.GE happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_3  8 happyReduction_17
happyReduction_17 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn8
		 (Bexp.LT happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 8 happyReduction_18
happyReduction_18 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Bexp.LE happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 4 8 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Bexp.Not happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 8 happyReduction_20
happyReduction_20 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Bexp.And happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 4 8 happyReduction_21
happyReduction_21 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Bexp.Or happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_3  9 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (State happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  9 happyReduction_23
happyReduction_23 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn9
		 (State happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_0  9 happyReduction_24
happyReduction_24  =  HappyAbsSyn9
		 (State []
	)

happyReduce_25 = happySpecReduce_1  10 happyReduction_25
happyReduction_25 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  10 happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  11 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn11
		 ([happy_var_2]
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 4 11 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (happy_var_1 ++ [happy_var_3]
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 4 12 happyReduction_29
happyReduction_29 ((HappyTerminal (TokenInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ((happy_var_1, happy_var_4)
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 5 12 happyReduction_30
happyReduction_30 ((HappyTerminal (TokenInt happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ((happy_var_1, -happy_var_5)
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_1  13 happyReduction_31
happyReduction_31 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  13 happyReduction_32
happyReduction_32 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (Stm.Comp happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  14 happyReduction_33
happyReduction_33 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  14 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  15 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn15
		 (Stm.Skip
	)

happyReduce_36 = happyReduce 4 15 happyReduction_36
happyReduction_36 ((HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Stm.Assign happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_37 = happyReduce 6 15 happyReduction_37
happyReduction_37 ((HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Stm.ITE happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_38 = happyReduce 4 15 happyReduction_38
happyReduction_38 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Stm.If happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 4 15 happyReduction_39
happyReduction_39 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Stm.While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 44 44 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt happy_dollar_dollar -> cont 16;
	TokenVar happy_dollar_dollar -> cont 17;
	TokenAdd -> cont 18;
	TokenMult -> cont 19;
	TokenSub -> cont 20;
	TokenOP -> cont 21;
	TokenCP -> cont 22;
	TokenTrue -> cont 23;
	TokenFalse -> cont 24;
	TokenEq -> cont 25;
	TokenLT -> cont 26;
	TokenNot -> cont 27;
	TokenAnd -> cont 28;
	TokenOr -> cont 29;
	TokenOSB -> cont 30;
	TokenCSB -> cont 31;
	TokenGT -> cont 32;
	TokenComma -> cont 33;
	TokenSemicolon -> cont 34;
	TokenSkip -> cont 35;
	TokenIf -> cont 36;
	TokenThen -> cont 37;
	TokenElse -> cont 38;
	TokenWhile -> cont 39;
	TokenDo -> cont 40;
	TokenOCB -> cont 41;
	TokenCCB -> cont 42;
	TokenColon -> cont 43;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 44 tk tks = happyError' (tks, explist)
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
readAexp tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn7 z -> happyReturn z; _other -> notHappyAtAll })

readBexp tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

readState tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn9 z -> happyReturn z; _other -> notHappyAtAll })

readStm tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn13 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- Called in case of parsing errors
parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens

-- Defines the tokens that the lexer can parse
data Token =
            -- Aexp tokens
             TokenInt State.Val
           | TokenVar State.Var
           | TokenAdd
           | TokenMult
           | TokenSub
           | TokenOP
           | TokenCP
           -- Bexp tokens
           | TokenTrue
           | TokenFalse
           | TokenEq
           | TokenLT
           | TokenNot
           | TokenAnd
           | TokenOr
           -- State tokens
           | TokenOSB
           | TokenCSB
           | TokenGT
           | TokenComma
           -- Stm tokens
           | TokenSemicolon
           | TokenSkip
           | TokenIf
           | TokenThen
           | TokenElse
           | TokenWhile
           | TokenDo
           | TokenOCB
           | TokenCCB
           | TokenColon

           deriving (Show)

-- The lexer splits the input string into tokens
lexer :: String -> [Token]
lexer [] = []

lexer ('+':cs) = TokenAdd : lexer cs
lexer ('-':cs) = TokenSub : lexer cs
lexer ('*':cs) = TokenMult : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs

lexer ('t':'r':'u':'e':cs) = TokenTrue : lexer cs
lexer ('f':'a':'l':'s':'e':cs) = TokenFalse : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('<':cs) = TokenLT : lexer cs
lexer ('!':cs) = TokenNot : lexer cs
lexer ('&':cs) = TokenAnd : lexer cs
lexer ('|':cs) = TokenOr : lexer cs

lexer ('[':cs) = TokenOSB : lexer cs
lexer (']':cs) = TokenCSB : lexer cs
lexer ('>':cs) = TokenGT : lexer cs
lexer (',':cs) = TokenComma : lexer cs

lexer (';':cs) = TokenSemicolon : lexer cs
lexer ('s':'k':'i':'p':cs) = TokenSkip : lexer cs
lexer ('i':'f':cs) = TokenIf : lexer cs
lexer ('t':'h':'e':'n':cs) = TokenThen : lexer cs
lexer ('e':'l':'s':'e':cs) = TokenElse : lexer cs
lexer ('w':'h':'i':'l':'e':cs) = TokenWhile : lexer cs
lexer ('d':'o':cs) = TokenDo : lexer cs
lexer ('{':cs) = TokenOCB : lexer cs
lexer ('}':cs) = TokenCCB : lexer cs
lexer (':':cs) = TokenColon : lexer cs

lexer ('"':cs) = lexer cs -- Ignore quotes around strings
lexer (c:cs) | isSpace c = lexer cs
             | isAlpha c = lexVar (c:cs)
             | isDigit c = lexNum (c:cs)

-- Lexer helper functions
lexVar cs = TokenVar var : lexer rest
            -- Nomes de variáveis podem ter dígitos em todos os caracteres, excepto no primeiro
            where (var, rest) = span (\x -> isAlpha x || isDigit x) cs

lexNum cs = TokenInt (read num) : lexer rest
            where (num, rest) = span isDigit cs

-- Define instances of read for the datatypes that can be parsed

instance Read Aexp.Aexp where
   readsPrec _ input = [(readAexp $ lexer input, "")]

instance Read Bexp.Bexp where
   readsPrec _ input = [(readBexp $ lexer input, "")]

instance Read State.State where
    readsPrec _ input = [(readState $ lexer input, "")]

instance Read Stm.Stm where
    readsPrec _ input = [(readStm $ lexer input, "")]
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
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
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

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
