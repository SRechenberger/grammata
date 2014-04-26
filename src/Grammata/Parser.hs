{-# OPTIONS_GHC -w #-}
{-|
Module      : Grammata.Parser
Description : parser produced by Happy Version 1.19.0
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : experimental
Portability : portable
-}

module Grammata.Parser 
(
    parse
)
where

import Grammata.Parser.Lexer (tokenize)
import Grammata.Parser.Analysis (Analysis, syntaxError)
import Grammata.Parser.Token (Token (Id, Num, Br, Sep, Key, Op))
import qualified Grammata.Parser.AST as AST (Program (Program), 
    Declaration (Num, Func), 
    Statement ((:=), For, While, DoWhile, If, Return), 
    Expression (Variable, Constant, Binary, Unary, Application))

import General (Identifier)

-- parser produced by Happy Version 1.19.0

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (AST.Program)
	| HappyAbsSyn5 ([AST.Declaration])
	| HappyAbsSyn6 ([Identifier])
	| HappyAbsSyn7 (Identifier)
	| HappyAbsSyn8 (AST.Declaration)
	| HappyAbsSyn9 ([AST.Statement])
	| HappyAbsSyn10 (AST.Statement)
	| HappyAbsSyn11 ([AST.Expression])
	| HappyAbsSyn12 (AST.Expression)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115 :: () => Int -> ({-HappyReduction (Analysis String String) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Analysis String String) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Analysis String String) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Analysis String String) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40 :: () => ({-HappyReduction (Analysis String String) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Analysis String String) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Analysis String String) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Analysis String String) HappyAbsSyn)

action_0 (19) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (19) = happyShift action_2
action_1 _ = happyFail

action_2 (15) = happyShift action_4
action_2 _ = happyFail

action_3 (44) = happyAccept
action_3 _ = happyFail

action_4 (20) = happyShift action_7
action_4 (21) = happyShift action_8
action_4 (5) = happyGoto action_5
action_4 (8) = happyGoto action_6
action_4 _ = happyReduce_3

action_5 (22) = happyShift action_14
action_5 (23) = happyShift action_15
action_5 (24) = happyShift action_16
action_5 (25) = happyShift action_17
action_5 (28) = happyShift action_18
action_5 (29) = happyShift action_19
action_5 (9) = happyGoto action_12
action_5 (10) = happyGoto action_13
action_5 _ = happyReduce_13

action_6 (18) = happyShift action_11
action_6 _ = happyFail

action_7 (29) = happyShift action_10
action_7 _ = happyFail

action_8 (29) = happyShift action_9
action_8 _ = happyFail

action_9 (31) = happyShift action_34
action_9 _ = happyFail

action_10 (31) = happyShift action_33
action_10 _ = happyReduce_9

action_11 (20) = happyShift action_7
action_11 (21) = happyShift action_8
action_11 (5) = happyGoto action_32
action_11 (8) = happyGoto action_6
action_11 _ = happyReduce_3

action_12 (16) = happyShift action_31
action_12 _ = happyFail

action_13 (18) = happyShift action_30
action_13 _ = happyFail

action_14 (13) = happyShift action_29
action_14 _ = happyFail

action_15 (13) = happyShift action_28
action_15 _ = happyFail

action_16 (13) = happyShift action_24
action_16 (29) = happyShift action_25
action_16 (30) = happyShift action_26
action_16 (39) = happyShift action_27
action_16 (12) = happyGoto action_23
action_16 _ = happyFail

action_17 (13) = happyShift action_22
action_17 _ = happyFail

action_18 (15) = happyShift action_21
action_18 _ = happyFail

action_19 (31) = happyShift action_20
action_19 _ = happyFail

action_20 (13) = happyShift action_24
action_20 (29) = happyShift action_25
action_20 (30) = happyShift action_26
action_20 (39) = happyShift action_27
action_20 (12) = happyGoto action_57
action_20 _ = happyFail

action_21 (22) = happyShift action_14
action_21 (23) = happyShift action_15
action_21 (24) = happyShift action_16
action_21 (25) = happyShift action_17
action_21 (28) = happyShift action_18
action_21 (29) = happyShift action_19
action_21 (9) = happyGoto action_56
action_21 (10) = happyGoto action_13
action_21 _ = happyReduce_13

action_22 (13) = happyShift action_24
action_22 (29) = happyShift action_25
action_22 (30) = happyShift action_26
action_22 (39) = happyShift action_27
action_22 (12) = happyGoto action_55
action_22 _ = happyFail

action_23 (32) = happyShift action_43
action_23 (33) = happyShift action_44
action_23 (34) = happyShift action_45
action_23 (35) = happyShift action_46
action_23 (36) = happyShift action_47
action_23 (37) = happyShift action_48
action_23 (38) = happyShift action_49
action_23 (39) = happyShift action_50
action_23 (40) = happyShift action_51
action_23 (41) = happyShift action_52
action_23 (42) = happyShift action_53
action_23 (43) = happyShift action_54
action_23 _ = happyReduce_20

action_24 (13) = happyShift action_24
action_24 (29) = happyShift action_25
action_24 (30) = happyShift action_26
action_24 (39) = happyShift action_27
action_24 (12) = happyGoto action_42
action_24 _ = happyFail

action_25 (13) = happyShift action_41
action_25 _ = happyReduce_24

action_26 _ = happyReduce_25

action_27 (13) = happyShift action_24
action_27 (29) = happyShift action_25
action_27 (30) = happyShift action_26
action_27 (39) = happyShift action_27
action_27 (12) = happyGoto action_40
action_27 _ = happyFail

action_28 (13) = happyShift action_24
action_28 (29) = happyShift action_25
action_28 (30) = happyShift action_26
action_28 (39) = happyShift action_27
action_28 (12) = happyGoto action_39
action_28 _ = happyFail

action_29 (29) = happyShift action_38
action_29 _ = happyFail

action_30 (22) = happyShift action_14
action_30 (23) = happyShift action_15
action_30 (24) = happyShift action_16
action_30 (25) = happyShift action_17
action_30 (28) = happyShift action_18
action_30 (29) = happyShift action_19
action_30 (9) = happyGoto action_37
action_30 (10) = happyGoto action_13
action_30 _ = happyReduce_13

action_31 _ = happyReduce_1

action_32 _ = happyReduce_2

action_33 (13) = happyShift action_24
action_33 (29) = happyShift action_25
action_33 (30) = happyShift action_26
action_33 (39) = happyShift action_27
action_33 (12) = happyGoto action_36
action_33 _ = happyFail

action_34 (21) = happyShift action_35
action_34 _ = happyFail

action_35 (13) = happyShift action_77
action_35 _ = happyFail

action_36 (32) = happyShift action_43
action_36 (33) = happyShift action_44
action_36 (34) = happyShift action_45
action_36 (35) = happyShift action_46
action_36 (36) = happyShift action_47
action_36 (37) = happyShift action_48
action_36 (38) = happyShift action_49
action_36 (39) = happyShift action_50
action_36 (40) = happyShift action_51
action_36 (41) = happyShift action_52
action_36 (42) = happyShift action_53
action_36 (43) = happyShift action_54
action_36 _ = happyReduce_10

action_37 _ = happyReduce_12

action_38 (18) = happyShift action_76
action_38 _ = happyFail

action_39 (14) = happyShift action_75
action_39 (32) = happyShift action_43
action_39 (33) = happyShift action_44
action_39 (34) = happyShift action_45
action_39 (35) = happyShift action_46
action_39 (36) = happyShift action_47
action_39 (37) = happyShift action_48
action_39 (38) = happyShift action_49
action_39 (39) = happyShift action_50
action_39 (40) = happyShift action_51
action_39 (41) = happyShift action_52
action_39 (42) = happyShift action_53
action_39 (43) = happyShift action_54
action_39 _ = happyFail

action_40 (32) = happyShift action_43
action_40 (33) = happyShift action_44
action_40 (34) = happyShift action_45
action_40 (35) = happyShift action_46
action_40 (36) = happyShift action_47
action_40 (37) = happyShift action_48
action_40 (38) = happyShift action_49
action_40 (39) = happyShift action_50
action_40 (40) = happyShift action_51
action_40 (41) = happyShift action_52
action_40 (42) = happyShift action_53
action_40 (43) = happyShift action_54
action_40 _ = happyReduce_38

action_41 (13) = happyShift action_24
action_41 (29) = happyShift action_25
action_41 (30) = happyShift action_26
action_41 (39) = happyShift action_27
action_41 (11) = happyGoto action_73
action_41 (12) = happyGoto action_74
action_41 _ = happyReduce_23

action_42 (14) = happyShift action_72
action_42 (32) = happyShift action_43
action_42 (33) = happyShift action_44
action_42 (34) = happyShift action_45
action_42 (35) = happyShift action_46
action_42 (36) = happyShift action_47
action_42 (37) = happyShift action_48
action_42 (38) = happyShift action_49
action_42 (39) = happyShift action_50
action_42 (40) = happyShift action_51
action_42 (41) = happyShift action_52
action_42 (42) = happyShift action_53
action_42 (43) = happyShift action_54
action_42 _ = happyFail

action_43 (13) = happyShift action_24
action_43 (29) = happyShift action_25
action_43 (30) = happyShift action_26
action_43 (39) = happyShift action_27
action_43 (12) = happyGoto action_71
action_43 _ = happyFail

action_44 (13) = happyShift action_24
action_44 (29) = happyShift action_25
action_44 (30) = happyShift action_26
action_44 (39) = happyShift action_27
action_44 (12) = happyGoto action_70
action_44 _ = happyFail

action_45 (13) = happyShift action_24
action_45 (29) = happyShift action_25
action_45 (30) = happyShift action_26
action_45 (39) = happyShift action_27
action_45 (12) = happyGoto action_69
action_45 _ = happyFail

action_46 (13) = happyShift action_24
action_46 (29) = happyShift action_25
action_46 (30) = happyShift action_26
action_46 (39) = happyShift action_27
action_46 (12) = happyGoto action_68
action_46 _ = happyFail

action_47 (13) = happyShift action_24
action_47 (29) = happyShift action_25
action_47 (30) = happyShift action_26
action_47 (39) = happyShift action_27
action_47 (12) = happyGoto action_67
action_47 _ = happyFail

action_48 (13) = happyShift action_24
action_48 (29) = happyShift action_25
action_48 (30) = happyShift action_26
action_48 (39) = happyShift action_27
action_48 (12) = happyGoto action_66
action_48 _ = happyFail

action_49 (13) = happyShift action_24
action_49 (29) = happyShift action_25
action_49 (30) = happyShift action_26
action_49 (39) = happyShift action_27
action_49 (12) = happyGoto action_65
action_49 _ = happyFail

action_50 (13) = happyShift action_24
action_50 (29) = happyShift action_25
action_50 (30) = happyShift action_26
action_50 (39) = happyShift action_27
action_50 (12) = happyGoto action_64
action_50 _ = happyFail

action_51 (13) = happyShift action_24
action_51 (29) = happyShift action_25
action_51 (30) = happyShift action_26
action_51 (39) = happyShift action_27
action_51 (12) = happyGoto action_63
action_51 _ = happyFail

action_52 (13) = happyShift action_24
action_52 (29) = happyShift action_25
action_52 (30) = happyShift action_26
action_52 (39) = happyShift action_27
action_52 (12) = happyGoto action_62
action_52 _ = happyFail

action_53 (13) = happyShift action_24
action_53 (29) = happyShift action_25
action_53 (30) = happyShift action_26
action_53 (39) = happyShift action_27
action_53 (12) = happyGoto action_61
action_53 _ = happyFail

action_54 (13) = happyShift action_24
action_54 (29) = happyShift action_25
action_54 (30) = happyShift action_26
action_54 (39) = happyShift action_27
action_54 (12) = happyGoto action_60
action_54 _ = happyFail

action_55 (14) = happyShift action_59
action_55 (32) = happyShift action_43
action_55 (33) = happyShift action_44
action_55 (34) = happyShift action_45
action_55 (35) = happyShift action_46
action_55 (36) = happyShift action_47
action_55 (37) = happyShift action_48
action_55 (38) = happyShift action_49
action_55 (39) = happyShift action_50
action_55 (40) = happyShift action_51
action_55 (41) = happyShift action_52
action_55 (42) = happyShift action_53
action_55 (43) = happyShift action_54
action_55 _ = happyFail

action_56 (16) = happyShift action_58
action_56 _ = happyFail

action_57 (32) = happyShift action_43
action_57 (33) = happyShift action_44
action_57 (34) = happyShift action_45
action_57 (35) = happyShift action_46
action_57 (36) = happyShift action_47
action_57 (37) = happyShift action_48
action_57 (38) = happyShift action_49
action_57 (39) = happyShift action_50
action_57 (40) = happyShift action_51
action_57 (41) = happyShift action_52
action_57 (42) = happyShift action_53
action_57 (43) = happyShift action_54
action_57 _ = happyReduce_14

action_58 (23) = happyShift action_87
action_58 _ = happyFail

action_59 (26) = happyShift action_86
action_59 _ = happyFail

action_60 (32) = happyShift action_43
action_60 (33) = happyShift action_44
action_60 (34) = happyShift action_45
action_60 (35) = happyShift action_46
action_60 (36) = happyShift action_47
action_60 (37) = happyShift action_48
action_60 (38) = happyShift action_49
action_60 (39) = happyShift action_50
action_60 _ = happyReduce_31

action_61 (32) = happyShift action_43
action_61 (33) = happyShift action_44
action_61 (34) = happyShift action_45
action_61 (35) = happyShift action_46
action_61 (36) = happyShift action_47
action_61 (37) = happyShift action_48
action_61 (38) = happyShift action_49
action_61 (39) = happyShift action_50
action_61 _ = happyReduce_30

action_62 (32) = happyShift action_43
action_62 (33) = happyShift action_44
action_62 (34) = happyShift action_45
action_62 (35) = happyShift action_46
action_62 (36) = happyShift action_47
action_62 (37) = happyShift action_48
action_62 (38) = happyShift action_49
action_62 (39) = happyShift action_50
action_62 _ = happyReduce_29

action_63 (32) = happyShift action_43
action_63 (33) = happyShift action_44
action_63 (34) = happyShift action_45
action_63 (35) = happyShift action_46
action_63 (36) = happyShift action_47
action_63 (37) = happyShift action_48
action_63 (38) = happyShift action_49
action_63 (39) = happyShift action_50
action_63 _ = happyReduce_28

action_64 (32) = happyShift action_43
action_64 (33) = happyShift action_44
action_64 (34) = happyShift action_45
action_64 (35) = happyShift action_46
action_64 (36) = happyShift action_47
action_64 (37) = happyShift action_48
action_64 _ = happyReduce_27

action_65 (32) = happyShift action_43
action_65 (33) = happyShift action_44
action_65 (34) = happyShift action_45
action_65 (35) = happyShift action_46
action_65 (36) = happyShift action_47
action_65 (37) = happyShift action_48
action_65 _ = happyReduce_26

action_66 _ = happyReduce_37

action_67 _ = happyReduce_36

action_68 _ = happyReduce_33

action_69 _ = happyReduce_32

action_70 _ = happyReduce_35

action_71 _ = happyReduce_34

action_72 _ = happyReduce_40

action_73 (14) = happyShift action_85
action_73 _ = happyFail

action_74 (17) = happyShift action_84
action_74 (32) = happyShift action_43
action_74 (33) = happyShift action_44
action_74 (34) = happyShift action_45
action_74 (35) = happyShift action_46
action_74 (36) = happyShift action_47
action_74 (37) = happyShift action_48
action_74 (38) = happyShift action_49
action_74 (39) = happyShift action_50
action_74 (40) = happyShift action_51
action_74 (41) = happyShift action_52
action_74 (42) = happyShift action_53
action_74 (43) = happyShift action_54
action_74 _ = happyReduce_22

action_75 (15) = happyShift action_83
action_75 _ = happyFail

action_76 (13) = happyShift action_24
action_76 (29) = happyShift action_25
action_76 (30) = happyShift action_26
action_76 (39) = happyShift action_27
action_76 (12) = happyGoto action_82
action_76 _ = happyFail

action_77 (20) = happyShift action_80
action_77 (21) = happyShift action_81
action_77 (6) = happyGoto action_78
action_77 (7) = happyGoto action_79
action_77 _ = happyReduce_6

action_78 (14) = happyShift action_96
action_78 _ = happyFail

action_79 (17) = happyShift action_95
action_79 _ = happyReduce_5

action_80 (29) = happyShift action_94
action_80 _ = happyFail

action_81 (29) = happyShift action_93
action_81 _ = happyFail

action_82 (18) = happyShift action_92
action_82 (32) = happyShift action_43
action_82 (33) = happyShift action_44
action_82 (34) = happyShift action_45
action_82 (35) = happyShift action_46
action_82 (36) = happyShift action_47
action_82 (37) = happyShift action_48
action_82 (38) = happyShift action_49
action_82 (39) = happyShift action_50
action_82 (40) = happyShift action_51
action_82 (41) = happyShift action_52
action_82 (42) = happyShift action_53
action_82 (43) = happyShift action_54
action_82 _ = happyFail

action_83 (22) = happyShift action_14
action_83 (23) = happyShift action_15
action_83 (24) = happyShift action_16
action_83 (25) = happyShift action_17
action_83 (28) = happyShift action_18
action_83 (29) = happyShift action_19
action_83 (9) = happyGoto action_91
action_83 (10) = happyGoto action_13
action_83 _ = happyReduce_13

action_84 (13) = happyShift action_24
action_84 (29) = happyShift action_25
action_84 (30) = happyShift action_26
action_84 (39) = happyShift action_27
action_84 (11) = happyGoto action_90
action_84 (12) = happyGoto action_74
action_84 _ = happyReduce_23

action_85 _ = happyReduce_39

action_86 (15) = happyShift action_89
action_86 _ = happyFail

action_87 (13) = happyShift action_88
action_87 _ = happyFail

action_88 (13) = happyShift action_24
action_88 (29) = happyShift action_25
action_88 (30) = happyShift action_26
action_88 (39) = happyShift action_27
action_88 (12) = happyGoto action_102
action_88 _ = happyFail

action_89 (22) = happyShift action_14
action_89 (23) = happyShift action_15
action_89 (24) = happyShift action_16
action_89 (25) = happyShift action_17
action_89 (28) = happyShift action_18
action_89 (29) = happyShift action_19
action_89 (9) = happyGoto action_101
action_89 (10) = happyGoto action_13
action_89 _ = happyReduce_13

action_90 _ = happyReduce_21

action_91 (16) = happyShift action_100
action_91 _ = happyFail

action_92 (13) = happyShift action_24
action_92 (29) = happyShift action_25
action_92 (30) = happyShift action_26
action_92 (39) = happyShift action_27
action_92 (12) = happyGoto action_99
action_92 _ = happyFail

action_93 _ = happyReduce_8

action_94 _ = happyReduce_7

action_95 (20) = happyShift action_80
action_95 (21) = happyShift action_81
action_95 (6) = happyGoto action_98
action_95 (7) = happyGoto action_79
action_95 _ = happyReduce_6

action_96 (15) = happyShift action_97
action_96 _ = happyFail

action_97 (20) = happyShift action_7
action_97 (21) = happyShift action_8
action_97 (5) = happyGoto action_106
action_97 (8) = happyGoto action_6
action_97 _ = happyReduce_3

action_98 _ = happyReduce_4

action_99 (14) = happyShift action_105
action_99 (32) = happyShift action_43
action_99 (33) = happyShift action_44
action_99 (34) = happyShift action_45
action_99 (35) = happyShift action_46
action_99 (36) = happyShift action_47
action_99 (37) = happyShift action_48
action_99 (38) = happyShift action_49
action_99 (39) = happyShift action_50
action_99 (40) = happyShift action_51
action_99 (41) = happyShift action_52
action_99 (42) = happyShift action_53
action_99 (43) = happyShift action_54
action_99 _ = happyFail

action_100 _ = happyReduce_16

action_101 (16) = happyShift action_104
action_101 _ = happyFail

action_102 (14) = happyShift action_103
action_102 (32) = happyShift action_43
action_102 (33) = happyShift action_44
action_102 (34) = happyShift action_45
action_102 (35) = happyShift action_46
action_102 (36) = happyShift action_47
action_102 (37) = happyShift action_48
action_102 (38) = happyShift action_49
action_102 (39) = happyShift action_50
action_102 (40) = happyShift action_51
action_102 (41) = happyShift action_52
action_102 (42) = happyShift action_53
action_102 (43) = happyShift action_54
action_102 _ = happyFail

action_103 _ = happyReduce_17

action_104 (27) = happyShift action_109
action_104 _ = happyReduce_18

action_105 (15) = happyShift action_108
action_105 _ = happyFail

action_106 (22) = happyShift action_14
action_106 (23) = happyShift action_15
action_106 (24) = happyShift action_16
action_106 (25) = happyShift action_17
action_106 (28) = happyShift action_18
action_106 (29) = happyShift action_19
action_106 (9) = happyGoto action_107
action_106 (10) = happyGoto action_13
action_106 _ = happyReduce_13

action_107 (16) = happyShift action_112
action_107 _ = happyFail

action_108 (22) = happyShift action_14
action_108 (23) = happyShift action_15
action_108 (24) = happyShift action_16
action_108 (25) = happyShift action_17
action_108 (28) = happyShift action_18
action_108 (29) = happyShift action_19
action_108 (9) = happyGoto action_111
action_108 (10) = happyGoto action_13
action_108 _ = happyReduce_13

action_109 (15) = happyShift action_110
action_109 _ = happyFail

action_110 (22) = happyShift action_14
action_110 (23) = happyShift action_15
action_110 (24) = happyShift action_16
action_110 (25) = happyShift action_17
action_110 (28) = happyShift action_18
action_110 (29) = happyShift action_19
action_110 (9) = happyGoto action_114
action_110 (10) = happyGoto action_13
action_110 _ = happyReduce_13

action_111 (16) = happyShift action_113
action_111 _ = happyFail

action_112 _ = happyReduce_11

action_113 _ = happyReduce_15

action_114 (16) = happyShift action_115
action_114 _ = happyFail

action_115 _ = happyReduce_19

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (AST.Program happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_0  6 happyReduction_6
happyReduction_6  =  HappyAbsSyn6
		 ([]
	)

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 (HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn7
		 ((\(Id _ id) -> id) happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happyMonadReduce 2 7 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest) tk
	 = happyThen (( syntaxError $ "Higher order functions are not implemented yet. Error at " ++ show happy_var_1)
	) (\r -> happyReturn (HappyAbsSyn7 r))

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 (HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn8
		 (AST.Num ((\(Id _ id) -> id) happy_var_2) Nothing
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 8 happyReduction_10
happyReduction_10 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (AST.Num ((\(Id _ id) -> id) happy_var_2) (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 11 8 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_10) `HappyStk`
	(HappyAbsSyn5  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (AST.Func ((\(Id _ id) -> id) happy_var_2) happy_var_6 happy_var_9 happy_var_10
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_0  9 happyReduction_13
happyReduction_13  =  HappyAbsSyn9
		 ([]
	)

happyReduce_14 = happySpecReduce_3  10 happyReduction_14
happyReduction_14 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (((\(Id _ id) -> id) happy_var_1) AST.:= happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 11 10 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (AST.For ((\(Id _ id) -> id) happy_var_3) happy_var_5 happy_var_7 happy_var_10
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 7 10 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (AST.While happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 8 10 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (AST.DoWhile happy_var_7 happy_var_3
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 8 10 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (AST.If happy_var_3 happy_var_7 []
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 12 10 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_11) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (AST.If happy_var_3 happy_var_7 happy_var_11
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_2  10 happyReduction_20
happyReduction_20 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (AST.Return happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  11 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  11 happyReduction_22
happyReduction_22 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_0  11 happyReduction_23
happyReduction_23  =  HappyAbsSyn11
		 ([]
	)

happyReduce_24 = happySpecReduce_1  12 happyReduction_24
happyReduction_24 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Variable ((\(Id _ id) -> id) happy_var_1)
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  12 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Constant ((\(Num _ n) -> n) happy_var_1)
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  12 happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (+) happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  12 happyReduction_27
happyReduction_27 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (-) happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  12 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (*) happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  12 happyReduction_29
happyReduction_29 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (/) happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  12 happyReduction_30
happyReduction_30 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> toEnum (fromEnum a `div` fromEnum b)) happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  12 happyReduction_31
happyReduction_31 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> toEnum (fromEnum a `mod` fromEnum b)) happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  12 happyReduction_32
happyReduction_32 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> if a < b then 1 else (-1)) happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  12 happyReduction_33
happyReduction_33 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> if a > b then 1 else (-1)) happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  12 happyReduction_34
happyReduction_34 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> if a <= b then 1 else (-1)) happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  12 happyReduction_35
happyReduction_35 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> if a >= b then 1 else (-1)) happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  12 happyReduction_36
happyReduction_36 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> if a == b then 1 else (-1)) happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  12 happyReduction_37
happyReduction_37 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> if a /= b then 1 else (-1)) happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_2  12 happyReduction_38
happyReduction_38 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (AST.Unary (\a -> negate a) happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happyReduce 4 12 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (AST.Application ((\(Id _ id) -> id) happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_3  12 happyReduction_40
happyReduction_40 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 44 44 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Br p '(' -> cont 13;
	Br p ')' -> cont 14;
	Br p '{' -> cont 15;
	Br p '}' -> cont 16;
	Sep p ',' -> cont 17;
	Sep p ';' -> cont 18;
	Key p "program" -> cont 19;
	Key p "num" -> cont 20;
	Key p "func" -> cont 21;
	Key p "for" -> cont 22;
	Key p "while" -> cont 23;
	Key p "return" -> cont 24;
	Key p "if" -> cont 25;
	Key p "then" -> cont 26;
	Key p "else" -> cont 27;
	Key p "do" -> cont 28;
	Id p id -> cont 29;
	Num p c -> cont 30;
	Op p ":=" -> cont 31;
	Op p "<=" -> cont 32;
	Op p ">=" -> cont 33;
	Op p "<" -> cont 34;
	Op p ">" -> cont 35;
	Op p "==" -> cont 36;
	Op p "!=" -> cont 37;
	Op p "+" -> cont 38;
	Op p "-" -> cont 39;
	Op p "*" -> cont 40;
	Op p "/" -> cont 41;
	Op p "div" -> cont 42;
	Op p "%" -> cont 43;
	_ -> happyError' (tk:tks)
	}

happyError_ 44 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Analysis String String a -> (a -> Analysis String String b) -> Analysis String String b
happyThen = (>>=)
happyReturn :: () => a -> Analysis String String a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Analysis String String a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> Analysis String String a
happyError' = happyError

parseGrammata tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- |Parses the script, returning the AST
parse :: String -> Analysis String String (AST.Program)
parse input = tokenize input >>= parseGrammata

-- |Function invoked on error.
happyError :: [Token] -> Analysis String String a
happyError tokens = case tokens of
    []  -> syntaxError "Unexpected end of file."
    t:_ -> syntaxError $ "Syntactical error at " ++ show t
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 5 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 45 "templates/GenericTemplate.hs" #-}








{-# LINE 66 "templates/GenericTemplate.hs" #-}

{-# LINE 76 "templates/GenericTemplate.hs" #-}

{-# LINE 85 "templates/GenericTemplate.hs" #-}

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

{-# LINE 154 "templates/GenericTemplate.hs" #-}

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
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 255 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 321 "templates/GenericTemplate.hs" #-}
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
