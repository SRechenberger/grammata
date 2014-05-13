{-# OPTIONS_GHC -w #-}
{-|
Module      : Grammata.Parser
Description : parser produced by Happy Version 1.19.0
Maintainer  : sascha.rechenberger@uni-ulm.de
Stability   : experimental
Portability : portable
Copyright   : (c) Sascha Rechenberger, 2014
License     : GPL-3

This file is part of grammata.

grammata is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

grammata is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with grammata. If not, see <http://www.gnu.org/licenses/>.
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
    Declaration (Var, Num, Func), 
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
 action_115,
 action_116 :: () => Int -> ({-HappyReduction (Analysis String String) = -}
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
 happyReduce_40,
 happyReduce_41,
 happyReduce_42 :: () => ({-HappyReduction (Analysis String String) = -}
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

action_3 (48) = happyAccept
action_3 _ = happyFail

action_4 (20) = happyShift action_7
action_4 (5) = happyGoto action_5
action_4 (8) = happyGoto action_6
action_4 _ = happyReduce_3

action_5 (23) = happyShift action_12
action_5 (24) = happyShift action_13
action_5 (25) = happyShift action_14
action_5 (26) = happyShift action_15
action_5 (29) = happyShift action_16
action_5 (30) = happyShift action_17
action_5 (9) = happyGoto action_10
action_5 (10) = happyGoto action_11
action_5 _ = happyReduce_12

action_6 (18) = happyShift action_9
action_6 _ = happyFail

action_7 (30) = happyShift action_8
action_7 _ = happyFail

action_8 (32) = happyShift action_32
action_8 _ = happyReduce_8

action_9 (20) = happyShift action_7
action_9 (5) = happyGoto action_31
action_9 (8) = happyGoto action_6
action_9 _ = happyReduce_3

action_10 (16) = happyShift action_30
action_10 _ = happyFail

action_11 (18) = happyShift action_29
action_11 _ = happyFail

action_12 (13) = happyShift action_28
action_12 _ = happyFail

action_13 (13) = happyShift action_27
action_13 _ = happyFail

action_14 (13) = happyShift action_22
action_14 (30) = happyShift action_23
action_14 (31) = happyShift action_24
action_14 (40) = happyShift action_25
action_14 (47) = happyShift action_26
action_14 (12) = happyGoto action_21
action_14 _ = happyFail

action_15 (13) = happyShift action_20
action_15 _ = happyFail

action_16 (15) = happyShift action_19
action_16 _ = happyFail

action_17 (32) = happyShift action_18
action_17 _ = happyFail

action_18 (13) = happyShift action_22
action_18 (30) = happyShift action_23
action_18 (31) = happyShift action_24
action_18 (40) = happyShift action_25
action_18 (47) = happyShift action_26
action_18 (12) = happyGoto action_58
action_18 _ = happyFail

action_19 (23) = happyShift action_12
action_19 (24) = happyShift action_13
action_19 (25) = happyShift action_14
action_19 (26) = happyShift action_15
action_19 (29) = happyShift action_16
action_19 (30) = happyShift action_17
action_19 (9) = happyGoto action_57
action_19 (10) = happyGoto action_11
action_19 _ = happyReduce_12

action_20 (13) = happyShift action_22
action_20 (30) = happyShift action_23
action_20 (31) = happyShift action_24
action_20 (40) = happyShift action_25
action_20 (47) = happyShift action_26
action_20 (12) = happyGoto action_56
action_20 _ = happyFail

action_21 (33) = happyShift action_42
action_21 (34) = happyShift action_43
action_21 (35) = happyShift action_44
action_21 (36) = happyShift action_45
action_21 (37) = happyShift action_46
action_21 (38) = happyShift action_47
action_21 (39) = happyShift action_48
action_21 (40) = happyShift action_49
action_21 (41) = happyShift action_50
action_21 (42) = happyShift action_51
action_21 (43) = happyShift action_52
action_21 (44) = happyShift action_53
action_21 (45) = happyShift action_54
action_21 (46) = happyShift action_55
action_21 _ = happyReduce_19

action_22 (13) = happyShift action_22
action_22 (30) = happyShift action_23
action_22 (31) = happyShift action_24
action_22 (40) = happyShift action_25
action_22 (47) = happyShift action_26
action_22 (12) = happyGoto action_41
action_22 _ = happyFail

action_23 (13) = happyShift action_40
action_23 _ = happyReduce_23

action_24 _ = happyReduce_24

action_25 (13) = happyShift action_22
action_25 (30) = happyShift action_23
action_25 (31) = happyShift action_24
action_25 (40) = happyShift action_25
action_25 (47) = happyShift action_26
action_25 (12) = happyGoto action_39
action_25 _ = happyFail

action_26 (13) = happyShift action_22
action_26 (30) = happyShift action_23
action_26 (31) = happyShift action_24
action_26 (40) = happyShift action_25
action_26 (47) = happyShift action_26
action_26 (12) = happyGoto action_38
action_26 _ = happyFail

action_27 (13) = happyShift action_22
action_27 (30) = happyShift action_23
action_27 (31) = happyShift action_24
action_27 (40) = happyShift action_25
action_27 (47) = happyShift action_26
action_27 (12) = happyGoto action_37
action_27 _ = happyFail

action_28 (30) = happyShift action_36
action_28 _ = happyFail

action_29 (23) = happyShift action_12
action_29 (24) = happyShift action_13
action_29 (25) = happyShift action_14
action_29 (26) = happyShift action_15
action_29 (29) = happyShift action_16
action_29 (30) = happyShift action_17
action_29 (9) = happyGoto action_35
action_29 (10) = happyGoto action_11
action_29 _ = happyReduce_12

action_30 _ = happyReduce_1

action_31 _ = happyReduce_2

action_32 (13) = happyShift action_22
action_32 (22) = happyShift action_34
action_32 (30) = happyShift action_23
action_32 (31) = happyShift action_24
action_32 (40) = happyShift action_25
action_32 (47) = happyShift action_26
action_32 (12) = happyGoto action_33
action_32 _ = happyFail

action_33 (33) = happyShift action_42
action_33 (34) = happyShift action_43
action_33 (35) = happyShift action_44
action_33 (36) = happyShift action_45
action_33 (37) = happyShift action_46
action_33 (38) = happyShift action_47
action_33 (39) = happyShift action_48
action_33 (40) = happyShift action_49
action_33 (41) = happyShift action_50
action_33 (42) = happyShift action_51
action_33 (43) = happyShift action_52
action_33 (44) = happyShift action_53
action_33 (45) = happyShift action_54
action_33 (46) = happyShift action_55
action_33 _ = happyReduce_9

action_34 (13) = happyShift action_80
action_34 _ = happyFail

action_35 _ = happyReduce_11

action_36 (18) = happyShift action_79
action_36 _ = happyFail

action_37 (14) = happyShift action_78
action_37 (33) = happyShift action_42
action_37 (34) = happyShift action_43
action_37 (35) = happyShift action_44
action_37 (36) = happyShift action_45
action_37 (37) = happyShift action_46
action_37 (38) = happyShift action_47
action_37 (39) = happyShift action_48
action_37 (40) = happyShift action_49
action_37 (41) = happyShift action_50
action_37 (42) = happyShift action_51
action_37 (43) = happyShift action_52
action_37 (44) = happyShift action_53
action_37 (45) = happyShift action_54
action_37 (46) = happyShift action_55
action_37 _ = happyFail

action_38 _ = happyReduce_40

action_39 _ = happyReduce_39

action_40 (13) = happyShift action_22
action_40 (30) = happyShift action_23
action_40 (31) = happyShift action_24
action_40 (40) = happyShift action_25
action_40 (47) = happyShift action_26
action_40 (11) = happyGoto action_76
action_40 (12) = happyGoto action_77
action_40 _ = happyReduce_22

action_41 (14) = happyShift action_75
action_41 (33) = happyShift action_42
action_41 (34) = happyShift action_43
action_41 (35) = happyShift action_44
action_41 (36) = happyShift action_45
action_41 (37) = happyShift action_46
action_41 (38) = happyShift action_47
action_41 (39) = happyShift action_48
action_41 (40) = happyShift action_49
action_41 (41) = happyShift action_50
action_41 (42) = happyShift action_51
action_41 (43) = happyShift action_52
action_41 (44) = happyShift action_53
action_41 (45) = happyShift action_54
action_41 (46) = happyShift action_55
action_41 _ = happyFail

action_42 (13) = happyShift action_22
action_42 (30) = happyShift action_23
action_42 (31) = happyShift action_24
action_42 (40) = happyShift action_25
action_42 (47) = happyShift action_26
action_42 (12) = happyGoto action_74
action_42 _ = happyFail

action_43 (13) = happyShift action_22
action_43 (30) = happyShift action_23
action_43 (31) = happyShift action_24
action_43 (40) = happyShift action_25
action_43 (47) = happyShift action_26
action_43 (12) = happyGoto action_73
action_43 _ = happyFail

action_44 (13) = happyShift action_22
action_44 (30) = happyShift action_23
action_44 (31) = happyShift action_24
action_44 (40) = happyShift action_25
action_44 (47) = happyShift action_26
action_44 (12) = happyGoto action_72
action_44 _ = happyFail

action_45 (13) = happyShift action_22
action_45 (30) = happyShift action_23
action_45 (31) = happyShift action_24
action_45 (40) = happyShift action_25
action_45 (47) = happyShift action_26
action_45 (12) = happyGoto action_71
action_45 _ = happyFail

action_46 (13) = happyShift action_22
action_46 (30) = happyShift action_23
action_46 (31) = happyShift action_24
action_46 (40) = happyShift action_25
action_46 (47) = happyShift action_26
action_46 (12) = happyGoto action_70
action_46 _ = happyFail

action_47 (13) = happyShift action_22
action_47 (30) = happyShift action_23
action_47 (31) = happyShift action_24
action_47 (40) = happyShift action_25
action_47 (47) = happyShift action_26
action_47 (12) = happyGoto action_69
action_47 _ = happyFail

action_48 (13) = happyShift action_22
action_48 (30) = happyShift action_23
action_48 (31) = happyShift action_24
action_48 (40) = happyShift action_25
action_48 (47) = happyShift action_26
action_48 (12) = happyGoto action_68
action_48 _ = happyFail

action_49 (13) = happyShift action_22
action_49 (30) = happyShift action_23
action_49 (31) = happyShift action_24
action_49 (40) = happyShift action_25
action_49 (47) = happyShift action_26
action_49 (12) = happyGoto action_67
action_49 _ = happyFail

action_50 (13) = happyShift action_22
action_50 (30) = happyShift action_23
action_50 (31) = happyShift action_24
action_50 (40) = happyShift action_25
action_50 (47) = happyShift action_26
action_50 (12) = happyGoto action_66
action_50 _ = happyFail

action_51 (13) = happyShift action_22
action_51 (30) = happyShift action_23
action_51 (31) = happyShift action_24
action_51 (40) = happyShift action_25
action_51 (47) = happyShift action_26
action_51 (12) = happyGoto action_65
action_51 _ = happyFail

action_52 (13) = happyShift action_22
action_52 (30) = happyShift action_23
action_52 (31) = happyShift action_24
action_52 (40) = happyShift action_25
action_52 (47) = happyShift action_26
action_52 (12) = happyGoto action_64
action_52 _ = happyFail

action_53 (13) = happyShift action_22
action_53 (30) = happyShift action_23
action_53 (31) = happyShift action_24
action_53 (40) = happyShift action_25
action_53 (47) = happyShift action_26
action_53 (12) = happyGoto action_63
action_53 _ = happyFail

action_54 (13) = happyShift action_22
action_54 (30) = happyShift action_23
action_54 (31) = happyShift action_24
action_54 (40) = happyShift action_25
action_54 (47) = happyShift action_26
action_54 (12) = happyGoto action_62
action_54 _ = happyFail

action_55 (13) = happyShift action_22
action_55 (30) = happyShift action_23
action_55 (31) = happyShift action_24
action_55 (40) = happyShift action_25
action_55 (47) = happyShift action_26
action_55 (12) = happyGoto action_61
action_55 _ = happyFail

action_56 (14) = happyShift action_60
action_56 (33) = happyShift action_42
action_56 (34) = happyShift action_43
action_56 (35) = happyShift action_44
action_56 (36) = happyShift action_45
action_56 (37) = happyShift action_46
action_56 (38) = happyShift action_47
action_56 (39) = happyShift action_48
action_56 (40) = happyShift action_49
action_56 (41) = happyShift action_50
action_56 (42) = happyShift action_51
action_56 (43) = happyShift action_52
action_56 (44) = happyShift action_53
action_56 (45) = happyShift action_54
action_56 (46) = happyShift action_55
action_56 _ = happyFail

action_57 (16) = happyShift action_59
action_57 _ = happyFail

action_58 (33) = happyShift action_42
action_58 (34) = happyShift action_43
action_58 (35) = happyShift action_44
action_58 (36) = happyShift action_45
action_58 (37) = happyShift action_46
action_58 (38) = happyShift action_47
action_58 (39) = happyShift action_48
action_58 (40) = happyShift action_49
action_58 (41) = happyShift action_50
action_58 (42) = happyShift action_51
action_58 (43) = happyShift action_52
action_58 (44) = happyShift action_53
action_58 (45) = happyShift action_54
action_58 (46) = happyShift action_55
action_58 _ = happyReduce_13

action_59 (24) = happyShift action_89
action_59 _ = happyFail

action_60 (27) = happyShift action_88
action_60 _ = happyFail

action_61 (33) = happyShift action_42
action_61 (34) = happyShift action_43
action_61 (35) = happyShift action_44
action_61 (36) = happyShift action_45
action_61 (37) = happyShift action_46
action_61 (38) = happyShift action_47
action_61 (39) = happyShift action_48
action_61 (40) = happyShift action_49
action_61 (41) = happyShift action_50
action_61 (42) = happyShift action_51
action_61 (43) = happyShift action_52
action_61 (44) = happyShift action_53
action_61 (45) = happyShift action_54
action_61 _ = happyReduce_38

action_62 (33) = happyShift action_42
action_62 (34) = happyShift action_43
action_62 (35) = happyShift action_44
action_62 (36) = happyShift action_45
action_62 (37) = happyShift action_46
action_62 (38) = happyShift action_47
action_62 (39) = happyShift action_48
action_62 (40) = happyShift action_49
action_62 (41) = happyShift action_50
action_62 (42) = happyShift action_51
action_62 (43) = happyShift action_52
action_62 (44) = happyShift action_53
action_62 _ = happyReduce_37

action_63 _ = happyReduce_30

action_64 _ = happyReduce_29

action_65 _ = happyReduce_28

action_66 _ = happyReduce_27

action_67 (41) = happyShift action_50
action_67 (42) = happyShift action_51
action_67 (43) = happyShift action_52
action_67 (44) = happyShift action_53
action_67 _ = happyReduce_26

action_68 (41) = happyShift action_50
action_68 (42) = happyShift action_51
action_68 (43) = happyShift action_52
action_68 (44) = happyShift action_53
action_68 _ = happyReduce_25

action_69 (39) = happyShift action_48
action_69 (40) = happyShift action_49
action_69 (41) = happyShift action_50
action_69 (42) = happyShift action_51
action_69 (43) = happyShift action_52
action_69 (44) = happyShift action_53
action_69 _ = happyReduce_36

action_70 (39) = happyShift action_48
action_70 (40) = happyShift action_49
action_70 (41) = happyShift action_50
action_70 (42) = happyShift action_51
action_70 (43) = happyShift action_52
action_70 (44) = happyShift action_53
action_70 _ = happyReduce_35

action_71 (39) = happyShift action_48
action_71 (40) = happyShift action_49
action_71 (41) = happyShift action_50
action_71 (42) = happyShift action_51
action_71 (43) = happyShift action_52
action_71 (44) = happyShift action_53
action_71 _ = happyReduce_32

action_72 (39) = happyShift action_48
action_72 (40) = happyShift action_49
action_72 (41) = happyShift action_50
action_72 (42) = happyShift action_51
action_72 (43) = happyShift action_52
action_72 (44) = happyShift action_53
action_72 _ = happyReduce_31

action_73 (39) = happyShift action_48
action_73 (40) = happyShift action_49
action_73 (41) = happyShift action_50
action_73 (42) = happyShift action_51
action_73 (43) = happyShift action_52
action_73 (44) = happyShift action_53
action_73 _ = happyReduce_34

action_74 (39) = happyShift action_48
action_74 (40) = happyShift action_49
action_74 (41) = happyShift action_50
action_74 (42) = happyShift action_51
action_74 (43) = happyShift action_52
action_74 (44) = happyShift action_53
action_74 _ = happyReduce_33

action_75 _ = happyReduce_42

action_76 (14) = happyShift action_87
action_76 _ = happyFail

action_77 (17) = happyShift action_86
action_77 (33) = happyShift action_42
action_77 (34) = happyShift action_43
action_77 (35) = happyShift action_44
action_77 (36) = happyShift action_45
action_77 (37) = happyShift action_46
action_77 (38) = happyShift action_47
action_77 (39) = happyShift action_48
action_77 (40) = happyShift action_49
action_77 (41) = happyShift action_50
action_77 (42) = happyShift action_51
action_77 (43) = happyShift action_52
action_77 (44) = happyShift action_53
action_77 (45) = happyShift action_54
action_77 (46) = happyShift action_55
action_77 _ = happyReduce_21

action_78 (15) = happyShift action_85
action_78 _ = happyFail

action_79 (13) = happyShift action_22
action_79 (30) = happyShift action_23
action_79 (31) = happyShift action_24
action_79 (40) = happyShift action_25
action_79 (47) = happyShift action_26
action_79 (12) = happyGoto action_84
action_79 _ = happyFail

action_80 (20) = happyShift action_83
action_80 (6) = happyGoto action_81
action_80 (7) = happyGoto action_82
action_80 _ = happyReduce_6

action_81 (14) = happyShift action_97
action_81 _ = happyFail

action_82 (17) = happyShift action_96
action_82 _ = happyReduce_5

action_83 (30) = happyShift action_95
action_83 _ = happyFail

action_84 (18) = happyShift action_94
action_84 (33) = happyShift action_42
action_84 (34) = happyShift action_43
action_84 (35) = happyShift action_44
action_84 (36) = happyShift action_45
action_84 (37) = happyShift action_46
action_84 (38) = happyShift action_47
action_84 (39) = happyShift action_48
action_84 (40) = happyShift action_49
action_84 (41) = happyShift action_50
action_84 (42) = happyShift action_51
action_84 (43) = happyShift action_52
action_84 (44) = happyShift action_53
action_84 (45) = happyShift action_54
action_84 (46) = happyShift action_55
action_84 _ = happyFail

action_85 (23) = happyShift action_12
action_85 (24) = happyShift action_13
action_85 (25) = happyShift action_14
action_85 (26) = happyShift action_15
action_85 (29) = happyShift action_16
action_85 (30) = happyShift action_17
action_85 (9) = happyGoto action_93
action_85 (10) = happyGoto action_11
action_85 _ = happyReduce_12

action_86 (13) = happyShift action_22
action_86 (30) = happyShift action_23
action_86 (31) = happyShift action_24
action_86 (40) = happyShift action_25
action_86 (47) = happyShift action_26
action_86 (11) = happyGoto action_92
action_86 (12) = happyGoto action_77
action_86 _ = happyReduce_22

action_87 _ = happyReduce_41

action_88 (15) = happyShift action_91
action_88 _ = happyFail

action_89 (13) = happyShift action_90
action_89 _ = happyFail

action_90 (13) = happyShift action_22
action_90 (30) = happyShift action_23
action_90 (31) = happyShift action_24
action_90 (40) = happyShift action_25
action_90 (47) = happyShift action_26
action_90 (12) = happyGoto action_103
action_90 _ = happyFail

action_91 (23) = happyShift action_12
action_91 (24) = happyShift action_13
action_91 (25) = happyShift action_14
action_91 (26) = happyShift action_15
action_91 (29) = happyShift action_16
action_91 (30) = happyShift action_17
action_91 (9) = happyGoto action_102
action_91 (10) = happyGoto action_11
action_91 _ = happyReduce_12

action_92 _ = happyReduce_20

action_93 (16) = happyShift action_101
action_93 _ = happyFail

action_94 (13) = happyShift action_22
action_94 (30) = happyShift action_23
action_94 (31) = happyShift action_24
action_94 (40) = happyShift action_25
action_94 (47) = happyShift action_26
action_94 (12) = happyGoto action_100
action_94 _ = happyFail

action_95 _ = happyReduce_7

action_96 (20) = happyShift action_83
action_96 (6) = happyGoto action_99
action_96 (7) = happyGoto action_82
action_96 _ = happyReduce_6

action_97 (15) = happyShift action_98
action_97 _ = happyFail

action_98 (20) = happyShift action_7
action_98 (5) = happyGoto action_107
action_98 (8) = happyGoto action_6
action_98 _ = happyReduce_3

action_99 _ = happyReduce_4

action_100 (14) = happyShift action_106
action_100 (33) = happyShift action_42
action_100 (34) = happyShift action_43
action_100 (35) = happyShift action_44
action_100 (36) = happyShift action_45
action_100 (37) = happyShift action_46
action_100 (38) = happyShift action_47
action_100 (39) = happyShift action_48
action_100 (40) = happyShift action_49
action_100 (41) = happyShift action_50
action_100 (42) = happyShift action_51
action_100 (43) = happyShift action_52
action_100 (44) = happyShift action_53
action_100 (45) = happyShift action_54
action_100 (46) = happyShift action_55
action_100 _ = happyFail

action_101 _ = happyReduce_15

action_102 (16) = happyShift action_105
action_102 _ = happyFail

action_103 (14) = happyShift action_104
action_103 (33) = happyShift action_42
action_103 (34) = happyShift action_43
action_103 (35) = happyShift action_44
action_103 (36) = happyShift action_45
action_103 (37) = happyShift action_46
action_103 (38) = happyShift action_47
action_103 (39) = happyShift action_48
action_103 (40) = happyShift action_49
action_103 (41) = happyShift action_50
action_103 (42) = happyShift action_51
action_103 (43) = happyShift action_52
action_103 (44) = happyShift action_53
action_103 (45) = happyShift action_54
action_103 (46) = happyShift action_55
action_103 _ = happyFail

action_104 _ = happyReduce_16

action_105 (28) = happyShift action_110
action_105 _ = happyReduce_17

action_106 (15) = happyShift action_109
action_106 _ = happyFail

action_107 (23) = happyShift action_12
action_107 (24) = happyShift action_13
action_107 (25) = happyShift action_14
action_107 (26) = happyShift action_15
action_107 (29) = happyShift action_16
action_107 (30) = happyShift action_17
action_107 (9) = happyGoto action_108
action_107 (10) = happyGoto action_11
action_107 _ = happyReduce_12

action_108 (16) = happyShift action_113
action_108 _ = happyFail

action_109 (23) = happyShift action_12
action_109 (24) = happyShift action_13
action_109 (25) = happyShift action_14
action_109 (26) = happyShift action_15
action_109 (29) = happyShift action_16
action_109 (30) = happyShift action_17
action_109 (9) = happyGoto action_112
action_109 (10) = happyGoto action_11
action_109 _ = happyReduce_12

action_110 (15) = happyShift action_111
action_110 _ = happyFail

action_111 (23) = happyShift action_12
action_111 (24) = happyShift action_13
action_111 (25) = happyShift action_14
action_111 (26) = happyShift action_15
action_111 (29) = happyShift action_16
action_111 (30) = happyShift action_17
action_111 (9) = happyGoto action_115
action_111 (10) = happyGoto action_11
action_111 _ = happyReduce_12

action_112 (16) = happyShift action_114
action_112 _ = happyFail

action_113 _ = happyReduce_10

action_114 _ = happyReduce_14

action_115 (16) = happyShift action_116
action_115 _ = happyFail

action_116 _ = happyReduce_18

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

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 (HappyTerminal happy_var_2)
	_
	 =  HappyAbsSyn8
		 (AST.Var ((\(Id _ id) -> id) happy_var_2)
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happyReduce 4 8 happyReduction_9
happyReduction_9 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (AST.Num ((\(Id _ id) -> id) happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 11 8 happyReduction_10
happyReduction_10 (_ `HappyStk`
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

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  9 happyReduction_12
happyReduction_12  =  HappyAbsSyn9
		 ([]
	)

happyReduce_13 = happySpecReduce_3  10 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (((\(Id _ id) -> id) happy_var_1) AST.:= happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 11 10 happyReduction_14
happyReduction_14 (_ `HappyStk`
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

happyReduce_15 = happyReduce 7 10 happyReduction_15
happyReduction_15 (_ `HappyStk`
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

happyReduce_16 = happyReduce 8 10 happyReduction_16
happyReduction_16 (_ `HappyStk`
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

happyReduce_17 = happyReduce 8 10 happyReduction_17
happyReduction_17 (_ `HappyStk`
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

happyReduce_18 = happyReduce 12 10 happyReduction_18
happyReduction_18 (_ `HappyStk`
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

happyReduce_19 = happySpecReduce_2  10 happyReduction_19
happyReduction_19 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (AST.Return happy_var_2
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  11 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  11 happyReduction_21
happyReduction_21 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_0  11 happyReduction_22
happyReduction_22  =  HappyAbsSyn11
		 ([]
	)

happyReduce_23 = happySpecReduce_1  12 happyReduction_23
happyReduction_23 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Variable ((\(Id _ id) -> Left id) happy_var_1)
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  12 happyReduction_24
happyReduction_24 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Constant ((\(Num _ n) -> n) happy_var_1)
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  12 happyReduction_25
happyReduction_25 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (+) happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  12 happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (-) happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  12 happyReduction_27
happyReduction_27 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (*) happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  12 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (/) happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  12 happyReduction_29
happyReduction_29 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> toEnum (fromEnum a `div` fromEnum b)) happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  12 happyReduction_30
happyReduction_30 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> toEnum (fromEnum a `mod` fromEnum b)) happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  12 happyReduction_31
happyReduction_31 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> if a < b then 1 else 0) happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  12 happyReduction_32
happyReduction_32 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> if a > b then 1 else 0) happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  12 happyReduction_33
happyReduction_33 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> if a <= b then 1 else 0) happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  12 happyReduction_34
happyReduction_34 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> if a >= b then 1 else 0) happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  12 happyReduction_35
happyReduction_35 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> if a == b then 1 else 0) happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  12 happyReduction_36
happyReduction_36 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> if a /= b then 1 else 0) happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  12 happyReduction_37
happyReduction_37 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> if a > 0 && b > 0 then 1 else 0) happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  12 happyReduction_38
happyReduction_38 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Binary (\a b -> if a > 0 || b > 0 then 1 else 0) happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  12 happyReduction_39
happyReduction_39 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (AST.Unary (\a -> negate a) happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  12 happyReduction_40
happyReduction_40 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (AST.Unary (\a -> if a > 0 then 0 else 1) happy_var_2
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happyReduce 4 12 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (AST.Application ((\(Id _ id) -> Left id) happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_3  12 happyReduction_42
happyReduction_42 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 48 48 notHappyAtAll (HappyState action) sts stk []

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
	Key p "var" -> cont 20;
	Key p "num" -> cont 21;
	Key p "func" -> cont 22;
	Key p "for" -> cont 23;
	Key p "while" -> cont 24;
	Key p "return" -> cont 25;
	Key p "if" -> cont 26;
	Key p "then" -> cont 27;
	Key p "else" -> cont 28;
	Key p "do" -> cont 29;
	Id p id -> cont 30;
	Num p c -> cont 31;
	Op p ":=" -> cont 32;
	Op p "<=" -> cont 33;
	Op p ">=" -> cont 34;
	Op p "<" -> cont 35;
	Op p ">" -> cont 36;
	Op p "==" -> cont 37;
	Op p "!=" -> cont 38;
	Op p "+" -> cont 39;
	Op p "-" -> cont 40;
	Op p "*" -> cont 41;
	Op p "/" -> cont 42;
	Op p "div" -> cont 43;
	Op p "%" -> cont 44;
	Op p "&&" -> cont 45;
	Op p "||" -> cont 46;
	Op p "!" -> cont 47;
	_ -> happyError' (tk:tks)
	}

happyError_ 48 tk tks = happyError' tks
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
    t:_ -> syntaxError . show $ t
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
