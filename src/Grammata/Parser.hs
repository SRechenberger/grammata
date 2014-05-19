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
    Declaration (Var, Num, Func, Proc), 
    Statement ((:=), For, While, DoWhile, If, Return, Call), 
    Arithmetical (Id, Con, Bin, Un, App))

import General (Identifier, Number)
import General.Expression (Expression (..))

-- parser produced by Happy Version 1.19.0

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (AST.Program Identifier (AST.Arithmetical Identifier Number String))
	| HappyAbsSyn5 ([AST.Declaration Identifier (AST.Arithmetical Identifier Number String)])
	| HappyAbsSyn6 ([Identifier])
	| HappyAbsSyn7 (Identifier)
	| HappyAbsSyn8 (AST.Declaration Identifier (AST.Arithmetical Identifier Number String))
	| HappyAbsSyn9 ([AST.Statement Identifier (AST.Arithmetical Identifier Number String)])
	| HappyAbsSyn10 (AST.Statement Identifier (AST.Arithmetical Identifier Number String))
	| HappyAbsSyn11 ([AST.Arithmetical Identifier Number String])
	| HappyAbsSyn12 (AST.Arithmetical Identifier Number String)

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
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129 :: () => Int -> ({-HappyReduction (Analysis String String sem) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Analysis String String sem) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Analysis String String sem) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Analysis String String sem) HappyAbsSyn)

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
 happyReduce_42,
 happyReduce_43,
 happyReduce_44 :: () => ({-HappyReduction (Analysis String String sem) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Analysis String String sem) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Analysis String String sem) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Analysis String String sem) HappyAbsSyn)

action_0 (19) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (19) = happyShift action_2
action_1 _ = happyFail

action_2 (15) = happyShift action_4
action_2 _ = happyFail

action_3 (49) = happyAccept
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
action_5 (31) = happyShift action_18
action_5 (9) = happyGoto action_10
action_5 (10) = happyGoto action_11
action_5 _ = happyReduce_13

action_6 (18) = happyShift action_9
action_6 _ = happyFail

action_7 (31) = happyShift action_8
action_7 _ = happyFail

action_8 (33) = happyShift action_34
action_8 _ = happyReduce_8

action_9 (20) = happyShift action_7
action_9 (5) = happyGoto action_33
action_9 (8) = happyGoto action_6
action_9 _ = happyReduce_3

action_10 (16) = happyShift action_32
action_10 _ = happyFail

action_11 (18) = happyShift action_31
action_11 _ = happyFail

action_12 (13) = happyShift action_30
action_12 _ = happyFail

action_13 (13) = happyShift action_29
action_13 _ = happyFail

action_14 (13) = happyShift action_24
action_14 (31) = happyShift action_25
action_14 (32) = happyShift action_26
action_14 (41) = happyShift action_27
action_14 (48) = happyShift action_28
action_14 (12) = happyGoto action_23
action_14 _ = happyFail

action_15 (13) = happyShift action_22
action_15 _ = happyFail

action_16 (15) = happyShift action_21
action_16 _ = happyFail

action_17 (31) = happyShift action_20
action_17 _ = happyFail

action_18 (33) = happyShift action_19
action_18 _ = happyFail

action_19 (13) = happyShift action_24
action_19 (31) = happyShift action_25
action_19 (32) = happyShift action_26
action_19 (41) = happyShift action_27
action_19 (48) = happyShift action_28
action_19 (12) = happyGoto action_62
action_19 _ = happyFail

action_20 (13) = happyShift action_61
action_20 _ = happyFail

action_21 (23) = happyShift action_12
action_21 (24) = happyShift action_13
action_21 (25) = happyShift action_14
action_21 (26) = happyShift action_15
action_21 (29) = happyShift action_16
action_21 (30) = happyShift action_17
action_21 (31) = happyShift action_18
action_21 (9) = happyGoto action_60
action_21 (10) = happyGoto action_11
action_21 _ = happyReduce_13

action_22 (13) = happyShift action_24
action_22 (31) = happyShift action_25
action_22 (32) = happyShift action_26
action_22 (41) = happyShift action_27
action_22 (48) = happyShift action_28
action_22 (12) = happyGoto action_59
action_22 _ = happyFail

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
action_23 (44) = happyShift action_55
action_23 (45) = happyShift action_56
action_23 (46) = happyShift action_57
action_23 (47) = happyShift action_58
action_23 _ = happyReduce_20

action_24 (13) = happyShift action_24
action_24 (31) = happyShift action_25
action_24 (32) = happyShift action_26
action_24 (41) = happyShift action_27
action_24 (48) = happyShift action_28
action_24 (12) = happyGoto action_44
action_24 _ = happyFail

action_25 (13) = happyShift action_43
action_25 _ = happyReduce_25

action_26 _ = happyReduce_26

action_27 (13) = happyShift action_24
action_27 (31) = happyShift action_25
action_27 (32) = happyShift action_26
action_27 (41) = happyShift action_27
action_27 (48) = happyShift action_28
action_27 (12) = happyGoto action_42
action_27 _ = happyFail

action_28 (13) = happyShift action_24
action_28 (31) = happyShift action_25
action_28 (32) = happyShift action_26
action_28 (41) = happyShift action_27
action_28 (48) = happyShift action_28
action_28 (12) = happyGoto action_41
action_28 _ = happyFail

action_29 (13) = happyShift action_24
action_29 (31) = happyShift action_25
action_29 (32) = happyShift action_26
action_29 (41) = happyShift action_27
action_29 (48) = happyShift action_28
action_29 (12) = happyGoto action_40
action_29 _ = happyFail

action_30 (31) = happyShift action_39
action_30 _ = happyFail

action_31 (23) = happyShift action_12
action_31 (24) = happyShift action_13
action_31 (25) = happyShift action_14
action_31 (26) = happyShift action_15
action_31 (29) = happyShift action_16
action_31 (30) = happyShift action_17
action_31 (31) = happyShift action_18
action_31 (9) = happyGoto action_38
action_31 (10) = happyGoto action_11
action_31 _ = happyReduce_13

action_32 _ = happyReduce_1

action_33 _ = happyReduce_2

action_34 (13) = happyShift action_24
action_34 (21) = happyShift action_36
action_34 (22) = happyShift action_37
action_34 (31) = happyShift action_25
action_34 (32) = happyShift action_26
action_34 (41) = happyShift action_27
action_34 (48) = happyShift action_28
action_34 (12) = happyGoto action_35
action_34 _ = happyFail

action_35 (34) = happyShift action_45
action_35 (35) = happyShift action_46
action_35 (36) = happyShift action_47
action_35 (37) = happyShift action_48
action_35 (38) = happyShift action_49
action_35 (39) = happyShift action_50
action_35 (40) = happyShift action_51
action_35 (41) = happyShift action_52
action_35 (42) = happyShift action_53
action_35 (43) = happyShift action_54
action_35 (44) = happyShift action_55
action_35 (45) = happyShift action_56
action_35 (46) = happyShift action_57
action_35 (47) = happyShift action_58
action_35 _ = happyReduce_9

action_36 (13) = happyShift action_86
action_36 _ = happyFail

action_37 (13) = happyShift action_85
action_37 _ = happyFail

action_38 _ = happyReduce_12

action_39 (18) = happyShift action_84
action_39 _ = happyFail

action_40 (14) = happyShift action_83
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
action_40 (44) = happyShift action_55
action_40 (45) = happyShift action_56
action_40 (46) = happyShift action_57
action_40 (47) = happyShift action_58
action_40 _ = happyFail

action_41 _ = happyReduce_42

action_42 _ = happyReduce_41

action_43 (13) = happyShift action_24
action_43 (31) = happyShift action_25
action_43 (32) = happyShift action_26
action_43 (41) = happyShift action_27
action_43 (48) = happyShift action_28
action_43 (11) = happyGoto action_82
action_43 (12) = happyGoto action_64
action_43 _ = happyReduce_24

action_44 (14) = happyShift action_81
action_44 (34) = happyShift action_45
action_44 (35) = happyShift action_46
action_44 (36) = happyShift action_47
action_44 (37) = happyShift action_48
action_44 (38) = happyShift action_49
action_44 (39) = happyShift action_50
action_44 (40) = happyShift action_51
action_44 (41) = happyShift action_52
action_44 (42) = happyShift action_53
action_44 (43) = happyShift action_54
action_44 (44) = happyShift action_55
action_44 (45) = happyShift action_56
action_44 (46) = happyShift action_57
action_44 (47) = happyShift action_58
action_44 _ = happyFail

action_45 (13) = happyShift action_24
action_45 (31) = happyShift action_25
action_45 (32) = happyShift action_26
action_45 (41) = happyShift action_27
action_45 (48) = happyShift action_28
action_45 (12) = happyGoto action_80
action_45 _ = happyFail

action_46 (13) = happyShift action_24
action_46 (31) = happyShift action_25
action_46 (32) = happyShift action_26
action_46 (41) = happyShift action_27
action_46 (48) = happyShift action_28
action_46 (12) = happyGoto action_79
action_46 _ = happyFail

action_47 (13) = happyShift action_24
action_47 (31) = happyShift action_25
action_47 (32) = happyShift action_26
action_47 (41) = happyShift action_27
action_47 (48) = happyShift action_28
action_47 (12) = happyGoto action_78
action_47 _ = happyFail

action_48 (13) = happyShift action_24
action_48 (31) = happyShift action_25
action_48 (32) = happyShift action_26
action_48 (41) = happyShift action_27
action_48 (48) = happyShift action_28
action_48 (12) = happyGoto action_77
action_48 _ = happyFail

action_49 (13) = happyShift action_24
action_49 (31) = happyShift action_25
action_49 (32) = happyShift action_26
action_49 (41) = happyShift action_27
action_49 (48) = happyShift action_28
action_49 (12) = happyGoto action_76
action_49 _ = happyFail

action_50 (13) = happyShift action_24
action_50 (31) = happyShift action_25
action_50 (32) = happyShift action_26
action_50 (41) = happyShift action_27
action_50 (48) = happyShift action_28
action_50 (12) = happyGoto action_75
action_50 _ = happyFail

action_51 (13) = happyShift action_24
action_51 (31) = happyShift action_25
action_51 (32) = happyShift action_26
action_51 (41) = happyShift action_27
action_51 (48) = happyShift action_28
action_51 (12) = happyGoto action_74
action_51 _ = happyFail

action_52 (13) = happyShift action_24
action_52 (31) = happyShift action_25
action_52 (32) = happyShift action_26
action_52 (41) = happyShift action_27
action_52 (48) = happyShift action_28
action_52 (12) = happyGoto action_73
action_52 _ = happyFail

action_53 (13) = happyShift action_24
action_53 (31) = happyShift action_25
action_53 (32) = happyShift action_26
action_53 (41) = happyShift action_27
action_53 (48) = happyShift action_28
action_53 (12) = happyGoto action_72
action_53 _ = happyFail

action_54 (13) = happyShift action_24
action_54 (31) = happyShift action_25
action_54 (32) = happyShift action_26
action_54 (41) = happyShift action_27
action_54 (48) = happyShift action_28
action_54 (12) = happyGoto action_71
action_54 _ = happyFail

action_55 (13) = happyShift action_24
action_55 (31) = happyShift action_25
action_55 (32) = happyShift action_26
action_55 (41) = happyShift action_27
action_55 (48) = happyShift action_28
action_55 (12) = happyGoto action_70
action_55 _ = happyFail

action_56 (13) = happyShift action_24
action_56 (31) = happyShift action_25
action_56 (32) = happyShift action_26
action_56 (41) = happyShift action_27
action_56 (48) = happyShift action_28
action_56 (12) = happyGoto action_69
action_56 _ = happyFail

action_57 (13) = happyShift action_24
action_57 (31) = happyShift action_25
action_57 (32) = happyShift action_26
action_57 (41) = happyShift action_27
action_57 (48) = happyShift action_28
action_57 (12) = happyGoto action_68
action_57 _ = happyFail

action_58 (13) = happyShift action_24
action_58 (31) = happyShift action_25
action_58 (32) = happyShift action_26
action_58 (41) = happyShift action_27
action_58 (48) = happyShift action_28
action_58 (12) = happyGoto action_67
action_58 _ = happyFail

action_59 (14) = happyShift action_66
action_59 (34) = happyShift action_45
action_59 (35) = happyShift action_46
action_59 (36) = happyShift action_47
action_59 (37) = happyShift action_48
action_59 (38) = happyShift action_49
action_59 (39) = happyShift action_50
action_59 (40) = happyShift action_51
action_59 (41) = happyShift action_52
action_59 (42) = happyShift action_53
action_59 (43) = happyShift action_54
action_59 (44) = happyShift action_55
action_59 (45) = happyShift action_56
action_59 (46) = happyShift action_57
action_59 (47) = happyShift action_58
action_59 _ = happyFail

action_60 (16) = happyShift action_65
action_60 _ = happyFail

action_61 (13) = happyShift action_24
action_61 (31) = happyShift action_25
action_61 (32) = happyShift action_26
action_61 (41) = happyShift action_27
action_61 (48) = happyShift action_28
action_61 (11) = happyGoto action_63
action_61 (12) = happyGoto action_64
action_61 _ = happyReduce_24

action_62 (34) = happyShift action_45
action_62 (35) = happyShift action_46
action_62 (36) = happyShift action_47
action_62 (37) = happyShift action_48
action_62 (38) = happyShift action_49
action_62 (39) = happyShift action_50
action_62 (40) = happyShift action_51
action_62 (41) = happyShift action_52
action_62 (42) = happyShift action_53
action_62 (43) = happyShift action_54
action_62 (44) = happyShift action_55
action_62 (45) = happyShift action_56
action_62 (46) = happyShift action_57
action_62 (47) = happyShift action_58
action_62 _ = happyReduce_14

action_63 (14) = happyShift action_97
action_63 _ = happyFail

action_64 (17) = happyShift action_96
action_64 (34) = happyShift action_45
action_64 (35) = happyShift action_46
action_64 (36) = happyShift action_47
action_64 (37) = happyShift action_48
action_64 (38) = happyShift action_49
action_64 (39) = happyShift action_50
action_64 (40) = happyShift action_51
action_64 (41) = happyShift action_52
action_64 (42) = happyShift action_53
action_64 (43) = happyShift action_54
action_64 (44) = happyShift action_55
action_64 (45) = happyShift action_56
action_64 (46) = happyShift action_57
action_64 (47) = happyShift action_58
action_64 _ = happyReduce_23

action_65 (24) = happyShift action_95
action_65 _ = happyFail

action_66 (27) = happyShift action_94
action_66 _ = happyFail

action_67 (34) = happyShift action_45
action_67 (35) = happyShift action_46
action_67 (36) = happyShift action_47
action_67 (37) = happyShift action_48
action_67 (38) = happyShift action_49
action_67 (39) = happyShift action_50
action_67 (40) = happyShift action_51
action_67 (41) = happyShift action_52
action_67 (42) = happyShift action_53
action_67 (43) = happyShift action_54
action_67 (44) = happyShift action_55
action_67 (45) = happyShift action_56
action_67 (46) = happyShift action_57
action_67 _ = happyReduce_40

action_68 (34) = happyShift action_45
action_68 (35) = happyShift action_46
action_68 (36) = happyShift action_47
action_68 (37) = happyShift action_48
action_68 (38) = happyShift action_49
action_68 (39) = happyShift action_50
action_68 (40) = happyShift action_51
action_68 (41) = happyShift action_52
action_68 (42) = happyShift action_53
action_68 (43) = happyShift action_54
action_68 (44) = happyShift action_55
action_68 (45) = happyShift action_56
action_68 _ = happyReduce_39

action_69 _ = happyReduce_32

action_70 _ = happyReduce_31

action_71 _ = happyReduce_30

action_72 _ = happyReduce_29

action_73 (42) = happyShift action_53
action_73 (43) = happyShift action_54
action_73 (44) = happyShift action_55
action_73 (45) = happyShift action_56
action_73 _ = happyReduce_28

action_74 (42) = happyShift action_53
action_74 (43) = happyShift action_54
action_74 (44) = happyShift action_55
action_74 (45) = happyShift action_56
action_74 _ = happyReduce_27

action_75 (40) = happyShift action_51
action_75 (41) = happyShift action_52
action_75 (42) = happyShift action_53
action_75 (43) = happyShift action_54
action_75 (44) = happyShift action_55
action_75 (45) = happyShift action_56
action_75 _ = happyReduce_38

action_76 (40) = happyShift action_51
action_76 (41) = happyShift action_52
action_76 (42) = happyShift action_53
action_76 (43) = happyShift action_54
action_76 (44) = happyShift action_55
action_76 (45) = happyShift action_56
action_76 _ = happyReduce_37

action_77 (40) = happyShift action_51
action_77 (41) = happyShift action_52
action_77 (42) = happyShift action_53
action_77 (43) = happyShift action_54
action_77 (44) = happyShift action_55
action_77 (45) = happyShift action_56
action_77 _ = happyReduce_34

action_78 (40) = happyShift action_51
action_78 (41) = happyShift action_52
action_78 (42) = happyShift action_53
action_78 (43) = happyShift action_54
action_78 (44) = happyShift action_55
action_78 (45) = happyShift action_56
action_78 _ = happyReduce_33

action_79 (40) = happyShift action_51
action_79 (41) = happyShift action_52
action_79 (42) = happyShift action_53
action_79 (43) = happyShift action_54
action_79 (44) = happyShift action_55
action_79 (45) = happyShift action_56
action_79 _ = happyReduce_36

action_80 (40) = happyShift action_51
action_80 (41) = happyShift action_52
action_80 (42) = happyShift action_53
action_80 (43) = happyShift action_54
action_80 (44) = happyShift action_55
action_80 (45) = happyShift action_56
action_80 _ = happyReduce_35

action_81 _ = happyReduce_44

action_82 (14) = happyShift action_93
action_82 _ = happyFail

action_83 (15) = happyShift action_92
action_83 _ = happyFail

action_84 (13) = happyShift action_24
action_84 (31) = happyShift action_25
action_84 (32) = happyShift action_26
action_84 (41) = happyShift action_27
action_84 (48) = happyShift action_28
action_84 (12) = happyGoto action_91
action_84 _ = happyFail

action_85 (20) = happyShift action_89
action_85 (6) = happyGoto action_90
action_85 (7) = happyGoto action_88
action_85 _ = happyReduce_6

action_86 (20) = happyShift action_89
action_86 (6) = happyGoto action_87
action_86 (7) = happyGoto action_88
action_86 _ = happyReduce_6

action_87 (14) = happyShift action_106
action_87 _ = happyFail

action_88 (17) = happyShift action_105
action_88 _ = happyReduce_5

action_89 (31) = happyShift action_104
action_89 _ = happyFail

action_90 (14) = happyShift action_103
action_90 _ = happyFail

action_91 (18) = happyShift action_102
action_91 (34) = happyShift action_45
action_91 (35) = happyShift action_46
action_91 (36) = happyShift action_47
action_91 (37) = happyShift action_48
action_91 (38) = happyShift action_49
action_91 (39) = happyShift action_50
action_91 (40) = happyShift action_51
action_91 (41) = happyShift action_52
action_91 (42) = happyShift action_53
action_91 (43) = happyShift action_54
action_91 (44) = happyShift action_55
action_91 (45) = happyShift action_56
action_91 (46) = happyShift action_57
action_91 (47) = happyShift action_58
action_91 _ = happyFail

action_92 (23) = happyShift action_12
action_92 (24) = happyShift action_13
action_92 (25) = happyShift action_14
action_92 (26) = happyShift action_15
action_92 (29) = happyShift action_16
action_92 (30) = happyShift action_17
action_92 (31) = happyShift action_18
action_92 (9) = happyGoto action_101
action_92 (10) = happyGoto action_11
action_92 _ = happyReduce_13

action_93 _ = happyReduce_43

action_94 (15) = happyShift action_100
action_94 _ = happyFail

action_95 (13) = happyShift action_99
action_95 _ = happyFail

action_96 (13) = happyShift action_24
action_96 (31) = happyShift action_25
action_96 (32) = happyShift action_26
action_96 (41) = happyShift action_27
action_96 (48) = happyShift action_28
action_96 (11) = happyGoto action_98
action_96 (12) = happyGoto action_64
action_96 _ = happyReduce_24

action_97 _ = happyReduce_21

action_98 _ = happyReduce_22

action_99 (13) = happyShift action_24
action_99 (31) = happyShift action_25
action_99 (32) = happyShift action_26
action_99 (41) = happyShift action_27
action_99 (48) = happyShift action_28
action_99 (12) = happyGoto action_113
action_99 _ = happyFail

action_100 (23) = happyShift action_12
action_100 (24) = happyShift action_13
action_100 (25) = happyShift action_14
action_100 (26) = happyShift action_15
action_100 (29) = happyShift action_16
action_100 (30) = happyShift action_17
action_100 (31) = happyShift action_18
action_100 (9) = happyGoto action_112
action_100 (10) = happyGoto action_11
action_100 _ = happyReduce_13

action_101 (16) = happyShift action_111
action_101 _ = happyFail

action_102 (13) = happyShift action_24
action_102 (31) = happyShift action_25
action_102 (32) = happyShift action_26
action_102 (41) = happyShift action_27
action_102 (48) = happyShift action_28
action_102 (12) = happyGoto action_110
action_102 _ = happyFail

action_103 (15) = happyShift action_109
action_103 _ = happyFail

action_104 _ = happyReduce_7

action_105 (20) = happyShift action_89
action_105 (6) = happyGoto action_108
action_105 (7) = happyGoto action_88
action_105 _ = happyReduce_6

action_106 (15) = happyShift action_107
action_106 _ = happyFail

action_107 (20) = happyShift action_7
action_107 (5) = happyGoto action_118
action_107 (8) = happyGoto action_6
action_107 _ = happyReduce_3

action_108 _ = happyReduce_4

action_109 (20) = happyShift action_7
action_109 (5) = happyGoto action_117
action_109 (8) = happyGoto action_6
action_109 _ = happyReduce_3

action_110 (14) = happyShift action_116
action_110 (34) = happyShift action_45
action_110 (35) = happyShift action_46
action_110 (36) = happyShift action_47
action_110 (37) = happyShift action_48
action_110 (38) = happyShift action_49
action_110 (39) = happyShift action_50
action_110 (40) = happyShift action_51
action_110 (41) = happyShift action_52
action_110 (42) = happyShift action_53
action_110 (43) = happyShift action_54
action_110 (44) = happyShift action_55
action_110 (45) = happyShift action_56
action_110 (46) = happyShift action_57
action_110 (47) = happyShift action_58
action_110 _ = happyFail

action_111 _ = happyReduce_16

action_112 (16) = happyShift action_115
action_112 _ = happyFail

action_113 (14) = happyShift action_114
action_113 (34) = happyShift action_45
action_113 (35) = happyShift action_46
action_113 (36) = happyShift action_47
action_113 (37) = happyShift action_48
action_113 (38) = happyShift action_49
action_113 (39) = happyShift action_50
action_113 (40) = happyShift action_51
action_113 (41) = happyShift action_52
action_113 (42) = happyShift action_53
action_113 (43) = happyShift action_54
action_113 (44) = happyShift action_55
action_113 (45) = happyShift action_56
action_113 (46) = happyShift action_57
action_113 (47) = happyShift action_58
action_113 _ = happyFail

action_114 _ = happyReduce_17

action_115 (28) = happyShift action_122
action_115 _ = happyReduce_18

action_116 (15) = happyShift action_121
action_116 _ = happyFail

action_117 (23) = happyShift action_12
action_117 (24) = happyShift action_13
action_117 (25) = happyShift action_14
action_117 (26) = happyShift action_15
action_117 (29) = happyShift action_16
action_117 (30) = happyShift action_17
action_117 (31) = happyShift action_18
action_117 (9) = happyGoto action_120
action_117 (10) = happyGoto action_11
action_117 _ = happyReduce_13

action_118 (23) = happyShift action_12
action_118 (24) = happyShift action_13
action_118 (25) = happyShift action_14
action_118 (26) = happyShift action_15
action_118 (29) = happyShift action_16
action_118 (30) = happyShift action_17
action_118 (31) = happyShift action_18
action_118 (9) = happyGoto action_119
action_118 (10) = happyGoto action_11
action_118 _ = happyReduce_13

action_119 (16) = happyShift action_126
action_119 _ = happyFail

action_120 (16) = happyShift action_125
action_120 _ = happyFail

action_121 (23) = happyShift action_12
action_121 (24) = happyShift action_13
action_121 (25) = happyShift action_14
action_121 (26) = happyShift action_15
action_121 (29) = happyShift action_16
action_121 (30) = happyShift action_17
action_121 (31) = happyShift action_18
action_121 (9) = happyGoto action_124
action_121 (10) = happyGoto action_11
action_121 _ = happyReduce_13

action_122 (15) = happyShift action_123
action_122 _ = happyFail

action_123 (23) = happyShift action_12
action_123 (24) = happyShift action_13
action_123 (25) = happyShift action_14
action_123 (26) = happyShift action_15
action_123 (29) = happyShift action_16
action_123 (30) = happyShift action_17
action_123 (31) = happyShift action_18
action_123 (9) = happyGoto action_128
action_123 (10) = happyGoto action_11
action_123 _ = happyReduce_13

action_124 (16) = happyShift action_127
action_124 _ = happyFail

action_125 _ = happyReduce_11

action_126 _ = happyReduce_10

action_127 _ = happyReduce_15

action_128 (16) = happyShift action_129
action_128 _ = happyFail

action_129 _ = happyReduce_19

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
		 (AST.Proc ((\(Id _ id) -> id) happy_var_2) happy_var_6 happy_var_9 happy_var_10
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

happyReduce_21 = happyReduce 5 10 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (AST.Call ((\(Id _ id) -> id) happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_3  11 happyReduction_22
happyReduction_22 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  11 happyReduction_23
happyReduction_23 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_0  11 happyReduction_24
happyReduction_24  =  HappyAbsSyn11
		 ([]
	)

happyReduce_25 = happySpecReduce_1  12 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Id ((\(Id _ id) -> id) happy_var_1)
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  12 happyReduction_26
happyReduction_26 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Con ((\(Num _ n) -> n) happy_var_1)
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  12 happyReduction_27
happyReduction_27 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Bin "+" happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  12 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Bin "-" happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  12 happyReduction_29
happyReduction_29 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Bin "*" happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  12 happyReduction_30
happyReduction_30 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Bin "/" happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  12 happyReduction_31
happyReduction_31 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Bin "div" happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  12 happyReduction_32
happyReduction_32 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Bin "%" happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  12 happyReduction_33
happyReduction_33 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Bin "<" happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  12 happyReduction_34
happyReduction_34 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Bin ">" happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  12 happyReduction_35
happyReduction_35 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Bin "<=" happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  12 happyReduction_36
happyReduction_36 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Bin ">=" happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  12 happyReduction_37
happyReduction_37 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Bin "==" happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  12 happyReduction_38
happyReduction_38 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Bin "!=" happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  12 happyReduction_39
happyReduction_39 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Bin "&&" happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  12 happyReduction_40
happyReduction_40 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (AST.Bin "||" happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  12 happyReduction_41
happyReduction_41 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (AST.Un "-" happy_var_2
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_2  12 happyReduction_42
happyReduction_42 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (AST.Un "not" happy_var_2
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happyReduce 4 12 happyReduction_43
happyReduction_43 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (AST.App ((\(Id _ id) ->  id) happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_3  12 happyReduction_44
happyReduction_44 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 49 49 notHappyAtAll (HappyState action) sts stk []

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
	Key p "func" -> cont 21;
	Key p "proc" -> cont 22;
	Key p "for" -> cont 23;
	Key p "while" -> cont 24;
	Key p "return" -> cont 25;
	Key p "if" -> cont 26;
	Key p "then" -> cont 27;
	Key p "else" -> cont 28;
	Key p "do" -> cont 29;
	Key p "call" -> cont 30;
	Id p id -> cont 31;
	Num p c -> cont 32;
	Op p ":=" -> cont 33;
	Op p "<=" -> cont 34;
	Op p ">=" -> cont 35;
	Op p "<" -> cont 36;
	Op p ">" -> cont 37;
	Op p "==" -> cont 38;
	Op p "!=" -> cont 39;
	Op p "+" -> cont 40;
	Op p "-" -> cont 41;
	Op p "*" -> cont 42;
	Op p "/" -> cont 43;
	Op p "div" -> cont 44;
	Op p "%" -> cont 45;
	Op p "&&" -> cont 46;
	Op p "||" -> cont 47;
	Op p "!" -> cont 48;
	_ -> happyError' (tk:tks)
	}

happyError_ 49 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Analysis String String sem a -> (a -> Analysis String String sem b) -> Analysis String String sem b
happyThen = (>>=)
happyReturn :: () => a -> Analysis String String sem a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Analysis String String sem a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> Analysis String String sem a
happyError' = happyError

parseGrammata tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- |Parses the script, returning the AST
parse :: String -> Analysis String String sem (AST.Program Identifier (AST.Arithmetical Identifier Number String))
parse input = tokenize input >>= parseGrammata

-- |Function invoked on error.
happyError :: [Token] -> Analysis String String sem a
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
