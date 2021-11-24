{-# OPTIONS_GHC -w #-}
module Kriti.Parser.Grammar where

import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.HashMap.Strict as M
import qualified Data.List as List
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import qualified Kriti.Error as E
import qualified Kriti.Parser.Lexer as L
import Kriti.Parser.Monad
import Kriti.Parser.Token
import Kriti.Parser.Spans
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (ValueExt)
	| HappyAbsSyn5 (V.Vector ValueExt)
	| HappyAbsSyn13 ([(T.Text, ValueExt)])
	| HappyAbsSyn14 ((T.Text, ValueExt))
	| HappyAbsSyn18 (ValueExt -> ValueExt)
	| HappyAbsSyn21 (Span -> ValueExt -> ValueExt)
	| HappyAbsSyn23 ((Span, V.Vector Accessor))
	| HappyAbsSyn25 (Accessor)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
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
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134 :: () => Prelude.Int -> ({-HappyReduction (Parser) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Parser) HappyAbsSyn)

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
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68 :: () => ({-HappyReduction (Parser) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Parser) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,406) ([0,63488,32,1360,0,4096,0,0,0,2048,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3968,2,85,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,512,0,0,904,0,0,15872,8,468,0,1984,32769,42,0,0,0,8,0,0,16386,0,0,0,0,0,0,0,0,0,3968,26,81,0,0,0,1,0,16446,4096,1,0,2048,32,0,0,0,0,0,0,256,32,0,0,32832,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,16,0,6144,0,2,0,0,0,0,0,8207,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,30720,256,0,0,0,2,0,0,1024,0,0,0,64,0,0,0,0,0,0,0,4096,512,0,0,0,0,0,0,8,0,0,16,2,0,0,0,2,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31744,16,0,4096,0,0,0,32892,8192,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,31744,208,648,0,0,0,8,0,0,0,0,0,26686,17408,1,49152,263,10880,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,63488,512,0,1984,8,34,0,248,16385,4,0,8223,34816,0,57344,1027,4352,0,31744,128,544,0,3968,2,85,0,0,2,0,0,16384,0,0,0,0,4096,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,256,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,57344,131,5440,0,0,256,0,0,0,0,0,0,0,4096,0,0,0,512,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,1,0,0,8192,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,1,0,0,8192,0,0,0,1024,0,63488,32,1360,0,0,0,0,0,0,0,0,0,0,8192,0,0,256,0,0,0,0,256,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","string_lit","string_template","template","num_lit","boolean","null","array","list_elements","object","object_fields","object_field","operator","iff","function_call","functions","function_params","range","range_decl","path","path_vector","path_tail","path_element","value","term","number","int","'true'","'false'","'s\"'","'\"e'","string","'if'","'else'","'end'","'null'","'range'","'escapeUri'","ident","'\\''","':'","'.'","','","'=='","'>'","'<'","'&&'","'||'","'_'","':='","'{'","'}'","'{{'","'}}'","'['","']'","'('","')'","%eof"]
        bit_start = st Prelude.* 61
        bit_end = (st Prelude.+ 1) Prelude.* 61
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..60]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (28) = happyShift action_15
action_0 (29) = happyShift action_16
action_0 (30) = happyShift action_17
action_0 (31) = happyShift action_18
action_0 (32) = happyShift action_2
action_0 (38) = happyShift action_19
action_0 (53) = happyShift action_20
action_0 (55) = happyShift action_21
action_0 (57) = happyShift action_22
action_0 (59) = happyShift action_23
action_0 (4) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 (12) = happyGoto action_8
action_0 (16) = happyGoto action_9
action_0 (17) = happyGoto action_10
action_0 (20) = happyGoto action_11
action_0 (21) = happyGoto action_12
action_0 (22) = happyGoto action_13
action_0 (27) = happyGoto action_14
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (32) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (34) = happyShift action_40
action_2 (55) = happyShift action_41
action_2 (5) = happyGoto action_39
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_59

action_4 _ = happyReduce_58

action_5 _ = happyReduce_60

action_6 _ = happyReduce_61

action_7 _ = happyReduce_62

action_8 _ = happyReduce_63

action_9 _ = happyReduce_65

action_10 _ = happyReduce_66

action_11 _ = happyReduce_67

action_12 (28) = happyShift action_15
action_12 (29) = happyShift action_16
action_12 (30) = happyShift action_17
action_12 (31) = happyShift action_18
action_12 (32) = happyShift action_2
action_12 (38) = happyShift action_19
action_12 (53) = happyShift action_20
action_12 (55) = happyShift action_21
action_12 (57) = happyShift action_22
action_12 (59) = happyShift action_23
action_12 (4) = happyGoto action_3
action_12 (7) = happyGoto action_4
action_12 (8) = happyGoto action_5
action_12 (9) = happyGoto action_6
action_12 (10) = happyGoto action_7
action_12 (12) = happyGoto action_8
action_12 (16) = happyGoto action_9
action_12 (17) = happyGoto action_10
action_12 (20) = happyGoto action_11
action_12 (21) = happyGoto action_12
action_12 (22) = happyGoto action_13
action_12 (27) = happyGoto action_38
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_64

action_14 (61) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_9

action_16 _ = happyReduce_10

action_17 _ = happyReduce_11

action_18 _ = happyReduce_12

action_19 _ = happyReduce_13

action_20 (32) = happyShift action_36
action_20 (54) = happyShift action_37
action_20 (13) = happyGoto action_34
action_20 (14) = happyGoto action_35
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (35) = happyShift action_30
action_21 (39) = happyShift action_31
action_21 (40) = happyShift action_32
action_21 (41) = happyShift action_33
action_21 (18) = happyGoto action_28
action_21 (23) = happyGoto action_29
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (28) = happyShift action_15
action_22 (29) = happyShift action_16
action_22 (30) = happyShift action_17
action_22 (31) = happyShift action_18
action_22 (32) = happyShift action_2
action_22 (38) = happyShift action_19
action_22 (53) = happyShift action_20
action_22 (55) = happyShift action_21
action_22 (57) = happyShift action_22
action_22 (58) = happyShift action_27
action_22 (59) = happyShift action_23
action_22 (4) = happyGoto action_3
action_22 (7) = happyGoto action_4
action_22 (8) = happyGoto action_5
action_22 (9) = happyGoto action_6
action_22 (10) = happyGoto action_7
action_22 (11) = happyGoto action_25
action_22 (12) = happyGoto action_8
action_22 (16) = happyGoto action_9
action_22 (17) = happyGoto action_10
action_22 (20) = happyGoto action_11
action_22 (21) = happyGoto action_12
action_22 (22) = happyGoto action_13
action_22 (27) = happyGoto action_26
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (28) = happyShift action_15
action_23 (29) = happyShift action_16
action_23 (30) = happyShift action_17
action_23 (31) = happyShift action_18
action_23 (32) = happyShift action_2
action_23 (38) = happyShift action_19
action_23 (53) = happyShift action_20
action_23 (55) = happyShift action_21
action_23 (57) = happyShift action_22
action_23 (59) = happyShift action_23
action_23 (4) = happyGoto action_3
action_23 (7) = happyGoto action_4
action_23 (8) = happyGoto action_5
action_23 (9) = happyGoto action_6
action_23 (10) = happyGoto action_7
action_23 (12) = happyGoto action_8
action_23 (16) = happyGoto action_9
action_23 (17) = happyGoto action_10
action_23 (20) = happyGoto action_11
action_23 (21) = happyGoto action_12
action_23 (22) = happyGoto action_13
action_23 (27) = happyGoto action_24
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (60) = happyShift action_81
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (45) = happyShift action_79
action_25 (58) = happyShift action_80
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_17

action_27 _ = happyReduce_16

action_28 (28) = happyShift action_15
action_28 (29) = happyShift action_16
action_28 (30) = happyShift action_17
action_28 (31) = happyShift action_18
action_28 (32) = happyShift action_2
action_28 (38) = happyShift action_19
action_28 (40) = happyShift action_32
action_28 (41) = happyShift action_33
action_28 (53) = happyShift action_20
action_28 (57) = happyShift action_22
action_28 (59) = happyShift action_78
action_28 (4) = happyGoto action_69
action_28 (7) = happyGoto action_70
action_28 (8) = happyGoto action_71
action_28 (9) = happyGoto action_72
action_28 (10) = happyGoto action_73
action_28 (12) = happyGoto action_74
action_28 (18) = happyGoto action_75
action_28 (19) = happyGoto action_76
action_28 (23) = happyGoto action_77
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (56) = happyShift action_68
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (28) = happyShift action_15
action_30 (29) = happyShift action_16
action_30 (30) = happyShift action_17
action_30 (31) = happyShift action_18
action_30 (32) = happyShift action_2
action_30 (41) = happyShift action_33
action_30 (55) = happyShift action_66
action_30 (59) = happyShift action_67
action_30 (4) = happyGoto action_59
action_30 (7) = happyGoto action_60
action_30 (8) = happyGoto action_61
action_30 (15) = happyGoto action_62
action_30 (16) = happyGoto action_63
action_30 (23) = happyGoto action_64
action_30 (26) = happyGoto action_65
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (41) = happyShift action_57
action_31 (51) = happyShift action_58
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_30

action_33 (44) = happyShift action_55
action_33 (57) = happyShift action_56
action_33 (24) = happyGoto action_53
action_33 (25) = happyGoto action_54
action_33 _ = happyReduce_45

action_34 (45) = happyShift action_51
action_34 (54) = happyShift action_52
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_20

action_36 (34) = happyShift action_50
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_14

action_38 (55) = happyShift action_49
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (33) = happyShift action_46
action_39 (34) = happyShift action_47
action_39 (55) = happyShift action_48
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_5

action_41 (28) = happyShift action_15
action_41 (29) = happyShift action_16
action_41 (30) = happyShift action_17
action_41 (31) = happyShift action_18
action_41 (41) = happyShift action_33
action_41 (6) = happyGoto action_42
action_41 (7) = happyGoto action_43
action_41 (8) = happyGoto action_44
action_41 (23) = happyGoto action_45
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (56) = happyShift action_103
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_8

action_44 _ = happyReduce_7

action_45 _ = happyReduce_6

action_46 _ = happyReduce_1

action_47 _ = happyReduce_3

action_48 (28) = happyShift action_15
action_48 (29) = happyShift action_16
action_48 (30) = happyShift action_17
action_48 (31) = happyShift action_18
action_48 (41) = happyShift action_33
action_48 (6) = happyGoto action_102
action_48 (7) = happyGoto action_43
action_48 (8) = happyGoto action_44
action_48 (23) = happyGoto action_45
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (37) = happyShift action_101
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (33) = happyShift action_100
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (32) = happyShift action_36
action_51 (14) = happyGoto action_99
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_19

action_53 (44) = happyShift action_55
action_53 (57) = happyShift action_56
action_53 (25) = happyGoto action_98
action_53 _ = happyReduce_44

action_54 _ = happyReduce_46

action_55 (41) = happyShift action_97
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (29) = happyShift action_95
action_56 (42) = happyShift action_96
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (45) = happyShift action_94
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (45) = happyShift action_93
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_56

action_60 _ = happyReduce_55

action_61 _ = happyReduce_54

action_62 _ = happyReduce_53

action_63 _ = happyReduce_52

action_64 _ = happyReduce_51

action_65 (46) = happyShift action_87
action_65 (47) = happyShift action_88
action_65 (48) = happyShift action_89
action_65 (49) = happyShift action_90
action_65 (50) = happyShift action_91
action_65 (56) = happyShift action_92
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (35) = happyShift action_30
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (28) = happyShift action_15
action_67 (29) = happyShift action_16
action_67 (30) = happyShift action_17
action_67 (31) = happyShift action_18
action_67 (32) = happyShift action_2
action_67 (41) = happyShift action_33
action_67 (55) = happyShift action_66
action_67 (59) = happyShift action_67
action_67 (4) = happyGoto action_59
action_67 (7) = happyGoto action_60
action_67 (8) = happyGoto action_61
action_67 (15) = happyGoto action_62
action_67 (16) = happyGoto action_63
action_67 (23) = happyGoto action_64
action_67 (26) = happyGoto action_86
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_43

action_69 _ = happyReduce_33

action_70 _ = happyReduce_34

action_71 _ = happyReduce_32

action_72 _ = happyReduce_31

action_73 _ = happyReduce_36

action_74 _ = happyReduce_37

action_75 (28) = happyShift action_15
action_75 (29) = happyShift action_16
action_75 (30) = happyShift action_17
action_75 (31) = happyShift action_18
action_75 (32) = happyShift action_2
action_75 (38) = happyShift action_19
action_75 (40) = happyShift action_32
action_75 (41) = happyShift action_33
action_75 (53) = happyShift action_20
action_75 (57) = happyShift action_22
action_75 (59) = happyShift action_78
action_75 (4) = happyGoto action_69
action_75 (7) = happyGoto action_70
action_75 (8) = happyGoto action_71
action_75 (9) = happyGoto action_72
action_75 (10) = happyGoto action_73
action_75 (12) = happyGoto action_74
action_75 (18) = happyGoto action_75
action_75 (19) = happyGoto action_85
action_75 (23) = happyGoto action_77
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (56) = happyShift action_84
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_35

action_78 (28) = happyShift action_15
action_78 (29) = happyShift action_16
action_78 (30) = happyShift action_17
action_78 (31) = happyShift action_18
action_78 (32) = happyShift action_2
action_78 (38) = happyShift action_19
action_78 (40) = happyShift action_32
action_78 (41) = happyShift action_33
action_78 (53) = happyShift action_20
action_78 (57) = happyShift action_22
action_78 (59) = happyShift action_78
action_78 (4) = happyGoto action_69
action_78 (7) = happyGoto action_70
action_78 (8) = happyGoto action_71
action_78 (9) = happyGoto action_72
action_78 (10) = happyGoto action_73
action_78 (12) = happyGoto action_74
action_78 (18) = happyGoto action_75
action_78 (19) = happyGoto action_83
action_78 (23) = happyGoto action_77
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (28) = happyShift action_15
action_79 (29) = happyShift action_16
action_79 (30) = happyShift action_17
action_79 (31) = happyShift action_18
action_79 (32) = happyShift action_2
action_79 (38) = happyShift action_19
action_79 (53) = happyShift action_20
action_79 (55) = happyShift action_21
action_79 (57) = happyShift action_22
action_79 (59) = happyShift action_23
action_79 (4) = happyGoto action_3
action_79 (7) = happyGoto action_4
action_79 (8) = happyGoto action_5
action_79 (9) = happyGoto action_6
action_79 (10) = happyGoto action_7
action_79 (12) = happyGoto action_8
action_79 (16) = happyGoto action_9
action_79 (17) = happyGoto action_10
action_79 (20) = happyGoto action_11
action_79 (21) = happyGoto action_12
action_79 (22) = happyGoto action_13
action_79 (27) = happyGoto action_82
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_15

action_81 _ = happyReduce_68

action_82 _ = happyReduce_18

action_83 (60) = happyShift action_118
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_29

action_85 _ = happyReduce_38

action_86 (46) = happyShift action_87
action_86 (47) = happyShift action_88
action_86 (48) = happyShift action_89
action_86 (49) = happyShift action_90
action_86 (50) = happyShift action_91
action_86 (60) = happyShift action_117
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (28) = happyShift action_15
action_87 (29) = happyShift action_16
action_87 (30) = happyShift action_17
action_87 (31) = happyShift action_18
action_87 (32) = happyShift action_2
action_87 (41) = happyShift action_33
action_87 (55) = happyShift action_66
action_87 (59) = happyShift action_67
action_87 (4) = happyGoto action_59
action_87 (7) = happyGoto action_60
action_87 (8) = happyGoto action_61
action_87 (15) = happyGoto action_62
action_87 (16) = happyGoto action_63
action_87 (23) = happyGoto action_64
action_87 (26) = happyGoto action_116
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (28) = happyShift action_15
action_88 (29) = happyShift action_16
action_88 (30) = happyShift action_17
action_88 (31) = happyShift action_18
action_88 (32) = happyShift action_2
action_88 (41) = happyShift action_33
action_88 (55) = happyShift action_66
action_88 (59) = happyShift action_67
action_88 (4) = happyGoto action_59
action_88 (7) = happyGoto action_60
action_88 (8) = happyGoto action_61
action_88 (15) = happyGoto action_62
action_88 (16) = happyGoto action_63
action_88 (23) = happyGoto action_64
action_88 (26) = happyGoto action_115
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (28) = happyShift action_15
action_89 (29) = happyShift action_16
action_89 (30) = happyShift action_17
action_89 (31) = happyShift action_18
action_89 (32) = happyShift action_2
action_89 (41) = happyShift action_33
action_89 (55) = happyShift action_66
action_89 (59) = happyShift action_67
action_89 (4) = happyGoto action_59
action_89 (7) = happyGoto action_60
action_89 (8) = happyGoto action_61
action_89 (15) = happyGoto action_62
action_89 (16) = happyGoto action_63
action_89 (23) = happyGoto action_64
action_89 (26) = happyGoto action_114
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (28) = happyShift action_15
action_90 (29) = happyShift action_16
action_90 (30) = happyShift action_17
action_90 (31) = happyShift action_18
action_90 (32) = happyShift action_2
action_90 (41) = happyShift action_33
action_90 (55) = happyShift action_66
action_90 (59) = happyShift action_67
action_90 (4) = happyGoto action_59
action_90 (7) = happyGoto action_60
action_90 (8) = happyGoto action_61
action_90 (15) = happyGoto action_62
action_90 (16) = happyGoto action_63
action_90 (23) = happyGoto action_64
action_90 (26) = happyGoto action_113
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (28) = happyShift action_15
action_91 (29) = happyShift action_16
action_91 (30) = happyShift action_17
action_91 (31) = happyShift action_18
action_91 (32) = happyShift action_2
action_91 (41) = happyShift action_33
action_91 (55) = happyShift action_66
action_91 (59) = happyShift action_67
action_91 (4) = happyGoto action_59
action_91 (7) = happyGoto action_60
action_91 (8) = happyGoto action_61
action_91 (15) = happyGoto action_62
action_91 (16) = happyGoto action_63
action_91 (23) = happyGoto action_64
action_91 (26) = happyGoto action_112
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (28) = happyShift action_15
action_92 (29) = happyShift action_16
action_92 (30) = happyShift action_17
action_92 (31) = happyShift action_18
action_92 (32) = happyShift action_2
action_92 (38) = happyShift action_19
action_92 (53) = happyShift action_20
action_92 (55) = happyShift action_21
action_92 (57) = happyShift action_22
action_92 (59) = happyShift action_23
action_92 (4) = happyGoto action_3
action_92 (7) = happyGoto action_4
action_92 (8) = happyGoto action_5
action_92 (9) = happyGoto action_6
action_92 (10) = happyGoto action_7
action_92 (12) = happyGoto action_8
action_92 (16) = happyGoto action_9
action_92 (17) = happyGoto action_10
action_92 (20) = happyGoto action_11
action_92 (21) = happyGoto action_12
action_92 (22) = happyGoto action_13
action_92 (27) = happyGoto action_111
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (41) = happyShift action_110
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (41) = happyShift action_109
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (58) = happyShift action_108
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (34) = happyShift action_107
action_96 _ = happyFail (happyExpListPerState 96)

action_97 _ = happyReduce_48

action_98 _ = happyReduce_47

action_99 _ = happyReduce_21

action_100 (43) = happyShift action_106
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (56) = happyShift action_105
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (56) = happyShift action_104
action_102 _ = happyFail (happyExpListPerState 102)

action_103 _ = happyReduce_4

action_104 _ = happyReduce_2

action_105 _ = happyReduce_40

action_106 (28) = happyShift action_15
action_106 (29) = happyShift action_16
action_106 (30) = happyShift action_17
action_106 (31) = happyShift action_18
action_106 (32) = happyShift action_2
action_106 (38) = happyShift action_19
action_106 (53) = happyShift action_20
action_106 (55) = happyShift action_21
action_106 (57) = happyShift action_22
action_106 (59) = happyShift action_23
action_106 (4) = happyGoto action_3
action_106 (7) = happyGoto action_4
action_106 (8) = happyGoto action_5
action_106 (9) = happyGoto action_6
action_106 (10) = happyGoto action_7
action_106 (12) = happyGoto action_8
action_106 (16) = happyGoto action_9
action_106 (17) = happyGoto action_10
action_106 (20) = happyGoto action_11
action_106 (21) = happyGoto action_12
action_106 (22) = happyGoto action_13
action_106 (27) = happyGoto action_123
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (42) = happyShift action_122
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_50

action_109 (52) = happyShift action_121
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (52) = happyShift action_120
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (55) = happyShift action_119
action_111 _ = happyFail (happyExpListPerState 111)

action_112 _ = happyReduce_27

action_113 _ = happyReduce_26

action_114 _ = happyReduce_24

action_115 _ = happyReduce_23

action_116 _ = happyReduce_25

action_117 _ = happyReduce_57

action_118 _ = happyReduce_39

action_119 (36) = happyShift action_127
action_119 _ = happyFail (happyExpListPerState 119)

action_120 (41) = happyShift action_33
action_120 (23) = happyGoto action_126
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (41) = happyShift action_33
action_121 (23) = happyGoto action_125
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (58) = happyShift action_124
action_122 _ = happyFail (happyExpListPerState 122)

action_123 _ = happyReduce_22

action_124 _ = happyReduce_49

action_125 (56) = happyShift action_130
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (56) = happyShift action_129
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (56) = happyShift action_128
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (28) = happyShift action_15
action_128 (29) = happyShift action_16
action_128 (30) = happyShift action_17
action_128 (31) = happyShift action_18
action_128 (32) = happyShift action_2
action_128 (38) = happyShift action_19
action_128 (53) = happyShift action_20
action_128 (55) = happyShift action_21
action_128 (57) = happyShift action_22
action_128 (59) = happyShift action_23
action_128 (4) = happyGoto action_3
action_128 (7) = happyGoto action_4
action_128 (8) = happyGoto action_5
action_128 (9) = happyGoto action_6
action_128 (10) = happyGoto action_7
action_128 (12) = happyGoto action_8
action_128 (16) = happyGoto action_9
action_128 (17) = happyGoto action_10
action_128 (20) = happyGoto action_11
action_128 (21) = happyGoto action_12
action_128 (22) = happyGoto action_13
action_128 (27) = happyGoto action_131
action_128 _ = happyFail (happyExpListPerState 128)

action_129 _ = happyReduce_42

action_130 _ = happyReduce_41

action_131 (55) = happyShift action_132
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (37) = happyShift action_133
action_132 _ = happyFail (happyExpListPerState 132)

action_133 (56) = happyShift action_134
action_133 _ = happyFail (happyExpListPerState 133)

action_134 _ = happyReduce_28

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokSymbol SymStringEnd happy_var_3))
	(HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokSymbol SymStringBegin happy_var_1))
	 =  HappyAbsSyn4
		 (StringTem (locate happy_var_1 <> happy_var_3) happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happyReduce 4 5 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (V.snoc happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyTerminal (TokStringLit happy_var_2))
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (V.snoc happy_var_1 (String (locate happy_var_2) (unlocate happy_var_2))
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (V.singleton happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyTerminal (TokStringLit happy_var_1))
	 =  HappyAbsSyn5
		 (V.singleton (String (locate happy_var_1) (unlocate happy_var_1))
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn4
		 (uncurry Path happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  7 happyReduction_9
happyReduction_9 (HappyTerminal (TokNumLit _ happy_var_1))
	 =  HappyAbsSyn4
		 (Number (locate happy_var_1) (unlocate happy_var_1)
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 (HappyTerminal (TokIntLit _ happy_var_1))
	 =  HappyAbsSyn4
		 (Number (locate happy_var_1) (S.scientific (fromIntegral (unlocate happy_var_1)) 0)
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  8 happyReduction_11
happyReduction_11 (HappyTerminal (TokBoolLit happy_var_1))
	 =  HappyAbsSyn4
		 (Boolean (locate happy_var_1) (unlocate happy_var_1)
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 (HappyTerminal (TokBoolLit happy_var_1))
	 =  HappyAbsSyn4
		 (Boolean (locate happy_var_1) (unlocate happy_var_1)
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 (HappyTerminal (TokIdentifier (Loc happy_var_1 "null" )))
	 =  HappyAbsSyn4
		 (Null (locate happy_var_1)
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  9 happyReduction_14
happyReduction_14 (HappyTerminal (TokSymbol SymCurlyClose happy_var_2))
	(HappyTerminal (TokSymbol SymCurlyOpen happy_var_1))
	 =  HappyAbsSyn4
		 (Null (locate happy_var_1 <> locate happy_var_2)
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 (HappyTerminal (TokSymbol SymSquareClose happy_var_3))
	(HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokSymbol SymSquareOpen happy_var_1))
	 =  HappyAbsSyn4
		 (Array (locate happy_var_1 <> locate happy_var_3) happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  10 happyReduction_16
happyReduction_16 (HappyTerminal (TokSymbol SymSquareClose happy_var_2))
	(HappyTerminal (TokSymbol SymSquareOpen happy_var_1))
	 =  HappyAbsSyn4
		 (Array (locate happy_var_1 <> locate happy_var_2) V.empty
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 (V.singleton happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (V.snoc happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 (HappyTerminal (TokSymbol SymCurlyClose happy_var_3))
	(HappyAbsSyn13  happy_var_2)
	(HappyTerminal (TokSymbol SymCurlyOpen happy_var_1))
	 =  HappyAbsSyn4
		 (Object (locate happy_var_1 <> locate happy_var_3) (M.fromList happy_var_2)
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  13 happyReduction_21
happyReduction_21 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_3 : happy_var_1
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 5 14 happyReduction_22
happyReduction_22 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokStringLit happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 ((unlocate happy_var_2, happy_var_5)
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_3  15 happyReduction_23
happyReduction_23 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Gt (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  15 happyReduction_24
happyReduction_24 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Lt (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  15 happyReduction_25
happyReduction_25 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Eq (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  15 happyReduction_26
happyReduction_26 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  15 happyReduction_27
happyReduction_27 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 12 16 happyReduction_28
happyReduction_28 ((HappyTerminal (TokSymbol SymDoubleCurlyClose happy_var_12)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokSymbol SymDoubleCurlyOpen happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Iff (locate happy_var_1 <> locate happy_var_12) happy_var_3 happy_var_5 happy_var_9
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 4 17 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_1  18 happyReduction_30
happyReduction_30 (HappyTerminal (TokIdentifier (Loc happy_var_1 "escapeUri")))
	 =  HappyAbsSyn18
		 (EscapeURI (locate happy_var_1)
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  19 happyReduction_31
happyReduction_31 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  19 happyReduction_32
happyReduction_32 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  19 happyReduction_33
happyReduction_33 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  19 happyReduction_34
happyReduction_34 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  19 happyReduction_35
happyReduction_35 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn4
		 (uncurry Path happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  19 happyReduction_36
happyReduction_36 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  19 happyReduction_37
happyReduction_37 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_2  19 happyReduction_38
happyReduction_38 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  19 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happyReduce 5 20 happyReduction_40
happyReduction_40 ((HappyTerminal (TokSymbol SymDoubleCurlyClose happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_1 (locate happy_var_5) happy_var_2
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 8 21 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdentifier happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokSymbol SymDoubleCurlyOpen happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (\s b -> Range (locate happy_var_1 <> s) (Just (unlocate happy_var_3)) (unlocate happy_var_5) (snd happy_var_7) b
	) `HappyStk` happyRest

happyReduce_42 = happyReduce 8 21 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdentifier happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokSymbol SymDoubleCurlyOpen happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (\s b -> Range (locate happy_var_1 <> s) Nothing (unlocate happy_var_5) (snd happy_var_7) b
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_3  22 happyReduction_43
happyReduction_43 (HappyTerminal (TokSymbol SymDoubleCurlyClose happy_var_3))
	(HappyAbsSyn23  happy_var_2)
	(HappyTerminal (TokSymbol SymDoubleCurlyOpen happy_var_1))
	 =  HappyAbsSyn4
		 (Path (locate happy_var_1 <> locate happy_var_3) (snd happy_var_2)
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  23 happyReduction_44
happyReduction_44 (HappyAbsSyn23  happy_var_2)
	(HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn23
		 ((locate happy_var_1 <> fst happy_var_2, V.cons (Obj (locate happy_var_1) (unlocate happy_var_1)) (snd happy_var_2))
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  23 happyReduction_45
happyReduction_45 (HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn23
		 ((locate happy_var_1, V.singleton (Obj (locate happy_var_1) (unlocate happy_var_1)))
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  24 happyReduction_46
happyReduction_46 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn23
		 ((locate happy_var_1, V.singleton happy_var_1)
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  24 happyReduction_47
happyReduction_47 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 ((fst happy_var_1 <> locate happy_var_2, V.snoc (snd happy_var_1) happy_var_2)
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  25 happyReduction_48
happyReduction_48 (HappyTerminal (TokIdentifier happy_var_2))
	(HappyTerminal (TokSymbol SymDot happy_var_1))
	 =  HappyAbsSyn25
		 (Obj (locate happy_var_1 <> locate happy_var_2) (unlocate happy_var_2)
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happyReduce 5 25 happyReduction_49
happyReduction_49 ((HappyTerminal (TokSymbol SymSquareClose happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokStringLit happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokSymbol SymSquareOpen happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (Obj (locate happy_var_1 <> locate happy_var_5) (unlocate happy_var_3)
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_3  25 happyReduction_50
happyReduction_50 (HappyTerminal (TokSymbol SymSquareClose happy_var_3))
	(HappyTerminal (TokIntLit _ happy_var_2))
	(HappyTerminal (TokSymbol SymSquareOpen happy_var_1))
	 =  HappyAbsSyn25
		 (Arr (locate happy_var_1 <> locate happy_var_3) (unlocate happy_var_2)
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  26 happyReduction_51
happyReduction_51 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn4
		 (uncurry Path happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  26 happyReduction_52
happyReduction_52 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  26 happyReduction_53
happyReduction_53 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  26 happyReduction_54
happyReduction_54 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  26 happyReduction_55
happyReduction_55 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  26 happyReduction_56
happyReduction_56 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  26 happyReduction_57
happyReduction_57 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  27 happyReduction_58
happyReduction_58 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  27 happyReduction_59
happyReduction_59 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  27 happyReduction_60
happyReduction_60 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  27 happyReduction_61
happyReduction_61 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  27 happyReduction_62
happyReduction_62 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  27 happyReduction_63
happyReduction_63 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  27 happyReduction_64
happyReduction_64 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  27 happyReduction_65
happyReduction_65 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  27 happyReduction_66
happyReduction_66 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  27 happyReduction_67
happyReduction_67 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  27 happyReduction_68
happyReduction_68 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 61 61 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokNumLit _ happy_dollar_dollar -> cont 28;
	TokIntLit _ happy_dollar_dollar -> cont 29;
	TokBoolLit happy_dollar_dollar -> cont 30;
	TokBoolLit happy_dollar_dollar -> cont 31;
	TokSymbol SymStringBegin happy_dollar_dollar -> cont 32;
	TokSymbol SymStringEnd happy_dollar_dollar -> cont 33;
	TokStringLit happy_dollar_dollar -> cont 34;
	TokIdentifier (Loc happy_dollar_dollar "if") -> cont 35;
	TokIdentifier (Loc happy_dollar_dollar "else") -> cont 36;
	TokIdentifier (Loc happy_dollar_dollar "end") -> cont 37;
	TokIdentifier (Loc happy_dollar_dollar "null" ) -> cont 38;
	TokIdentifier (Loc happy_dollar_dollar "range") -> cont 39;
	TokIdentifier (Loc happy_dollar_dollar "escapeUri") -> cont 40;
	TokIdentifier happy_dollar_dollar -> cont 41;
	TokSymbol SymSingleQuote happy_dollar_dollar -> cont 42;
	TokSymbol SymColon happy_dollar_dollar -> cont 43;
	TokSymbol SymDot happy_dollar_dollar -> cont 44;
	TokSymbol SymComma happy_dollar_dollar -> cont 45;
	TokSymbol SymEq happy_dollar_dollar -> cont 46;
	TokSymbol SymGt happy_dollar_dollar -> cont 47;
	TokSymbol SymLt happy_dollar_dollar -> cont 48;
	TokSymbol SymAnd happy_dollar_dollar -> cont 49;
	TokSymbol SymOr happy_dollar_dollar -> cont 50;
	TokSymbol SymUnderscore happy_dollar_dollar -> cont 51;
	TokSymbol SymAssignment happy_dollar_dollar -> cont 52;
	TokSymbol SymCurlyOpen happy_dollar_dollar -> cont 53;
	TokSymbol SymCurlyClose happy_dollar_dollar -> cont 54;
	TokSymbol SymDoubleCurlyOpen happy_dollar_dollar -> cont 55;
	TokSymbol SymDoubleCurlyClose happy_dollar_dollar -> cont 56;
	TokSymbol SymSquareOpen happy_dollar_dollar -> cont 57;
	TokSymbol SymSquareClose happy_dollar_dollar -> cont 58;
	TokSymbol SymParenOpen happy_dollar_dollar -> cont 59;
	TokSymbol SymParenClose happy_dollar_dollar -> cont 60;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 61 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Parser a -> (a -> Parser b) -> Parser b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Parser a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Parser a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Parser a
happyError' = (\(tokens, _) -> failure tokens)
parser tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


failure :: [Token] -> Parser a
failure [] = parseError EmptyTokenStream
failure (tok:_) = do
  sp <- location
  parseError $ UnexpectedToken (Loc sp tok)
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































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
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
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
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
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





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

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
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
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
