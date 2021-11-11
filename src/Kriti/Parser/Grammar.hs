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
 action_129 :: () => Prelude.Int -> ({-HappyReduction (Parser) = -}
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
 happyReduce_65 :: () => ({-HappyReduction (Parser) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Parser) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,386) ([0,63488,128,5440,0,16384,0,0,0,32768,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,2063,21504,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,8,0,32768,56,0,0,992,2,117,0,496,32769,42,0,0,0,32,0,0,32,4,0,0,0,0,0,0,0,0,32768,26639,17408,1,0,0,4096,0,57344,4099,17408,0,0,2048,32,0,0,0,0,0,0,4096,512,0,0,4096,32,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,4,0,2560,0,0,0,0,0,0,0,0,2,0,0,512,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,256,0,0,61440,257,10880,0,0,1024,0,0,0,0,0,0,0,2048,256,0,0,0,0,0,0,64,0,0,128,64,0,0,0,256,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2110,0,0,32,0,0,63488,1024,4352,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33016,16390,20,0,0,0,1,0,0,0,0,0,53279,34816,2,32768,2063,21504,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,61440,1025,0,3968,64,272,0,1984,32,136,0,992,16,68,0,496,8,34,0,248,4,17,0,16508,40960,10,0,0,1,0,0,32768,0,0,0,0,32768,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,4096,0,0,0,2048,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4,0,0,0,64,0,0,0,32,0,0,0,0,32,0,0,0,0,0,0,0,2,0,0,0,1,0,0,32768,0,0,4127,43008,2,0,0,0,0,0,0,0,0,0,0,1024,0,0,128,0,0,0,0,512,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","string_lit","string_template","template","num_lit","boolean","null","array","list_elements","object","object_fields","object_field","operator","iff","function_call","functions","function_params","range","range_decl","path","path_vector","path_tail","path_element","value","term","number","int","'true'","'false'","'s\"'","'\"e'","string","'s{'","'}e'","'if'","'else'","'end'","'null'","'range'","'escapeUri'","ident","'\\''","':'","'.'","','","'=='","'>'","'<'","'&&'","'||'","'_'","':='","'{'","'}'","'{{'","'}}'","'['","']'","'('","')'","%eof"]
        bit_start = st Prelude.* 63
        bit_end = (st Prelude.+ 1) Prelude.* 63
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..62]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (28) = happyShift action_15
action_0 (29) = happyShift action_16
action_0 (30) = happyShift action_17
action_0 (31) = happyShift action_18
action_0 (32) = happyShift action_2
action_0 (40) = happyShift action_19
action_0 (55) = happyShift action_20
action_0 (57) = happyShift action_21
action_0 (59) = happyShift action_22
action_0 (61) = happyShift action_23
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
action_2 (35) = happyShift action_41
action_2 (5) = happyGoto action_39
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_56

action_4 _ = happyReduce_55

action_5 _ = happyReduce_57

action_6 _ = happyReduce_58

action_7 _ = happyReduce_59

action_8 _ = happyReduce_60

action_9 _ = happyReduce_62

action_10 _ = happyReduce_63

action_11 _ = happyReduce_64

action_12 (28) = happyShift action_15
action_12 (29) = happyShift action_16
action_12 (30) = happyShift action_17
action_12 (31) = happyShift action_18
action_12 (32) = happyShift action_2
action_12 (40) = happyShift action_19
action_12 (55) = happyShift action_20
action_12 (57) = happyShift action_21
action_12 (59) = happyShift action_22
action_12 (61) = happyShift action_23
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

action_13 _ = happyReduce_61

action_14 (63) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_6

action_16 _ = happyReduce_7

action_17 _ = happyReduce_8

action_18 _ = happyReduce_9

action_19 _ = happyReduce_10

action_20 (43) = happyShift action_36
action_20 (56) = happyShift action_37
action_20 (13) = happyGoto action_34
action_20 (14) = happyGoto action_35
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (37) = happyShift action_30
action_21 (41) = happyShift action_31
action_21 (42) = happyShift action_32
action_21 (43) = happyShift action_33
action_21 (18) = happyGoto action_28
action_21 (23) = happyGoto action_29
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (28) = happyShift action_15
action_22 (29) = happyShift action_16
action_22 (30) = happyShift action_17
action_22 (31) = happyShift action_18
action_22 (32) = happyShift action_2
action_22 (40) = happyShift action_19
action_22 (55) = happyShift action_20
action_22 (57) = happyShift action_21
action_22 (59) = happyShift action_22
action_22 (60) = happyShift action_27
action_22 (61) = happyShift action_23
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
action_23 (40) = happyShift action_19
action_23 (55) = happyShift action_20
action_23 (57) = happyShift action_21
action_23 (59) = happyShift action_22
action_23 (61) = happyShift action_23
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

action_24 (62) = happyShift action_78
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (47) = happyShift action_76
action_25 (60) = happyShift action_77
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_14

action_27 _ = happyReduce_13

action_28 (28) = happyShift action_15
action_28 (29) = happyShift action_16
action_28 (30) = happyShift action_17
action_28 (31) = happyShift action_18
action_28 (32) = happyShift action_2
action_28 (40) = happyShift action_19
action_28 (42) = happyShift action_32
action_28 (43) = happyShift action_33
action_28 (55) = happyShift action_20
action_28 (59) = happyShift action_22
action_28 (61) = happyShift action_75
action_28 (4) = happyGoto action_66
action_28 (7) = happyGoto action_67
action_28 (8) = happyGoto action_68
action_28 (9) = happyGoto action_69
action_28 (10) = happyGoto action_70
action_28 (12) = happyGoto action_71
action_28 (18) = happyGoto action_72
action_28 (19) = happyGoto action_73
action_28 (23) = happyGoto action_74
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (58) = happyShift action_65
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (28) = happyShift action_15
action_30 (29) = happyShift action_16
action_30 (30) = happyShift action_17
action_30 (31) = happyShift action_18
action_30 (32) = happyShift action_2
action_30 (43) = happyShift action_33
action_30 (57) = happyShift action_63
action_30 (61) = happyShift action_64
action_30 (4) = happyGoto action_56
action_30 (7) = happyGoto action_57
action_30 (8) = happyGoto action_58
action_30 (15) = happyGoto action_59
action_30 (16) = happyGoto action_60
action_30 (23) = happyGoto action_61
action_30 (26) = happyGoto action_62
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (43) = happyShift action_54
action_31 (53) = happyShift action_55
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_27

action_33 (46) = happyShift action_52
action_33 (59) = happyShift action_53
action_33 (24) = happyGoto action_50
action_33 (25) = happyGoto action_51
action_33 _ = happyReduce_42

action_34 (47) = happyShift action_48
action_34 (56) = happyShift action_49
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_17

action_36 (45) = happyShift action_47
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_11

action_38 (57) = happyShift action_46
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (33) = happyShift action_44
action_39 (35) = happyShift action_45
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_4

action_41 (43) = happyShift action_33
action_41 (6) = happyGoto action_42
action_41 (23) = happyGoto action_43
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (36) = happyShift action_100
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_5

action_44 _ = happyReduce_1

action_45 (43) = happyShift action_33
action_45 (6) = happyGoto action_99
action_45 (23) = happyGoto action_43
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (39) = happyShift action_98
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (28) = happyShift action_15
action_47 (29) = happyShift action_16
action_47 (30) = happyShift action_17
action_47 (31) = happyShift action_18
action_47 (32) = happyShift action_2
action_47 (40) = happyShift action_19
action_47 (55) = happyShift action_20
action_47 (57) = happyShift action_21
action_47 (59) = happyShift action_22
action_47 (61) = happyShift action_23
action_47 (4) = happyGoto action_3
action_47 (7) = happyGoto action_4
action_47 (8) = happyGoto action_5
action_47 (9) = happyGoto action_6
action_47 (10) = happyGoto action_7
action_47 (12) = happyGoto action_8
action_47 (16) = happyGoto action_9
action_47 (17) = happyGoto action_10
action_47 (20) = happyGoto action_11
action_47 (21) = happyGoto action_12
action_47 (22) = happyGoto action_13
action_47 (27) = happyGoto action_97
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (43) = happyShift action_36
action_48 (14) = happyGoto action_96
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_16

action_50 (46) = happyShift action_52
action_50 (59) = happyShift action_53
action_50 (25) = happyGoto action_95
action_50 _ = happyReduce_41

action_51 _ = happyReduce_43

action_52 (43) = happyShift action_94
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (29) = happyShift action_92
action_53 (44) = happyShift action_93
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (47) = happyShift action_91
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (47) = happyShift action_90
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_53

action_57 _ = happyReduce_52

action_58 _ = happyReduce_51

action_59 _ = happyReduce_50

action_60 _ = happyReduce_49

action_61 _ = happyReduce_48

action_62 (48) = happyShift action_84
action_62 (49) = happyShift action_85
action_62 (50) = happyShift action_86
action_62 (51) = happyShift action_87
action_62 (52) = happyShift action_88
action_62 (58) = happyShift action_89
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (37) = happyShift action_30
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (28) = happyShift action_15
action_64 (29) = happyShift action_16
action_64 (30) = happyShift action_17
action_64 (31) = happyShift action_18
action_64 (32) = happyShift action_2
action_64 (43) = happyShift action_33
action_64 (57) = happyShift action_63
action_64 (61) = happyShift action_64
action_64 (4) = happyGoto action_56
action_64 (7) = happyGoto action_57
action_64 (8) = happyGoto action_58
action_64 (15) = happyGoto action_59
action_64 (16) = happyGoto action_60
action_64 (23) = happyGoto action_61
action_64 (26) = happyGoto action_83
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_40

action_66 _ = happyReduce_30

action_67 _ = happyReduce_31

action_68 _ = happyReduce_29

action_69 _ = happyReduce_28

action_70 _ = happyReduce_33

action_71 _ = happyReduce_34

action_72 (28) = happyShift action_15
action_72 (29) = happyShift action_16
action_72 (30) = happyShift action_17
action_72 (31) = happyShift action_18
action_72 (32) = happyShift action_2
action_72 (40) = happyShift action_19
action_72 (42) = happyShift action_32
action_72 (43) = happyShift action_33
action_72 (55) = happyShift action_20
action_72 (59) = happyShift action_22
action_72 (61) = happyShift action_75
action_72 (4) = happyGoto action_66
action_72 (7) = happyGoto action_67
action_72 (8) = happyGoto action_68
action_72 (9) = happyGoto action_69
action_72 (10) = happyGoto action_70
action_72 (12) = happyGoto action_71
action_72 (18) = happyGoto action_72
action_72 (19) = happyGoto action_82
action_72 (23) = happyGoto action_74
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (58) = happyShift action_81
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_32

action_75 (28) = happyShift action_15
action_75 (29) = happyShift action_16
action_75 (30) = happyShift action_17
action_75 (31) = happyShift action_18
action_75 (32) = happyShift action_2
action_75 (40) = happyShift action_19
action_75 (42) = happyShift action_32
action_75 (43) = happyShift action_33
action_75 (55) = happyShift action_20
action_75 (59) = happyShift action_22
action_75 (61) = happyShift action_75
action_75 (4) = happyGoto action_66
action_75 (7) = happyGoto action_67
action_75 (8) = happyGoto action_68
action_75 (9) = happyGoto action_69
action_75 (10) = happyGoto action_70
action_75 (12) = happyGoto action_71
action_75 (18) = happyGoto action_72
action_75 (19) = happyGoto action_80
action_75 (23) = happyGoto action_74
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (28) = happyShift action_15
action_76 (29) = happyShift action_16
action_76 (30) = happyShift action_17
action_76 (31) = happyShift action_18
action_76 (32) = happyShift action_2
action_76 (40) = happyShift action_19
action_76 (55) = happyShift action_20
action_76 (57) = happyShift action_21
action_76 (59) = happyShift action_22
action_76 (61) = happyShift action_23
action_76 (4) = happyGoto action_3
action_76 (7) = happyGoto action_4
action_76 (8) = happyGoto action_5
action_76 (9) = happyGoto action_6
action_76 (10) = happyGoto action_7
action_76 (12) = happyGoto action_8
action_76 (16) = happyGoto action_9
action_76 (17) = happyGoto action_10
action_76 (20) = happyGoto action_11
action_76 (21) = happyGoto action_12
action_76 (22) = happyGoto action_13
action_76 (27) = happyGoto action_79
action_76 _ = happyFail (happyExpListPerState 76)

action_77 _ = happyReduce_12

action_78 _ = happyReduce_65

action_79 _ = happyReduce_15

action_80 (62) = happyShift action_114
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_26

action_82 _ = happyReduce_35

action_83 (48) = happyShift action_84
action_83 (49) = happyShift action_85
action_83 (50) = happyShift action_86
action_83 (51) = happyShift action_87
action_83 (52) = happyShift action_88
action_83 (62) = happyShift action_113
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (28) = happyShift action_15
action_84 (29) = happyShift action_16
action_84 (30) = happyShift action_17
action_84 (31) = happyShift action_18
action_84 (32) = happyShift action_2
action_84 (43) = happyShift action_33
action_84 (57) = happyShift action_63
action_84 (61) = happyShift action_64
action_84 (4) = happyGoto action_56
action_84 (7) = happyGoto action_57
action_84 (8) = happyGoto action_58
action_84 (15) = happyGoto action_59
action_84 (16) = happyGoto action_60
action_84 (23) = happyGoto action_61
action_84 (26) = happyGoto action_112
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (28) = happyShift action_15
action_85 (29) = happyShift action_16
action_85 (30) = happyShift action_17
action_85 (31) = happyShift action_18
action_85 (32) = happyShift action_2
action_85 (43) = happyShift action_33
action_85 (57) = happyShift action_63
action_85 (61) = happyShift action_64
action_85 (4) = happyGoto action_56
action_85 (7) = happyGoto action_57
action_85 (8) = happyGoto action_58
action_85 (15) = happyGoto action_59
action_85 (16) = happyGoto action_60
action_85 (23) = happyGoto action_61
action_85 (26) = happyGoto action_111
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (28) = happyShift action_15
action_86 (29) = happyShift action_16
action_86 (30) = happyShift action_17
action_86 (31) = happyShift action_18
action_86 (32) = happyShift action_2
action_86 (43) = happyShift action_33
action_86 (57) = happyShift action_63
action_86 (61) = happyShift action_64
action_86 (4) = happyGoto action_56
action_86 (7) = happyGoto action_57
action_86 (8) = happyGoto action_58
action_86 (15) = happyGoto action_59
action_86 (16) = happyGoto action_60
action_86 (23) = happyGoto action_61
action_86 (26) = happyGoto action_110
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (28) = happyShift action_15
action_87 (29) = happyShift action_16
action_87 (30) = happyShift action_17
action_87 (31) = happyShift action_18
action_87 (32) = happyShift action_2
action_87 (43) = happyShift action_33
action_87 (57) = happyShift action_63
action_87 (61) = happyShift action_64
action_87 (4) = happyGoto action_56
action_87 (7) = happyGoto action_57
action_87 (8) = happyGoto action_58
action_87 (15) = happyGoto action_59
action_87 (16) = happyGoto action_60
action_87 (23) = happyGoto action_61
action_87 (26) = happyGoto action_109
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (28) = happyShift action_15
action_88 (29) = happyShift action_16
action_88 (30) = happyShift action_17
action_88 (31) = happyShift action_18
action_88 (32) = happyShift action_2
action_88 (43) = happyShift action_33
action_88 (57) = happyShift action_63
action_88 (61) = happyShift action_64
action_88 (4) = happyGoto action_56
action_88 (7) = happyGoto action_57
action_88 (8) = happyGoto action_58
action_88 (15) = happyGoto action_59
action_88 (16) = happyGoto action_60
action_88 (23) = happyGoto action_61
action_88 (26) = happyGoto action_108
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (28) = happyShift action_15
action_89 (29) = happyShift action_16
action_89 (30) = happyShift action_17
action_89 (31) = happyShift action_18
action_89 (32) = happyShift action_2
action_89 (40) = happyShift action_19
action_89 (55) = happyShift action_20
action_89 (57) = happyShift action_21
action_89 (59) = happyShift action_22
action_89 (61) = happyShift action_23
action_89 (4) = happyGoto action_3
action_89 (7) = happyGoto action_4
action_89 (8) = happyGoto action_5
action_89 (9) = happyGoto action_6
action_89 (10) = happyGoto action_7
action_89 (12) = happyGoto action_8
action_89 (16) = happyGoto action_9
action_89 (17) = happyGoto action_10
action_89 (20) = happyGoto action_11
action_89 (21) = happyGoto action_12
action_89 (22) = happyGoto action_13
action_89 (27) = happyGoto action_107
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (43) = happyShift action_106
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (43) = happyShift action_105
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (60) = happyShift action_104
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (43) = happyShift action_103
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_45

action_95 _ = happyReduce_44

action_96 _ = happyReduce_18

action_97 _ = happyReduce_19

action_98 (58) = happyShift action_102
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (36) = happyShift action_101
action_99 _ = happyFail (happyExpListPerState 99)

action_100 _ = happyReduce_3

action_101 _ = happyReduce_2

action_102 _ = happyReduce_37

action_103 (44) = happyShift action_118
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_47

action_105 (54) = happyShift action_117
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (54) = happyShift action_116
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (57) = happyShift action_115
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_24

action_109 _ = happyReduce_23

action_110 _ = happyReduce_21

action_111 _ = happyReduce_20

action_112 _ = happyReduce_22

action_113 _ = happyReduce_54

action_114 _ = happyReduce_36

action_115 (38) = happyShift action_122
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (43) = happyShift action_33
action_116 (23) = happyGoto action_121
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (43) = happyShift action_33
action_117 (23) = happyGoto action_120
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (60) = happyShift action_119
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_46

action_120 (58) = happyShift action_125
action_120 _ = happyFail (happyExpListPerState 120)

action_121 (58) = happyShift action_124
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (58) = happyShift action_123
action_122 _ = happyFail (happyExpListPerState 122)

action_123 (28) = happyShift action_15
action_123 (29) = happyShift action_16
action_123 (30) = happyShift action_17
action_123 (31) = happyShift action_18
action_123 (32) = happyShift action_2
action_123 (40) = happyShift action_19
action_123 (55) = happyShift action_20
action_123 (57) = happyShift action_21
action_123 (59) = happyShift action_22
action_123 (61) = happyShift action_23
action_123 (4) = happyGoto action_3
action_123 (7) = happyGoto action_4
action_123 (8) = happyGoto action_5
action_123 (9) = happyGoto action_6
action_123 (10) = happyGoto action_7
action_123 (12) = happyGoto action_8
action_123 (16) = happyGoto action_9
action_123 (17) = happyGoto action_10
action_123 (20) = happyGoto action_11
action_123 (21) = happyGoto action_12
action_123 (22) = happyGoto action_13
action_123 (27) = happyGoto action_126
action_123 _ = happyFail (happyExpListPerState 123)

action_124 _ = happyReduce_39

action_125 _ = happyReduce_38

action_126 (57) = happyShift action_127
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (39) = happyShift action_128
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (58) = happyShift action_129
action_128 _ = happyFail (happyExpListPerState 128)

action_129 _ = happyReduce_25

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyTerminal (StringEnd happy_var_3))
	(HappyAbsSyn5  happy_var_2)
	(HappyTerminal (StringBegin happy_var_1))
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

happyReduce_3 = happySpecReduce_3  5 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (V.singleton happy_var_2
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyTerminal (TokStringLit happy_var_1))
	 =  HappyAbsSyn5
		 (V.singleton (String (locate happy_var_1) (unlocate happy_var_1))
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn4
		 (uncurry Path happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 (HappyTerminal (TokNumLit _ happy_var_1))
	 =  HappyAbsSyn4
		 (Number (locate happy_var_1) (unlocate happy_var_1)
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyTerminal (TokIntLit _ happy_var_1))
	 =  HappyAbsSyn4
		 (Number (locate happy_var_1) (S.scientific (fromIntegral (unlocate happy_var_1)) 0)
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyTerminal (TokBoolLit happy_var_1))
	 =  HappyAbsSyn4
		 (Boolean (locate happy_var_1) (unlocate happy_var_1)
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  8 happyReduction_9
happyReduction_9 (HappyTerminal (TokBoolLit happy_var_1))
	 =  HappyAbsSyn4
		 (Boolean (locate happy_var_1) (unlocate happy_var_1)
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 (HappyTerminal (TokIdentifier (Loc happy_var_1 "null" )))
	 =  HappyAbsSyn4
		 (Null (locate happy_var_1)
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  9 happyReduction_11
happyReduction_11 (HappyTerminal (TokSymbol SymCurlyClose happy_var_2))
	(HappyTerminal (TokSymbol SymCurlyOpen happy_var_1))
	 =  HappyAbsSyn4
		 (Null (locate happy_var_1 <> locate happy_var_2)
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 (HappyTerminal (TokSymbol SymSquareClose happy_var_3))
	(HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokSymbol SymSquareOpen happy_var_1))
	 =  HappyAbsSyn4
		 (Array (locate happy_var_1 <> locate happy_var_3) happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  10 happyReduction_13
happyReduction_13 (HappyTerminal (TokSymbol SymSquareClose happy_var_2))
	(HappyTerminal (TokSymbol SymSquareOpen happy_var_1))
	 =  HappyAbsSyn4
		 (Array (locate happy_var_1 <> locate happy_var_2) V.empty
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  11 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 (V.singleton happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  11 happyReduction_15
happyReduction_15 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (V.snoc happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  12 happyReduction_16
happyReduction_16 (HappyTerminal (TokSymbol SymCurlyClose happy_var_3))
	(HappyAbsSyn13  happy_var_2)
	(HappyTerminal (TokSymbol SymCurlyOpen happy_var_1))
	 =  HappyAbsSyn4
		 (Object (locate happy_var_1 <> locate happy_var_3) (M.fromList happy_var_2)
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  13 happyReduction_17
happyReduction_17 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  13 happyReduction_18
happyReduction_18 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_3 : happy_var_1
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  14 happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn14
		 ((unlocate happy_var_1, happy_var_3)
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  15 happyReduction_20
happyReduction_20 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Gt (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  15 happyReduction_21
happyReduction_21 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Lt (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  15 happyReduction_22
happyReduction_22 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Eq (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  15 happyReduction_23
happyReduction_23 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  15 happyReduction_24
happyReduction_24 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happyReduce 12 16 happyReduction_25
happyReduction_25 ((HappyTerminal (TokSymbol SymDoubleCurlyClose happy_var_12)) `HappyStk`
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

happyReduce_26 = happyReduce 4 17 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_1  18 happyReduction_27
happyReduction_27 (HappyTerminal (TokIdentifier (Loc happy_var_1 "escapeUri")))
	 =  HappyAbsSyn18
		 (EscapeURI (locate happy_var_1)
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  19 happyReduction_28
happyReduction_28 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  19 happyReduction_29
happyReduction_29 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  19 happyReduction_30
happyReduction_30 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  19 happyReduction_31
happyReduction_31 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  19 happyReduction_32
happyReduction_32 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn4
		 (uncurry Path happy_var_1
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

happyReduce_35 = happySpecReduce_2  19 happyReduction_35
happyReduction_35 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 happy_var_2
	)
happyReduction_35 _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  19 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happyReduce 5 20 happyReduction_37
happyReduction_37 ((HappyTerminal (TokSymbol SymDoubleCurlyClose happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn21  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_1 (locate happy_var_5) happy_var_2
	) `HappyStk` happyRest

happyReduce_38 = happyReduce 8 21 happyReduction_38
happyReduction_38 (_ `HappyStk`
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

happyReduce_39 = happyReduce 8 21 happyReduction_39
happyReduction_39 (_ `HappyStk`
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

happyReduce_40 = happySpecReduce_3  22 happyReduction_40
happyReduction_40 (HappyTerminal (TokSymbol SymDoubleCurlyClose happy_var_3))
	(HappyAbsSyn23  happy_var_2)
	(HappyTerminal (TokSymbol SymDoubleCurlyOpen happy_var_1))
	 =  HappyAbsSyn4
		 (Path (locate happy_var_1 <> locate happy_var_3) (snd happy_var_2)
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  23 happyReduction_41
happyReduction_41 (HappyAbsSyn23  happy_var_2)
	(HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn23
		 ((locate happy_var_1 <> fst happy_var_2, V.cons (Obj (locate happy_var_1) (unlocate happy_var_1)) (snd happy_var_2))
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  23 happyReduction_42
happyReduction_42 (HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn23
		 ((locate happy_var_1, V.singleton (Obj (locate happy_var_1) (unlocate happy_var_1)))
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  24 happyReduction_43
happyReduction_43 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn23
		 ((locate happy_var_1, V.singleton happy_var_1)
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  24 happyReduction_44
happyReduction_44 (HappyAbsSyn25  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 ((fst happy_var_1 <> locate happy_var_2, V.snoc (snd happy_var_1) happy_var_2)
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  25 happyReduction_45
happyReduction_45 (HappyTerminal (TokIdentifier happy_var_2))
	(HappyTerminal (TokSymbol SymDot happy_var_1))
	 =  HappyAbsSyn25
		 (Obj (locate happy_var_1 <> locate happy_var_2) (unlocate happy_var_2)
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happyReduce 5 25 happyReduction_46
happyReduction_46 ((HappyTerminal (TokSymbol SymSquareClose happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokSymbol SymSquareOpen happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 (Obj (locate happy_var_1 <> locate happy_var_5) (unlocate happy_var_3)
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_3  25 happyReduction_47
happyReduction_47 (HappyTerminal (TokSymbol SymSquareClose happy_var_3))
	(HappyTerminal (TokIntLit _ happy_var_2))
	(HappyTerminal (TokSymbol SymSquareOpen happy_var_1))
	 =  HappyAbsSyn25
		 (Arr (locate happy_var_1 <> locate happy_var_3) (unlocate happy_var_2)
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  26 happyReduction_48
happyReduction_48 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn4
		 (uncurry Path happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  26 happyReduction_49
happyReduction_49 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  26 happyReduction_50
happyReduction_50 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  26 happyReduction_51
happyReduction_51 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
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

happyReduce_54 = happySpecReduce_3  26 happyReduction_54
happyReduction_54 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  27 happyReduction_55
happyReduction_55 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  27 happyReduction_56
happyReduction_56 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  27 happyReduction_57
happyReduction_57 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

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

happyReduce_65 = happySpecReduce_3  27 happyReduction_65
happyReduction_65 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 63 63 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokNumLit _ happy_dollar_dollar -> cont 28;
	TokIntLit _ happy_dollar_dollar -> cont 29;
	TokBoolLit happy_dollar_dollar -> cont 30;
	TokBoolLit happy_dollar_dollar -> cont 31;
	StringBegin happy_dollar_dollar -> cont 32;
	StringEnd happy_dollar_dollar -> cont 33;
	TokStringLit happy_dollar_dollar -> cont 34;
	TemplateBegin happy_dollar_dollar -> cont 35;
	TemplateEnd happy_dollar_dollar -> cont 36;
	TokIdentifier (Loc happy_dollar_dollar "if") -> cont 37;
	TokIdentifier (Loc happy_dollar_dollar "else") -> cont 38;
	TokIdentifier (Loc happy_dollar_dollar "end") -> cont 39;
	TokIdentifier (Loc happy_dollar_dollar "null" ) -> cont 40;
	TokIdentifier (Loc happy_dollar_dollar "range") -> cont 41;
	TokIdentifier (Loc happy_dollar_dollar "escapeUri") -> cont 42;
	TokIdentifier happy_dollar_dollar -> cont 43;
	TokSymbol SymSingleQuote happy_dollar_dollar -> cont 44;
	TokSymbol SymColon happy_dollar_dollar -> cont 45;
	TokSymbol SymDot happy_dollar_dollar -> cont 46;
	TokSymbol SymComma happy_dollar_dollar -> cont 47;
	TokSymbol SymEq happy_dollar_dollar -> cont 48;
	TokSymbol SymGt happy_dollar_dollar -> cont 49;
	TokSymbol SymLt happy_dollar_dollar -> cont 50;
	TokSymbol SymAnd happy_dollar_dollar -> cont 51;
	TokSymbol SymOr happy_dollar_dollar -> cont 52;
	TokSymbol SymUnderscore happy_dollar_dollar -> cont 53;
	TokSymbol SymAssignment happy_dollar_dollar -> cont 54;
	TokSymbol SymCurlyOpen happy_dollar_dollar -> cont 55;
	TokSymbol SymCurlyClose happy_dollar_dollar -> cont 56;
	TokSymbol SymDoubleCurlyOpen happy_dollar_dollar -> cont 57;
	TokSymbol SymDoubleCurlyClose happy_dollar_dollar -> cont 58;
	TokSymbol SymSquareOpen happy_dollar_dollar -> cont 59;
	TokSymbol SymSquareClose happy_dollar_dollar -> cont 60;
	TokSymbol SymParenOpen happy_dollar_dollar -> cont 61;
	TokSymbol SymParenClose happy_dollar_dollar -> cont 62;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 63 tk tks = happyError' (tks, explist)
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
