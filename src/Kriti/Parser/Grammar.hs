{-# OPTIONS_GHC -w #-}
-- We have to disable -XStrictData here, as it doesn't play nicely with Happy.
{-# LANGUAGE NoStrictData #-}
module Kriti.Parser.Grammar where

import Control.Monad.State (gets)
import qualified Data.Aeson as J
import Data.Bifunctor (first)
import qualified Data.HashMap.Strict as M
import qualified Data.List as List
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import qualified Kriti.Aeson.Compat as Compat
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
	| HappyAbsSyn15 (Loc T.Text)
	| HappyAbsSyn19 (ValueExt -> ValueExt)
	| HappyAbsSyn22 (Span -> ValueExt -> ValueExt)
	| HappyAbsSyn24 ((Span, V.Vector Accessor))
	| HappyAbsSyn26 (Accessor)

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
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175 :: () => Prelude.Int -> ({-HappyReduction (Parser) = -}
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
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90 :: () => ({-HappyReduction (Parser) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Parser) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Parser) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,826) ([0,61440,65,20480,5,0,32,0,0,0,4096,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7936,4,21760,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,3,8192,0,0,0,0,0,0,512,0,64,0,31744,754,21504,1,32768,527,32768,58,0,16880,0,1360,0,0,0,0,1,0,0,2,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,33760,22,2720,0,0,0,0,0,0,0,0,4,0,0,65028,3,0,15872,360,43520,0,0,8192,4096,0,0,0,0,0,0,0,0,0,0,0,32768,2,2,0,8704,0,0,0,3968,90,10880,0,0,32768,8192,0,0,0,0,0,0,6144,0,0,0,0,0,0,0,0,96,0,4,0,0,0,0,0,0,0,0,0,32768,22535,0,0,0,0,0,64,0,0,4,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,57344,5763,40960,10,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24606,1,0,0,6144,0,0,0,0,4096,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,16880,11,1360,0,0,0,0,0,0,4096,4088,32,0,0,160,128,0,0,0,0,0,0,4096,0,0,0,0,16,64,0,256,128,0,0,0,32768,0,0,0,0,16,0,0,0,63504,527,0,63488,1440,43008,2,0,46111,0,85,0,33760,22,2720,0,31744,720,21504,1,32768,23055,32768,42,0,16880,11,1360,0,15872,360,43520,0,49152,11527,16384,21,0,41208,5,680,0,7936,180,21760,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,33760,22,2720,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,15872,360,43520,0,0,0,0,0,0,8440,0,680,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,12320,0,0,0,1024,6,0,0,32768,192,0,0,0,6160,0,0,0,512,3,0,0,16384,96,0,0,0,0,0,0,0,65282,1,0,7936,4,21760,0,0,4096,0,0,0,0,2,0,0,0,0,4096,0,0,4,0,0,0,0,1,0,0,128,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16880,0,1360,0,0,1024,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,0,0,2110,0,170,0,0,0,0,0,0,0,0,1,0,64,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,8,0,0,0,256,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,4096,0,0,0,0,2,0,0,0,0,4096,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,256,0,0,0,8192,0,0,0,0,4,0,16880,0,1360,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,64,0,0,0,0,0,32,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","string_lit","string_template","template","num_lit","boolean","null","array","list_elements","object","object_fields","object_field","object_key","operator","iff","function_call","functions","function_params","range","range_decl","path","path_vector","path_tail","path_element","value","term","number","int","'true'","'false'","'s\"'","'\"e'","string","'if'","'else'","'end'","'null'","'range'","'escapeUri'","'not'","'in'","ident","'\\''","':'","'.'","','","'?'","'??'","'=='","'!='","'>'","'<'","'<='","'>='","'&&'","'||'","'_'","':='","'{'","'}'","'{{'","'}}'","'['","']'","'('","')'","%eof"]
        bit_start = st Prelude.* 69
        bit_end = (st Prelude.+ 1) Prelude.* 69
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..68]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (29) = happyShift action_15
action_0 (30) = happyShift action_16
action_0 (31) = happyShift action_17
action_0 (32) = happyShift action_18
action_0 (33) = happyShift action_19
action_0 (39) = happyShift action_20
action_0 (61) = happyShift action_21
action_0 (63) = happyShift action_22
action_0 (65) = happyShift action_23
action_0 (67) = happyShift action_24
action_0 (4) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 (12) = happyGoto action_8
action_0 (17) = happyGoto action_9
action_0 (18) = happyGoto action_10
action_0 (21) = happyGoto action_11
action_0 (22) = happyGoto action_12
action_0 (23) = happyGoto action_13
action_0 (28) = happyGoto action_14
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (33) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (35) = happyShift action_54
action_2 (63) = happyShift action_55
action_2 (5) = happyGoto action_52
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_80

action_4 _ = happyReduce_79

action_5 _ = happyReduce_81

action_6 _ = happyReduce_82

action_7 _ = happyReduce_83

action_8 _ = happyReduce_84

action_9 _ = happyReduce_86

action_10 _ = happyReduce_87

action_11 _ = happyReduce_88

action_12 (29) = happyShift action_15
action_12 (30) = happyShift action_16
action_12 (31) = happyShift action_17
action_12 (32) = happyShift action_18
action_12 (33) = happyShift action_19
action_12 (39) = happyShift action_20
action_12 (61) = happyShift action_21
action_12 (63) = happyShift action_22
action_12 (65) = happyShift action_23
action_12 (67) = happyShift action_24
action_12 (4) = happyGoto action_3
action_12 (7) = happyGoto action_4
action_12 (8) = happyGoto action_5
action_12 (9) = happyGoto action_6
action_12 (10) = happyGoto action_7
action_12 (12) = happyGoto action_8
action_12 (17) = happyGoto action_9
action_12 (18) = happyGoto action_10
action_12 (21) = happyGoto action_11
action_12 (22) = happyGoto action_12
action_12 (23) = happyGoto action_13
action_12 (28) = happyGoto action_56
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_85

action_14 (69) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_11

action_16 _ = happyReduce_12

action_17 _ = happyReduce_13

action_18 _ = happyReduce_14

action_19 (34) = happyShift action_53
action_19 (35) = happyShift action_54
action_19 (63) = happyShift action_55
action_19 (5) = happyGoto action_52
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_15

action_21 (33) = happyShift action_50
action_21 (62) = happyShift action_51
action_21 (13) = happyGoto action_48
action_21 (14) = happyGoto action_49
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (29) = happyShift action_15
action_22 (30) = happyShift action_16
action_22 (31) = happyShift action_17
action_22 (32) = happyShift action_18
action_22 (33) = happyShift action_19
action_22 (36) = happyShift action_41
action_22 (39) = happyShift action_20
action_22 (40) = happyShift action_42
action_22 (41) = happyShift action_43
action_22 (42) = happyShift action_44
action_22 (44) = happyShift action_45
action_22 (61) = happyShift action_21
action_22 (63) = happyShift action_46
action_22 (65) = happyShift action_23
action_22 (67) = happyShift action_47
action_22 (4) = happyGoto action_29
action_22 (7) = happyGoto action_30
action_22 (8) = happyGoto action_31
action_22 (9) = happyGoto action_32
action_22 (10) = happyGoto action_33
action_22 (12) = happyGoto action_34
action_22 (16) = happyGoto action_35
action_22 (17) = happyGoto action_36
action_22 (19) = happyGoto action_37
action_22 (21) = happyGoto action_38
action_22 (22) = happyGoto action_12
action_22 (24) = happyGoto action_39
action_22 (27) = happyGoto action_40
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (29) = happyShift action_15
action_23 (30) = happyShift action_16
action_23 (31) = happyShift action_17
action_23 (32) = happyShift action_18
action_23 (33) = happyShift action_19
action_23 (39) = happyShift action_20
action_23 (61) = happyShift action_21
action_23 (63) = happyShift action_22
action_23 (65) = happyShift action_23
action_23 (66) = happyShift action_28
action_23 (67) = happyShift action_24
action_23 (4) = happyGoto action_3
action_23 (7) = happyGoto action_4
action_23 (8) = happyGoto action_5
action_23 (9) = happyGoto action_6
action_23 (10) = happyGoto action_7
action_23 (11) = happyGoto action_26
action_23 (12) = happyGoto action_8
action_23 (17) = happyGoto action_9
action_23 (18) = happyGoto action_10
action_23 (21) = happyGoto action_11
action_23 (22) = happyGoto action_12
action_23 (23) = happyGoto action_13
action_23 (28) = happyGoto action_27
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (29) = happyShift action_15
action_24 (30) = happyShift action_16
action_24 (31) = happyShift action_17
action_24 (32) = happyShift action_18
action_24 (33) = happyShift action_19
action_24 (39) = happyShift action_20
action_24 (61) = happyShift action_21
action_24 (63) = happyShift action_22
action_24 (65) = happyShift action_23
action_24 (67) = happyShift action_24
action_24 (4) = happyGoto action_3
action_24 (7) = happyGoto action_4
action_24 (8) = happyGoto action_5
action_24 (9) = happyGoto action_6
action_24 (10) = happyGoto action_7
action_24 (12) = happyGoto action_8
action_24 (17) = happyGoto action_9
action_24 (18) = happyGoto action_10
action_24 (21) = happyGoto action_11
action_24 (22) = happyGoto action_12
action_24 (23) = happyGoto action_13
action_24 (28) = happyGoto action_25
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (68) = happyShift action_109
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (48) = happyShift action_107
action_26 (66) = happyShift action_108
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_18

action_28 _ = happyReduce_17

action_29 _ = happyReduce_69

action_30 _ = happyReduce_68

action_31 _ = happyReduce_70

action_32 _ = happyReduce_67

action_33 _ = happyReduce_71

action_34 _ = happyReduce_72

action_35 (64) = happyShift action_106
action_35 _ = happyReduce_75

action_36 _ = happyReduce_74

action_37 (29) = happyShift action_15
action_37 (30) = happyShift action_16
action_37 (31) = happyShift action_17
action_37 (32) = happyShift action_18
action_37 (33) = happyShift action_19
action_37 (39) = happyShift action_20
action_37 (41) = happyShift action_43
action_37 (42) = happyShift action_44
action_37 (44) = happyShift action_45
action_37 (61) = happyShift action_21
action_37 (63) = happyShift action_46
action_37 (65) = happyShift action_23
action_37 (67) = happyShift action_105
action_37 (4) = happyGoto action_94
action_37 (7) = happyGoto action_95
action_37 (8) = happyGoto action_96
action_37 (9) = happyGoto action_97
action_37 (10) = happyGoto action_98
action_37 (12) = happyGoto action_99
action_37 (17) = happyGoto action_100
action_37 (19) = happyGoto action_101
action_37 (20) = happyGoto action_102
action_37 (21) = happyGoto action_103
action_37 (22) = happyGoto action_12
action_37 (24) = happyGoto action_104
action_37 _ = happyFail (happyExpListPerState 37)

action_38 _ = happyReduce_76

action_39 (64) = happyShift action_93
action_39 _ = happyReduce_73

action_40 (43) = happyShift action_83
action_40 (50) = happyShift action_84
action_40 (51) = happyShift action_85
action_40 (52) = happyShift action_86
action_40 (53) = happyShift action_87
action_40 (54) = happyShift action_88
action_40 (55) = happyShift action_89
action_40 (56) = happyShift action_90
action_40 (57) = happyShift action_91
action_40 (58) = happyShift action_92
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (29) = happyShift action_15
action_41 (30) = happyShift action_16
action_41 (31) = happyShift action_17
action_41 (32) = happyShift action_18
action_41 (33) = happyShift action_19
action_41 (39) = happyShift action_20
action_41 (41) = happyShift action_43
action_41 (42) = happyShift action_44
action_41 (44) = happyShift action_45
action_41 (61) = happyShift action_21
action_41 (63) = happyShift action_46
action_41 (65) = happyShift action_23
action_41 (67) = happyShift action_47
action_41 (4) = happyGoto action_29
action_41 (7) = happyGoto action_30
action_41 (8) = happyGoto action_31
action_41 (9) = happyGoto action_32
action_41 (10) = happyGoto action_33
action_41 (12) = happyGoto action_34
action_41 (16) = happyGoto action_71
action_41 (17) = happyGoto action_36
action_41 (19) = happyGoto action_72
action_41 (21) = happyGoto action_38
action_41 (22) = happyGoto action_12
action_41 (24) = happyGoto action_73
action_41 (27) = happyGoto action_82
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (44) = happyShift action_80
action_42 (59) = happyShift action_81
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_40

action_44 _ = happyReduce_41

action_45 (47) = happyShift action_77
action_45 (49) = happyShift action_78
action_45 (65) = happyShift action_79
action_45 (25) = happyGoto action_75
action_45 (26) = happyGoto action_76
action_45 _ = happyReduce_58

action_46 (36) = happyShift action_41
action_46 (40) = happyShift action_42
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (29) = happyShift action_15
action_47 (30) = happyShift action_16
action_47 (31) = happyShift action_17
action_47 (32) = happyShift action_18
action_47 (33) = happyShift action_19
action_47 (39) = happyShift action_20
action_47 (41) = happyShift action_43
action_47 (42) = happyShift action_44
action_47 (44) = happyShift action_45
action_47 (61) = happyShift action_21
action_47 (63) = happyShift action_46
action_47 (65) = happyShift action_23
action_47 (67) = happyShift action_47
action_47 (4) = happyGoto action_29
action_47 (7) = happyGoto action_30
action_47 (8) = happyGoto action_31
action_47 (9) = happyGoto action_32
action_47 (10) = happyGoto action_33
action_47 (12) = happyGoto action_34
action_47 (16) = happyGoto action_71
action_47 (17) = happyGoto action_36
action_47 (19) = happyGoto action_72
action_47 (21) = happyGoto action_38
action_47 (22) = happyGoto action_12
action_47 (24) = happyGoto action_73
action_47 (27) = happyGoto action_74
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (48) = happyShift action_69
action_48 (62) = happyShift action_70
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_22

action_50 (34) = happyShift action_67
action_50 (35) = happyShift action_68
action_50 (15) = happyGoto action_66
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_21

action_52 (34) = happyShift action_63
action_52 (35) = happyShift action_64
action_52 (63) = happyShift action_65
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_2

action_54 _ = happyReduce_6

action_55 (29) = happyShift action_15
action_55 (30) = happyShift action_16
action_55 (31) = happyShift action_17
action_55 (32) = happyShift action_18
action_55 (41) = happyShift action_43
action_55 (42) = happyShift action_44
action_55 (44) = happyShift action_45
action_55 (6) = happyGoto action_58
action_55 (7) = happyGoto action_59
action_55 (8) = happyGoto action_60
action_55 (19) = happyGoto action_61
action_55 (24) = happyGoto action_62
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (63) = happyShift action_57
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (38) = happyShift action_142
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (64) = happyShift action_141
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_10

action_60 _ = happyReduce_9

action_61 (29) = happyShift action_15
action_61 (30) = happyShift action_16
action_61 (31) = happyShift action_17
action_61 (32) = happyShift action_18
action_61 (33) = happyShift action_19
action_61 (39) = happyShift action_20
action_61 (41) = happyShift action_43
action_61 (42) = happyShift action_44
action_61 (44) = happyShift action_45
action_61 (61) = happyShift action_21
action_61 (63) = happyShift action_46
action_61 (65) = happyShift action_23
action_61 (67) = happyShift action_105
action_61 (4) = happyGoto action_94
action_61 (7) = happyGoto action_95
action_61 (8) = happyGoto action_96
action_61 (9) = happyGoto action_97
action_61 (10) = happyGoto action_98
action_61 (12) = happyGoto action_99
action_61 (17) = happyGoto action_100
action_61 (19) = happyGoto action_101
action_61 (20) = happyGoto action_140
action_61 (21) = happyGoto action_103
action_61 (22) = happyGoto action_12
action_61 (24) = happyGoto action_104
action_61 _ = happyFail (happyExpListPerState 61)

action_62 _ = happyReduce_7

action_63 _ = happyReduce_1

action_64 _ = happyReduce_4

action_65 (29) = happyShift action_15
action_65 (30) = happyShift action_16
action_65 (31) = happyShift action_17
action_65 (32) = happyShift action_18
action_65 (41) = happyShift action_43
action_65 (42) = happyShift action_44
action_65 (44) = happyShift action_45
action_65 (6) = happyGoto action_139
action_65 (7) = happyGoto action_59
action_65 (8) = happyGoto action_60
action_65 (19) = happyGoto action_61
action_65 (24) = happyGoto action_62
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (34) = happyShift action_137
action_66 (35) = happyShift action_138
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (46) = happyShift action_136
action_67 _ = happyFail (happyExpListPerState 67)

action_68 _ = happyReduce_27

action_69 (33) = happyShift action_50
action_69 (14) = happyGoto action_135
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_20

action_71 _ = happyReduce_75

action_72 (29) = happyShift action_15
action_72 (30) = happyShift action_16
action_72 (31) = happyShift action_17
action_72 (32) = happyShift action_18
action_72 (33) = happyShift action_19
action_72 (39) = happyShift action_20
action_72 (41) = happyShift action_43
action_72 (42) = happyShift action_44
action_72 (44) = happyShift action_45
action_72 (61) = happyShift action_21
action_72 (63) = happyShift action_46
action_72 (65) = happyShift action_23
action_72 (67) = happyShift action_105
action_72 (4) = happyGoto action_94
action_72 (7) = happyGoto action_95
action_72 (8) = happyGoto action_96
action_72 (9) = happyGoto action_97
action_72 (10) = happyGoto action_98
action_72 (12) = happyGoto action_99
action_72 (17) = happyGoto action_100
action_72 (19) = happyGoto action_101
action_72 (20) = happyGoto action_134
action_72 (21) = happyGoto action_103
action_72 (22) = happyGoto action_12
action_72 (24) = happyGoto action_104
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_73

action_74 (43) = happyShift action_83
action_74 (50) = happyShift action_84
action_74 (51) = happyShift action_85
action_74 (52) = happyShift action_86
action_74 (53) = happyShift action_87
action_74 (54) = happyShift action_88
action_74 (55) = happyShift action_89
action_74 (56) = happyShift action_90
action_74 (57) = happyShift action_91
action_74 (58) = happyShift action_92
action_74 (68) = happyShift action_133
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (47) = happyShift action_77
action_75 (49) = happyShift action_78
action_75 (65) = happyShift action_79
action_75 (26) = happyGoto action_132
action_75 _ = happyReduce_57

action_76 _ = happyReduce_59

action_77 (44) = happyShift action_131
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (47) = happyShift action_129
action_78 (65) = happyShift action_130
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (30) = happyShift action_127
action_79 (45) = happyShift action_128
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (48) = happyShift action_126
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (48) = happyShift action_125
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (43) = happyShift action_83
action_82 (50) = happyShift action_84
action_82 (51) = happyShift action_85
action_82 (52) = happyShift action_86
action_82 (53) = happyShift action_87
action_82 (54) = happyShift action_88
action_82 (55) = happyShift action_89
action_82 (56) = happyShift action_90
action_82 (57) = happyShift action_91
action_82 (58) = happyShift action_92
action_82 (64) = happyShift action_124
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (29) = happyShift action_15
action_83 (30) = happyShift action_16
action_83 (31) = happyShift action_17
action_83 (32) = happyShift action_18
action_83 (33) = happyShift action_19
action_83 (39) = happyShift action_20
action_83 (41) = happyShift action_43
action_83 (42) = happyShift action_44
action_83 (44) = happyShift action_45
action_83 (61) = happyShift action_21
action_83 (63) = happyShift action_46
action_83 (65) = happyShift action_23
action_83 (67) = happyShift action_47
action_83 (4) = happyGoto action_29
action_83 (7) = happyGoto action_30
action_83 (8) = happyGoto action_31
action_83 (9) = happyGoto action_32
action_83 (10) = happyGoto action_33
action_83 (12) = happyGoto action_34
action_83 (16) = happyGoto action_71
action_83 (17) = happyGoto action_36
action_83 (19) = happyGoto action_72
action_83 (21) = happyGoto action_38
action_83 (22) = happyGoto action_12
action_83 (24) = happyGoto action_73
action_83 (27) = happyGoto action_123
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (29) = happyShift action_15
action_84 (30) = happyShift action_16
action_84 (31) = happyShift action_17
action_84 (32) = happyShift action_18
action_84 (33) = happyShift action_19
action_84 (39) = happyShift action_20
action_84 (41) = happyShift action_43
action_84 (42) = happyShift action_44
action_84 (44) = happyShift action_45
action_84 (61) = happyShift action_21
action_84 (63) = happyShift action_46
action_84 (65) = happyShift action_23
action_84 (67) = happyShift action_47
action_84 (4) = happyGoto action_29
action_84 (7) = happyGoto action_30
action_84 (8) = happyGoto action_31
action_84 (9) = happyGoto action_32
action_84 (10) = happyGoto action_33
action_84 (12) = happyGoto action_34
action_84 (16) = happyGoto action_71
action_84 (17) = happyGoto action_36
action_84 (19) = happyGoto action_72
action_84 (21) = happyGoto action_38
action_84 (22) = happyGoto action_12
action_84 (24) = happyGoto action_73
action_84 (27) = happyGoto action_122
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (29) = happyShift action_15
action_85 (30) = happyShift action_16
action_85 (31) = happyShift action_17
action_85 (32) = happyShift action_18
action_85 (33) = happyShift action_19
action_85 (39) = happyShift action_20
action_85 (41) = happyShift action_43
action_85 (42) = happyShift action_44
action_85 (44) = happyShift action_45
action_85 (61) = happyShift action_21
action_85 (63) = happyShift action_46
action_85 (65) = happyShift action_23
action_85 (67) = happyShift action_47
action_85 (4) = happyGoto action_29
action_85 (7) = happyGoto action_30
action_85 (8) = happyGoto action_31
action_85 (9) = happyGoto action_32
action_85 (10) = happyGoto action_33
action_85 (12) = happyGoto action_34
action_85 (16) = happyGoto action_71
action_85 (17) = happyGoto action_36
action_85 (19) = happyGoto action_72
action_85 (21) = happyGoto action_38
action_85 (22) = happyGoto action_12
action_85 (24) = happyGoto action_73
action_85 (27) = happyGoto action_121
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (29) = happyShift action_15
action_86 (30) = happyShift action_16
action_86 (31) = happyShift action_17
action_86 (32) = happyShift action_18
action_86 (33) = happyShift action_19
action_86 (39) = happyShift action_20
action_86 (41) = happyShift action_43
action_86 (42) = happyShift action_44
action_86 (44) = happyShift action_45
action_86 (61) = happyShift action_21
action_86 (63) = happyShift action_46
action_86 (65) = happyShift action_23
action_86 (67) = happyShift action_47
action_86 (4) = happyGoto action_29
action_86 (7) = happyGoto action_30
action_86 (8) = happyGoto action_31
action_86 (9) = happyGoto action_32
action_86 (10) = happyGoto action_33
action_86 (12) = happyGoto action_34
action_86 (16) = happyGoto action_71
action_86 (17) = happyGoto action_36
action_86 (19) = happyGoto action_72
action_86 (21) = happyGoto action_38
action_86 (22) = happyGoto action_12
action_86 (24) = happyGoto action_73
action_86 (27) = happyGoto action_120
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (29) = happyShift action_15
action_87 (30) = happyShift action_16
action_87 (31) = happyShift action_17
action_87 (32) = happyShift action_18
action_87 (33) = happyShift action_19
action_87 (39) = happyShift action_20
action_87 (41) = happyShift action_43
action_87 (42) = happyShift action_44
action_87 (44) = happyShift action_45
action_87 (61) = happyShift action_21
action_87 (63) = happyShift action_46
action_87 (65) = happyShift action_23
action_87 (67) = happyShift action_47
action_87 (4) = happyGoto action_29
action_87 (7) = happyGoto action_30
action_87 (8) = happyGoto action_31
action_87 (9) = happyGoto action_32
action_87 (10) = happyGoto action_33
action_87 (12) = happyGoto action_34
action_87 (16) = happyGoto action_71
action_87 (17) = happyGoto action_36
action_87 (19) = happyGoto action_72
action_87 (21) = happyGoto action_38
action_87 (22) = happyGoto action_12
action_87 (24) = happyGoto action_73
action_87 (27) = happyGoto action_119
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (29) = happyShift action_15
action_88 (30) = happyShift action_16
action_88 (31) = happyShift action_17
action_88 (32) = happyShift action_18
action_88 (33) = happyShift action_19
action_88 (39) = happyShift action_20
action_88 (41) = happyShift action_43
action_88 (42) = happyShift action_44
action_88 (44) = happyShift action_45
action_88 (61) = happyShift action_21
action_88 (63) = happyShift action_46
action_88 (65) = happyShift action_23
action_88 (67) = happyShift action_47
action_88 (4) = happyGoto action_29
action_88 (7) = happyGoto action_30
action_88 (8) = happyGoto action_31
action_88 (9) = happyGoto action_32
action_88 (10) = happyGoto action_33
action_88 (12) = happyGoto action_34
action_88 (16) = happyGoto action_71
action_88 (17) = happyGoto action_36
action_88 (19) = happyGoto action_72
action_88 (21) = happyGoto action_38
action_88 (22) = happyGoto action_12
action_88 (24) = happyGoto action_73
action_88 (27) = happyGoto action_118
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (29) = happyShift action_15
action_89 (30) = happyShift action_16
action_89 (31) = happyShift action_17
action_89 (32) = happyShift action_18
action_89 (33) = happyShift action_19
action_89 (39) = happyShift action_20
action_89 (41) = happyShift action_43
action_89 (42) = happyShift action_44
action_89 (44) = happyShift action_45
action_89 (61) = happyShift action_21
action_89 (63) = happyShift action_46
action_89 (65) = happyShift action_23
action_89 (67) = happyShift action_47
action_89 (4) = happyGoto action_29
action_89 (7) = happyGoto action_30
action_89 (8) = happyGoto action_31
action_89 (9) = happyGoto action_32
action_89 (10) = happyGoto action_33
action_89 (12) = happyGoto action_34
action_89 (16) = happyGoto action_71
action_89 (17) = happyGoto action_36
action_89 (19) = happyGoto action_72
action_89 (21) = happyGoto action_38
action_89 (22) = happyGoto action_12
action_89 (24) = happyGoto action_73
action_89 (27) = happyGoto action_117
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (29) = happyShift action_15
action_90 (30) = happyShift action_16
action_90 (31) = happyShift action_17
action_90 (32) = happyShift action_18
action_90 (33) = happyShift action_19
action_90 (39) = happyShift action_20
action_90 (41) = happyShift action_43
action_90 (42) = happyShift action_44
action_90 (44) = happyShift action_45
action_90 (61) = happyShift action_21
action_90 (63) = happyShift action_46
action_90 (65) = happyShift action_23
action_90 (67) = happyShift action_47
action_90 (4) = happyGoto action_29
action_90 (7) = happyGoto action_30
action_90 (8) = happyGoto action_31
action_90 (9) = happyGoto action_32
action_90 (10) = happyGoto action_33
action_90 (12) = happyGoto action_34
action_90 (16) = happyGoto action_71
action_90 (17) = happyGoto action_36
action_90 (19) = happyGoto action_72
action_90 (21) = happyGoto action_38
action_90 (22) = happyGoto action_12
action_90 (24) = happyGoto action_73
action_90 (27) = happyGoto action_116
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (29) = happyShift action_15
action_91 (30) = happyShift action_16
action_91 (31) = happyShift action_17
action_91 (32) = happyShift action_18
action_91 (33) = happyShift action_19
action_91 (39) = happyShift action_20
action_91 (41) = happyShift action_43
action_91 (42) = happyShift action_44
action_91 (44) = happyShift action_45
action_91 (61) = happyShift action_21
action_91 (63) = happyShift action_46
action_91 (65) = happyShift action_23
action_91 (67) = happyShift action_47
action_91 (4) = happyGoto action_29
action_91 (7) = happyGoto action_30
action_91 (8) = happyGoto action_31
action_91 (9) = happyGoto action_32
action_91 (10) = happyGoto action_33
action_91 (12) = happyGoto action_34
action_91 (16) = happyGoto action_71
action_91 (17) = happyGoto action_36
action_91 (19) = happyGoto action_72
action_91 (21) = happyGoto action_38
action_91 (22) = happyGoto action_12
action_91 (24) = happyGoto action_73
action_91 (27) = happyGoto action_115
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (29) = happyShift action_15
action_92 (30) = happyShift action_16
action_92 (31) = happyShift action_17
action_92 (32) = happyShift action_18
action_92 (33) = happyShift action_19
action_92 (39) = happyShift action_20
action_92 (41) = happyShift action_43
action_92 (42) = happyShift action_44
action_92 (44) = happyShift action_45
action_92 (61) = happyShift action_21
action_92 (63) = happyShift action_46
action_92 (65) = happyShift action_23
action_92 (67) = happyShift action_47
action_92 (4) = happyGoto action_29
action_92 (7) = happyGoto action_30
action_92 (8) = happyGoto action_31
action_92 (9) = happyGoto action_32
action_92 (10) = happyGoto action_33
action_92 (12) = happyGoto action_34
action_92 (16) = happyGoto action_71
action_92 (17) = happyGoto action_36
action_92 (19) = happyGoto action_72
action_92 (21) = happyGoto action_38
action_92 (22) = happyGoto action_12
action_92 (24) = happyGoto action_73
action_92 (27) = happyGoto action_114
action_92 _ = happyFail (happyExpListPerState 92)

action_93 _ = happyReduce_56

action_94 _ = happyReduce_44

action_95 _ = happyReduce_43

action_96 _ = happyReduce_45

action_97 _ = happyReduce_42

action_98 _ = happyReduce_46

action_99 _ = happyReduce_47

action_100 _ = happyReduce_49

action_101 (29) = happyShift action_15
action_101 (30) = happyShift action_16
action_101 (31) = happyShift action_17
action_101 (32) = happyShift action_18
action_101 (33) = happyShift action_19
action_101 (39) = happyShift action_20
action_101 (41) = happyShift action_43
action_101 (42) = happyShift action_44
action_101 (44) = happyShift action_45
action_101 (61) = happyShift action_21
action_101 (63) = happyShift action_46
action_101 (65) = happyShift action_23
action_101 (67) = happyShift action_105
action_101 (4) = happyGoto action_94
action_101 (7) = happyGoto action_95
action_101 (8) = happyGoto action_96
action_101 (9) = happyGoto action_97
action_101 (10) = happyGoto action_98
action_101 (12) = happyGoto action_99
action_101 (17) = happyGoto action_100
action_101 (19) = happyGoto action_101
action_101 (20) = happyGoto action_113
action_101 (21) = happyGoto action_103
action_101 (22) = happyGoto action_12
action_101 (24) = happyGoto action_104
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (64) = happyShift action_112
action_102 _ = happyReduce_77

action_103 _ = happyReduce_50

action_104 _ = happyReduce_48

action_105 (29) = happyShift action_15
action_105 (30) = happyShift action_16
action_105 (31) = happyShift action_17
action_105 (32) = happyShift action_18
action_105 (33) = happyShift action_19
action_105 (39) = happyShift action_20
action_105 (41) = happyShift action_43
action_105 (42) = happyShift action_44
action_105 (44) = happyShift action_45
action_105 (61) = happyShift action_21
action_105 (63) = happyShift action_46
action_105 (65) = happyShift action_23
action_105 (67) = happyShift action_105
action_105 (4) = happyGoto action_94
action_105 (7) = happyGoto action_95
action_105 (8) = happyGoto action_96
action_105 (9) = happyGoto action_97
action_105 (10) = happyGoto action_98
action_105 (12) = happyGoto action_99
action_105 (17) = happyGoto action_100
action_105 (19) = happyGoto action_101
action_105 (20) = happyGoto action_111
action_105 (21) = happyGoto action_103
action_105 (22) = happyGoto action_12
action_105 (24) = happyGoto action_104
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_89

action_107 (29) = happyShift action_15
action_107 (30) = happyShift action_16
action_107 (31) = happyShift action_17
action_107 (32) = happyShift action_18
action_107 (33) = happyShift action_19
action_107 (39) = happyShift action_20
action_107 (61) = happyShift action_21
action_107 (63) = happyShift action_22
action_107 (65) = happyShift action_23
action_107 (67) = happyShift action_24
action_107 (4) = happyGoto action_3
action_107 (7) = happyGoto action_4
action_107 (8) = happyGoto action_5
action_107 (9) = happyGoto action_6
action_107 (10) = happyGoto action_7
action_107 (12) = happyGoto action_8
action_107 (17) = happyGoto action_9
action_107 (18) = happyGoto action_10
action_107 (21) = happyGoto action_11
action_107 (22) = happyGoto action_12
action_107 (23) = happyGoto action_13
action_107 (28) = happyGoto action_110
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_16

action_109 _ = happyReduce_90

action_110 _ = happyReduce_19

action_111 (68) = happyShift action_155
action_111 _ = happyFail (happyExpListPerState 111)

action_112 _ = happyReduce_39

action_113 _ = happyReduce_51

action_114 _ = happyReduce_35

action_115 _ = happyReduce_34

action_116 (50) = happyShift action_84
action_116 (51) = happyFail []
action_116 (52) = happyFail []
action_116 (53) = happyFail []
action_116 (54) = happyFail []
action_116 (55) = happyFail []
action_116 (56) = happyFail []
action_116 (57) = happyShift action_91
action_116 (58) = happyShift action_92
action_116 _ = happyReduce_30

action_117 (50) = happyShift action_84
action_117 (51) = happyFail []
action_117 (52) = happyFail []
action_117 (53) = happyFail []
action_117 (54) = happyFail []
action_117 (55) = happyFail []
action_117 (56) = happyFail []
action_117 (57) = happyShift action_91
action_117 (58) = happyShift action_92
action_117 _ = happyReduce_31

action_118 (50) = happyShift action_84
action_118 (51) = happyFail []
action_118 (52) = happyFail []
action_118 (53) = happyFail []
action_118 (54) = happyFail []
action_118 (55) = happyFail []
action_118 (56) = happyFail []
action_118 (57) = happyShift action_91
action_118 (58) = happyShift action_92
action_118 _ = happyReduce_29

action_119 (50) = happyShift action_84
action_119 (51) = happyFail []
action_119 (52) = happyFail []
action_119 (53) = happyFail []
action_119 (54) = happyFail []
action_119 (55) = happyFail []
action_119 (56) = happyFail []
action_119 (57) = happyShift action_91
action_119 (58) = happyShift action_92
action_119 _ = happyReduce_28

action_120 (50) = happyShift action_84
action_120 (51) = happyFail []
action_120 (52) = happyFail []
action_120 (53) = happyFail []
action_120 (54) = happyFail []
action_120 (55) = happyFail []
action_120 (56) = happyFail []
action_120 (57) = happyShift action_91
action_120 (58) = happyShift action_92
action_120 _ = happyReduce_32

action_121 (50) = happyShift action_84
action_121 (51) = happyFail []
action_121 (52) = happyFail []
action_121 (53) = happyFail []
action_121 (54) = happyFail []
action_121 (55) = happyFail []
action_121 (56) = happyFail []
action_121 (57) = happyShift action_91
action_121 (58) = happyShift action_92
action_121 _ = happyReduce_33

action_122 _ = happyReduce_37

action_123 (43) = happyShift action_83
action_123 (50) = happyShift action_84
action_123 (51) = happyShift action_85
action_123 (52) = happyShift action_86
action_123 (53) = happyShift action_87
action_123 (54) = happyShift action_88
action_123 (55) = happyShift action_89
action_123 (56) = happyShift action_90
action_123 (57) = happyShift action_91
action_123 (58) = happyShift action_92
action_123 _ = happyReduce_36

action_124 (29) = happyShift action_15
action_124 (30) = happyShift action_16
action_124 (31) = happyShift action_17
action_124 (32) = happyShift action_18
action_124 (33) = happyShift action_19
action_124 (39) = happyShift action_20
action_124 (61) = happyShift action_21
action_124 (63) = happyShift action_22
action_124 (65) = happyShift action_23
action_124 (67) = happyShift action_24
action_124 (4) = happyGoto action_3
action_124 (7) = happyGoto action_4
action_124 (8) = happyGoto action_5
action_124 (9) = happyGoto action_6
action_124 (10) = happyGoto action_7
action_124 (12) = happyGoto action_8
action_124 (17) = happyGoto action_9
action_124 (18) = happyGoto action_10
action_124 (21) = happyGoto action_11
action_124 (22) = happyGoto action_12
action_124 (23) = happyGoto action_13
action_124 (28) = happyGoto action_154
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (44) = happyShift action_153
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (44) = happyShift action_152
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (66) = happyShift action_151
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (35) = happyShift action_150
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (44) = happyShift action_149
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (30) = happyShift action_147
action_130 (45) = happyShift action_148
action_130 _ = happyFail (happyExpListPerState 130)

action_131 _ = happyReduce_61

action_132 _ = happyReduce_60

action_133 _ = happyReduce_78

action_134 _ = happyReduce_77

action_135 _ = happyReduce_23

action_136 (29) = happyShift action_15
action_136 (30) = happyShift action_16
action_136 (31) = happyShift action_17
action_136 (32) = happyShift action_18
action_136 (33) = happyShift action_19
action_136 (39) = happyShift action_20
action_136 (61) = happyShift action_21
action_136 (63) = happyShift action_22
action_136 (65) = happyShift action_23
action_136 (67) = happyShift action_24
action_136 (4) = happyGoto action_3
action_136 (7) = happyGoto action_4
action_136 (8) = happyGoto action_5
action_136 (9) = happyGoto action_6
action_136 (10) = happyGoto action_7
action_136 (12) = happyGoto action_8
action_136 (17) = happyGoto action_9
action_136 (18) = happyGoto action_10
action_136 (21) = happyGoto action_11
action_136 (22) = happyGoto action_12
action_136 (23) = happyGoto action_13
action_136 (28) = happyGoto action_146
action_136 _ = happyFail (happyExpListPerState 136)

action_137 (46) = happyShift action_145
action_137 _ = happyFail (happyExpListPerState 137)

action_138 _ = happyReduce_26

action_139 (64) = happyShift action_144
action_139 _ = happyFail (happyExpListPerState 139)

action_140 _ = happyReduce_8

action_141 _ = happyReduce_5

action_142 (64) = happyShift action_143
action_142 _ = happyFail (happyExpListPerState 142)

action_143 _ = happyReduce_53

action_144 _ = happyReduce_3

action_145 (29) = happyShift action_15
action_145 (30) = happyShift action_16
action_145 (31) = happyShift action_17
action_145 (32) = happyShift action_18
action_145 (33) = happyShift action_19
action_145 (39) = happyShift action_20
action_145 (61) = happyShift action_21
action_145 (63) = happyShift action_22
action_145 (65) = happyShift action_23
action_145 (67) = happyShift action_24
action_145 (4) = happyGoto action_3
action_145 (7) = happyGoto action_4
action_145 (8) = happyGoto action_5
action_145 (9) = happyGoto action_6
action_145 (10) = happyGoto action_7
action_145 (12) = happyGoto action_8
action_145 (17) = happyGoto action_9
action_145 (18) = happyGoto action_10
action_145 (21) = happyGoto action_11
action_145 (22) = happyGoto action_12
action_145 (23) = happyGoto action_13
action_145 (28) = happyGoto action_162
action_145 _ = happyFail (happyExpListPerState 145)

action_146 _ = happyReduce_25

action_147 (66) = happyShift action_161
action_147 _ = happyFail (happyExpListPerState 147)

action_148 (35) = happyShift action_160
action_148 _ = happyFail (happyExpListPerState 148)

action_149 _ = happyReduce_62

action_150 (45) = happyShift action_159
action_150 _ = happyFail (happyExpListPerState 150)

action_151 _ = happyReduce_65

action_152 (60) = happyShift action_158
action_152 _ = happyFail (happyExpListPerState 152)

action_153 (60) = happyShift action_157
action_153 _ = happyFail (happyExpListPerState 153)

action_154 (63) = happyShift action_156
action_154 _ = happyFail (happyExpListPerState 154)

action_155 _ = happyReduce_52

action_156 (37) = happyShift action_167
action_156 _ = happyFail (happyExpListPerState 156)

action_157 (44) = happyShift action_45
action_157 (24) = happyGoto action_166
action_157 _ = happyFail (happyExpListPerState 157)

action_158 (44) = happyShift action_45
action_158 (24) = happyGoto action_165
action_158 _ = happyFail (happyExpListPerState 158)

action_159 (66) = happyShift action_164
action_159 _ = happyFail (happyExpListPerState 159)

action_160 (45) = happyShift action_163
action_160 _ = happyFail (happyExpListPerState 160)

action_161 _ = happyReduce_66

action_162 _ = happyReduce_24

action_163 (66) = happyShift action_171
action_163 _ = happyFail (happyExpListPerState 163)

action_164 _ = happyReduce_63

action_165 (64) = happyShift action_170
action_165 _ = happyFail (happyExpListPerState 165)

action_166 (64) = happyShift action_169
action_166 _ = happyFail (happyExpListPerState 166)

action_167 (64) = happyShift action_168
action_167 _ = happyFail (happyExpListPerState 167)

action_168 (29) = happyShift action_15
action_168 (30) = happyShift action_16
action_168 (31) = happyShift action_17
action_168 (32) = happyShift action_18
action_168 (33) = happyShift action_19
action_168 (39) = happyShift action_20
action_168 (61) = happyShift action_21
action_168 (63) = happyShift action_22
action_168 (65) = happyShift action_23
action_168 (67) = happyShift action_24
action_168 (4) = happyGoto action_3
action_168 (7) = happyGoto action_4
action_168 (8) = happyGoto action_5
action_168 (9) = happyGoto action_6
action_168 (10) = happyGoto action_7
action_168 (12) = happyGoto action_8
action_168 (17) = happyGoto action_9
action_168 (18) = happyGoto action_10
action_168 (21) = happyGoto action_11
action_168 (22) = happyGoto action_12
action_168 (23) = happyGoto action_13
action_168 (28) = happyGoto action_172
action_168 _ = happyFail (happyExpListPerState 168)

action_169 _ = happyReduce_55

action_170 _ = happyReduce_54

action_171 _ = happyReduce_64

action_172 (63) = happyShift action_173
action_172 _ = happyFail (happyExpListPerState 172)

action_173 (38) = happyShift action_174
action_173 _ = happyFail (happyExpListPerState 173)

action_174 (64) = happyShift action_175
action_174 _ = happyFail (happyExpListPerState 174)

action_175 _ = happyReduce_38

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokSymbol (Loc happy_var_3 SymStringEnd)))
	(HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymStringBegin)))
	 =  HappyAbsSyn4
		 (StringTem (locate happy_var_1 <> happy_var_3) happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyTerminal (TokSymbol (Loc happy_var_2 SymStringEnd)))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymStringBegin)))
	 =  HappyAbsSyn4
		 (StringTem (locate happy_var_1 <> locate happy_var_2) mempty
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 4 5 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (V.snoc happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 (HappyTerminal (TokStringLit happy_var_2))
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (V.snoc happy_var_1 (String (locate happy_var_2) (unLoc happy_var_2))
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (V.singleton happy_var_2
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyTerminal (TokStringLit happy_var_1))
	 =  HappyAbsSyn5
		 (V.singleton (String (locate happy_var_1) (unLoc happy_var_1))
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  6 happyReduction_7
happyReduction_7 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn4
		 (uncurry Path happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  6 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  6 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 (HappyTerminal (TokNumLit _ happy_var_1))
	 =  HappyAbsSyn4
		 (Number (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 (HappyTerminal (TokIntLit _ happy_var_1))
	 =  HappyAbsSyn4
		 (Number (locate happy_var_1) (S.scientific (fromIntegral (unLoc happy_var_1)) 0)
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  8 happyReduction_13
happyReduction_13 (HappyTerminal (TokBoolLit happy_var_1))
	 =  HappyAbsSyn4
		 (Boolean (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  8 happyReduction_14
happyReduction_14 (HappyTerminal (TokBoolLit happy_var_1))
	 =  HappyAbsSyn4
		 (Boolean (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  9 happyReduction_15
happyReduction_15 (HappyTerminal (TokIdentifier (Loc happy_var_1 "null" )))
	 =  HappyAbsSyn4
		 (Null (locate happy_var_1)
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  10 happyReduction_16
happyReduction_16 (HappyTerminal (TokSymbol (Loc happy_var_3 SymSquareClose)))
	(HappyAbsSyn5  happy_var_2)
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymSquareOpen)))
	 =  HappyAbsSyn4
		 (Array (locate happy_var_1 <> locate happy_var_3) happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  10 happyReduction_17
happyReduction_17 (HappyTerminal (TokSymbol (Loc happy_var_2 SymSquareClose)))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymSquareOpen)))
	 =  HappyAbsSyn4
		 (Array (locate happy_var_1 <> locate happy_var_2) V.empty
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  11 happyReduction_18
happyReduction_18 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 (V.singleton happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  11 happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (V.snoc happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  12 happyReduction_20
happyReduction_20 (HappyTerminal (TokSymbol (Loc happy_var_3 SymCurlyClose)))
	(HappyAbsSyn13  happy_var_2)
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymCurlyOpen)))
	 =  HappyAbsSyn4
		 (Object (locate happy_var_1 <> locate happy_var_3) (Compat.fromList happy_var_2)
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  12 happyReduction_21
happyReduction_21 (HappyTerminal (TokSymbol (Loc happy_var_2 SymCurlyClose)))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymCurlyOpen)))
	 =  HappyAbsSyn4
		 (Object (locate happy_var_1 <> locate happy_var_2) mempty
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  13 happyReduction_22
happyReduction_22 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  13 happyReduction_23
happyReduction_23 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_3 : happy_var_1
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 5 14 happyReduction_24
happyReduction_24 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 ((unLoc happy_var_2, happy_var_5)
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 4 14 happyReduction_25
happyReduction_25 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (("", happy_var_4)
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_2  15 happyReduction_26
happyReduction_26 (HappyTerminal (TokStringLit happy_var_2))
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 <> happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  15 happyReduction_27
happyReduction_27 (HappyTerminal (TokStringLit happy_var_1))
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  16 happyReduction_28
happyReduction_28 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Gt (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  16 happyReduction_29
happyReduction_29 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Lt (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  16 happyReduction_30
happyReduction_30 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Gte (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  16 happyReduction_31
happyReduction_31 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Lte (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  16 happyReduction_32
happyReduction_32 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (NotEq (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  16 happyReduction_33
happyReduction_33 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Eq (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  16 happyReduction_34
happyReduction_34 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  16 happyReduction_35
happyReduction_35 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  16 happyReduction_36
happyReduction_36 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (In (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  16 happyReduction_37
happyReduction_37 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Defaulting (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happyReduce 12 17 happyReduction_38
happyReduction_38 ((HappyTerminal (TokSymbol (Loc happy_var_12 SymDoubleCurlyClose))) `HappyStk`
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
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymDoubleCurlyOpen))) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Iff (locate happy_var_1 <> locate happy_var_12) happy_var_3 happy_var_5 happy_var_9
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 4 18 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn19  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_1  19 happyReduction_40
happyReduction_40 (HappyTerminal (TokIdentifier (Loc happy_var_1 "escapeUri")))
	 =  HappyAbsSyn19
		 (buildFunc EscapeURI (locate happy_var_1)
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  19 happyReduction_41
happyReduction_41 (HappyTerminal (TokIdentifier (Loc happy_var_1 "not")))
	 =  HappyAbsSyn19
		 (buildFunc Not (locate happy_var_1)
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  20 happyReduction_42
happyReduction_42 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  20 happyReduction_43
happyReduction_43 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  20 happyReduction_44
happyReduction_44 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  20 happyReduction_45
happyReduction_45 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  20 happyReduction_46
happyReduction_46 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  20 happyReduction_47
happyReduction_47 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  20 happyReduction_48
happyReduction_48 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn4
		 (uncurry Path happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  20 happyReduction_49
happyReduction_49 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  20 happyReduction_50
happyReduction_50 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_2  20 happyReduction_51
happyReduction_51 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  20 happyReduction_52
happyReduction_52 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happyReduce 5 21 happyReduction_53
happyReduction_53 ((HappyTerminal (TokSymbol (Loc happy_var_5 SymDoubleCurlyClose))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn22  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_1 (locate happy_var_5) happy_var_2
	) `HappyStk` happyRest

happyReduce_54 = happyReduce 8 22 happyReduction_54
happyReduction_54 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdentifier happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdentifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymDoubleCurlyOpen))) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (\s b -> Range (locate happy_var_1 <> s) (Just (unLoc happy_var_3)) (unLoc happy_var_5) (snd happy_var_7) b
	) `HappyStk` happyRest

happyReduce_55 = happyReduce 8 22 happyReduction_55
happyReduction_55 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdentifier happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymDoubleCurlyOpen))) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (\s b -> Range (locate happy_var_1 <> s) Nothing (unLoc happy_var_5) (snd happy_var_7) b
	) `HappyStk` happyRest

happyReduce_56 = happySpecReduce_3  23 happyReduction_56
happyReduction_56 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Path (fst happy_var_2) (snd happy_var_2)
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_2  24 happyReduction_57
happyReduction_57 (HappyAbsSyn24  happy_var_2)
	(HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn24
		 ((locate happy_var_1 <> fst happy_var_2, V.cons (Obj (locate happy_var_1) NotOptional (unLoc happy_var_1) Head) (snd happy_var_2))
	)
happyReduction_57 _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  24 happyReduction_58
happyReduction_58 (HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn24
		 ((locate happy_var_1, V.singleton (Obj (locate happy_var_1) NotOptional (unLoc happy_var_1) Head))
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  25 happyReduction_59
happyReduction_59 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn24
		 ((locate happy_var_1, V.singleton happy_var_1)
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_2  25 happyReduction_60
happyReduction_60 (HappyAbsSyn26  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 ((fst happy_var_1 <> locate happy_var_2, V.snoc (snd happy_var_1) happy_var_2)
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_2  26 happyReduction_61
happyReduction_61 (HappyTerminal (TokIdentifier happy_var_2))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymDot)))
	 =  HappyAbsSyn26
		 (Obj (locate happy_var_1 <> locate happy_var_2) NotOptional (unLoc happy_var_2) DotAccess
	)
happyReduction_61 _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  26 happyReduction_62
happyReduction_62 (HappyTerminal (TokIdentifier happy_var_3))
	_
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymQuestionMark)))
	 =  HappyAbsSyn26
		 (Obj (locate happy_var_1 <> locate happy_var_3) Optional (unLoc happy_var_3) DotAccess
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happyReduce 5 26 happyReduction_63
happyReduction_63 ((HappyTerminal (TokSymbol (Loc happy_var_5 SymSquareClose))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokStringLit happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymSquareOpen))) `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (Obj (locate happy_var_1 <> locate happy_var_5) NotOptional (unLoc happy_var_3) BracketAccess
	) `HappyStk` happyRest

happyReduce_64 = happyReduce 6 26 happyReduction_64
happyReduction_64 ((HappyTerminal (TokSymbol (Loc happy_var_6 SymSquareClose))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokStringLit happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymQuestionMark))) `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (Obj (locate happy_var_1 <> locate happy_var_6) Optional (unLoc happy_var_4) BracketAccess
	) `HappyStk` happyRest

happyReduce_65 = happySpecReduce_3  26 happyReduction_65
happyReduction_65 (HappyTerminal (TokSymbol (Loc happy_var_3 SymSquareClose)))
	(HappyTerminal (TokIntLit _ happy_var_2))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymSquareOpen)))
	 =  HappyAbsSyn26
		 (Arr (locate happy_var_1 <> locate happy_var_3) NotOptional (unLoc happy_var_2)
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happyReduce 4 26 happyReduction_66
happyReduction_66 ((HappyTerminal (TokSymbol (Loc happy_var_4 SymSquareClose))) `HappyStk`
	(HappyTerminal (TokIntLit _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymQuestionMark))) `HappyStk`
	happyRest)
	 = HappyAbsSyn26
		 (Arr (locate happy_var_1 <> locate happy_var_4) Optional (unLoc happy_var_3)
	) `HappyStk` happyRest

happyReduce_67 = happySpecReduce_1  27 happyReduction_67
happyReduction_67 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  27 happyReduction_68
happyReduction_68 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_1  27 happyReduction_69
happyReduction_69 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_69 _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  27 happyReduction_70
happyReduction_70 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  27 happyReduction_71
happyReduction_71 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  27 happyReduction_72
happyReduction_72 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  27 happyReduction_73
happyReduction_73 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn4
		 (uncurry Path happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  27 happyReduction_74
happyReduction_74 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_1  27 happyReduction_75
happyReduction_75 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_75 _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_1  27 happyReduction_76
happyReduction_76 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_76 _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_2  27 happyReduction_77
happyReduction_77 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 happy_var_2
	)
happyReduction_77 _ _  = notHappyAtAll 

happyReduce_78 = happySpecReduce_3  27 happyReduction_78
happyReduction_78 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_78 _ _ _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_1  28 happyReduction_79
happyReduction_79 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_79 _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_1  28 happyReduction_80
happyReduction_80 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_80 _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_1  28 happyReduction_81
happyReduction_81 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_81 _  = notHappyAtAll 

happyReduce_82 = happySpecReduce_1  28 happyReduction_82
happyReduction_82 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  28 happyReduction_83
happyReduction_83 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  28 happyReduction_84
happyReduction_84 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyReduce_85 = happySpecReduce_1  28 happyReduction_85
happyReduction_85 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_85 _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  28 happyReduction_86
happyReduction_86 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  28 happyReduction_87
happyReduction_87 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  28 happyReduction_88
happyReduction_88 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_3  28 happyReduction_89
happyReduction_89 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_89 _ _ _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_3  28 happyReduction_90
happyReduction_90 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_90 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 69 69 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokNumLit _ happy_dollar_dollar -> cont 29;
	TokIntLit _ happy_dollar_dollar -> cont 30;
	TokBoolLit happy_dollar_dollar -> cont 31;
	TokBoolLit happy_dollar_dollar -> cont 32;
	TokSymbol (Loc happy_dollar_dollar SymStringBegin) -> cont 33;
	TokSymbol (Loc happy_dollar_dollar SymStringEnd) -> cont 34;
	TokStringLit happy_dollar_dollar -> cont 35;
	TokIdentifier (Loc happy_dollar_dollar "if") -> cont 36;
	TokIdentifier (Loc happy_dollar_dollar "else") -> cont 37;
	TokIdentifier (Loc happy_dollar_dollar "end") -> cont 38;
	TokIdentifier (Loc happy_dollar_dollar "null" ) -> cont 39;
	TokIdentifier (Loc happy_dollar_dollar "range") -> cont 40;
	TokIdentifier (Loc happy_dollar_dollar "escapeUri") -> cont 41;
	TokIdentifier (Loc happy_dollar_dollar "not") -> cont 42;
	TokIdentifier (Loc happy_dollar_dollar "in") -> cont 43;
	TokIdentifier happy_dollar_dollar -> cont 44;
	TokSymbol (Loc happy_dollar_dollar SymSingleQuote) -> cont 45;
	TokSymbol (Loc happy_dollar_dollar SymColon) -> cont 46;
	TokSymbol (Loc happy_dollar_dollar SymDot) -> cont 47;
	TokSymbol (Loc happy_dollar_dollar SymComma) -> cont 48;
	TokSymbol (Loc happy_dollar_dollar SymQuestionMark) -> cont 49;
	TokSymbol (Loc happy_dollar_dollar SymDoubleQuestionMark) -> cont 50;
	TokSymbol (Loc happy_dollar_dollar SymEq) -> cont 51;
	TokSymbol (Loc happy_dollar_dollar SymNotEq) -> cont 52;
	TokSymbol (Loc happy_dollar_dollar SymGt) -> cont 53;
	TokSymbol (Loc happy_dollar_dollar SymLt) -> cont 54;
	TokSymbol (Loc happy_dollar_dollar SymLte) -> cont 55;
	TokSymbol (Loc happy_dollar_dollar SymGte) -> cont 56;
	TokSymbol (Loc happy_dollar_dollar SymAnd) -> cont 57;
	TokSymbol (Loc happy_dollar_dollar SymOr) -> cont 58;
	TokSymbol (Loc happy_dollar_dollar SymUnderscore) -> cont 59;
	TokSymbol (Loc happy_dollar_dollar SymAssignment) -> cont 60;
	TokSymbol (Loc happy_dollar_dollar SymCurlyOpen) -> cont 61;
	TokSymbol (Loc happy_dollar_dollar SymCurlyClose) -> cont 62;
	TokSymbol (Loc happy_dollar_dollar SymDoubleCurlyOpen) -> cont 63;
	TokSymbol (Loc happy_dollar_dollar SymDoubleCurlyClose) -> cont 64;
	TokSymbol (Loc happy_dollar_dollar SymSquareOpen) -> cont 65;
	TokSymbol (Loc happy_dollar_dollar SymSquareClose) -> cont 66;
	TokSymbol (Loc happy_dollar_dollar SymParenOpen) -> cont 67;
	TokSymbol (Loc happy_dollar_dollar SymParenClose) -> cont 68;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 69 tk tks = happyError' (tks, explist)
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
failure [] = do
  sp <- location
  src <- gets parseSource
  parseError $ EmptyTokenStream sp src
failure (tok:_) = do
  sp <- location
  src <- gets parseSource
  -- TODO: fix source position capture here. I think we need the prior span.
  parseError $ UnexpectedToken (Loc sp tok) src

buildFunc :: (Span -> ValueExt -> ValueExt) -> Span -> ValueExt -> ValueExt
buildFunc f sp param = f (sp <> locate param) param
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
