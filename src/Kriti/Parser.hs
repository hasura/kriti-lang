{-# OPTIONS_GHC -w #-}
module Kriti.Parser where

import qualified Data.HashMap.Strict as M
import qualified Data.List as List
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Kriti.Error as E
import qualified Kriti.Lexer as L
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23 t24 t25
	= HappyTerminal (L.TokenExt)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20
	| HappyAbsSyn21 t21
	| HappyAbsSyn22 t22
	| HappyAbsSyn23 t23
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,394) ([0,36864,29,85,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,55552,20481,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,32,0,19520,0,0,0,1892,7488,0,51200,32782,42,0,0,32768,0,0,512,64,0,0,0,0,0,0,0,0,64768,4097,5,0,0,256,0,57344,7,17,0,256,32,0,0,0,0,0,0,8193,0,0,1024,8,0,0,0,0,0,1024,0,0,0,0,0,0,0,256,0,1024,0,0,0,7568,21760,0,0,8,0,0,0,0,0,0,1024,128,0,0,0,0,0,64,0,0,0,12,0,0,32768,0,0,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,527,0,0,0,0,0,2,0,0,0,63,136,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16288,41472,0,0,0,32,0,0,0,0,0,509,1296,0,45568,40963,10,0,0,0,0,0,0,0,0,0,0,0,0,0,256,0,0,0,0,0,0,0,0,0,992,8,0,1008,2176,0,57344,7,17,0,4032,8704,0,32768,31,68,0,16128,34816,0,16384,118,340,0,4096,0,0,0,32,0,0,0,0,4,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,1024,0,0,0,0,0,0,0,64,0,0,32768,0,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,0,2048,0,0,0,16,0,0,0,0,2,0,0,0,0,0,0,2,0,0,1024,0,0,0,8,0,15136,43520,0,0,0,0,0,0,0,0,0,0,64,0,256,0,0,0,0,512,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","string_lit","num_lit","boolean","null","array","list_elements","object","object_fields","object_field","operator","iff","function_call","functions","function_params","predicate","range","range_decl","path","path_vector","path_tail","path_element","value","'if'","'else'","'end'","'null'","'range'","'escapeUri'","'true'","'false'","ident","string","number","int","'\\''","':'","'.'","','","'=='","'>'","'<'","'&&'","'||'","'_'","':='","'{'","'}'","'{{'","'}}'","'['","']'","'('","')'","%eof"]
        bit_start = st Prelude.* 57
        bit_end = (st Prelude.+ 1) Prelude.* 57
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..56]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (29) = happyShift action_15
action_0 (32) = happyShift action_16
action_0 (33) = happyShift action_17
action_0 (35) = happyShift action_2
action_0 (36) = happyShift action_18
action_0 (37) = happyShift action_19
action_0 (49) = happyShift action_20
action_0 (51) = happyShift action_21
action_0 (53) = happyShift action_22
action_0 (55) = happyShift action_23
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 (10) = happyGoto action_8
action_0 (14) = happyGoto action_9
action_0 (15) = happyGoto action_10
action_0 (19) = happyGoto action_11
action_0 (20) = happyGoto action_12
action_0 (21) = happyGoto action_13
action_0 (25) = happyGoto action_14
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (35) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 _ = happyReduce_51

action_4 _ = happyReduce_52

action_5 _ = happyReduce_53

action_6 _ = happyReduce_54

action_7 _ = happyReduce_55

action_8 _ = happyReduce_56

action_9 _ = happyReduce_58

action_10 _ = happyReduce_59

action_11 _ = happyReduce_60

action_12 (29) = happyShift action_15
action_12 (32) = happyShift action_16
action_12 (33) = happyShift action_17
action_12 (35) = happyShift action_2
action_12 (36) = happyShift action_18
action_12 (37) = happyShift action_19
action_12 (49) = happyShift action_20
action_12 (51) = happyShift action_21
action_12 (53) = happyShift action_22
action_12 (55) = happyShift action_23
action_12 (4) = happyGoto action_3
action_12 (5) = happyGoto action_4
action_12 (6) = happyGoto action_5
action_12 (7) = happyGoto action_6
action_12 (8) = happyGoto action_7
action_12 (10) = happyGoto action_8
action_12 (14) = happyGoto action_9
action_12 (15) = happyGoto action_10
action_12 (19) = happyGoto action_11
action_12 (20) = happyGoto action_12
action_12 (21) = happyGoto action_13
action_12 (25) = happyGoto action_38
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_57

action_14 (57) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_6

action_16 _ = happyReduce_4

action_17 _ = happyReduce_5

action_18 _ = happyReduce_2

action_19 _ = happyReduce_3

action_20 (35) = happyShift action_36
action_20 (50) = happyShift action_37
action_20 (11) = happyGoto action_34
action_20 (12) = happyGoto action_35
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (26) = happyShift action_30
action_21 (30) = happyShift action_31
action_21 (31) = happyShift action_32
action_21 (34) = happyShift action_33
action_21 (16) = happyGoto action_28
action_21 (22) = happyGoto action_29
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (29) = happyShift action_15
action_22 (32) = happyShift action_16
action_22 (33) = happyShift action_17
action_22 (35) = happyShift action_2
action_22 (36) = happyShift action_18
action_22 (37) = happyShift action_19
action_22 (49) = happyShift action_20
action_22 (51) = happyShift action_21
action_22 (53) = happyShift action_22
action_22 (54) = happyShift action_27
action_22 (55) = happyShift action_23
action_22 (4) = happyGoto action_3
action_22 (5) = happyGoto action_4
action_22 (6) = happyGoto action_5
action_22 (7) = happyGoto action_6
action_22 (8) = happyGoto action_7
action_22 (9) = happyGoto action_25
action_22 (10) = happyGoto action_8
action_22 (14) = happyGoto action_9
action_22 (15) = happyGoto action_10
action_22 (19) = happyGoto action_11
action_22 (20) = happyGoto action_12
action_22 (21) = happyGoto action_13
action_22 (25) = happyGoto action_26
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (29) = happyShift action_15
action_23 (32) = happyShift action_16
action_23 (33) = happyShift action_17
action_23 (35) = happyShift action_2
action_23 (36) = happyShift action_18
action_23 (37) = happyShift action_19
action_23 (49) = happyShift action_20
action_23 (51) = happyShift action_21
action_23 (53) = happyShift action_22
action_23 (55) = happyShift action_23
action_23 (4) = happyGoto action_3
action_23 (5) = happyGoto action_4
action_23 (6) = happyGoto action_5
action_23 (7) = happyGoto action_6
action_23 (8) = happyGoto action_7
action_23 (10) = happyGoto action_8
action_23 (14) = happyGoto action_9
action_23 (15) = happyGoto action_10
action_23 (19) = happyGoto action_11
action_23 (20) = happyGoto action_12
action_23 (21) = happyGoto action_13
action_23 (25) = happyGoto action_24
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (56) = happyShift action_71
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (41) = happyShift action_69
action_25 (54) = happyShift action_70
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_10

action_27 _ = happyReduce_9

action_28 (29) = happyShift action_15
action_28 (31) = happyShift action_32
action_28 (32) = happyShift action_16
action_28 (33) = happyShift action_17
action_28 (34) = happyShift action_33
action_28 (35) = happyShift action_2
action_28 (36) = happyShift action_18
action_28 (37) = happyShift action_19
action_28 (49) = happyShift action_20
action_28 (53) = happyShift action_22
action_28 (55) = happyShift action_68
action_28 (4) = happyGoto action_59
action_28 (5) = happyGoto action_60
action_28 (6) = happyGoto action_61
action_28 (7) = happyGoto action_62
action_28 (8) = happyGoto action_63
action_28 (10) = happyGoto action_64
action_28 (16) = happyGoto action_65
action_28 (17) = happyGoto action_66
action_28 (22) = happyGoto action_67
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (52) = happyShift action_58
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (32) = happyShift action_16
action_30 (33) = happyShift action_17
action_30 (34) = happyShift action_33
action_30 (35) = happyShift action_2
action_30 (36) = happyShift action_18
action_30 (37) = happyShift action_19
action_30 (51) = happyShift action_56
action_30 (55) = happyShift action_57
action_30 (4) = happyGoto action_49
action_30 (5) = happyGoto action_50
action_30 (6) = happyGoto action_51
action_30 (13) = happyGoto action_52
action_30 (14) = happyGoto action_53
action_30 (18) = happyGoto action_54
action_30 (22) = happyGoto action_55
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (34) = happyShift action_47
action_31 (47) = happyShift action_48
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_23

action_33 (40) = happyShift action_45
action_33 (53) = happyShift action_46
action_33 (23) = happyGoto action_43
action_33 (24) = happyGoto action_44
action_33 _ = happyReduce_45

action_34 (41) = happyShift action_41
action_34 (50) = happyShift action_42
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_13

action_36 (39) = happyShift action_40
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_7

action_38 (51) = happyShift action_39
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (28) = happyShift action_91
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (29) = happyShift action_15
action_40 (32) = happyShift action_16
action_40 (33) = happyShift action_17
action_40 (35) = happyShift action_2
action_40 (36) = happyShift action_18
action_40 (37) = happyShift action_19
action_40 (49) = happyShift action_20
action_40 (51) = happyShift action_21
action_40 (53) = happyShift action_22
action_40 (55) = happyShift action_23
action_40 (4) = happyGoto action_3
action_40 (5) = happyGoto action_4
action_40 (6) = happyGoto action_5
action_40 (7) = happyGoto action_6
action_40 (8) = happyGoto action_7
action_40 (10) = happyGoto action_8
action_40 (14) = happyGoto action_9
action_40 (15) = happyGoto action_10
action_40 (19) = happyGoto action_11
action_40 (20) = happyGoto action_12
action_40 (21) = happyGoto action_13
action_40 (25) = happyGoto action_90
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (35) = happyShift action_36
action_41 (12) = happyGoto action_89
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_12

action_43 (40) = happyShift action_45
action_43 (53) = happyShift action_46
action_43 (24) = happyGoto action_88
action_43 _ = happyReduce_44

action_44 _ = happyReduce_46

action_45 (34) = happyShift action_87
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (37) = happyShift action_85
action_46 (38) = happyShift action_86
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (41) = happyShift action_84
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (41) = happyShift action_83
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_38

action_50 _ = happyReduce_37

action_51 _ = happyReduce_36

action_52 _ = happyReduce_35

action_53 _ = happyReduce_34

action_54 (42) = happyShift action_77
action_54 (43) = happyShift action_78
action_54 (44) = happyShift action_79
action_54 (45) = happyShift action_80
action_54 (46) = happyShift action_81
action_54 (52) = happyShift action_82
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_33

action_56 (26) = happyShift action_30
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (32) = happyShift action_16
action_57 (33) = happyShift action_17
action_57 (34) = happyShift action_33
action_57 (35) = happyShift action_2
action_57 (36) = happyShift action_18
action_57 (37) = happyShift action_19
action_57 (51) = happyShift action_56
action_57 (55) = happyShift action_57
action_57 (4) = happyGoto action_49
action_57 (5) = happyGoto action_50
action_57 (6) = happyGoto action_51
action_57 (13) = happyGoto action_52
action_57 (14) = happyGoto action_53
action_57 (18) = happyGoto action_76
action_57 (22) = happyGoto action_55
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_43

action_59 _ = happyReduce_26

action_60 _ = happyReduce_27

action_61 _ = happyReduce_25

action_62 _ = happyReduce_24

action_63 _ = happyReduce_29

action_64 _ = happyReduce_30

action_65 (29) = happyShift action_15
action_65 (31) = happyShift action_32
action_65 (32) = happyShift action_16
action_65 (33) = happyShift action_17
action_65 (34) = happyShift action_33
action_65 (35) = happyShift action_2
action_65 (36) = happyShift action_18
action_65 (37) = happyShift action_19
action_65 (49) = happyShift action_20
action_65 (53) = happyShift action_22
action_65 (55) = happyShift action_68
action_65 (4) = happyGoto action_59
action_65 (5) = happyGoto action_60
action_65 (6) = happyGoto action_61
action_65 (7) = happyGoto action_62
action_65 (8) = happyGoto action_63
action_65 (10) = happyGoto action_64
action_65 (16) = happyGoto action_65
action_65 (17) = happyGoto action_75
action_65 (22) = happyGoto action_67
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (52) = happyShift action_74
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_28

action_68 (29) = happyShift action_15
action_68 (31) = happyShift action_32
action_68 (32) = happyShift action_16
action_68 (33) = happyShift action_17
action_68 (34) = happyShift action_33
action_68 (35) = happyShift action_2
action_68 (36) = happyShift action_18
action_68 (37) = happyShift action_19
action_68 (49) = happyShift action_20
action_68 (53) = happyShift action_22
action_68 (55) = happyShift action_68
action_68 (4) = happyGoto action_59
action_68 (5) = happyGoto action_60
action_68 (6) = happyGoto action_61
action_68 (7) = happyGoto action_62
action_68 (8) = happyGoto action_63
action_68 (10) = happyGoto action_64
action_68 (16) = happyGoto action_65
action_68 (17) = happyGoto action_73
action_68 (22) = happyGoto action_67
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (29) = happyShift action_15
action_69 (32) = happyShift action_16
action_69 (33) = happyShift action_17
action_69 (35) = happyShift action_2
action_69 (36) = happyShift action_18
action_69 (37) = happyShift action_19
action_69 (49) = happyShift action_20
action_69 (51) = happyShift action_21
action_69 (53) = happyShift action_22
action_69 (55) = happyShift action_23
action_69 (4) = happyGoto action_3
action_69 (5) = happyGoto action_4
action_69 (6) = happyGoto action_5
action_69 (7) = happyGoto action_6
action_69 (8) = happyGoto action_7
action_69 (10) = happyGoto action_8
action_69 (14) = happyGoto action_9
action_69 (15) = happyGoto action_10
action_69 (19) = happyGoto action_11
action_69 (20) = happyGoto action_12
action_69 (21) = happyGoto action_13
action_69 (25) = happyGoto action_72
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_8

action_71 _ = happyReduce_61

action_72 _ = happyReduce_11

action_73 (56) = happyShift action_104
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_22

action_75 _ = happyReduce_31

action_76 (42) = happyShift action_77
action_76 (43) = happyShift action_78
action_76 (44) = happyShift action_79
action_76 (45) = happyShift action_80
action_76 (46) = happyShift action_81
action_76 (56) = happyShift action_103
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (32) = happyShift action_16
action_77 (33) = happyShift action_17
action_77 (34) = happyShift action_33
action_77 (35) = happyShift action_2
action_77 (36) = happyShift action_18
action_77 (37) = happyShift action_19
action_77 (51) = happyShift action_56
action_77 (55) = happyShift action_57
action_77 (4) = happyGoto action_49
action_77 (5) = happyGoto action_50
action_77 (6) = happyGoto action_51
action_77 (13) = happyGoto action_52
action_77 (14) = happyGoto action_53
action_77 (18) = happyGoto action_102
action_77 (22) = happyGoto action_55
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (32) = happyShift action_16
action_78 (33) = happyShift action_17
action_78 (34) = happyShift action_33
action_78 (35) = happyShift action_2
action_78 (36) = happyShift action_18
action_78 (37) = happyShift action_19
action_78 (51) = happyShift action_56
action_78 (55) = happyShift action_57
action_78 (4) = happyGoto action_49
action_78 (5) = happyGoto action_50
action_78 (6) = happyGoto action_51
action_78 (13) = happyGoto action_52
action_78 (14) = happyGoto action_53
action_78 (18) = happyGoto action_101
action_78 (22) = happyGoto action_55
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (32) = happyShift action_16
action_79 (33) = happyShift action_17
action_79 (34) = happyShift action_33
action_79 (35) = happyShift action_2
action_79 (36) = happyShift action_18
action_79 (37) = happyShift action_19
action_79 (51) = happyShift action_56
action_79 (55) = happyShift action_57
action_79 (4) = happyGoto action_49
action_79 (5) = happyGoto action_50
action_79 (6) = happyGoto action_51
action_79 (13) = happyGoto action_52
action_79 (14) = happyGoto action_53
action_79 (18) = happyGoto action_100
action_79 (22) = happyGoto action_55
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (32) = happyShift action_16
action_80 (33) = happyShift action_17
action_80 (34) = happyShift action_33
action_80 (35) = happyShift action_2
action_80 (36) = happyShift action_18
action_80 (37) = happyShift action_19
action_80 (51) = happyShift action_56
action_80 (55) = happyShift action_57
action_80 (4) = happyGoto action_49
action_80 (5) = happyGoto action_50
action_80 (6) = happyGoto action_51
action_80 (13) = happyGoto action_52
action_80 (14) = happyGoto action_53
action_80 (18) = happyGoto action_99
action_80 (22) = happyGoto action_55
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (32) = happyShift action_16
action_81 (33) = happyShift action_17
action_81 (34) = happyShift action_33
action_81 (35) = happyShift action_2
action_81 (36) = happyShift action_18
action_81 (37) = happyShift action_19
action_81 (51) = happyShift action_56
action_81 (55) = happyShift action_57
action_81 (4) = happyGoto action_49
action_81 (5) = happyGoto action_50
action_81 (6) = happyGoto action_51
action_81 (13) = happyGoto action_52
action_81 (14) = happyGoto action_53
action_81 (18) = happyGoto action_98
action_81 (22) = happyGoto action_55
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (29) = happyShift action_15
action_82 (32) = happyShift action_16
action_82 (33) = happyShift action_17
action_82 (35) = happyShift action_2
action_82 (36) = happyShift action_18
action_82 (37) = happyShift action_19
action_82 (49) = happyShift action_20
action_82 (51) = happyShift action_21
action_82 (53) = happyShift action_22
action_82 (55) = happyShift action_23
action_82 (4) = happyGoto action_3
action_82 (5) = happyGoto action_4
action_82 (6) = happyGoto action_5
action_82 (7) = happyGoto action_6
action_82 (8) = happyGoto action_7
action_82 (10) = happyGoto action_8
action_82 (14) = happyGoto action_9
action_82 (15) = happyGoto action_10
action_82 (19) = happyGoto action_11
action_82 (20) = happyGoto action_12
action_82 (21) = happyGoto action_13
action_82 (25) = happyGoto action_97
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (34) = happyShift action_96
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (34) = happyShift action_95
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (54) = happyShift action_94
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (34) = happyShift action_93
action_86 _ = happyFail (happyExpListPerState 86)

action_87 _ = happyReduce_48

action_88 _ = happyReduce_47

action_89 _ = happyReduce_14

action_90 _ = happyReduce_15

action_91 (52) = happyShift action_92
action_91 _ = happyFail (happyExpListPerState 91)

action_92 _ = happyReduce_40

action_93 (38) = happyShift action_108
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_50

action_95 (48) = happyShift action_107
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (48) = happyShift action_106
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (51) = happyShift action_105
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_20

action_99 _ = happyReduce_19

action_100 _ = happyReduce_17

action_101 _ = happyReduce_16

action_102 _ = happyReduce_18

action_103 _ = happyReduce_39

action_104 _ = happyReduce_32

action_105 (27) = happyShift action_112
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (34) = happyShift action_33
action_106 (22) = happyGoto action_111
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (34) = happyShift action_33
action_107 (22) = happyGoto action_110
action_107 _ = happyFail (happyExpListPerState 107)

action_108 (54) = happyShift action_109
action_108 _ = happyFail (happyExpListPerState 108)

action_109 _ = happyReduce_49

action_110 (52) = happyShift action_115
action_110 _ = happyFail (happyExpListPerState 110)

action_111 (52) = happyShift action_114
action_111 _ = happyFail (happyExpListPerState 111)

action_112 (52) = happyShift action_113
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (29) = happyShift action_15
action_113 (32) = happyShift action_16
action_113 (33) = happyShift action_17
action_113 (35) = happyShift action_2
action_113 (36) = happyShift action_18
action_113 (37) = happyShift action_19
action_113 (49) = happyShift action_20
action_113 (51) = happyShift action_21
action_113 (53) = happyShift action_22
action_113 (55) = happyShift action_23
action_113 (4) = happyGoto action_3
action_113 (5) = happyGoto action_4
action_113 (6) = happyGoto action_5
action_113 (7) = happyGoto action_6
action_113 (8) = happyGoto action_7
action_113 (10) = happyGoto action_8
action_113 (14) = happyGoto action_9
action_113 (15) = happyGoto action_10
action_113 (19) = happyGoto action_11
action_113 (20) = happyGoto action_12
action_113 (21) = happyGoto action_13
action_113 (25) = happyGoto action_116
action_113 _ = happyFail (happyExpListPerState 113)

action_114 _ = happyReduce_42

action_115 _ = happyReduce_41

action_116 (51) = happyShift action_117
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (28) = happyShift action_118
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (52) = happyShift action_119
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_21

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (L.TokenExt (L.StringTem happy_var_1)  _ _ _))
	 =  HappyAbsSyn4
		 (mkTemplate happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (L.TokenExt (L.NumLit _ happy_var_1)  _ _ _))
	 =  HappyAbsSyn5
		 (Number happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyTerminal (L.TokenExt (L.IntLit _ happy_var_1)  _ _ _))
	 =  HappyAbsSyn5
		 (Number (S.scientific (fromIntegral happy_var_1) 0)
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn6
		 (Boolean True
	)

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn6
		 (Boolean False
	)

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn7
		 (Null
	)

happyReduce_7 = happySpecReduce_2  7 happyReduction_7
happyReduction_7 _
	_
	 =  HappyAbsSyn7
		 (Null
	)

happyReduce_8 = happySpecReduce_3  8 happyReduction_8
happyReduction_8 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Array happy_var_2
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 _
	_
	 =  HappyAbsSyn8
		 (Array V.empty
	)

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn9
		 (V.singleton happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (V.snoc happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  10 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Object (M.fromList happy_var_2)
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_3 : happy_var_1
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  12 happyReduction_15
happyReduction_15 (HappyAbsSyn25  happy_var_3)
	_
	(HappyTerminal (L.TokenExt (L.StringTem happy_var_1)  _ _ _))
	 =  HappyAbsSyn12
		 ((happy_var_1, happy_var_3)
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  13 happyReduction_16
happyReduction_16 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn13
		 (Gt happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  13 happyReduction_17
happyReduction_17 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn13
		 (Lt happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  13 happyReduction_18
happyReduction_18 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn13
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  13 happyReduction_19
happyReduction_19 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn13
		 (And happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  13 happyReduction_20
happyReduction_20 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn13
		 (Or happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 12 14 happyReduction_21
happyReduction_21 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Iff happy_var_3 happy_var_5 happy_var_9
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 4 15 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_1  16 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn16
		 (\p -> EscapeURI p
	)

happyReduce_24 = happySpecReduce_1  17 happyReduction_24
happyReduction_24 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  17 happyReduction_25
happyReduction_25 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  17 happyReduction_26
happyReduction_26 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  17 happyReduction_27
happyReduction_27 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  17 happyReduction_28
happyReduction_28 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn17
		 (Path happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  17 happyReduction_29
happyReduction_29 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  17 happyReduction_31
happyReduction_31 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  17 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn18
		 (Path happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  18 happyReduction_35
happyReduction_35 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  18 happyReduction_36
happyReduction_36 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  18 happyReduction_37
happyReduction_37 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  18 happyReduction_38
happyReduction_38 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  18 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happyReduce 5 19 happyReduction_40
happyReduction_40 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (happy_var_1 happy_var_2
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 8 20 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TokenExt (L.Identifier happy_var_5) _ _ _)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TokenExt (L.Identifier happy_var_3) _ _ _)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (\b -> Range (Just happy_var_3) happy_var_5 happy_var_7 b
	) `HappyStk` happyRest

happyReduce_42 = happyReduce 8 20 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TokenExt (L.Identifier happy_var_5) _ _ _)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (\b -> Range Nothing happy_var_5 happy_var_7 b
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_3  21 happyReduction_43
happyReduction_43 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (Path happy_var_2
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_2  22 happyReduction_44
happyReduction_44 (HappyAbsSyn23  happy_var_2)
	(HappyTerminal (L.TokenExt (L.Identifier happy_var_1) _ _ _))
	 =  HappyAbsSyn22
		 (V.cons (Obj happy_var_1) happy_var_2
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  22 happyReduction_45
happyReduction_45 (HappyTerminal (L.TokenExt (L.Identifier happy_var_1) _ _ _))
	 =  HappyAbsSyn22
		 (V.singleton (Obj happy_var_1)
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  23 happyReduction_46
happyReduction_46 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn23
		 (V.singleton happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  23 happyReduction_47
happyReduction_47 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn23
		 (V.snoc happy_var_1 happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_2  24 happyReduction_48
happyReduction_48 (HappyTerminal (L.TokenExt (L.Identifier happy_var_2) _ _ _))
	_
	 =  HappyAbsSyn24
		 (Obj happy_var_2
	)
happyReduction_48 _ _  = notHappyAtAll 

happyReduce_49 = happyReduce 5 24 happyReduction_49
happyReduction_49 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TokenExt (L.Identifier happy_var_3) _ _ _)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 (Obj happy_var_3
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_3  24 happyReduction_50
happyReduction_50 _
	(HappyTerminal (L.TokenExt (L.IntLit _ happy_var_2)  _ _ _))
	_
	 =  HappyAbsSyn24
		 (Arr happy_var_2
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  25 happyReduction_51
happyReduction_51 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  25 happyReduction_52
happyReduction_52 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  25 happyReduction_53
happyReduction_53 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  25 happyReduction_54
happyReduction_54 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  25 happyReduction_55
happyReduction_55 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  25 happyReduction_56
happyReduction_56 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  25 happyReduction_57
happyReduction_57 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  25 happyReduction_58
happyReduction_58 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  25 happyReduction_59
happyReduction_59 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  25 happyReduction_60
happyReduction_60 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  25 happyReduction_61
happyReduction_61 _
	(HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 57 57 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	L.TokenExt (L.Identifier "if") _ _ _ -> cont 26;
	L.TokenExt (L.Identifier "else") _ _ _ -> cont 27;
	L.TokenExt (L.Identifier "end") _ _ _ -> cont 28;
	L.TokenExt (L.Identifier "null")  _ _ _ -> cont 29;
	L.TokenExt (L.Identifier "range")  _ _ _ -> cont 30;
	L.TokenExt (L.Identifier "escapeUri")  _ _ _ -> cont 31;
	L.TokenExt (L.BoolLit True)  _ _ _ -> cont 32;
	L.TokenExt (L.BoolLit False)  _ _ _ -> cont 33;
	L.TokenExt (L.Identifier happy_dollar_dollar) _ _ _ -> cont 34;
	L.TokenExt (L.StringTem happy_dollar_dollar)  _ _ _ -> cont 35;
	L.TokenExt (L.NumLit _ happy_dollar_dollar)  _ _ _ -> cont 36;
	L.TokenExt (L.IntLit _ happy_dollar_dollar)  _ _ _ -> cont 37;
	L.TokenExt L.SingleQuote  _ _ _ -> cont 38;
	L.TokenExt L.Colon  _ _ _ -> cont 39;
	L.TokenExt L.Dot  _ _ _ -> cont 40;
	L.TokenExt L.Comma  _ _ _ -> cont 41;
	L.TokenExt L.Eq  _ _ _ -> cont 42;
	L.TokenExt L.Gt  _ _ _ -> cont 43;
	L.TokenExt L.Lt  _ _ _ -> cont 44;
	L.TokenExt L.And _ _ _ -> cont 45;
	L.TokenExt L.Or  _ _ _ -> cont 46;
	L.TokenExt L.Underscore  _ _ _ -> cont 47;
	L.TokenExt L.Assignment  _ _ _ -> cont 48;
	L.TokenExt L.CurlyOpen  _ _ _ -> cont 49;
	L.TokenExt L.CurlyClose  _ _ _ -> cont 50;
	L.TokenExt L.DoubleCurlyOpen  _ _ _ -> cont 51;
	L.TokenExt L.DoubleCurlyClose  _ _ _ -> cont 52;
	L.TokenExt L.SquareOpen  _ _ _ -> cont 53;
	L.TokenExt L.SquareClose  _ _ _ -> cont 54;
	L.TokenExt L.ParenOpen  _ _ _ -> cont 55;
	L.TokenExt L.ParenClose  _ _ _ -> cont 56;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 57 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either ParseError a -> (a -> Either ParseError b) -> Either ParseError b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Either ParseError a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either ParseError a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(L.TokenExt)], [Prelude.String]) -> Either ParseError a
happyError' = (\(tokens, _) -> parseError tokens)
parser tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn25 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


mkTemplate = undefined

data ParseError = EmptyTokenStream | UnexpectedToken L.TokenExt
  deriving Show

instance E.RenderError ParseError where
  render EmptyTokenStream =
    E.RenderedError { _code = E.ParseErrorCode
                  , _message = "ParseError: Empty token stream."
                  , _span = (undefined, Nothing)
                  }

  render (UnexpectedToken L.TokenExt{..}) =
    let tok = L.serialize teType
    in E.RenderedError { _code = E.ParseErrorCode
                  , _message = "ParseError: Unexpected token '" <> tok <> "'."
                  , _span = (teStartPos, Just teEndPos)
                  }

parseError :: [L.TokenExt] -> Either ParseError a
parseError [] = Left EmptyTokenStream
parseError (tok:_) = Left $ UnexpectedToken tok

data Accessor = Obj T.Text | Arr Int
  deriving (Show, Eq, Read)

renderAccessor :: Accessor -> T.Text
renderAccessor = \case
  Obj txt -> txt
  Arr i -> T.pack $ show i

renderPath :: V.Vector Accessor -> T.Text
renderPath = mconcat . List.intersperse "." . V.toList . fmap renderAccessor

data ValueExt
  = -- Core Aeson Terms
    Object (M.HashMap T.Text ValueExt)
  | Array (V.Vector ValueExt)
  | String T.Text
  | Number S.Scientific
  | Boolean Bool
  | Null
  | -- | Extended Terms
    StringTem [ValueExt]
  | Path (V.Vector Accessor)
  | Iff ValueExt ValueExt ValueExt
  | Eq ValueExt ValueExt
  | Gt ValueExt ValueExt
  | Lt ValueExt ValueExt
  | And ValueExt ValueExt
  | Or ValueExt ValueExt
  | Member ValueExt ValueExt
  | Range (Maybe T.Text) T.Text (V.Vector Accessor) ValueExt
  | EscapeURI ValueExt
  deriving (Show, Eq, Read)
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
