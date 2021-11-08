{-# OPTIONS_GHC -w #-}
module Kriti.Parser where

import qualified Data.HashMap.Strict as M
import qualified Data.List as List
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Kriti.Lexer as L
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22
	= HappyTerminal (L.Token)
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,300) ([0,41344,40963,10,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,929,2720,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24576,232,680,0,0,0,0,0,0,0,32768,0,64,0,4368,0,0,6144,58,234,0,3718,10880,0,0,0,16,0,2048,256,0,0,0,0,0,0,0,0,4096,512,0,49152,8192,2,1024,32768,0,0,32768,256,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,128,0,1024,0,0,6144,58,170,0,2,0,0,0,0,0,0,2048,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,31744,16,0,0,0,0,1088,0,0,0,192,544,0,0,12289,0,0,0,0,16384,0,0,0,128,1,0,6144,58,170,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16415,0,0,32771,8,0,192,544,0,12288,34816,0,0,12,34,0,768,2176,0,59488,43008,2,1024,0,0,0,1,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,16,0,0,1024,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,4096,0,2048,0,0,4096,0,0,0,4,0,0,0,64,8,0,4096,512,0,0,16384,0,0,0,0,0,3718,10880,0,0,16,3,0,1024,192,0,0,0,0,0,0,0,0,0,128,0,1024,0,0,0,0,16,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","string_lit","num_lit","boolean","null","array","list_elements","object","object_fields","object_field","operator","functions","iff","predicate","range","range_decl","path","path_list","path_element","value","ident","string","number","int","'if'","'else'","'end'","'null'","'range'","'escapeUri'","'true'","'false'","'\\''","':'","'.'","','","'=='","'>'","'<'","'&&'","'||'","'_'","':='","'{'","'}'","'{{'","'}}'","'['","']'","'('","')'","%eof"]
        bit_start = st Prelude.* 54
        bit_end = (st Prelude.+ 1) Prelude.* 54
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..53]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (24) = happyShift action_2
action_0 (25) = happyShift action_15
action_0 (30) = happyShift action_16
action_0 (32) = happyShift action_17
action_0 (33) = happyShift action_18
action_0 (34) = happyShift action_19
action_0 (46) = happyShift action_20
action_0 (48) = happyShift action_21
action_0 (50) = happyShift action_22
action_0 (52) = happyShift action_23
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 (10) = happyGoto action_8
action_0 (14) = happyGoto action_9
action_0 (15) = happyGoto action_10
action_0 (17) = happyGoto action_11
action_0 (18) = happyGoto action_12
action_0 (19) = happyGoto action_13
action_0 (22) = happyGoto action_14
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (24) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 _ = happyReduce_36

action_4 _ = happyReduce_37

action_5 _ = happyReduce_38

action_6 _ = happyReduce_39

action_7 _ = happyReduce_40

action_8 _ = happyReduce_41

action_9 _ = happyReduce_44

action_10 _ = happyReduce_43

action_11 _ = happyReduce_45

action_12 (24) = happyShift action_2
action_12 (25) = happyShift action_15
action_12 (30) = happyShift action_16
action_12 (32) = happyShift action_17
action_12 (33) = happyShift action_18
action_12 (34) = happyShift action_19
action_12 (46) = happyShift action_20
action_12 (48) = happyShift action_21
action_12 (50) = happyShift action_22
action_12 (52) = happyShift action_23
action_12 (4) = happyGoto action_3
action_12 (5) = happyGoto action_4
action_12 (6) = happyGoto action_5
action_12 (7) = happyGoto action_6
action_12 (8) = happyGoto action_7
action_12 (10) = happyGoto action_8
action_12 (14) = happyGoto action_9
action_12 (15) = happyGoto action_10
action_12 (17) = happyGoto action_11
action_12 (18) = happyGoto action_12
action_12 (19) = happyGoto action_13
action_12 (22) = happyGoto action_36
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_42

action_14 (54) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_2

action_16 _ = happyReduce_5

action_17 (24) = happyShift action_2
action_17 (25) = happyShift action_15
action_17 (30) = happyShift action_16
action_17 (32) = happyShift action_17
action_17 (33) = happyShift action_18
action_17 (34) = happyShift action_19
action_17 (46) = happyShift action_20
action_17 (48) = happyShift action_21
action_17 (50) = happyShift action_22
action_17 (52) = happyShift action_23
action_17 (4) = happyGoto action_3
action_17 (5) = happyGoto action_4
action_17 (6) = happyGoto action_5
action_17 (7) = happyGoto action_6
action_17 (8) = happyGoto action_7
action_17 (10) = happyGoto action_8
action_17 (14) = happyGoto action_9
action_17 (15) = happyGoto action_10
action_17 (17) = happyGoto action_11
action_17 (18) = happyGoto action_12
action_17 (19) = happyGoto action_13
action_17 (22) = happyGoto action_35
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_3

action_19 _ = happyReduce_4

action_20 (24) = happyShift action_33
action_20 (47) = happyShift action_34
action_20 (11) = happyGoto action_31
action_20 (12) = happyGoto action_32
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (23) = happyShift action_28
action_21 (27) = happyShift action_29
action_21 (31) = happyShift action_30
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (24) = happyShift action_2
action_22 (25) = happyShift action_15
action_22 (30) = happyShift action_16
action_22 (32) = happyShift action_17
action_22 (33) = happyShift action_18
action_22 (34) = happyShift action_19
action_22 (46) = happyShift action_20
action_22 (48) = happyShift action_21
action_22 (50) = happyShift action_22
action_22 (51) = happyShift action_27
action_22 (52) = happyShift action_23
action_22 (4) = happyGoto action_3
action_22 (5) = happyGoto action_4
action_22 (6) = happyGoto action_5
action_22 (7) = happyGoto action_6
action_22 (8) = happyGoto action_7
action_22 (9) = happyGoto action_25
action_22 (10) = happyGoto action_8
action_22 (14) = happyGoto action_9
action_22 (15) = happyGoto action_10
action_22 (17) = happyGoto action_11
action_22 (18) = happyGoto action_12
action_22 (19) = happyGoto action_13
action_22 (22) = happyGoto action_26
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (24) = happyShift action_2
action_23 (25) = happyShift action_15
action_23 (30) = happyShift action_16
action_23 (32) = happyShift action_17
action_23 (33) = happyShift action_18
action_23 (34) = happyShift action_19
action_23 (46) = happyShift action_20
action_23 (48) = happyShift action_21
action_23 (50) = happyShift action_22
action_23 (52) = happyShift action_23
action_23 (4) = happyGoto action_3
action_23 (5) = happyGoto action_4
action_23 (6) = happyGoto action_5
action_23 (7) = happyGoto action_6
action_23 (8) = happyGoto action_7
action_23 (10) = happyGoto action_8
action_23 (14) = happyGoto action_9
action_23 (15) = happyGoto action_10
action_23 (17) = happyGoto action_11
action_23 (18) = happyGoto action_12
action_23 (19) = happyGoto action_13
action_23 (22) = happyGoto action_24
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (53) = happyShift action_56
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (38) = happyShift action_54
action_25 (51) = happyShift action_55
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_9

action_27 _ = happyReduce_8

action_28 (37) = happyShift action_52
action_28 (50) = happyShift action_53
action_28 (20) = happyGoto action_50
action_28 (21) = happyGoto action_51
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (33) = happyShift action_18
action_29 (34) = happyShift action_19
action_29 (48) = happyShift action_48
action_29 (52) = happyShift action_49
action_29 (6) = happyGoto action_43
action_29 (13) = happyGoto action_44
action_29 (15) = happyGoto action_45
action_29 (16) = happyGoto action_46
action_29 (19) = happyGoto action_47
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (23) = happyShift action_41
action_30 (44) = happyShift action_42
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (38) = happyShift action_39
action_31 (47) = happyShift action_40
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_12

action_33 (36) = happyShift action_38
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_6

action_35 _ = happyReduce_20

action_36 (48) = happyShift action_37
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (29) = happyShift action_74
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (24) = happyShift action_2
action_38 (25) = happyShift action_15
action_38 (30) = happyShift action_16
action_38 (32) = happyShift action_17
action_38 (33) = happyShift action_18
action_38 (34) = happyShift action_19
action_38 (46) = happyShift action_20
action_38 (48) = happyShift action_21
action_38 (50) = happyShift action_22
action_38 (52) = happyShift action_23
action_38 (4) = happyGoto action_3
action_38 (5) = happyGoto action_4
action_38 (6) = happyGoto action_5
action_38 (7) = happyGoto action_6
action_38 (8) = happyGoto action_7
action_38 (10) = happyGoto action_8
action_38 (14) = happyGoto action_9
action_38 (15) = happyGoto action_10
action_38 (17) = happyGoto action_11
action_38 (18) = happyGoto action_12
action_38 (19) = happyGoto action_13
action_38 (22) = happyGoto action_73
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (24) = happyShift action_33
action_39 (12) = happyGoto action_72
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_11

action_41 (38) = happyShift action_71
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (38) = happyShift action_70
action_42 _ = happyFail (happyExpListPerState 42)

action_43 _ = happyReduce_25

action_44 _ = happyReduce_24

action_45 _ = happyReduce_23

action_46 (39) = happyShift action_64
action_46 (40) = happyShift action_65
action_46 (41) = happyShift action_66
action_46 (42) = happyShift action_67
action_46 (43) = happyShift action_68
action_46 (49) = happyShift action_69
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_22

action_48 (23) = happyShift action_28
action_48 (27) = happyShift action_29
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (33) = happyShift action_18
action_49 (34) = happyShift action_19
action_49 (48) = happyShift action_48
action_49 (52) = happyShift action_49
action_49 (6) = happyGoto action_43
action_49 (13) = happyGoto action_44
action_49 (15) = happyGoto action_45
action_49 (16) = happyGoto action_63
action_49 (19) = happyGoto action_47
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (37) = happyShift action_52
action_50 (49) = happyShift action_62
action_50 (50) = happyShift action_53
action_50 (21) = happyGoto action_61
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_31

action_52 (23) = happyShift action_60
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (26) = happyShift action_58
action_53 (35) = happyShift action_59
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (24) = happyShift action_2
action_54 (25) = happyShift action_15
action_54 (30) = happyShift action_16
action_54 (32) = happyShift action_17
action_54 (33) = happyShift action_18
action_54 (34) = happyShift action_19
action_54 (46) = happyShift action_20
action_54 (48) = happyShift action_21
action_54 (50) = happyShift action_22
action_54 (52) = happyShift action_23
action_54 (4) = happyGoto action_3
action_54 (5) = happyGoto action_4
action_54 (6) = happyGoto action_5
action_54 (7) = happyGoto action_6
action_54 (8) = happyGoto action_7
action_54 (10) = happyGoto action_8
action_54 (14) = happyGoto action_9
action_54 (15) = happyGoto action_10
action_54 (17) = happyGoto action_11
action_54 (18) = happyGoto action_12
action_54 (19) = happyGoto action_13
action_54 (22) = happyGoto action_57
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_7

action_56 _ = happyReduce_46

action_57 _ = happyReduce_10

action_58 (51) = happyShift action_86
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (23) = happyShift action_85
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_33

action_61 _ = happyReduce_32

action_62 _ = happyReduce_30

action_63 (39) = happyShift action_64
action_63 (40) = happyShift action_65
action_63 (41) = happyShift action_66
action_63 (42) = happyShift action_67
action_63 (43) = happyShift action_68
action_63 (53) = happyShift action_84
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (33) = happyShift action_18
action_64 (34) = happyShift action_19
action_64 (48) = happyShift action_48
action_64 (52) = happyShift action_49
action_64 (6) = happyGoto action_43
action_64 (13) = happyGoto action_44
action_64 (15) = happyGoto action_45
action_64 (16) = happyGoto action_83
action_64 (19) = happyGoto action_47
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (33) = happyShift action_18
action_65 (34) = happyShift action_19
action_65 (48) = happyShift action_48
action_65 (52) = happyShift action_49
action_65 (6) = happyGoto action_43
action_65 (13) = happyGoto action_44
action_65 (15) = happyGoto action_45
action_65 (16) = happyGoto action_82
action_65 (19) = happyGoto action_47
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (33) = happyShift action_18
action_66 (34) = happyShift action_19
action_66 (48) = happyShift action_48
action_66 (52) = happyShift action_49
action_66 (6) = happyGoto action_43
action_66 (13) = happyGoto action_44
action_66 (15) = happyGoto action_45
action_66 (16) = happyGoto action_81
action_66 (19) = happyGoto action_47
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (33) = happyShift action_18
action_67 (34) = happyShift action_19
action_67 (48) = happyShift action_48
action_67 (52) = happyShift action_49
action_67 (6) = happyGoto action_43
action_67 (13) = happyGoto action_44
action_67 (15) = happyGoto action_45
action_67 (16) = happyGoto action_80
action_67 (19) = happyGoto action_47
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (33) = happyShift action_18
action_68 (34) = happyShift action_19
action_68 (48) = happyShift action_48
action_68 (52) = happyShift action_49
action_68 (6) = happyGoto action_43
action_68 (13) = happyGoto action_44
action_68 (15) = happyGoto action_45
action_68 (16) = happyGoto action_79
action_68 (19) = happyGoto action_47
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (24) = happyShift action_2
action_69 (25) = happyShift action_15
action_69 (30) = happyShift action_16
action_69 (32) = happyShift action_17
action_69 (33) = happyShift action_18
action_69 (34) = happyShift action_19
action_69 (46) = happyShift action_20
action_69 (48) = happyShift action_21
action_69 (50) = happyShift action_22
action_69 (52) = happyShift action_23
action_69 (4) = happyGoto action_3
action_69 (5) = happyGoto action_4
action_69 (6) = happyGoto action_5
action_69 (7) = happyGoto action_6
action_69 (8) = happyGoto action_7
action_69 (10) = happyGoto action_8
action_69 (14) = happyGoto action_9
action_69 (15) = happyGoto action_10
action_69 (17) = happyGoto action_11
action_69 (18) = happyGoto action_12
action_69 (19) = happyGoto action_13
action_69 (22) = happyGoto action_78
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (23) = happyShift action_77
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (23) = happyShift action_76
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_13

action_73 _ = happyReduce_14

action_74 (49) = happyShift action_75
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_27

action_76 (45) = happyShift action_90
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (45) = happyShift action_89
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (48) = happyShift action_88
action_78 _ = happyFail (happyExpListPerState 78)

action_79 _ = happyReduce_19

action_80 _ = happyReduce_18

action_81 _ = happyReduce_16

action_82 _ = happyReduce_15

action_83 _ = happyReduce_17

action_84 _ = happyReduce_26

action_85 (35) = happyShift action_87
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_35

action_87 (51) = happyShift action_94
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (28) = happyShift action_93
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (23) = happyShift action_92
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (23) = happyShift action_91
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (37) = happyShift action_52
action_91 (50) = happyShift action_53
action_91 (20) = happyGoto action_97
action_91 (21) = happyGoto action_51
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (37) = happyShift action_52
action_92 (50) = happyShift action_53
action_92 (20) = happyGoto action_96
action_92 (21) = happyGoto action_51
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (49) = happyShift action_95
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_34

action_95 (24) = happyShift action_2
action_95 (25) = happyShift action_15
action_95 (30) = happyShift action_16
action_95 (32) = happyShift action_17
action_95 (33) = happyShift action_18
action_95 (34) = happyShift action_19
action_95 (46) = happyShift action_20
action_95 (48) = happyShift action_21
action_95 (50) = happyShift action_22
action_95 (52) = happyShift action_23
action_95 (4) = happyGoto action_3
action_95 (5) = happyGoto action_4
action_95 (6) = happyGoto action_5
action_95 (7) = happyGoto action_6
action_95 (8) = happyGoto action_7
action_95 (10) = happyGoto action_8
action_95 (14) = happyGoto action_9
action_95 (15) = happyGoto action_10
action_95 (17) = happyGoto action_11
action_95 (18) = happyGoto action_12
action_95 (19) = happyGoto action_13
action_95 (22) = happyGoto action_100
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (37) = happyShift action_52
action_96 (49) = happyShift action_99
action_96 (50) = happyShift action_53
action_96 (21) = happyGoto action_61
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (37) = happyShift action_52
action_97 (49) = happyShift action_98
action_97 (50) = happyShift action_53
action_97 (21) = happyGoto action_61
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_28

action_99 _ = happyReduce_29

action_100 (48) = happyShift action_101
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (29) = happyShift action_102
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (49) = happyShift action_103
action_102 _ = happyFail (happyExpListPerState 102)

action_103 _ = happyReduce_21

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (L.StringTem happy_var_1))
	 =  HappyAbsSyn4
		 (String happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (L.NumLit _ happy_var_1))
	 =  HappyAbsSyn5
		 (Number happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn6
		 (Boolean True
	)

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 _
	 =  HappyAbsSyn6
		 (Boolean False
	)

happyReduce_5 = happySpecReduce_1  7 happyReduction_5
happyReduction_5 _
	 =  HappyAbsSyn7
		 (Null
	)

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 _
	_
	 =  HappyAbsSyn7
		 (Null
	)

happyReduce_7 = happySpecReduce_3  8 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Array happy_var_2
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 _
	_
	 =  HappyAbsSyn8
		 (Array V.empty
	)

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn9
		 (V.singleton happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (V.cons happy_var_3 happy_var_1
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Object (M.fromList happy_var_2)
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  11 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  11 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_3 : happy_var_1
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  12 happyReduction_14
happyReduction_14 (HappyAbsSyn22  happy_var_3)
	_
	(HappyTerminal (L.StringTem happy_var_1))
	 =  HappyAbsSyn12
		 ((happy_var_1, happy_var_3)
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  13 happyReduction_15
happyReduction_15 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn13
		 (Gt happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  13 happyReduction_16
happyReduction_16 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn13
		 (Lt happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  13 happyReduction_17
happyReduction_17 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn13
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  13 happyReduction_18
happyReduction_18 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn13
		 (And happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  13 happyReduction_19
happyReduction_19 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn13
		 (Or happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_2  14 happyReduction_20
happyReduction_20 (HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (EscapeURI happy_var_2
	)
happyReduction_20 _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 12 15 happyReduction_21
happyReduction_21 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Iff happy_var_3 happy_var_5 happy_var_9
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_1  16 happyReduction_22
happyReduction_22 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  16 happyReduction_23
happyReduction_23 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  16 happyReduction_24
happyReduction_24 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  16 happyReduction_25
happyReduction_25 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  16 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 5 17 happyReduction_27
happyReduction_27 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn22  happy_var_2) `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (happy_var_1 happy_var_2
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 9 18 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_8) `HappyStk`
	(HappyTerminal (L.Identifier happy_var_7)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Identifier happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Identifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (\b -> Range (Just happy_var_3) happy_var_5 (Obj happy_var_7 : happy_var_8) b
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 9 18 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_8) `HappyStk`
	(HappyTerminal (L.Identifier happy_var_7)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Identifier happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (\b -> Range Nothing happy_var_5 (Obj happy_var_7 : happy_var_8) b
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 4 19 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	(HappyTerminal (L.Identifier happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (Path (Obj happy_var_2 : happy_var_3)
	) `HappyStk` happyRest

happyReduce_31 = happySpecReduce_1  20 happyReduction_31
happyReduction_31 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn20
		 ([ happy_var_1 ]
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  20 happyReduction_32
happyReduction_32 (HappyAbsSyn21  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_2 : happy_var_1
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_2  21 happyReduction_33
happyReduction_33 (HappyTerminal (L.Identifier happy_var_2))
	_
	 =  HappyAbsSyn21
		 (Obj happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happyReduce 5 21 happyReduction_34
happyReduction_34 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Identifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Obj happy_var_3
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_3  21 happyReduction_35
happyReduction_35 _
	(HappyTerminal (L.IntLit _ happy_var_2))
	_
	 =  HappyAbsSyn21
		 (Arr happy_var_2
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  22 happyReduction_36
happyReduction_36 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  22 happyReduction_37
happyReduction_37 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  22 happyReduction_38
happyReduction_38 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  22 happyReduction_39
happyReduction_39 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  22 happyReduction_40
happyReduction_40 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  22 happyReduction_41
happyReduction_41 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  22 happyReduction_42
happyReduction_42 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  22 happyReduction_43
happyReduction_43 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  22 happyReduction_44
happyReduction_44 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  22 happyReduction_45
happyReduction_45 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  22 happyReduction_46
happyReduction_46 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (happy_var_2
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 54 54 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	L.Identifier happy_dollar_dollar -> cont 23;
	L.StringTem happy_dollar_dollar -> cont 24;
	L.NumLit _ happy_dollar_dollar -> cont 25;
	L.IntLit _ happy_dollar_dollar -> cont 26;
	L.Identifier "if" -> cont 27;
	L.Identifier "else" -> cont 28;
	L.Identifier "end" -> cont 29;
	L.Identifier "null" -> cont 30;
	L.Identifier "range" -> cont 31;
	L.Identifier "escapeUri" -> cont 32;
	L.BoolLit True -> cont 33;
	L.BoolLit False -> cont 34;
	L.SingleQuote -> cont 35;
	L.Colon -> cont 36;
	L.Dot -> cont 37;
	L.Comma -> cont 38;
	L.Eq -> cont 39;
	L.Gt -> cont 40;
	L.Lt -> cont 41;
	L.And -> cont 42;
	L.Or -> cont 43;
	L.Underscore -> cont 44;
	L.Assignment -> cont 45;
	L.CurlyOpen -> cont 46;
	L.CurlyClose -> cont 47;
	L.DoubleCurlyOpen -> cont 48;
	L.DoubleCurlyClose -> cont 49;
	L.SquareOpen -> cont 50;
	L.SquareClose -> cont 51;
	L.ParenOpen -> cont 52;
	L.ParenClose -> cont 53;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 54 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(L.Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parser tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [L.Token] -> a
parseError toks = error $ "Parse error: " <> show toks

data Accessor = Obj T.Text | Arr Int
  deriving (Show, Eq, Read)

renderAccessor :: Accessor -> T.Text
renderAccessor = \case
  Obj txt -> txt
  Arr i -> T.pack $ show i

renderPath :: [Accessor] -> T.Text
renderPath = mconcat . List.intersperse "." . fmap renderAccessor

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
  | Path [Accessor]
  | Iff ValueExt ValueExt ValueExt
  | Eq ValueExt ValueExt
  | Gt ValueExt ValueExt
  | Lt ValueExt ValueExt
  | And ValueExt ValueExt
  | Or ValueExt ValueExt
  | Member ValueExt ValueExt
  | Range (Maybe T.Text) T.Text [Accessor] ValueExt
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
