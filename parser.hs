{-# OPTIONS_GHC -w #-}
module Main where

import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22
	= HappyTerminal (Token)
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
happyExpList = Happy_Data_Array.listArray (0,327) ([0,53696,21505,1,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7452,5440,0,0,0,0,0,0,0,0,16384,512,0,0,0,0,0,0,0,0,1863,1360,0,0,0,0,0,0,0,0,1,16,0,2176,0,0,36352,40974,14,49152,465,340,0,0,16384,0,0,64,2,0,0,0,0,0,0,0,128,8195,2,4096,0,256,0,32768,64,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,128,4,0,0,0,0,16,0,0,2048,16,0,0,0,16,0,256,0,0,0,0,2,8192,0,0,0,0,0,0,0,0,0,28672,116,85,0,4,0,0,0,0,0,0,512,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,0,0,2296,0,0,0,0,0,2,0,0,12296,8704,0,18176,20487,5,0,0,0,0,0,0,0,0,0,0,0,63488,128,0,3074,2176,0,32832,4097,1,2048,48,34,0,1537,1088,0,49184,34816,0,7168,16413,21,32768,0,0,0,16,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,16,0,0,0,0,0,0,1,0,0,512,0,0,16384,0,0,4096,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,49152,465,340,0,14904,10880,0,0,0,0,0,0,16,0,0,512,0,0,16384,0,28672,116,85,0,0,0,0,0,0,0,0,0,2,0,32,0,0,0,4096,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseKriti","string_lit","num_lit","boolean","null","array","list_elements","object","object_fields","object_field","operator","functions","iff","predicate","range","range_decl","path","path_list","path_element","value","ident","string","number","'if'","'else'","'end'","'null'","'range'","'escapeUri'","'true'","'false'","'\\''","':'","'.'","','","'=='","'>'","'<'","'&&'","'||'","'{'","'}'","'{{'","'}}'","'['","']'","'('","')'","'_'","':='","%eof"]
        bit_start = st Prelude.* 53
        bit_end = (st Prelude.+ 1) Prelude.* 53
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..52]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (23) = happyShift action_15
action_0 (24) = happyShift action_2
action_0 (25) = happyShift action_16
action_0 (29) = happyShift action_17
action_0 (31) = happyShift action_18
action_0 (32) = happyShift action_19
action_0 (33) = happyShift action_20
action_0 (43) = happyShift action_21
action_0 (45) = happyShift action_22
action_0 (47) = happyShift action_23
action_0 (49) = happyShift action_24
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

action_12 (23) = happyShift action_15
action_12 (24) = happyShift action_2
action_12 (25) = happyShift action_16
action_12 (29) = happyShift action_17
action_12 (31) = happyShift action_18
action_12 (32) = happyShift action_19
action_12 (33) = happyShift action_20
action_12 (43) = happyShift action_21
action_12 (45) = happyShift action_22
action_12 (47) = happyShift action_23
action_12 (49) = happyShift action_24
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
action_12 (22) = happyGoto action_40
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_42

action_14 (53) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (36) = happyShift action_38
action_15 (47) = happyShift action_39
action_15 (20) = happyGoto action_36
action_15 (21) = happyGoto action_37
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_2

action_17 _ = happyReduce_5

action_18 (23) = happyShift action_15
action_18 (24) = happyShift action_2
action_18 (25) = happyShift action_16
action_18 (29) = happyShift action_17
action_18 (31) = happyShift action_18
action_18 (32) = happyShift action_19
action_18 (33) = happyShift action_20
action_18 (43) = happyShift action_21
action_18 (45) = happyShift action_22
action_18 (47) = happyShift action_23
action_18 (49) = happyShift action_24
action_18 (4) = happyGoto action_3
action_18 (5) = happyGoto action_4
action_18 (6) = happyGoto action_5
action_18 (7) = happyGoto action_6
action_18 (8) = happyGoto action_7
action_18 (10) = happyGoto action_8
action_18 (14) = happyGoto action_9
action_18 (15) = happyGoto action_10
action_18 (17) = happyGoto action_11
action_18 (18) = happyGoto action_12
action_18 (19) = happyGoto action_13
action_18 (22) = happyGoto action_35
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_3

action_20 _ = happyReduce_4

action_21 (24) = happyShift action_33
action_21 (44) = happyShift action_34
action_21 (11) = happyGoto action_31
action_21 (12) = happyGoto action_32
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (26) = happyShift action_29
action_22 (30) = happyShift action_30
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (23) = happyShift action_15
action_23 (24) = happyShift action_2
action_23 (25) = happyShift action_16
action_23 (29) = happyShift action_17
action_23 (31) = happyShift action_18
action_23 (32) = happyShift action_19
action_23 (33) = happyShift action_20
action_23 (43) = happyShift action_21
action_23 (45) = happyShift action_22
action_23 (47) = happyShift action_23
action_23 (48) = happyShift action_28
action_23 (49) = happyShift action_24
action_23 (4) = happyGoto action_3
action_23 (5) = happyGoto action_4
action_23 (6) = happyGoto action_5
action_23 (7) = happyGoto action_6
action_23 (8) = happyGoto action_7
action_23 (9) = happyGoto action_26
action_23 (10) = happyGoto action_8
action_23 (14) = happyGoto action_9
action_23 (15) = happyGoto action_10
action_23 (17) = happyGoto action_11
action_23 (18) = happyGoto action_12
action_23 (19) = happyGoto action_13
action_23 (22) = happyGoto action_27
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (23) = happyShift action_15
action_24 (24) = happyShift action_2
action_24 (25) = happyShift action_16
action_24 (29) = happyShift action_17
action_24 (31) = happyShift action_18
action_24 (32) = happyShift action_19
action_24 (33) = happyShift action_20
action_24 (43) = happyShift action_21
action_24 (45) = happyShift action_22
action_24 (47) = happyShift action_23
action_24 (49) = happyShift action_24
action_24 (4) = happyGoto action_3
action_24 (5) = happyGoto action_4
action_24 (6) = happyGoto action_5
action_24 (7) = happyGoto action_6
action_24 (8) = happyGoto action_7
action_24 (10) = happyGoto action_8
action_24 (14) = happyGoto action_9
action_24 (15) = happyGoto action_10
action_24 (17) = happyGoto action_11
action_24 (18) = happyGoto action_12
action_24 (19) = happyGoto action_13
action_24 (22) = happyGoto action_25
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (50) = happyShift action_60
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (37) = happyShift action_58
action_26 (48) = happyShift action_59
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_9

action_28 _ = happyReduce_8

action_29 (23) = happyShift action_15
action_29 (32) = happyShift action_19
action_29 (33) = happyShift action_20
action_29 (45) = happyShift action_56
action_29 (49) = happyShift action_57
action_29 (6) = happyGoto action_51
action_29 (13) = happyGoto action_52
action_29 (15) = happyGoto action_53
action_29 (16) = happyGoto action_54
action_29 (19) = happyGoto action_55
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (23) = happyShift action_49
action_30 (51) = happyShift action_50
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (37) = happyShift action_47
action_31 (44) = happyShift action_48
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_12

action_33 (35) = happyShift action_46
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_6

action_35 _ = happyReduce_20

action_36 (36) = happyShift action_38
action_36 (47) = happyShift action_39
action_36 (21) = happyGoto action_45
action_36 _ = happyReduce_30

action_37 _ = happyReduce_31

action_38 (23) = happyShift action_44
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (25) = happyShift action_42
action_39 (34) = happyShift action_43
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (45) = happyShift action_41
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (28) = happyShift action_75
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (48) = happyShift action_74
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (23) = happyShift action_73
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_33

action_45 _ = happyReduce_32

action_46 (23) = happyShift action_15
action_46 (24) = happyShift action_2
action_46 (25) = happyShift action_16
action_46 (29) = happyShift action_17
action_46 (31) = happyShift action_18
action_46 (32) = happyShift action_19
action_46 (33) = happyShift action_20
action_46 (43) = happyShift action_21
action_46 (45) = happyShift action_22
action_46 (47) = happyShift action_23
action_46 (49) = happyShift action_24
action_46 (4) = happyGoto action_3
action_46 (5) = happyGoto action_4
action_46 (6) = happyGoto action_5
action_46 (7) = happyGoto action_6
action_46 (8) = happyGoto action_7
action_46 (10) = happyGoto action_8
action_46 (14) = happyGoto action_9
action_46 (15) = happyGoto action_10
action_46 (17) = happyGoto action_11
action_46 (18) = happyGoto action_12
action_46 (19) = happyGoto action_13
action_46 (22) = happyGoto action_72
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (24) = happyShift action_33
action_47 (12) = happyGoto action_71
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_11

action_49 (37) = happyShift action_70
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (37) = happyShift action_69
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_25

action_52 _ = happyReduce_24

action_53 _ = happyReduce_23

action_54 (38) = happyShift action_63
action_54 (39) = happyShift action_64
action_54 (40) = happyShift action_65
action_54 (41) = happyShift action_66
action_54 (42) = happyShift action_67
action_54 (46) = happyShift action_68
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_22

action_56 (26) = happyShift action_29
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (23) = happyShift action_15
action_57 (32) = happyShift action_19
action_57 (33) = happyShift action_20
action_57 (45) = happyShift action_56
action_57 (49) = happyShift action_57
action_57 (6) = happyGoto action_51
action_57 (13) = happyGoto action_52
action_57 (15) = happyGoto action_53
action_57 (16) = happyGoto action_62
action_57 (19) = happyGoto action_55
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (23) = happyShift action_15
action_58 (24) = happyShift action_2
action_58 (25) = happyShift action_16
action_58 (29) = happyShift action_17
action_58 (31) = happyShift action_18
action_58 (32) = happyShift action_19
action_58 (33) = happyShift action_20
action_58 (43) = happyShift action_21
action_58 (45) = happyShift action_22
action_58 (47) = happyShift action_23
action_58 (49) = happyShift action_24
action_58 (4) = happyGoto action_3
action_58 (5) = happyGoto action_4
action_58 (6) = happyGoto action_5
action_58 (7) = happyGoto action_6
action_58 (8) = happyGoto action_7
action_58 (10) = happyGoto action_8
action_58 (14) = happyGoto action_9
action_58 (15) = happyGoto action_10
action_58 (17) = happyGoto action_11
action_58 (18) = happyGoto action_12
action_58 (19) = happyGoto action_13
action_58 (22) = happyGoto action_61
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_7

action_60 _ = happyReduce_46

action_61 _ = happyReduce_10

action_62 (38) = happyShift action_63
action_62 (39) = happyShift action_64
action_62 (40) = happyShift action_65
action_62 (41) = happyShift action_66
action_62 (42) = happyShift action_67
action_62 (50) = happyShift action_86
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (23) = happyShift action_15
action_63 (32) = happyShift action_19
action_63 (33) = happyShift action_20
action_63 (45) = happyShift action_56
action_63 (49) = happyShift action_57
action_63 (6) = happyGoto action_51
action_63 (13) = happyGoto action_52
action_63 (15) = happyGoto action_53
action_63 (16) = happyGoto action_85
action_63 (19) = happyGoto action_55
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (23) = happyShift action_15
action_64 (32) = happyShift action_19
action_64 (33) = happyShift action_20
action_64 (45) = happyShift action_56
action_64 (49) = happyShift action_57
action_64 (6) = happyGoto action_51
action_64 (13) = happyGoto action_52
action_64 (15) = happyGoto action_53
action_64 (16) = happyGoto action_84
action_64 (19) = happyGoto action_55
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (23) = happyShift action_15
action_65 (32) = happyShift action_19
action_65 (33) = happyShift action_20
action_65 (45) = happyShift action_56
action_65 (49) = happyShift action_57
action_65 (6) = happyGoto action_51
action_65 (13) = happyGoto action_52
action_65 (15) = happyGoto action_53
action_65 (16) = happyGoto action_83
action_65 (19) = happyGoto action_55
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (23) = happyShift action_15
action_66 (32) = happyShift action_19
action_66 (33) = happyShift action_20
action_66 (45) = happyShift action_56
action_66 (49) = happyShift action_57
action_66 (6) = happyGoto action_51
action_66 (13) = happyGoto action_52
action_66 (15) = happyGoto action_53
action_66 (16) = happyGoto action_82
action_66 (19) = happyGoto action_55
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (23) = happyShift action_15
action_67 (32) = happyShift action_19
action_67 (33) = happyShift action_20
action_67 (45) = happyShift action_56
action_67 (49) = happyShift action_57
action_67 (6) = happyGoto action_51
action_67 (13) = happyGoto action_52
action_67 (15) = happyGoto action_53
action_67 (16) = happyGoto action_81
action_67 (19) = happyGoto action_55
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (23) = happyShift action_15
action_68 (24) = happyShift action_2
action_68 (25) = happyShift action_16
action_68 (29) = happyShift action_17
action_68 (31) = happyShift action_18
action_68 (32) = happyShift action_19
action_68 (33) = happyShift action_20
action_68 (43) = happyShift action_21
action_68 (45) = happyShift action_22
action_68 (47) = happyShift action_23
action_68 (49) = happyShift action_24
action_68 (4) = happyGoto action_3
action_68 (5) = happyGoto action_4
action_68 (6) = happyGoto action_5
action_68 (7) = happyGoto action_6
action_68 (8) = happyGoto action_7
action_68 (10) = happyGoto action_8
action_68 (14) = happyGoto action_9
action_68 (15) = happyGoto action_10
action_68 (17) = happyGoto action_11
action_68 (18) = happyGoto action_12
action_68 (19) = happyGoto action_13
action_68 (22) = happyGoto action_80
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (23) = happyShift action_79
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (23) = happyShift action_78
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_13

action_72 _ = happyReduce_14

action_73 (34) = happyShift action_77
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_35

action_75 (46) = happyShift action_76
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_27

action_77 (48) = happyShift action_90
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (52) = happyShift action_89
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (52) = happyShift action_88
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (45) = happyShift action_87
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_19

action_82 _ = happyReduce_18

action_83 _ = happyReduce_16

action_84 _ = happyReduce_15

action_85 _ = happyReduce_17

action_86 _ = happyReduce_26

action_87 (27) = happyShift action_93
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (23) = happyShift action_15
action_88 (24) = happyShift action_2
action_88 (25) = happyShift action_16
action_88 (29) = happyShift action_17
action_88 (31) = happyShift action_18
action_88 (32) = happyShift action_19
action_88 (33) = happyShift action_20
action_88 (43) = happyShift action_21
action_88 (45) = happyShift action_22
action_88 (47) = happyShift action_23
action_88 (49) = happyShift action_24
action_88 (4) = happyGoto action_3
action_88 (5) = happyGoto action_4
action_88 (6) = happyGoto action_5
action_88 (7) = happyGoto action_6
action_88 (8) = happyGoto action_7
action_88 (10) = happyGoto action_8
action_88 (14) = happyGoto action_9
action_88 (15) = happyGoto action_10
action_88 (17) = happyGoto action_11
action_88 (18) = happyGoto action_12
action_88 (19) = happyGoto action_13
action_88 (22) = happyGoto action_92
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (23) = happyShift action_15
action_89 (24) = happyShift action_2
action_89 (25) = happyShift action_16
action_89 (29) = happyShift action_17
action_89 (31) = happyShift action_18
action_89 (32) = happyShift action_19
action_89 (33) = happyShift action_20
action_89 (43) = happyShift action_21
action_89 (45) = happyShift action_22
action_89 (47) = happyShift action_23
action_89 (49) = happyShift action_24
action_89 (4) = happyGoto action_3
action_89 (5) = happyGoto action_4
action_89 (6) = happyGoto action_5
action_89 (7) = happyGoto action_6
action_89 (8) = happyGoto action_7
action_89 (10) = happyGoto action_8
action_89 (14) = happyGoto action_9
action_89 (15) = happyGoto action_10
action_89 (17) = happyGoto action_11
action_89 (18) = happyGoto action_12
action_89 (19) = happyGoto action_13
action_89 (22) = happyGoto action_91
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_34

action_91 (46) = happyShift action_96
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (46) = happyShift action_95
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (46) = happyShift action_94
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (23) = happyShift action_15
action_94 (24) = happyShift action_2
action_94 (25) = happyShift action_16
action_94 (29) = happyShift action_17
action_94 (31) = happyShift action_18
action_94 (32) = happyShift action_19
action_94 (33) = happyShift action_20
action_94 (43) = happyShift action_21
action_94 (45) = happyShift action_22
action_94 (47) = happyShift action_23
action_94 (49) = happyShift action_24
action_94 (4) = happyGoto action_3
action_94 (5) = happyGoto action_4
action_94 (6) = happyGoto action_5
action_94 (7) = happyGoto action_6
action_94 (8) = happyGoto action_7
action_94 (10) = happyGoto action_8
action_94 (14) = happyGoto action_9
action_94 (15) = happyGoto action_10
action_94 (17) = happyGoto action_11
action_94 (18) = happyGoto action_12
action_94 (19) = happyGoto action_13
action_94 (22) = happyGoto action_97
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_29

action_96 _ = happyReduce_28

action_97 (45) = happyShift action_98
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (28) = happyShift action_99
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (46) = happyShift action_100
action_99 _ = happyFail (happyExpListPerState 99)

action_100 _ = happyReduce_21

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (StringLit happy_var_1))
	 =  HappyAbsSyn4
		 (StringLit happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (NumLit happy_var_1))
	 =  HappyAbsSyn5
		 (NumLit happy_var_1
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
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn8
		 (Array happy_var_1
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 _
	_
	 =  HappyAbsSyn8
		 (Array []
	)

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn9
		 ([ happy_var_1 ]
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_3 : happy_var_1
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 _
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn10
		 (Object (M.fromList happy_var_1)
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
happyReduction_14 _
	(HappyTerminal happy_var_2)
	(HappyTerminal (StringLit happy_var_1))
	 =  HappyAbsSyn12
		 ((happy_var_1, happy_var_2)
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

happyReduce_28 = happyReduce 8 18 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Identifier happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Identifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (\b -> Range (Just happy_var_3) happy_var_5 happy_var_7 b
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 8 18 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Identifier happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (\b -> Range Nothing happy_var_5 happy_var_7 b
	) `HappyStk` happyRest

happyReduce_30 = happySpecReduce_2  19 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_2)
	(HappyTerminal (Identifier happy_var_1))
	 =  HappyAbsSyn19
		 (Path (Obj happy_var_1 : happy_var_2 )
	)
happyReduction_30 _ _  = notHappyAtAll 

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
happyReduction_33 (HappyTerminal (Identifier happy_var_2))
	_
	 =  HappyAbsSyn21
		 (Obj happy_var_2
	)
happyReduction_33 _ _  = notHappyAtAll 

happyReduce_34 = happyReduce 5 21 happyReduction_34
happyReduction_34 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Identifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 (Obj happy_var_3
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_3  21 happyReduction_35
happyReduction_35 _
	(HappyTerminal (NumLit happy_var_2))
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
	action 53 53 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Identifier happy_dollar_dollar -> cont 23;
	StringLit happy_dollar_dollar -> cont 24;
	NumLit happy_dollar_dollar -> cont 25;
	Identifier "if" -> cont 26;
	Identifier "else" -> cont 27;
	Identifier "end" -> cont 28;
	Identifier "null" -> cont 29;
	Identifier 'range' -> cont 30;
	Identifier "escapeUri" -> cont 31;
	BoolLit True -> cont 32;
	BoolLit False -> cont 33;
	SingleQuote -> cont 34;
	Colon -> cont 35;
	Colon -> cont 36;
	Comma -> cont 37;
	Eq -> cont 38;
	Gt -> cont 39;
	Lt -> cont 40;
	And -> cont 41;
	Or -> cont 42;
	CurlyOpen -> cont 43;
	CurlyClose -> cont 44;
	DoubleCurlyOpen -> cont 45;
	DoubleCurlyClose -> cont 46;
	SquareOpen -> cont 47;
	SquareClose -> cont 48;
	ParenOpen -> cont 49;
	ParenClose -> cont 50;
	Underscore -> cont 51;
	Assignment -> cont 52;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 53 tk tks = happyError' (tks, explist)
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
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parseKriti tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn22 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Accessor = Obj Text | Arr Int
  deriving (Show, Eq, Read)

data ValueExt
  = -- Core Aeson Terms
    Object (M.HashMap Text ValueExt)
  | Array (V.Vector ValueExt)
  | String Text
  | Number Scientific
  | Boolean Bool
  | Null
  | -- Extended Terms
    StringInterp [ValueExt]
  | Path [Accessor]
  | Iff ValueExt ValueExt ValueExt
  | Eq ValueExt ValueExt
  | Gt ValueExt ValueExt
  | Lt ValueExt ValueExt
  | And ValueExt ValueExt
  | Or ValueExt ValueExt
  | Member ValueExt ValueExt
  | Range (Maybe Text) Text [Accessor] ValueExt
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
