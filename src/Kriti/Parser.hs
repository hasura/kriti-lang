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

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21 t22 t23
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
	| HappyAbsSyn23 t23

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,277) ([0,25600,16391,21,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,118,340,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,2048,0,50176,4,0,0,7568,29952,0,51200,32782,42,0,0,8192,0,0,32,4,0,0,0,0,0,0,0,0,0,32,0,16128,34816,0,0,16386,0,0,3784,10880,0,0,32,4,0,8192,64,0,0,0,0,0,512,0,0,0,0,0,0,0,8,0,8,0,0,51200,32782,42,0,256,0,0,0,0,0,0,2048,256,0,0,0,0,0,8,0,0,24576,0,0,0,0,8,0,32768,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2110,0,0,0,0,32768,0,0,0,61440,32771,8,0,0,0,0,60544,43008,2,0,0,0,0,0,0,0,0,0,0,0,0,16415,0,57344,7,17,0,1008,2176,0,63488,16385,4,0,252,544,0,32256,4096,1,8192,59,170,0,512,0,0,0,1,0,0,0,0,0,0,0,4,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,1024,0,0,0,0,0,0,0,4,0,0,512,0,0,0,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,0,0,0,1024,0,0,0,2,0,0,0,4096,0,0,0,0,0,0,256,0,0,32768,0,0,0,64,0,30272,21504,1,0,0,0,0,0,0,0,0,0,2,0,2,0,0,0,0,1,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","string_lit","num_lit","boolean","null","array","list_elements","object","object_fields","object_field","operator","functions","iff","predicate","range","range_decl","path","path_vector","path_tail","path_element","value","'if'","'else'","'end'","'null'","'range'","'escapeUri'","'true'","'false'","ident","string","number","int","'\\''","':'","'.'","','","'=='","'>'","'<'","'&&'","'||'","'_'","':='","'{'","'}'","'{{'","'}}'","'['","']'","'('","')'","%eof"]
        bit_start = st Prelude.* 55
        bit_end = (st Prelude.+ 1) Prelude.* 55
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..54]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (27) = happyShift action_15
action_0 (30) = happyShift action_16
action_0 (31) = happyShift action_17
action_0 (33) = happyShift action_2
action_0 (34) = happyShift action_18
action_0 (35) = happyShift action_19
action_0 (47) = happyShift action_20
action_0 (49) = happyShift action_21
action_0 (51) = happyShift action_22
action_0 (53) = happyShift action_23
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
action_0 (23) = happyGoto action_14
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (33) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 _ = happyReduce_41

action_4 _ = happyReduce_42

action_5 _ = happyReduce_43

action_6 _ = happyReduce_44

action_7 _ = happyReduce_45

action_8 _ = happyReduce_46

action_9 _ = happyReduce_49

action_10 _ = happyReduce_48

action_11 _ = happyReduce_50

action_12 (27) = happyShift action_15
action_12 (30) = happyShift action_16
action_12 (31) = happyShift action_17
action_12 (33) = happyShift action_2
action_12 (34) = happyShift action_18
action_12 (35) = happyShift action_19
action_12 (47) = happyShift action_20
action_12 (49) = happyShift action_21
action_12 (51) = happyShift action_22
action_12 (53) = happyShift action_23
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
action_12 (23) = happyGoto action_37
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_47

action_14 (55) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 _ = happyReduce_6

action_16 _ = happyReduce_4

action_17 _ = happyReduce_5

action_18 _ = happyReduce_2

action_19 _ = happyReduce_3

action_20 (33) = happyShift action_35
action_20 (48) = happyShift action_36
action_20 (11) = happyGoto action_33
action_20 (12) = happyGoto action_34
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (24) = happyShift action_29
action_21 (28) = happyShift action_30
action_21 (29) = happyShift action_31
action_21 (32) = happyShift action_32
action_21 (20) = happyGoto action_28
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (27) = happyShift action_15
action_22 (30) = happyShift action_16
action_22 (31) = happyShift action_17
action_22 (33) = happyShift action_2
action_22 (34) = happyShift action_18
action_22 (35) = happyShift action_19
action_22 (47) = happyShift action_20
action_22 (49) = happyShift action_21
action_22 (51) = happyShift action_22
action_22 (52) = happyShift action_27
action_22 (53) = happyShift action_23
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
action_22 (23) = happyGoto action_26
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (27) = happyShift action_15
action_23 (30) = happyShift action_16
action_23 (31) = happyShift action_17
action_23 (33) = happyShift action_2
action_23 (34) = happyShift action_18
action_23 (35) = happyShift action_19
action_23 (47) = happyShift action_20
action_23 (49) = happyShift action_21
action_23 (51) = happyShift action_22
action_23 (53) = happyShift action_23
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
action_23 (23) = happyGoto action_24
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (54) = happyShift action_61
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (39) = happyShift action_59
action_25 (52) = happyShift action_60
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_10

action_27 _ = happyReduce_9

action_28 (50) = happyShift action_58
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (30) = happyShift action_16
action_29 (31) = happyShift action_17
action_29 (32) = happyShift action_32
action_29 (33) = happyShift action_2
action_29 (34) = happyShift action_18
action_29 (35) = happyShift action_19
action_29 (49) = happyShift action_56
action_29 (53) = happyShift action_57
action_29 (4) = happyGoto action_49
action_29 (5) = happyGoto action_50
action_29 (6) = happyGoto action_51
action_29 (13) = happyGoto action_52
action_29 (15) = happyGoto action_53
action_29 (16) = happyGoto action_54
action_29 (20) = happyGoto action_55
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (32) = happyShift action_47
action_30 (45) = happyShift action_48
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (27) = happyShift action_15
action_31 (30) = happyShift action_16
action_31 (31) = happyShift action_17
action_31 (33) = happyShift action_2
action_31 (34) = happyShift action_18
action_31 (35) = happyShift action_19
action_31 (47) = happyShift action_20
action_31 (49) = happyShift action_21
action_31 (51) = happyShift action_22
action_31 (53) = happyShift action_23
action_31 (4) = happyGoto action_3
action_31 (5) = happyGoto action_4
action_31 (6) = happyGoto action_5
action_31 (7) = happyGoto action_6
action_31 (8) = happyGoto action_7
action_31 (10) = happyGoto action_8
action_31 (14) = happyGoto action_9
action_31 (15) = happyGoto action_10
action_31 (17) = happyGoto action_11
action_31 (18) = happyGoto action_12
action_31 (19) = happyGoto action_13
action_31 (23) = happyGoto action_46
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (38) = happyShift action_44
action_32 (51) = happyShift action_45
action_32 (21) = happyGoto action_42
action_32 (22) = happyGoto action_43
action_32 _ = happyReduce_35

action_33 (39) = happyShift action_40
action_33 (48) = happyShift action_41
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_13

action_35 (37) = happyShift action_39
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_7

action_37 (49) = happyShift action_38
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (26) = happyShift action_79
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (27) = happyShift action_15
action_39 (30) = happyShift action_16
action_39 (31) = happyShift action_17
action_39 (33) = happyShift action_2
action_39 (34) = happyShift action_18
action_39 (35) = happyShift action_19
action_39 (47) = happyShift action_20
action_39 (49) = happyShift action_21
action_39 (51) = happyShift action_22
action_39 (53) = happyShift action_23
action_39 (4) = happyGoto action_3
action_39 (5) = happyGoto action_4
action_39 (6) = happyGoto action_5
action_39 (7) = happyGoto action_6
action_39 (8) = happyGoto action_7
action_39 (10) = happyGoto action_8
action_39 (14) = happyGoto action_9
action_39 (15) = happyGoto action_10
action_39 (17) = happyGoto action_11
action_39 (18) = happyGoto action_12
action_39 (19) = happyGoto action_13
action_39 (23) = happyGoto action_78
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (33) = happyShift action_35
action_40 (12) = happyGoto action_77
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_12

action_42 (38) = happyShift action_44
action_42 (51) = happyShift action_45
action_42 (22) = happyGoto action_76
action_42 _ = happyReduce_34

action_43 _ = happyReduce_36

action_44 (32) = happyShift action_75
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (35) = happyShift action_73
action_45 (36) = happyShift action_74
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (50) = happyShift action_72
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (39) = happyShift action_71
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (39) = happyShift action_70
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_28

action_50 _ = happyReduce_27

action_51 _ = happyReduce_26

action_52 _ = happyReduce_25

action_53 _ = happyReduce_24

action_54 (40) = happyShift action_64
action_54 (41) = happyShift action_65
action_54 (42) = happyShift action_66
action_54 (43) = happyShift action_67
action_54 (44) = happyShift action_68
action_54 (50) = happyShift action_69
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_23

action_56 (24) = happyShift action_29
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (30) = happyShift action_16
action_57 (31) = happyShift action_17
action_57 (32) = happyShift action_32
action_57 (33) = happyShift action_2
action_57 (34) = happyShift action_18
action_57 (35) = happyShift action_19
action_57 (49) = happyShift action_56
action_57 (53) = happyShift action_57
action_57 (4) = happyGoto action_49
action_57 (5) = happyGoto action_50
action_57 (6) = happyGoto action_51
action_57 (13) = happyGoto action_52
action_57 (15) = happyGoto action_53
action_57 (16) = happyGoto action_63
action_57 (20) = happyGoto action_55
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_33

action_59 (27) = happyShift action_15
action_59 (30) = happyShift action_16
action_59 (31) = happyShift action_17
action_59 (33) = happyShift action_2
action_59 (34) = happyShift action_18
action_59 (35) = happyShift action_19
action_59 (47) = happyShift action_20
action_59 (49) = happyShift action_21
action_59 (51) = happyShift action_22
action_59 (53) = happyShift action_23
action_59 (4) = happyGoto action_3
action_59 (5) = happyGoto action_4
action_59 (6) = happyGoto action_5
action_59 (7) = happyGoto action_6
action_59 (8) = happyGoto action_7
action_59 (10) = happyGoto action_8
action_59 (14) = happyGoto action_9
action_59 (15) = happyGoto action_10
action_59 (17) = happyGoto action_11
action_59 (18) = happyGoto action_12
action_59 (19) = happyGoto action_13
action_59 (23) = happyGoto action_62
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_8

action_61 _ = happyReduce_51

action_62 _ = happyReduce_11

action_63 (40) = happyShift action_64
action_63 (41) = happyShift action_65
action_63 (42) = happyShift action_66
action_63 (43) = happyShift action_67
action_63 (44) = happyShift action_68
action_63 (54) = happyShift action_91
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (30) = happyShift action_16
action_64 (31) = happyShift action_17
action_64 (32) = happyShift action_32
action_64 (33) = happyShift action_2
action_64 (34) = happyShift action_18
action_64 (35) = happyShift action_19
action_64 (49) = happyShift action_56
action_64 (53) = happyShift action_57
action_64 (4) = happyGoto action_49
action_64 (5) = happyGoto action_50
action_64 (6) = happyGoto action_51
action_64 (13) = happyGoto action_52
action_64 (15) = happyGoto action_53
action_64 (16) = happyGoto action_90
action_64 (20) = happyGoto action_55
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (30) = happyShift action_16
action_65 (31) = happyShift action_17
action_65 (32) = happyShift action_32
action_65 (33) = happyShift action_2
action_65 (34) = happyShift action_18
action_65 (35) = happyShift action_19
action_65 (49) = happyShift action_56
action_65 (53) = happyShift action_57
action_65 (4) = happyGoto action_49
action_65 (5) = happyGoto action_50
action_65 (6) = happyGoto action_51
action_65 (13) = happyGoto action_52
action_65 (15) = happyGoto action_53
action_65 (16) = happyGoto action_89
action_65 (20) = happyGoto action_55
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (30) = happyShift action_16
action_66 (31) = happyShift action_17
action_66 (32) = happyShift action_32
action_66 (33) = happyShift action_2
action_66 (34) = happyShift action_18
action_66 (35) = happyShift action_19
action_66 (49) = happyShift action_56
action_66 (53) = happyShift action_57
action_66 (4) = happyGoto action_49
action_66 (5) = happyGoto action_50
action_66 (6) = happyGoto action_51
action_66 (13) = happyGoto action_52
action_66 (15) = happyGoto action_53
action_66 (16) = happyGoto action_88
action_66 (20) = happyGoto action_55
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (30) = happyShift action_16
action_67 (31) = happyShift action_17
action_67 (32) = happyShift action_32
action_67 (33) = happyShift action_2
action_67 (34) = happyShift action_18
action_67 (35) = happyShift action_19
action_67 (49) = happyShift action_56
action_67 (53) = happyShift action_57
action_67 (4) = happyGoto action_49
action_67 (5) = happyGoto action_50
action_67 (6) = happyGoto action_51
action_67 (13) = happyGoto action_52
action_67 (15) = happyGoto action_53
action_67 (16) = happyGoto action_87
action_67 (20) = happyGoto action_55
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (30) = happyShift action_16
action_68 (31) = happyShift action_17
action_68 (32) = happyShift action_32
action_68 (33) = happyShift action_2
action_68 (34) = happyShift action_18
action_68 (35) = happyShift action_19
action_68 (49) = happyShift action_56
action_68 (53) = happyShift action_57
action_68 (4) = happyGoto action_49
action_68 (5) = happyGoto action_50
action_68 (6) = happyGoto action_51
action_68 (13) = happyGoto action_52
action_68 (15) = happyGoto action_53
action_68 (16) = happyGoto action_86
action_68 (20) = happyGoto action_55
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (27) = happyShift action_15
action_69 (30) = happyShift action_16
action_69 (31) = happyShift action_17
action_69 (33) = happyShift action_2
action_69 (34) = happyShift action_18
action_69 (35) = happyShift action_19
action_69 (47) = happyShift action_20
action_69 (49) = happyShift action_21
action_69 (51) = happyShift action_22
action_69 (53) = happyShift action_23
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
action_69 (23) = happyGoto action_85
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (32) = happyShift action_84
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (32) = happyShift action_83
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_21

action_73 (52) = happyShift action_82
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (32) = happyShift action_81
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_38

action_76 _ = happyReduce_37

action_77 _ = happyReduce_14

action_78 _ = happyReduce_15

action_79 (50) = happyShift action_80
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_30

action_81 (36) = happyShift action_95
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_40

action_83 (46) = happyShift action_94
action_83 _ = happyFail (happyExpListPerState 83)

action_84 (46) = happyShift action_93
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (49) = happyShift action_92
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_20

action_87 _ = happyReduce_19

action_88 _ = happyReduce_17

action_89 _ = happyReduce_16

action_90 _ = happyReduce_18

action_91 _ = happyReduce_29

action_92 (25) = happyShift action_99
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (32) = happyShift action_32
action_93 (20) = happyGoto action_98
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (32) = happyShift action_32
action_94 (20) = happyGoto action_97
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (52) = happyShift action_96
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_39

action_97 (50) = happyShift action_102
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (50) = happyShift action_101
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (50) = happyShift action_100
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (27) = happyShift action_15
action_100 (30) = happyShift action_16
action_100 (31) = happyShift action_17
action_100 (33) = happyShift action_2
action_100 (34) = happyShift action_18
action_100 (35) = happyShift action_19
action_100 (47) = happyShift action_20
action_100 (49) = happyShift action_21
action_100 (51) = happyShift action_22
action_100 (53) = happyShift action_23
action_100 (4) = happyGoto action_3
action_100 (5) = happyGoto action_4
action_100 (6) = happyGoto action_5
action_100 (7) = happyGoto action_6
action_100 (8) = happyGoto action_7
action_100 (10) = happyGoto action_8
action_100 (14) = happyGoto action_9
action_100 (15) = happyGoto action_10
action_100 (17) = happyGoto action_11
action_100 (18) = happyGoto action_12
action_100 (19) = happyGoto action_13
action_100 (23) = happyGoto action_103
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_32

action_102 _ = happyReduce_31

action_103 (49) = happyShift action_104
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (26) = happyShift action_105
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (50) = happyShift action_106
action_105 _ = happyFail (happyExpListPerState 105)

action_106 _ = happyReduce_22

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

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyTerminal (L.IntLit _ happy_var_1))
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
happyReduction_10 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn9
		 (V.singleton happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  9 happyReduction_11
happyReduction_11 (HappyAbsSyn23  happy_var_3)
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
happyReduction_15 (HappyAbsSyn23  happy_var_3)
	_
	(HappyTerminal (L.StringTem happy_var_1))
	 =  HappyAbsSyn12
		 ((happy_var_1, happy_var_3)
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  13 happyReduction_16
happyReduction_16 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn13
		 (Gt happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  13 happyReduction_17
happyReduction_17 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn13
		 (Lt happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  13 happyReduction_18
happyReduction_18 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn13
		 (Eq happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  13 happyReduction_19
happyReduction_19 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn13
		 (And happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  13 happyReduction_20
happyReduction_20 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn13
		 (Or happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 4 14 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (EscapeURI happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 12 15 happyReduction_22
happyReduction_22 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Iff happy_var_3 happy_var_5 happy_var_9
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_1  16 happyReduction_23
happyReduction_23 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn16
		 (Path happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  16 happyReduction_24
happyReduction_24 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  16 happyReduction_25
happyReduction_25 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  16 happyReduction_26
happyReduction_26 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  16 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happyReduce 5 17 happyReduction_30
happyReduction_30 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (happy_var_1 happy_var_2
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 8 18 happyReduction_31
happyReduction_31 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Identifier happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Identifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (\b -> Range (Just happy_var_3) happy_var_5 happy_var_7 b
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 8 18 happyReduction_32
happyReduction_32 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Identifier happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (\b -> Range Nothing happy_var_5 happy_var_7 b
	) `HappyStk` happyRest

happyReduce_33 = happySpecReduce_3  19 happyReduction_33
happyReduction_33 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (Path happy_var_2
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  20 happyReduction_34
happyReduction_34 (HappyAbsSyn21  happy_var_2)
	(HappyTerminal (L.Identifier happy_var_1))
	 =  HappyAbsSyn20
		 (V.cons (Obj happy_var_1) happy_var_2
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  20 happyReduction_35
happyReduction_35 (HappyTerminal (L.Identifier happy_var_1))
	 =  HappyAbsSyn20
		 (V.singleton (Obj happy_var_1)
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  21 happyReduction_36
happyReduction_36 (HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn21
		 (V.singleton happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  21 happyReduction_37
happyReduction_37 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (V.snoc happy_var_1 happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_2  22 happyReduction_38
happyReduction_38 (HappyTerminal (L.Identifier happy_var_2))
	_
	 =  HappyAbsSyn22
		 (Obj happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happyReduce 5 22 happyReduction_39
happyReduction_39 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.Identifier happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (Obj happy_var_3
	) `HappyStk` happyRest

happyReduce_40 = happySpecReduce_3  22 happyReduction_40
happyReduction_40 _
	(HappyTerminal (L.IntLit _ happy_var_2))
	_
	 =  HappyAbsSyn22
		 (Arr happy_var_2
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  23 happyReduction_41
happyReduction_41 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  23 happyReduction_42
happyReduction_42 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  23 happyReduction_43
happyReduction_43 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  23 happyReduction_44
happyReduction_44 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  23 happyReduction_45
happyReduction_45 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  23 happyReduction_46
happyReduction_46 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  23 happyReduction_47
happyReduction_47 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  23 happyReduction_48
happyReduction_48 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  23 happyReduction_49
happyReduction_49 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  23 happyReduction_50
happyReduction_50 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn23
		 (happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  23 happyReduction_51
happyReduction_51 _
	(HappyAbsSyn23  happy_var_2)
	_
	 =  HappyAbsSyn23
		 (happy_var_2
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 55 55 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	L.Identifier "if" -> cont 24;
	L.Identifier "else" -> cont 25;
	L.Identifier "end" -> cont 26;
	L.Identifier "null" -> cont 27;
	L.Identifier "range" -> cont 28;
	L.Identifier "escapeUri" -> cont 29;
	L.BoolLit True -> cont 30;
	L.BoolLit False -> cont 31;
	L.Identifier happy_dollar_dollar -> cont 32;
	L.StringTem happy_dollar_dollar -> cont 33;
	L.NumLit _ happy_dollar_dollar -> cont 34;
	L.IntLit _ happy_dollar_dollar -> cont 35;
	L.SingleQuote -> cont 36;
	L.Colon -> cont 37;
	L.Dot -> cont 38;
	L.Comma -> cont 39;
	L.Eq -> cont 40;
	L.Gt -> cont 41;
	L.Lt -> cont 42;
	L.And -> cont 43;
	L.Or -> cont 44;
	L.Underscore -> cont 45;
	L.Assignment -> cont 46;
	L.CurlyOpen -> cont 47;
	L.CurlyClose -> cont 48;
	L.DoubleCurlyOpen -> cont 49;
	L.DoubleCurlyClose -> cont 50;
	L.SquareOpen -> cont 51;
	L.SquareClose -> cont 52;
	L.ParenOpen -> cont 53;
	L.ParenClose -> cont 54;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 55 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either E.ErrorCode a -> (a -> Either E.ErrorCode b) -> Either E.ErrorCode b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Either E.ErrorCode a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either E.ErrorCode a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(L.Token)], [Prelude.String]) -> Either E.ErrorCode a
happyError' = (\(tokens, _) -> parseError tokens)
parser tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn23 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [L.Token] -> a
parseError toks = error $ "Parse error: " <> show toks

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
