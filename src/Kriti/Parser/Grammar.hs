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

data HappyAbsSyn t24 t25
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 (ValueExt)
	| HappyAbsSyn8 (V.Vector ValueExt)
	| HappyAbsSyn16 ([(T.Text, ValueExt)])
	| HappyAbsSyn17 ((T.Text, ValueExt))
	| HappyAbsSyn18 (Loc T.Text)
	| HappyAbsSyn20 ((Optionality, ValueExt))
	| HappyAbsSyn23 (Maybe (Loc T.Text))
	| HappyAbsSyn24 t24
	| HappyAbsSyn25 t25

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,884) ([0,15872,72,10880,0,15872,72,10880,0,0,61472,31,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,49152,0,512,0,0,0,0,0,0,0,0,0,8192,0,256,0,15872,89,10880,0,15872,72,14976,0,15872,72,10880,0,0,61472,31,0,15872,72,10880,0,15872,72,10880,0,15872,72,10880,0,15872,72,10880,0,15872,72,10880,0,15872,72,10880,0,15872,72,10880,0,15872,72,10880,0,15872,72,10880,0,15872,72,10880,0,0,61472,16415,0,0,61472,31,0,0,1024,4096,0,0,0,0,0,0,61472,1055,0,15872,72,10880,0,0,64,32,0,0,1024,256,0,0,0,0,0,49152,0,0,0,0,0,0,0,49152,0,512,0,0,0,0,0,0,0,0,0,7680,64,0,0,0,0,0,0,0,2560,2048,0,15872,72,10880,0,0,61472,16415,0,0,0,0,0,0,64,0,0,0,512,2048,0,15872,200,10880,0,0,0,8192,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,7680,64,0,0,49152,0,0,0,0,256,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,61472,1055,0,0,0,0,0,15872,72,10880,0,0,0,0,0,0,0,0,0,0,61440,31,0,0,61440,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,61440,7,0,0,61472,31,0,0,61472,31,0,15872,72,10880,0,0,64,0,0,0,0,0,0,15872,72,10880,0,0,256,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,15872,72,10880,0,0,61472,4127,0,32768,0,0,0,0,64,0,0,15872,200,10880,0,0,0,0,0,0,0,0,0,0,61472,4127,0,32768,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,61472,16415,0,0,0,0,0,15872,72,10880,0,0,61472,31,0,0,0,64,0,0,61472,543,0,0,2,0,0,15872,72,10880,0,0,61472,31,0,0,0,0,0,0,0,4096,0,0,128,0,0,0,0,0,0,0,0,4096,0,0,0,0,0,0,61472,1055,0,0,0,1024,0,15872,72,10880,0,15872,72,10880,0,0,0,0,0,0,61472,543,0,0,61472,543,0,0,4,0,0,0,4,0,0,0,0,1024,0,0,0,1024,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parser","expr","atom","var","string_template","string_template_","template","num_lit","boolean","null","array","list_elements","object","object_fields","object_field","object_key","fields","field","iff","range","mident","many__field__","many_rev__field__","number","int","'true'","'false'","'s\"'","'\"e'","string","'if'","'else'","'end'","'null'","'range'","'in'","ident","'\\''","':'","'.'","','","'?'","'??'","'=='","'!='","'>'","'<'","'<='","'>='","'&&'","'||'","'_'","':='","'{'","'}'","'{{'","'}}'","'['","']'","'('","')'","%eof"]
        bit_start = st Prelude.* 64
        bit_end = (st Prelude.+ 1) Prelude.* 64
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..63]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (26) = happyShift action_14
action_0 (27) = happyShift action_15
action_0 (28) = happyShift action_16
action_0 (29) = happyShift action_17
action_0 (30) = happyShift action_18
action_0 (36) = happyShift action_19
action_0 (39) = happyShift action_20
action_0 (56) = happyShift action_21
action_0 (58) = happyShift action_22
action_0 (60) = happyShift action_23
action_0 (62) = happyShift action_24
action_0 (4) = happyGoto action_25
action_0 (5) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 (7) = happyGoto action_5
action_0 (10) = happyGoto action_6
action_0 (11) = happyGoto action_7
action_0 (12) = happyGoto action_8
action_0 (13) = happyGoto action_9
action_0 (15) = happyGoto action_10
action_0 (19) = happyGoto action_11
action_0 (21) = happyGoto action_12
action_0 (22) = happyGoto action_13
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (26) = happyShift action_14
action_1 (27) = happyShift action_15
action_1 (28) = happyShift action_16
action_1 (29) = happyShift action_17
action_1 (30) = happyShift action_18
action_1 (36) = happyShift action_19
action_1 (39) = happyShift action_20
action_1 (56) = happyShift action_21
action_1 (58) = happyShift action_22
action_1 (60) = happyShift action_23
action_1 (62) = happyShift action_24
action_1 (4) = happyGoto action_2
action_1 (5) = happyGoto action_3
action_1 (6) = happyGoto action_4
action_1 (7) = happyGoto action_5
action_1 (10) = happyGoto action_6
action_1 (11) = happyGoto action_7
action_1 (12) = happyGoto action_8
action_1 (13) = happyGoto action_9
action_1 (15) = happyGoto action_10
action_1 (19) = happyGoto action_11
action_1 (21) = happyGoto action_12
action_1 (22) = happyGoto action_13
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (38) = happyShift action_26
action_2 (45) = happyShift action_27
action_2 (46) = happyShift action_28
action_2 (47) = happyShift action_29
action_2 (48) = happyShift action_30
action_2 (49) = happyShift action_31
action_2 (50) = happyShift action_32
action_2 (51) = happyShift action_33
action_2 (52) = happyShift action_34
action_2 (53) = happyShift action_35
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_11

action_4 (62) = happyShift action_53
action_4 (24) = happyGoto action_51
action_4 (25) = happyGoto action_52
action_4 _ = happyReduce_64

action_5 _ = happyReduce_15

action_6 _ = happyReduce_16

action_7 _ = happyReduce_17

action_8 _ = happyReduce_18

action_9 _ = happyReduce_19

action_10 _ = happyReduce_20

action_11 _ = happyReduce_21

action_12 _ = happyReduce_22

action_13 _ = happyReduce_23

action_14 _ = happyReduce_35

action_15 _ = happyReduce_36

action_16 _ = happyReduce_37

action_17 _ = happyReduce_38

action_18 (31) = happyShift action_48
action_18 (32) = happyShift action_49
action_18 (58) = happyShift action_50
action_18 (8) = happyGoto action_47
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_39

action_20 _ = happyReduce_24

action_21 (30) = happyShift action_45
action_21 (57) = happyShift action_46
action_21 (16) = happyGoto action_43
action_21 (17) = happyGoto action_44
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (26) = happyShift action_14
action_22 (27) = happyShift action_15
action_22 (28) = happyShift action_16
action_22 (29) = happyShift action_17
action_22 (30) = happyShift action_18
action_22 (33) = happyShift action_41
action_22 (36) = happyShift action_19
action_22 (37) = happyShift action_42
action_22 (39) = happyShift action_20
action_22 (56) = happyShift action_21
action_22 (58) = happyShift action_22
action_22 (60) = happyShift action_23
action_22 (62) = happyShift action_24
action_22 (4) = happyGoto action_40
action_22 (5) = happyGoto action_3
action_22 (6) = happyGoto action_4
action_22 (7) = happyGoto action_5
action_22 (10) = happyGoto action_6
action_22 (11) = happyGoto action_7
action_22 (12) = happyGoto action_8
action_22 (13) = happyGoto action_9
action_22 (15) = happyGoto action_10
action_22 (19) = happyGoto action_11
action_22 (21) = happyGoto action_12
action_22 (22) = happyGoto action_13
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (26) = happyShift action_14
action_23 (27) = happyShift action_15
action_23 (28) = happyShift action_16
action_23 (29) = happyShift action_17
action_23 (30) = happyShift action_18
action_23 (36) = happyShift action_19
action_23 (39) = happyShift action_20
action_23 (56) = happyShift action_21
action_23 (58) = happyShift action_22
action_23 (60) = happyShift action_23
action_23 (61) = happyShift action_39
action_23 (62) = happyShift action_24
action_23 (4) = happyGoto action_37
action_23 (5) = happyGoto action_3
action_23 (6) = happyGoto action_4
action_23 (7) = happyGoto action_5
action_23 (10) = happyGoto action_6
action_23 (11) = happyGoto action_7
action_23 (12) = happyGoto action_8
action_23 (13) = happyGoto action_9
action_23 (14) = happyGoto action_38
action_23 (15) = happyGoto action_10
action_23 (19) = happyGoto action_11
action_23 (21) = happyGoto action_12
action_23 (22) = happyGoto action_13
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (26) = happyShift action_14
action_24 (27) = happyShift action_15
action_24 (28) = happyShift action_16
action_24 (29) = happyShift action_17
action_24 (30) = happyShift action_18
action_24 (36) = happyShift action_19
action_24 (39) = happyShift action_20
action_24 (56) = happyShift action_21
action_24 (58) = happyShift action_22
action_24 (60) = happyShift action_23
action_24 (62) = happyShift action_24
action_24 (4) = happyGoto action_36
action_24 (5) = happyGoto action_3
action_24 (6) = happyGoto action_4
action_24 (7) = happyGoto action_5
action_24 (10) = happyGoto action_6
action_24 (11) = happyGoto action_7
action_24 (12) = happyGoto action_8
action_24 (13) = happyGoto action_9
action_24 (15) = happyGoto action_10
action_24 (19) = happyGoto action_11
action_24 (21) = happyGoto action_12
action_24 (22) = happyGoto action_13
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (38) = happyShift action_26
action_25 (45) = happyShift action_27
action_25 (46) = happyShift action_28
action_25 (47) = happyShift action_29
action_25 (48) = happyShift action_30
action_25 (49) = happyShift action_31
action_25 (50) = happyShift action_32
action_25 (51) = happyShift action_33
action_25 (52) = happyShift action_34
action_25 (53) = happyShift action_35
action_25 (64) = happyAccept
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (26) = happyShift action_14
action_26 (27) = happyShift action_15
action_26 (28) = happyShift action_16
action_26 (29) = happyShift action_17
action_26 (30) = happyShift action_18
action_26 (36) = happyShift action_19
action_26 (39) = happyShift action_20
action_26 (56) = happyShift action_21
action_26 (58) = happyShift action_22
action_26 (60) = happyShift action_23
action_26 (62) = happyShift action_24
action_26 (4) = happyGoto action_89
action_26 (5) = happyGoto action_3
action_26 (6) = happyGoto action_4
action_26 (7) = happyGoto action_5
action_26 (10) = happyGoto action_6
action_26 (11) = happyGoto action_7
action_26 (12) = happyGoto action_8
action_26 (13) = happyGoto action_9
action_26 (15) = happyGoto action_10
action_26 (19) = happyGoto action_11
action_26 (21) = happyGoto action_12
action_26 (22) = happyGoto action_13
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (26) = happyShift action_14
action_27 (27) = happyShift action_15
action_27 (28) = happyShift action_16
action_27 (29) = happyShift action_17
action_27 (30) = happyShift action_18
action_27 (36) = happyShift action_19
action_27 (39) = happyShift action_20
action_27 (56) = happyShift action_21
action_27 (58) = happyShift action_22
action_27 (60) = happyShift action_23
action_27 (62) = happyShift action_24
action_27 (4) = happyGoto action_88
action_27 (5) = happyGoto action_3
action_27 (6) = happyGoto action_4
action_27 (7) = happyGoto action_5
action_27 (10) = happyGoto action_6
action_27 (11) = happyGoto action_7
action_27 (12) = happyGoto action_8
action_27 (13) = happyGoto action_9
action_27 (15) = happyGoto action_10
action_27 (19) = happyGoto action_11
action_27 (21) = happyGoto action_12
action_27 (22) = happyGoto action_13
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (26) = happyShift action_14
action_28 (27) = happyShift action_15
action_28 (28) = happyShift action_16
action_28 (29) = happyShift action_17
action_28 (30) = happyShift action_18
action_28 (36) = happyShift action_19
action_28 (39) = happyShift action_20
action_28 (56) = happyShift action_21
action_28 (58) = happyShift action_22
action_28 (60) = happyShift action_23
action_28 (62) = happyShift action_24
action_28 (4) = happyGoto action_87
action_28 (5) = happyGoto action_3
action_28 (6) = happyGoto action_4
action_28 (7) = happyGoto action_5
action_28 (10) = happyGoto action_6
action_28 (11) = happyGoto action_7
action_28 (12) = happyGoto action_8
action_28 (13) = happyGoto action_9
action_28 (15) = happyGoto action_10
action_28 (19) = happyGoto action_11
action_28 (21) = happyGoto action_12
action_28 (22) = happyGoto action_13
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (26) = happyShift action_14
action_29 (27) = happyShift action_15
action_29 (28) = happyShift action_16
action_29 (29) = happyShift action_17
action_29 (30) = happyShift action_18
action_29 (36) = happyShift action_19
action_29 (39) = happyShift action_20
action_29 (56) = happyShift action_21
action_29 (58) = happyShift action_22
action_29 (60) = happyShift action_23
action_29 (62) = happyShift action_24
action_29 (4) = happyGoto action_86
action_29 (5) = happyGoto action_3
action_29 (6) = happyGoto action_4
action_29 (7) = happyGoto action_5
action_29 (10) = happyGoto action_6
action_29 (11) = happyGoto action_7
action_29 (12) = happyGoto action_8
action_29 (13) = happyGoto action_9
action_29 (15) = happyGoto action_10
action_29 (19) = happyGoto action_11
action_29 (21) = happyGoto action_12
action_29 (22) = happyGoto action_13
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (26) = happyShift action_14
action_30 (27) = happyShift action_15
action_30 (28) = happyShift action_16
action_30 (29) = happyShift action_17
action_30 (30) = happyShift action_18
action_30 (36) = happyShift action_19
action_30 (39) = happyShift action_20
action_30 (56) = happyShift action_21
action_30 (58) = happyShift action_22
action_30 (60) = happyShift action_23
action_30 (62) = happyShift action_24
action_30 (4) = happyGoto action_85
action_30 (5) = happyGoto action_3
action_30 (6) = happyGoto action_4
action_30 (7) = happyGoto action_5
action_30 (10) = happyGoto action_6
action_30 (11) = happyGoto action_7
action_30 (12) = happyGoto action_8
action_30 (13) = happyGoto action_9
action_30 (15) = happyGoto action_10
action_30 (19) = happyGoto action_11
action_30 (21) = happyGoto action_12
action_30 (22) = happyGoto action_13
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (26) = happyShift action_14
action_31 (27) = happyShift action_15
action_31 (28) = happyShift action_16
action_31 (29) = happyShift action_17
action_31 (30) = happyShift action_18
action_31 (36) = happyShift action_19
action_31 (39) = happyShift action_20
action_31 (56) = happyShift action_21
action_31 (58) = happyShift action_22
action_31 (60) = happyShift action_23
action_31 (62) = happyShift action_24
action_31 (4) = happyGoto action_84
action_31 (5) = happyGoto action_3
action_31 (6) = happyGoto action_4
action_31 (7) = happyGoto action_5
action_31 (10) = happyGoto action_6
action_31 (11) = happyGoto action_7
action_31 (12) = happyGoto action_8
action_31 (13) = happyGoto action_9
action_31 (15) = happyGoto action_10
action_31 (19) = happyGoto action_11
action_31 (21) = happyGoto action_12
action_31 (22) = happyGoto action_13
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (26) = happyShift action_14
action_32 (27) = happyShift action_15
action_32 (28) = happyShift action_16
action_32 (29) = happyShift action_17
action_32 (30) = happyShift action_18
action_32 (36) = happyShift action_19
action_32 (39) = happyShift action_20
action_32 (56) = happyShift action_21
action_32 (58) = happyShift action_22
action_32 (60) = happyShift action_23
action_32 (62) = happyShift action_24
action_32 (4) = happyGoto action_83
action_32 (5) = happyGoto action_3
action_32 (6) = happyGoto action_4
action_32 (7) = happyGoto action_5
action_32 (10) = happyGoto action_6
action_32 (11) = happyGoto action_7
action_32 (12) = happyGoto action_8
action_32 (13) = happyGoto action_9
action_32 (15) = happyGoto action_10
action_32 (19) = happyGoto action_11
action_32 (21) = happyGoto action_12
action_32 (22) = happyGoto action_13
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (26) = happyShift action_14
action_33 (27) = happyShift action_15
action_33 (28) = happyShift action_16
action_33 (29) = happyShift action_17
action_33 (30) = happyShift action_18
action_33 (36) = happyShift action_19
action_33 (39) = happyShift action_20
action_33 (56) = happyShift action_21
action_33 (58) = happyShift action_22
action_33 (60) = happyShift action_23
action_33 (62) = happyShift action_24
action_33 (4) = happyGoto action_82
action_33 (5) = happyGoto action_3
action_33 (6) = happyGoto action_4
action_33 (7) = happyGoto action_5
action_33 (10) = happyGoto action_6
action_33 (11) = happyGoto action_7
action_33 (12) = happyGoto action_8
action_33 (13) = happyGoto action_9
action_33 (15) = happyGoto action_10
action_33 (19) = happyGoto action_11
action_33 (21) = happyGoto action_12
action_33 (22) = happyGoto action_13
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (26) = happyShift action_14
action_34 (27) = happyShift action_15
action_34 (28) = happyShift action_16
action_34 (29) = happyShift action_17
action_34 (30) = happyShift action_18
action_34 (36) = happyShift action_19
action_34 (39) = happyShift action_20
action_34 (56) = happyShift action_21
action_34 (58) = happyShift action_22
action_34 (60) = happyShift action_23
action_34 (62) = happyShift action_24
action_34 (4) = happyGoto action_81
action_34 (5) = happyGoto action_3
action_34 (6) = happyGoto action_4
action_34 (7) = happyGoto action_5
action_34 (10) = happyGoto action_6
action_34 (11) = happyGoto action_7
action_34 (12) = happyGoto action_8
action_34 (13) = happyGoto action_9
action_34 (15) = happyGoto action_10
action_34 (19) = happyGoto action_11
action_34 (21) = happyGoto action_12
action_34 (22) = happyGoto action_13
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (26) = happyShift action_14
action_35 (27) = happyShift action_15
action_35 (28) = happyShift action_16
action_35 (29) = happyShift action_17
action_35 (30) = happyShift action_18
action_35 (36) = happyShift action_19
action_35 (39) = happyShift action_20
action_35 (56) = happyShift action_21
action_35 (58) = happyShift action_22
action_35 (60) = happyShift action_23
action_35 (62) = happyShift action_24
action_35 (4) = happyGoto action_80
action_35 (5) = happyGoto action_3
action_35 (6) = happyGoto action_4
action_35 (7) = happyGoto action_5
action_35 (10) = happyGoto action_6
action_35 (11) = happyGoto action_7
action_35 (12) = happyGoto action_8
action_35 (13) = happyGoto action_9
action_35 (15) = happyGoto action_10
action_35 (19) = happyGoto action_11
action_35 (21) = happyGoto action_12
action_35 (22) = happyGoto action_13
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (38) = happyShift action_26
action_36 (45) = happyShift action_27
action_36 (46) = happyShift action_28
action_36 (47) = happyShift action_29
action_36 (48) = happyShift action_30
action_36 (49) = happyShift action_31
action_36 (50) = happyShift action_32
action_36 (51) = happyShift action_33
action_36 (52) = happyShift action_34
action_36 (53) = happyShift action_35
action_36 (63) = happyShift action_79
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (38) = happyShift action_26
action_37 (45) = happyShift action_27
action_37 (46) = happyShift action_28
action_37 (47) = happyShift action_29
action_37 (48) = happyShift action_30
action_37 (49) = happyShift action_31
action_37 (50) = happyShift action_32
action_37 (51) = happyShift action_33
action_37 (52) = happyShift action_34
action_37 (53) = happyShift action_35
action_37 _ = happyReduce_42

action_38 (43) = happyShift action_77
action_38 (61) = happyShift action_78
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_41

action_40 (38) = happyShift action_26
action_40 (45) = happyShift action_27
action_40 (46) = happyShift action_28
action_40 (47) = happyShift action_29
action_40 (48) = happyShift action_30
action_40 (49) = happyShift action_31
action_40 (50) = happyShift action_32
action_40 (51) = happyShift action_33
action_40 (52) = happyShift action_34
action_40 (53) = happyShift action_35
action_40 (59) = happyShift action_76
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (26) = happyShift action_14
action_41 (27) = happyShift action_15
action_41 (28) = happyShift action_16
action_41 (29) = happyShift action_17
action_41 (30) = happyShift action_18
action_41 (36) = happyShift action_19
action_41 (39) = happyShift action_20
action_41 (56) = happyShift action_21
action_41 (58) = happyShift action_22
action_41 (60) = happyShift action_23
action_41 (62) = happyShift action_24
action_41 (4) = happyGoto action_75
action_41 (5) = happyGoto action_3
action_41 (6) = happyGoto action_4
action_41 (7) = happyGoto action_5
action_41 (10) = happyGoto action_6
action_41 (11) = happyGoto action_7
action_41 (12) = happyGoto action_8
action_41 (13) = happyGoto action_9
action_41 (15) = happyGoto action_10
action_41 (19) = happyGoto action_11
action_41 (21) = happyGoto action_12
action_41 (22) = happyGoto action_13
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (39) = happyShift action_73
action_42 (54) = happyShift action_74
action_42 (23) = happyGoto action_72
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (43) = happyShift action_70
action_43 (57) = happyShift action_71
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_46

action_45 (31) = happyShift action_68
action_45 (32) = happyShift action_69
action_45 (18) = happyGoto action_67
action_45 _ = happyFail (happyExpListPerState 45)

action_46 _ = happyReduce_45

action_47 (31) = happyShift action_64
action_47 (32) = happyShift action_65
action_47 (58) = happyShift action_66
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_26

action_49 _ = happyReduce_30

action_50 (26) = happyShift action_14
action_50 (27) = happyShift action_15
action_50 (28) = happyShift action_16
action_50 (29) = happyShift action_17
action_50 (39) = happyShift action_20
action_50 (6) = happyGoto action_59
action_50 (9) = happyGoto action_60
action_50 (10) = happyGoto action_61
action_50 (11) = happyGoto action_62
action_50 (19) = happyGoto action_63
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_52

action_52 (42) = happyShift action_56
action_52 (44) = happyShift action_57
action_52 (60) = happyShift action_58
action_52 (20) = happyGoto action_55
action_52 _ = happyReduce_63

action_53 (26) = happyShift action_14
action_53 (27) = happyShift action_15
action_53 (28) = happyShift action_16
action_53 (29) = happyShift action_17
action_53 (30) = happyShift action_18
action_53 (36) = happyShift action_19
action_53 (39) = happyShift action_20
action_53 (56) = happyShift action_21
action_53 (58) = happyShift action_22
action_53 (60) = happyShift action_23
action_53 (62) = happyShift action_24
action_53 (4) = happyGoto action_54
action_53 (5) = happyGoto action_3
action_53 (6) = happyGoto action_4
action_53 (7) = happyGoto action_5
action_53 (10) = happyGoto action_6
action_53 (11) = happyGoto action_7
action_53 (12) = happyGoto action_8
action_53 (13) = happyGoto action_9
action_53 (15) = happyGoto action_10
action_53 (19) = happyGoto action_11
action_53 (21) = happyGoto action_12
action_53 (22) = happyGoto action_13
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (38) = happyShift action_26
action_54 (45) = happyShift action_27
action_54 (46) = happyShift action_28
action_54 (47) = happyShift action_29
action_54 (48) = happyShift action_30
action_54 (49) = happyShift action_31
action_54 (50) = happyShift action_32
action_54 (51) = happyShift action_33
action_54 (52) = happyShift action_34
action_54 (53) = happyShift action_35
action_54 (63) = happyShift action_105
action_54 _ = happyFail (happyExpListPerState 54)

action_55 _ = happyReduce_65

action_56 (39) = happyShift action_104
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (42) = happyShift action_102
action_57 (60) = happyShift action_103
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (26) = happyShift action_14
action_58 (27) = happyShift action_15
action_58 (28) = happyShift action_16
action_58 (29) = happyShift action_17
action_58 (30) = happyShift action_18
action_58 (36) = happyShift action_19
action_58 (39) = happyShift action_20
action_58 (40) = happyShift action_101
action_58 (56) = happyShift action_21
action_58 (58) = happyShift action_22
action_58 (60) = happyShift action_23
action_58 (62) = happyShift action_24
action_58 (4) = happyGoto action_100
action_58 (5) = happyGoto action_3
action_58 (6) = happyGoto action_4
action_58 (7) = happyGoto action_5
action_58 (10) = happyGoto action_6
action_58 (11) = happyGoto action_7
action_58 (12) = happyGoto action_8
action_58 (13) = happyGoto action_9
action_58 (15) = happyGoto action_10
action_58 (19) = happyGoto action_11
action_58 (21) = happyGoto action_12
action_58 (22) = happyGoto action_13
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (62) = happyShift action_99
action_59 (24) = happyGoto action_51
action_59 (25) = happyGoto action_52
action_59 _ = happyReduce_64

action_60 (59) = happyShift action_98
action_60 _ = happyFail (happyExpListPerState 60)

action_61 _ = happyReduce_32

action_62 _ = happyReduce_31

action_63 _ = happyReduce_33

action_64 _ = happyReduce_25

action_65 _ = happyReduce_28

action_66 (26) = happyShift action_14
action_66 (27) = happyShift action_15
action_66 (28) = happyShift action_16
action_66 (29) = happyShift action_17
action_66 (39) = happyShift action_20
action_66 (6) = happyGoto action_59
action_66 (9) = happyGoto action_97
action_66 (10) = happyGoto action_61
action_66 (11) = happyGoto action_62
action_66 (19) = happyGoto action_63
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (31) = happyShift action_95
action_67 (32) = happyShift action_96
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (41) = happyShift action_94
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_51

action_70 (30) = happyShift action_45
action_70 (17) = happyGoto action_93
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_44

action_72 (43) = happyShift action_92
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_62

action_74 _ = happyReduce_61

action_75 (38) = happyShift action_26
action_75 (45) = happyShift action_27
action_75 (46) = happyShift action_28
action_75 (47) = happyShift action_29
action_75 (48) = happyShift action_30
action_75 (49) = happyShift action_31
action_75 (50) = happyShift action_32
action_75 (51) = happyShift action_33
action_75 (52) = happyShift action_34
action_75 (53) = happyShift action_35
action_75 (59) = happyShift action_91
action_75 _ = happyFail (happyExpListPerState 75)

action_76 _ = happyReduce_14

action_77 (26) = happyShift action_14
action_77 (27) = happyShift action_15
action_77 (28) = happyShift action_16
action_77 (29) = happyShift action_17
action_77 (30) = happyShift action_18
action_77 (36) = happyShift action_19
action_77 (39) = happyShift action_20
action_77 (56) = happyShift action_21
action_77 (58) = happyShift action_22
action_77 (60) = happyShift action_23
action_77 (62) = happyShift action_24
action_77 (4) = happyGoto action_90
action_77 (5) = happyGoto action_3
action_77 (6) = happyGoto action_4
action_77 (7) = happyGoto action_5
action_77 (10) = happyGoto action_6
action_77 (11) = happyGoto action_7
action_77 (12) = happyGoto action_8
action_77 (13) = happyGoto action_9
action_77 (15) = happyGoto action_10
action_77 (19) = happyGoto action_11
action_77 (21) = happyGoto action_12
action_77 (22) = happyGoto action_13
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_40

action_79 _ = happyReduce_13

action_80 (45) = happyShift action_27
action_80 (46) = happyShift action_28
action_80 (47) = happyShift action_29
action_80 (48) = happyShift action_30
action_80 (49) = happyShift action_31
action_80 (50) = happyShift action_32
action_80 (51) = happyShift action_33
action_80 (52) = happyShift action_34
action_80 (53) = happyShift action_35
action_80 _ = happyReduce_8

action_81 (45) = happyShift action_27
action_81 (46) = happyShift action_28
action_81 (47) = happyShift action_29
action_81 (48) = happyShift action_30
action_81 (49) = happyShift action_31
action_81 (50) = happyShift action_32
action_81 (51) = happyShift action_33
action_81 (52) = happyShift action_34
action_81 _ = happyReduce_7

action_82 (46) = happyFail []
action_82 (47) = happyFail []
action_82 (48) = happyFail []
action_82 (49) = happyFail []
action_82 (50) = happyFail []
action_82 (51) = happyFail []
action_82 _ = happyReduce_3

action_83 (46) = happyFail []
action_83 (47) = happyFail []
action_83 (48) = happyFail []
action_83 (49) = happyFail []
action_83 (50) = happyFail []
action_83 (51) = happyFail []
action_83 _ = happyReduce_4

action_84 (46) = happyFail []
action_84 (47) = happyFail []
action_84 (48) = happyFail []
action_84 (49) = happyFail []
action_84 (50) = happyFail []
action_84 (51) = happyFail []
action_84 _ = happyReduce_2

action_85 (46) = happyFail []
action_85 (47) = happyFail []
action_85 (48) = happyFail []
action_85 (49) = happyFail []
action_85 (50) = happyFail []
action_85 (51) = happyFail []
action_85 _ = happyReduce_1

action_86 (46) = happyFail []
action_86 (47) = happyFail []
action_86 (48) = happyFail []
action_86 (49) = happyFail []
action_86 (50) = happyFail []
action_86 (51) = happyFail []
action_86 _ = happyReduce_5

action_87 (46) = happyFail []
action_87 (47) = happyFail []
action_87 (48) = happyFail []
action_87 (49) = happyFail []
action_87 (50) = happyFail []
action_87 (51) = happyFail []
action_87 _ = happyReduce_6

action_88 (45) = happyShift action_27
action_88 (46) = happyShift action_28
action_88 (47) = happyShift action_29
action_88 (48) = happyShift action_30
action_88 (49) = happyShift action_31
action_88 (50) = happyShift action_32
action_88 (51) = happyShift action_33
action_88 _ = happyReduce_10

action_89 (38) = happyShift action_26
action_89 (45) = happyShift action_27
action_89 (46) = happyShift action_28
action_89 (47) = happyShift action_29
action_89 (48) = happyShift action_30
action_89 (49) = happyShift action_31
action_89 (50) = happyShift action_32
action_89 (51) = happyShift action_33
action_89 (52) = happyShift action_34
action_89 (53) = happyShift action_35
action_89 _ = happyReduce_9

action_90 (38) = happyShift action_26
action_90 (45) = happyShift action_27
action_90 (46) = happyShift action_28
action_90 (47) = happyShift action_29
action_90 (48) = happyShift action_30
action_90 (49) = happyShift action_31
action_90 (50) = happyShift action_32
action_90 (51) = happyShift action_33
action_90 (52) = happyShift action_34
action_90 (53) = happyShift action_35
action_90 _ = happyReduce_43

action_91 (26) = happyShift action_14
action_91 (27) = happyShift action_15
action_91 (28) = happyShift action_16
action_91 (29) = happyShift action_17
action_91 (30) = happyShift action_18
action_91 (36) = happyShift action_19
action_91 (39) = happyShift action_20
action_91 (56) = happyShift action_21
action_91 (58) = happyShift action_22
action_91 (60) = happyShift action_23
action_91 (62) = happyShift action_24
action_91 (4) = happyGoto action_116
action_91 (5) = happyGoto action_3
action_91 (6) = happyGoto action_4
action_91 (7) = happyGoto action_5
action_91 (10) = happyGoto action_6
action_91 (11) = happyGoto action_7
action_91 (12) = happyGoto action_8
action_91 (13) = happyGoto action_9
action_91 (15) = happyGoto action_10
action_91 (19) = happyGoto action_11
action_91 (21) = happyGoto action_12
action_91 (22) = happyGoto action_13
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (39) = happyShift action_115
action_92 _ = happyFail (happyExpListPerState 92)

action_93 _ = happyReduce_47

action_94 (26) = happyShift action_14
action_94 (27) = happyShift action_15
action_94 (28) = happyShift action_16
action_94 (29) = happyShift action_17
action_94 (30) = happyShift action_18
action_94 (36) = happyShift action_19
action_94 (39) = happyShift action_20
action_94 (56) = happyShift action_21
action_94 (58) = happyShift action_22
action_94 (60) = happyShift action_23
action_94 (62) = happyShift action_24
action_94 (4) = happyGoto action_114
action_94 (5) = happyGoto action_3
action_94 (6) = happyGoto action_4
action_94 (7) = happyGoto action_5
action_94 (10) = happyGoto action_6
action_94 (11) = happyGoto action_7
action_94 (12) = happyGoto action_8
action_94 (13) = happyGoto action_9
action_94 (15) = happyGoto action_10
action_94 (19) = happyGoto action_11
action_94 (21) = happyGoto action_12
action_94 (22) = happyGoto action_13
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (41) = happyShift action_113
action_95 _ = happyFail (happyExpListPerState 95)

action_96 _ = happyReduce_50

action_97 (59) = happyShift action_112
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_29

action_99 (26) = happyShift action_14
action_99 (27) = happyShift action_15
action_99 (28) = happyShift action_16
action_99 (29) = happyShift action_17
action_99 (30) = happyShift action_18
action_99 (36) = happyShift action_19
action_99 (39) = happyShift action_20
action_99 (56) = happyShift action_21
action_99 (58) = happyShift action_22
action_99 (60) = happyShift action_23
action_99 (62) = happyShift action_24
action_99 (4) = happyGoto action_111
action_99 (5) = happyGoto action_3
action_99 (6) = happyGoto action_4
action_99 (7) = happyGoto action_5
action_99 (10) = happyGoto action_6
action_99 (11) = happyGoto action_7
action_99 (12) = happyGoto action_8
action_99 (13) = happyGoto action_9
action_99 (15) = happyGoto action_10
action_99 (19) = happyGoto action_11
action_99 (21) = happyGoto action_12
action_99 (22) = happyGoto action_13
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (38) = happyShift action_26
action_100 (45) = happyShift action_27
action_100 (46) = happyShift action_28
action_100 (47) = happyShift action_29
action_100 (48) = happyShift action_30
action_100 (49) = happyShift action_31
action_100 (50) = happyShift action_32
action_100 (51) = happyShift action_33
action_100 (52) = happyShift action_34
action_100 (53) = happyShift action_35
action_100 (61) = happyShift action_110
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (32) = happyShift action_109
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (39) = happyShift action_108
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (26) = happyShift action_14
action_103 (27) = happyShift action_15
action_103 (28) = happyShift action_16
action_103 (29) = happyShift action_17
action_103 (30) = happyShift action_18
action_103 (36) = happyShift action_19
action_103 (39) = happyShift action_20
action_103 (40) = happyShift action_107
action_103 (56) = happyShift action_21
action_103 (58) = happyShift action_22
action_103 (60) = happyShift action_23
action_103 (62) = happyShift action_24
action_103 (4) = happyGoto action_106
action_103 (5) = happyGoto action_3
action_103 (6) = happyGoto action_4
action_103 (7) = happyGoto action_5
action_103 (10) = happyGoto action_6
action_103 (11) = happyGoto action_7
action_103 (12) = happyGoto action_8
action_103 (13) = happyGoto action_9
action_103 (15) = happyGoto action_10
action_103 (19) = happyGoto action_11
action_103 (21) = happyGoto action_12
action_103 (22) = happyGoto action_13
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_53

action_105 _ = happyReduce_12

action_106 (38) = happyShift action_26
action_106 (45) = happyShift action_27
action_106 (46) = happyShift action_28
action_106 (47) = happyShift action_29
action_106 (48) = happyShift action_30
action_106 (49) = happyShift action_31
action_106 (50) = happyShift action_32
action_106 (51) = happyShift action_33
action_106 (52) = happyShift action_34
action_106 (53) = happyShift action_35
action_106 (61) = happyShift action_123
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (32) = happyShift action_122
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_54

action_109 (40) = happyShift action_121
action_109 _ = happyFail (happyExpListPerState 109)

action_110 _ = happyReduce_57

action_111 (38) = happyShift action_26
action_111 (45) = happyShift action_27
action_111 (46) = happyShift action_28
action_111 (47) = happyShift action_29
action_111 (48) = happyShift action_30
action_111 (49) = happyShift action_31
action_111 (50) = happyShift action_32
action_111 (51) = happyShift action_33
action_111 (52) = happyShift action_34
action_111 (53) = happyShift action_35
action_111 (63) = happyShift action_120
action_111 _ = happyFail (happyExpListPerState 111)

action_112 _ = happyReduce_27

action_113 (26) = happyShift action_14
action_113 (27) = happyShift action_15
action_113 (28) = happyShift action_16
action_113 (29) = happyShift action_17
action_113 (30) = happyShift action_18
action_113 (36) = happyShift action_19
action_113 (39) = happyShift action_20
action_113 (56) = happyShift action_21
action_113 (58) = happyShift action_22
action_113 (60) = happyShift action_23
action_113 (62) = happyShift action_24
action_113 (4) = happyGoto action_119
action_113 (5) = happyGoto action_3
action_113 (6) = happyGoto action_4
action_113 (7) = happyGoto action_5
action_113 (10) = happyGoto action_6
action_113 (11) = happyGoto action_7
action_113 (12) = happyGoto action_8
action_113 (13) = happyGoto action_9
action_113 (15) = happyGoto action_10
action_113 (19) = happyGoto action_11
action_113 (21) = happyGoto action_12
action_113 (22) = happyGoto action_13
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (38) = happyShift action_26
action_114 (45) = happyShift action_27
action_114 (46) = happyShift action_28
action_114 (47) = happyShift action_29
action_114 (48) = happyShift action_30
action_114 (49) = happyShift action_31
action_114 (50) = happyShift action_32
action_114 (51) = happyShift action_33
action_114 (52) = happyShift action_34
action_114 (53) = happyShift action_35
action_114 _ = happyReduce_49

action_115 (55) = happyShift action_118
action_115 _ = happyFail (happyExpListPerState 115)

action_116 (38) = happyShift action_26
action_116 (45) = happyShift action_27
action_116 (46) = happyShift action_28
action_116 (47) = happyShift action_29
action_116 (48) = happyShift action_30
action_116 (49) = happyShift action_31
action_116 (50) = happyShift action_32
action_116 (51) = happyShift action_33
action_116 (52) = happyShift action_34
action_116 (53) = happyShift action_35
action_116 (58) = happyShift action_117
action_116 _ = happyFail (happyExpListPerState 116)

action_117 (34) = happyShift action_127
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (26) = happyShift action_14
action_118 (27) = happyShift action_15
action_118 (28) = happyShift action_16
action_118 (29) = happyShift action_17
action_118 (30) = happyShift action_18
action_118 (36) = happyShift action_19
action_118 (39) = happyShift action_20
action_118 (56) = happyShift action_21
action_118 (58) = happyShift action_22
action_118 (60) = happyShift action_23
action_118 (62) = happyShift action_24
action_118 (4) = happyGoto action_126
action_118 (5) = happyGoto action_3
action_118 (6) = happyGoto action_4
action_118 (7) = happyGoto action_5
action_118 (10) = happyGoto action_6
action_118 (11) = happyGoto action_7
action_118 (12) = happyGoto action_8
action_118 (13) = happyGoto action_9
action_118 (15) = happyGoto action_10
action_118 (19) = happyGoto action_11
action_118 (21) = happyGoto action_12
action_118 (22) = happyGoto action_13
action_118 _ = happyFail (happyExpListPerState 118)

action_119 (38) = happyShift action_26
action_119 (45) = happyShift action_27
action_119 (46) = happyShift action_28
action_119 (47) = happyShift action_29
action_119 (48) = happyShift action_30
action_119 (49) = happyShift action_31
action_119 (50) = happyShift action_32
action_119 (51) = happyShift action_33
action_119 (52) = happyShift action_34
action_119 (53) = happyShift action_35
action_119 _ = happyReduce_48

action_120 _ = happyReduce_34

action_121 (61) = happyShift action_125
action_121 _ = happyFail (happyExpListPerState 121)

action_122 (40) = happyShift action_124
action_122 _ = happyFail (happyExpListPerState 122)

action_123 _ = happyReduce_58

action_124 (61) = happyShift action_130
action_124 _ = happyFail (happyExpListPerState 124)

action_125 _ = happyReduce_56

action_126 (38) = happyShift action_26
action_126 (45) = happyShift action_27
action_126 (46) = happyShift action_28
action_126 (47) = happyShift action_29
action_126 (48) = happyShift action_30
action_126 (49) = happyShift action_31
action_126 (50) = happyShift action_32
action_126 (51) = happyShift action_33
action_126 (52) = happyShift action_34
action_126 (53) = happyShift action_35
action_126 (59) = happyShift action_129
action_126 _ = happyFail (happyExpListPerState 126)

action_127 (59) = happyShift action_128
action_127 _ = happyFail (happyExpListPerState 127)

action_128 (26) = happyShift action_14
action_128 (27) = happyShift action_15
action_128 (28) = happyShift action_16
action_128 (29) = happyShift action_17
action_128 (30) = happyShift action_18
action_128 (36) = happyShift action_19
action_128 (39) = happyShift action_20
action_128 (56) = happyShift action_21
action_128 (58) = happyShift action_22
action_128 (60) = happyShift action_23
action_128 (62) = happyShift action_24
action_128 (4) = happyGoto action_132
action_128 (5) = happyGoto action_3
action_128 (6) = happyGoto action_4
action_128 (7) = happyGoto action_5
action_128 (10) = happyGoto action_6
action_128 (11) = happyGoto action_7
action_128 (12) = happyGoto action_8
action_128 (13) = happyGoto action_9
action_128 (15) = happyGoto action_10
action_128 (19) = happyGoto action_11
action_128 (21) = happyGoto action_12
action_128 (22) = happyGoto action_13
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (26) = happyShift action_14
action_129 (27) = happyShift action_15
action_129 (28) = happyShift action_16
action_129 (29) = happyShift action_17
action_129 (30) = happyShift action_18
action_129 (36) = happyShift action_19
action_129 (39) = happyShift action_20
action_129 (56) = happyShift action_21
action_129 (58) = happyShift action_22
action_129 (60) = happyShift action_23
action_129 (62) = happyShift action_24
action_129 (4) = happyGoto action_131
action_129 (5) = happyGoto action_3
action_129 (6) = happyGoto action_4
action_129 (7) = happyGoto action_5
action_129 (10) = happyGoto action_6
action_129 (11) = happyGoto action_7
action_129 (12) = happyGoto action_8
action_129 (13) = happyGoto action_9
action_129 (15) = happyGoto action_10
action_129 (19) = happyGoto action_11
action_129 (21) = happyGoto action_12
action_129 (22) = happyGoto action_13
action_129 _ = happyFail (happyExpListPerState 129)

action_130 _ = happyReduce_55

action_131 (38) = happyShift action_26
action_131 (45) = happyShift action_27
action_131 (46) = happyShift action_28
action_131 (47) = happyShift action_29
action_131 (48) = happyShift action_30
action_131 (49) = happyShift action_31
action_131 (50) = happyShift action_32
action_131 (51) = happyShift action_33
action_131 (52) = happyShift action_34
action_131 (53) = happyShift action_35
action_131 (58) = happyShift action_134
action_131 _ = happyFail (happyExpListPerState 131)

action_132 (38) = happyShift action_26
action_132 (45) = happyShift action_27
action_132 (46) = happyShift action_28
action_132 (47) = happyShift action_29
action_132 (48) = happyShift action_30
action_132 (49) = happyShift action_31
action_132 (50) = happyShift action_32
action_132 (51) = happyShift action_33
action_132 (52) = happyShift action_34
action_132 (53) = happyShift action_35
action_132 (58) = happyShift action_133
action_132 _ = happyFail (happyExpListPerState 132)

action_133 (35) = happyShift action_136
action_133 _ = happyFail (happyExpListPerState 133)

action_134 (35) = happyShift action_135
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (59) = happyShift action_138
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (59) = happyShift action_137
action_136 _ = happyFail (happyExpListPerState 136)

action_137 _ = happyReduce_59

action_138 _ = happyReduce_60

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Gt (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Lt (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Gte (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Lte (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (NotEq (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  4 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Eq (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (And (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Or (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (In (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Defaulting (locate happy_var_1 <> locate happy_var_3) happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  4 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happyReduce 4 4 happyReduction_12
happyReduction_12 ((HappyTerminal (TokSymbol (Loc happy_var_4 SymParenClose))) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Ap (locate happy_var_1 <> locate happy_var_4) happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_3  4 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  4 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  5 happyReduction_15
happyReduction_15 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  5 happyReduction_16
happyReduction_16 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  5 happyReduction_17
happyReduction_17 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  5 happyReduction_18
happyReduction_18 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  5 happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  5 happyReduction_20
happyReduction_20 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  5 happyReduction_21
happyReduction_21 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  5 happyReduction_22
happyReduction_22 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  5 happyReduction_23
happyReduction_23 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  6 happyReduction_24
happyReduction_24 (HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn4
		 (Var (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  7 happyReduction_25
happyReduction_25 (HappyTerminal (TokSymbol (Loc happy_var_3 SymStringEnd)))
	(HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymStringBegin)))
	 =  HappyAbsSyn4
		 (StringTem (locate happy_var_1 <> happy_var_3) happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  7 happyReduction_26
happyReduction_26 (HappyTerminal (TokSymbol (Loc happy_var_2 SymStringEnd)))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymStringBegin)))
	 =  HappyAbsSyn4
		 (StringTem (locate happy_var_1 <> locate happy_var_2) mempty
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happyReduce 4 8 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (V.snoc happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_2  8 happyReduction_28
happyReduction_28 (HappyTerminal (TokStringLit happy_var_2))
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (V.snoc happy_var_1 (String (locate happy_var_2) (unLoc happy_var_2))
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  8 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (V.singleton happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  8 happyReduction_30
happyReduction_30 (HappyTerminal (TokStringLit happy_var_1))
	 =  HappyAbsSyn8
		 (V.singleton (String (locate happy_var_1) (unLoc happy_var_1))
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  9 happyReduction_31
happyReduction_31 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  9 happyReduction_32
happyReduction_32 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  9 happyReduction_33
happyReduction_33 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happyReduce 4 9 happyReduction_34
happyReduction_34 ((HappyTerminal (TokSymbol (Loc happy_var_4 SymParenClose))) `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Ap (locate happy_var_1 <> locate happy_var_4) happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_1  10 happyReduction_35
happyReduction_35 (HappyTerminal (TokNumLit _ happy_var_1))
	 =  HappyAbsSyn4
		 (Number (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  10 happyReduction_36
happyReduction_36 (HappyTerminal (TokIntLit _ happy_var_1))
	 =  HappyAbsSyn4
		 (Number (locate happy_var_1) (S.scientific (fromIntegral (unLoc happy_var_1)) 0)
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  11 happyReduction_37
happyReduction_37 (HappyTerminal (TokBoolLit happy_var_1))
	 =  HappyAbsSyn4
		 (Boolean (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  11 happyReduction_38
happyReduction_38 (HappyTerminal (TokBoolLit happy_var_1))
	 =  HappyAbsSyn4
		 (Boolean (locate happy_var_1) (unLoc happy_var_1)
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  12 happyReduction_39
happyReduction_39 (HappyTerminal (TokIdentifier (Loc happy_var_1 "null" )))
	 =  HappyAbsSyn4
		 (Null (locate happy_var_1)
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  13 happyReduction_40
happyReduction_40 (HappyTerminal (TokSymbol (Loc happy_var_3 SymSquareClose)))
	(HappyAbsSyn8  happy_var_2)
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymSquareOpen)))
	 =  HappyAbsSyn4
		 (Array (locate happy_var_1 <> locate happy_var_3) happy_var_2
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  13 happyReduction_41
happyReduction_41 (HappyTerminal (TokSymbol (Loc happy_var_2 SymSquareClose)))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymSquareOpen)))
	 =  HappyAbsSyn4
		 (Array (locate happy_var_1 <> locate happy_var_2) V.empty
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  14 happyReduction_42
happyReduction_42 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn8
		 (V.singleton happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  14 happyReduction_43
happyReduction_43 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (V.snoc happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  15 happyReduction_44
happyReduction_44 (HappyTerminal (TokSymbol (Loc happy_var_3 SymCurlyClose)))
	(HappyAbsSyn16  happy_var_2)
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymCurlyOpen)))
	 =  HappyAbsSyn4
		 (Object (locate happy_var_1 <> locate happy_var_3) (Compat.fromList happy_var_2)
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_2  15 happyReduction_45
happyReduction_45 (HappyTerminal (TokSymbol (Loc happy_var_2 SymCurlyClose)))
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymCurlyOpen)))
	 =  HappyAbsSyn4
		 (Object (locate happy_var_1 <> locate happy_var_2) mempty
	)
happyReduction_45 _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_1  16 happyReduction_46
happyReduction_46 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_46 _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  16 happyReduction_47
happyReduction_47 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_3 : happy_var_1
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happyReduce 5 17 happyReduction_48
happyReduction_48 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 ((unLoc happy_var_2, happy_var_5)
	) `HappyStk` happyRest

happyReduce_49 = happyReduce 4 17 happyReduction_49
happyReduction_49 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (("", happy_var_4)
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_2  18 happyReduction_50
happyReduction_50 (HappyTerminal (TokStringLit happy_var_2))
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 <> happy_var_2
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  18 happyReduction_51
happyReduction_51 (HappyTerminal (TokStringLit happy_var_1))
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_2  19 happyReduction_52
happyReduction_52 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (foldFields happy_var_1 happy_var_2
	)
happyReduction_52 _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  20 happyReduction_53
happyReduction_53 (HappyTerminal (TokIdentifier happy_var_2))
	_
	 =  HappyAbsSyn20
		 ((NotOptional, Var (locate happy_var_2) (unLoc happy_var_2))
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  20 happyReduction_54
happyReduction_54 (HappyTerminal (TokIdentifier happy_var_3))
	_
	_
	 =  HappyAbsSyn20
		 ((Optional, Var (locate happy_var_3) (unLoc happy_var_3))
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happyReduce 6 20 happyReduction_55
happyReduction_55 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokStringLit happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 ((Optional, Var (locate happy_var_4) (unLoc happy_var_4))
	) `HappyStk` happyRest

happyReduce_56 = happyReduce 5 20 happyReduction_56
happyReduction_56 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokStringLit happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 ((NotOptional, Var (locate happy_var_3) (unLoc happy_var_3))
	) `HappyStk` happyRest

happyReduce_57 = happySpecReduce_3  20 happyReduction_57
happyReduction_57 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn20
		 ((NotOptional, happy_var_2)
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happyReduce 4 20 happyReduction_58
happyReduction_58 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 ((Optional, happy_var_3)
	) `HappyStk` happyRest

happyReduce_59 = happyReduce 12 21 happyReduction_59
happyReduction_59 ((HappyTerminal (TokSymbol (Loc happy_var_12 SymDoubleCurlyClose))) `HappyStk`
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

happyReduce_60 = happyReduce 12 22 happyReduction_60
happyReduction_60 ((HappyTerminal (TokSymbol (Loc happy_var_12 SymDoubleCurlyClose))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokIdentifier happy_var_5)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokSymbol (Loc happy_var_1 SymDoubleCurlyOpen))) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Range (locate happy_var_1 <> locate happy_var_12) (fmap unLoc happy_var_3) (unLoc happy_var_5) happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_61 = happySpecReduce_1  23 happyReduction_61
happyReduction_61 _
	 =  HappyAbsSyn23
		 (Nothing
	)

happyReduce_62 = happySpecReduce_1  23 happyReduction_62
happyReduction_62 (HappyTerminal (TokIdentifier happy_var_1))
	 =  HappyAbsSyn23
		 (Just happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  24 happyReduction_63
happyReduction_63 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 (reverse happy_var_1
	)
happyReduction_63 _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_0  25 happyReduction_64
happyReduction_64  =  HappyAbsSyn25
		 ([]
	)

happyReduce_65 = happySpecReduce_2  25 happyReduction_65
happyReduction_65 (HappyAbsSyn20  happy_var_2)
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_2 : happy_var_1
	)
happyReduction_65 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 64 64 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokNumLit _ happy_dollar_dollar -> cont 26;
	TokIntLit _ happy_dollar_dollar -> cont 27;
	TokBoolLit happy_dollar_dollar -> cont 28;
	TokBoolLit happy_dollar_dollar -> cont 29;
	TokSymbol (Loc happy_dollar_dollar SymStringBegin) -> cont 30;
	TokSymbol (Loc happy_dollar_dollar SymStringEnd) -> cont 31;
	TokStringLit happy_dollar_dollar -> cont 32;
	TokIdentifier (Loc happy_dollar_dollar "if") -> cont 33;
	TokIdentifier (Loc happy_dollar_dollar "else") -> cont 34;
	TokIdentifier (Loc happy_dollar_dollar "end") -> cont 35;
	TokIdentifier (Loc happy_dollar_dollar "null" ) -> cont 36;
	TokIdentifier (Loc happy_dollar_dollar "range") -> cont 37;
	TokIdentifier (Loc happy_dollar_dollar "in") -> cont 38;
	TokIdentifier happy_dollar_dollar -> cont 39;
	TokSymbol (Loc happy_dollar_dollar SymSingleQuote) -> cont 40;
	TokSymbol (Loc happy_dollar_dollar SymColon) -> cont 41;
	TokSymbol (Loc happy_dollar_dollar SymDot) -> cont 42;
	TokSymbol (Loc happy_dollar_dollar SymComma) -> cont 43;
	TokSymbol (Loc happy_dollar_dollar SymQuestionMark) -> cont 44;
	TokSymbol (Loc happy_dollar_dollar SymDoubleQuestionMark) -> cont 45;
	TokSymbol (Loc happy_dollar_dollar SymEq) -> cont 46;
	TokSymbol (Loc happy_dollar_dollar SymNotEq) -> cont 47;
	TokSymbol (Loc happy_dollar_dollar SymGt) -> cont 48;
	TokSymbol (Loc happy_dollar_dollar SymLt) -> cont 49;
	TokSymbol (Loc happy_dollar_dollar SymLte) -> cont 50;
	TokSymbol (Loc happy_dollar_dollar SymGte) -> cont 51;
	TokSymbol (Loc happy_dollar_dollar SymAnd) -> cont 52;
	TokSymbol (Loc happy_dollar_dollar SymOr) -> cont 53;
	TokSymbol (Loc happy_dollar_dollar SymUnderscore) -> cont 54;
	TokSymbol (Loc happy_dollar_dollar SymAssignment) -> cont 55;
	TokSymbol (Loc happy_dollar_dollar SymCurlyOpen) -> cont 56;
	TokSymbol (Loc happy_dollar_dollar SymCurlyClose) -> cont 57;
	TokSymbol (Loc happy_dollar_dollar SymDoubleCurlyOpen) -> cont 58;
	TokSymbol (Loc happy_dollar_dollar SymDoubleCurlyClose) -> cont 59;
	TokSymbol (Loc happy_dollar_dollar SymSquareOpen) -> cont 60;
	TokSymbol (Loc happy_dollar_dollar SymSquareClose) -> cont 61;
	TokSymbol (Loc happy_dollar_dollar SymParenOpen) -> cont 62;
	TokSymbol (Loc happy_dollar_dollar SymParenClose) -> cont 63;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 64 tk tks = happyError' (tks, explist)
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

foldFields :: ValueExt -> [(Optionality, ValueExt)] -> ValueExt
foldFields rec [] = rec
foldFields rec [(opt, field)] = Field (locate rec <> locate field) opt rec field
foldFields rec ((opt, field):xs) = Field (locate rec <> locate field) opt rec (foldFields field xs)
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
