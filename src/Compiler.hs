{-# LANGUAGE OverloadedStrings #-}
module Compiler where

import qualified Data.ByteString.Lazy as B
import GHC.Int
import Parser
import Lexer
import qualified Writer
-- import Codec.Binary.UTF8.String

type U1 = Int8
type U2 = Int16
type U4 = Int32

data ConstantPool = 
    MethodRef U2 U2
    | FieldRef U2 U2
    | Class U2
    | Utf8 B.ByteString
    | NameAndType U2 U2

data AccessFlag = ACC_PUBLIC | ACC_FINAL | ACC_SUPER | ACC_INTERFACE | ACC_ABSTRACT | ACC_SYNTHETIC | ACC_ANNOTATION | ACC_ENUM | ACC_PRIVATE | ACC_PROTECTED | ACC_STATIC | ACC_VOLATILE | ACC_TRANSIENT

compile :: Program -> IO()
compile prg = do
                Writer.init -- 書き込み先ファイルの初期化
                magic -- CAFEBABEの代入
                version 0 52 -- version情報を記入
                constPools [
                    MethodRef 5 13,
                    FieldRef 14 15,
                    MethodRef 16 17,
                    Class 12,
                    Class 18, -- 5
                    Utf8 "<init>",
                    Utf8 "()V",
                    Utf8 "Code",
                    Utf8 "main",
                    Utf8 "([Ljava/lang/String;)V", -- 10
                    Utf8 "SourceFile",
                    Utf8 "HIRAETH",
                    NameAndType 6 7,
                    Class 17,
                    NameAndType 20 21, -- 15
                    Class 22,
                    NameAndType 23 24,
                    Utf8 "java/lang/Object",
                    Utf8 "java/lang/System",
                    Utf8 "out", -- 20
                    Utf8 "Ljava/io/PrintStream;",
                    Utf8 "java/io/PrintStream",
                    Utf8 "println",
                    Utf8 "(I)V"
                    ]
                access_flag [ACC_SUPER]
                Writer.u2Write' [4, 5, 0, 0, 2] -- this_class, super_class, interfaces_count, fields_count, methods_count
                super_class
                main_class prg
                attributes 


-- magic
magic :: IO()
magic = Writer.u1Write' [0xCA, 0xFE, 0xBA, 0xBE]

-- version
version :: Int16 -> Int16 -> IO()
version minor major = Writer.u2Write' [minor, major]


-- constpools 
constPools :: [ConstantPool] -> IO()
constPools cp = do
                    let const_pool_count = (fromIntegral(1 + length cp)) :: Int16 
                    Writer.u2Write const_pool_count
                    constPools' cp

constPools' :: [ConstantPool] -> IO()
constPools' (c:[]) = constPoolWrite c
constPools' (c:cp) = do
                        constPoolWrite c
                        constPools' cp

constPoolWrite :: ConstantPool -> IO()
constPoolWrite (MethodRef cls_idx nat_idx) = 
    do
        Writer.u1Write 10
        Writer.u2Write' [cls_idx, nat_idx]
constPoolWrite (FieldRef cls_idx nat_idx) =
    do
        Writer.u1Write 9
        Writer.u2Write' [cls_idx, nat_idx]
constPoolWrite (Class name_idx) = 
    do
        Writer.u1Write 7
        Writer.u2Write name_idx
constPoolWrite (Utf8 bstr) = 
    do
        Writer.u1Write 1
        Writer.u2Write ((fromIntegral (B.length bstr))::Int16)
        Writer.bstrWrite bstr
constPoolWrite (NameAndType name_idx desc_idx) =
    do
        Writer.u1Write 12
        Writer.u2Write' [name_idx, desc_idx]


-- access_flag
access_flag :: [AccessFlag] -> IO()
access_flag afs = Writer.u2Write $ foldr aflag_calc 0 afs

aflag_calc :: AccessFlag -> Int16 -> Int16
aflag_calc flag i = case flag of
    ACC_PUBLIC     -> i + 0x0001
    ACC_PRIVATE    -> i + 0x0002
    ACC_PROTECTED  -> i + 0x0004
    ACC_STATIC     -> i + 0x0008
    ACC_FINAL      -> i + 0x0010
    ACC_SUPER      -> i + 0x0020
    ACC_VOLATILE   -> i + 0x0040
    ACC_TRANSIENT  -> i + 0x0080
    ACC_INTERFACE  -> i + 0x0200
    ACC_ABSTRACT   -> i + 0x0400
    ACC_SYNTHETIC  -> i + 0x1000
    ACC_ANNOTATION -> i + 0x2000
    ACC_ENUM       -> i + 0x4000


-- methods
super_class :: IO()
super_class = do
                access_flag []
                Writer.u2Write' [6, 7, 1, 8] -- name_index, descripter_index, attributes_count, attribute_name_index
                Writer.u4Write 15 -- attribute_length
                Writer.u2Write' [1,1] -- max_stack, max_locals
                Writer.u4Write 5 -- attribute_length
                Writer.u1Write' [0x2A, 0xB7, 0x00, 0x01, 0xB1] -- code
                Writer.u2Write' [0,0] -- exception_name_index, attributes_count

main_class :: Program -> IO()
main_class prg = do
                access_flag [ACC_PUBLIC, ACC_STATIC]
                Writer.u2Write' [9, 10, 1, 8] -- name_index, descripter_index, attributes_count, attribute_name_index
                let codes = analyze prg
                let codes_len = fromIntegral $ length codes :: Int32
                Writer.u4Write (codes_len + 12)
                Writer.u2Write' [3, 1] -- max_stack, max_locals
                Writer.u4Write codes_len
                Writer.u1Write' codes
                Writer.u2Write' [0, 0]
                


analyze :: Program -> [U1]
analyze prg = case prg of
    PrgNone -> []
    PrgP (Print expr) -> printPrg expr
    PrgA (AssignExpr ident expr) -> assign ident expr
    Prg p1 p2 -> (analyze p1) ++ (analyze p2)

printPrg :: Expr -> [U1]
printPrg exp = [0xb2, 0x00, 0x02] ++ expr exp ++ [0xb6, 0x00, 0x03]

assign :: Ident -> Expr -> [U1]
assign _ _ = []

expr :: Expr -> [U1]
expr ExpNone = []
expr (ExpI i) = [0x00]
expr (ExpN (Number i)) = [0x10, ((fromIntegral i)::Int8)]
expr (ExpO op) = case op of
    Plus -> [96]
    Minus -> [100]
    Mult -> [104]
expr (Expr exp1 exp2) = (expr exp1) ++ (expr exp2)


attributes :: IO()
attributes = do
                Writer.u2Write 11
                Writer.u4Write 2
                Writer.u2Write 12
