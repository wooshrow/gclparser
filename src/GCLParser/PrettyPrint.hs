module GCLParser.PrettyPrint where

import Prelude hiding ((<>))
import Data.List      (intersperse)
import Text.PrettyPrint

import GCLParser.GCLDatatype

ppProgram2String :: Program -> String
ppProgram2String prg = show . ppProgram $ prg

ppProgram :: Program -> Doc
ppProgram = vcat . intersperse (text "") . map ppProcedure . procedures

ppProcedure :: Procedure -> Doc
ppProcedure Procedure {name,input,output,preCondition,postCondition,stmt}
    =   maybe mempty pre preCondition
    $+$ (text name <> char '(' <> args <> text ") {")
    $+$ tab (ppStmt stmt)
    $+$ char '}'
    $+$ maybe mempty post postCondition
    where
        pre   p = text "pre {" <> ppExpr p <> char '}'
        post  p = text "post {" <> ppExpr p <> char '}'
        args    = input' <> text " | " <> output'
        input'  = commaSeparated ppVarDeclaration input
        output' = commaSeparated ppVarDeclaration output


ppExpr :: Expr -> Doc
ppExpr e = text . show $ e


ppStmt :: Stmt -> Doc
ppStmt Skip                 = text "skip"
ppStmt (Assert e)           = text "assert " <> ppExpr e 
ppStmt (Assume e)           = text "assume " <> ppExpr e 
ppStmt (Assign x e)         = text x <> text " := " <> ppExpr e
ppStmt (DrefAssign x e)     = text x <> text ".val := " <> ppExpr e
ppStmt (AAssign x i e)      = text x <> char '[' <> ppExpr i <> char ']' <> text " := " <> ppExpr e
ppStmt (Call vars f args)   = text "(" <> commaSeparated text vars <> text ") := " <> text f <> text "(" <> commaSeparated ppExpr args <> text ")"
ppStmt (Seq s1 s2)          = ppStmt s1 <> char ';' $+$ ppStmt s2
ppStmt (IfThenElse g s1 s2) =   text "if " <> ppExpr g <> text " then {"
                            $+$ tab (ppStmt s1)
                            $+$ text "} else {"
                            $+$ tab (ppStmt s2)
                            $+$ char '}'
ppStmt (While g s)          = text "while " <> ppExpr g <> text " do {"
                            $+$ tab (ppStmt s)
                            $+$ char '}'
ppStmt (TryCatch e s h) = text "try{ " 
                          $+$ tab (ppStmt s <> text "}")
                          $+$ text ("catch(" ++ e ++ "){")
                          $+$ tab (ppStmt h <> text "}")     
                                            
ppStmt (Block vardecls s) = (text "var " <> commaSeparated ppVarDeclaration vardecls <> text " {")
                          $+$ tab (ppStmt s <> text "}")


ppVarDeclaration :: VarDeclaration -> Doc
ppVarDeclaration (VarDeclaration s t)
    = text s <> char ':' <> ppType t

ppType :: Type -> Doc
ppType (PType t)   = ppPrimitiveType t
ppType RefType     = text "ref"
ppType (AType t) = brackets (text "") <> ppPrimitiveType t

ppPrimitiveType :: PrimitiveType -> Doc
ppPrimitiveType PTInt  = text "int"
ppPrimitiveType PTBool = text "bool"

tab :: Doc -> Doc
tab = nest 4

commaSeparated :: (a -> Doc) -> [a] -> Doc
commaSeparated f = hcat . intersperse comma . map f
