import Prelude hiding (sequence)
import Language.Mes.Compiler
import Language.Mes.Language

main :: IO ()
main = do
  putStrLn $ compileToString $
           Script $ sequence [ (SDefFun "print" [ParamDef ("msg", TString)] TUnit
                                            (SRun (EStringLit "echo") [EVar "msg"]))
                             , (SDefFun "twice" [ParamDef ("f", TFun [TString] TUnit)
                                                ,ParamDef ("p1", TString)
                                                ]
                                TUnit
                                $ sequence [ SCall (EVar "f") [EVar "p1"]
                                           , SCall (EVar "f") [EVar "p1"]
                                           ]
                               )
                             , (SDefFun "forever" [ParamDef ("action", TFun [] TUnit)] TUnit
                                        $ sequence [ SCall (EVar "action") []
                                                   , SCall (EFunRef "forever") [EVar "action"]
                                                   ]
                               )
                             , (SDefFun "id" [ParamDef ("value", TString)] TString
                                        $ SReturn (EVar "value")
                               )
                             , (SVarDecl "x" $ EStringLit "alma")
                             , (SVarDecl "y" $ EApply (EFunRef "id") [EVar "x"])
                             , (SCall (EFunRef "print") [EVar "y"])
                             , (SCall (EFunRef "twice") [EFunRef "print", EVar "y"])
                             , (SDefFun "printkorte" [] TUnit $ SCall (EFunRef "print") [EStringLit "korte"])
                             , (SCall (EFunRef "forever") [EFunRef "printkorte"])
                             ]
