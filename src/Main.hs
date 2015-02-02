import Prelude hiding (sequence)
import Language.Mes.Compiler
import Language.Mes.Language
import Language.Mes.PrettyPrint

main :: IO ()
main = do
  putStrLn $ pprint $
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
                                                   , SCall (EVar "forever") [EVar "action"]
                                                   ]
                               )
                             , (SDefFun "id" [ParamDef ("value", TString)] TString
                                        $ SReturn (EVar "value")
                               )
                             , (SVarDecl "x" $ EStringLit "alma")
                             , (SVarDecl "y" $ EApply (EVar "id") [EVar "x"])
                             , (SCall (EVar "print") [EVar "y"])
                             , (SCall (EVar "twice") [EVar "print", EVar "y"])
                             , (SDefFun "printkorte" [] TUnit $ SCall (EVar "print") [EStringLit "korte"])
                             , (SCall (EVar "forever") [EVar "printkorte"])
                             ]
