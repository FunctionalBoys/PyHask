module Parser.ExecutableCreator (checkPlaceholder, displayTypeMemoryBlock, displayMemoryBlock, displayVarTmpMemoryBlock) where

import           Parser.ParserTypes

checkPlaceholder :: Quad -> Either String String
checkPlaceholder (QuadFloatConvert (Address t1) (Address t2)) = Right $ "FloatConvert " ++ show t1 ++ " null " ++ show t2
checkPlaceholder (QuadNot (Address t1) (Address t2)) = Right $ "Not " ++ show t1 ++ " null " ++ show t2
checkPlaceholder (QuadNeg (Address t1) (Address t2)) = Right $ "Neg " ++ show t1 ++ " null " ++ show t2
checkPlaceholder (QuadRead (Address t1)) = Right $ "Read " ++ show t1 ++ " null null"
checkPlaceholder (QuadPrint (Address t1)) = Right $ "Print " ++ show t1 ++ " null null"
checkPlaceholder (QuadAssign (Address t1) (Address t2)) = Right $ "Assign " ++ show t1 ++ " null " ++ show t2
checkPlaceholder (QuadFPlaceholder _) = Left "No deberian existir Placeholders en esta fase!"
checkPlaceholder (QuadF (Address t1) t2) = Right $ "GOTOF " ++ show t1 ++ " null " ++ show t2
checkPlaceholder (QuadTPlaceholder _) = Left "No deberian existir Placeholders en esta fase!"
checkPlaceholder (QuadT (Address t1) t2) = Right $ "GOTOT " ++ show t1 ++ " null " ++ show t2
checkPlaceholder QuadGOTOPlaceholder = Left "No deberian existir Placeholders en esta fase!"
checkPlaceholder (QuadGOTO t1) = Right $ "GOTO " ++ "null null " ++ show t1
checkPlaceholder (QuadGOSUB t) = Right $ "GOSUB " ++ show t
checkPlaceholder (QuadEra t) = Right $ "Era " ++ show t
checkPlaceholder (QuadFuncParam (Address t1) t2) = Right $ "FuncParam " ++ show t1 ++ " null " ++ show t2
checkPlaceholder QuadEndFunc = Right "EndFunc null null null"
checkPlaceholder (QuadVerify (Address t1) (Address t2) (Address t3)) = Right $ "ArrayVerify " ++ show t1 ++ " " ++ show t2 ++ " " ++ show t3
checkPlaceholder (QuadArrayAccess (Address t1) (Address t2) (Address t3)) = Right $ "ArrayAccess " ++ show t1 ++ " " ++ show t2 ++ " " ++ show t3
checkPlaceholder (QuadArrayAssign (Address t1) (Address t2) (Address t3)) = Right $ "ArrayAssign " ++ show t1 ++ " " ++ show t2 ++ " " ++ show t3
checkPlaceholder QuadNoOP = Right "NoOp null null null"
checkPlaceholder QuadEnd = Right "End null null null"
checkPlaceholder (QuadOp op (Address t1) (Address t2) (Address t3)) = Right $ show op ++ " " ++ show t1 ++ " " ++ show t2 ++ " " ++ show t3

displayTypeMemoryBlock :: TypeMemoryBlock -> String
displayTypeMemoryBlock (TypeMemoryBlock mlb mub cd) = show mlb ++ " " ++ show mub ++ " " ++ show cd

displayMemoryBlock :: MemoryBlock ->  String
displayMemoryBlock (MemoryBlock mbi mbf mbc mbb) = displayTypeMemoryBlock mbi ++ "\n" ++ displayTypeMemoryBlock mbf ++ "\n" ++ displayTypeMemoryBlock mbc ++ "\n" ++ displayTypeMemoryBlock mbb

displayVarTmpMemoryBlock :: MemoryBlock -> MemoryBlock -> String
displayVarTmpMemoryBlock (MemoryBlock mbi1 mbf1 mbc1 mbb1) (MemoryBlock mbi2 mbf2 mbc2 mbb2) = 
    displayTypeMemoryBlock mbi1 ++ " " ++ displayTypeMemoryBlock mbi2 ++ "\n" ++
    displayTypeMemoryBlock mbf1 ++ " " ++ displayTypeMemoryBlock mbf2 ++ "\n" ++
    displayTypeMemoryBlock mbc1 ++ " " ++ displayTypeMemoryBlock mbc2 ++ "\n" ++
    displayTypeMemoryBlock mbb1 ++ " " ++ displayTypeMemoryBlock mbb2 ++ "\n"