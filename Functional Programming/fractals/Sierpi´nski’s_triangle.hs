type Point = (Float,Float, Float)
type Triangle = (Point, Point, Point)
type Shape = [Triangle]

createTriangleDef :: Triangle -> String
createTriangleDef ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)) = "  facet\n" ++
                                                       "    outer loop\n" ++
                                                       "      vertex " ++ (show x1) ++ " " ++ (show y1) ++ " " ++ (show z1) ++ "\n" ++
                                                       "      vertex " ++ (show x2) ++ " " ++ (show y2) ++ " " ++ (show z2) ++ "\n" ++
                                                       "      vertex " ++ (show x3) ++ " " ++ (show y3) ++ " " ++ (show z3) ++ "\n" ++
                                                       "    endloop\n" ++
                                                       "  endfacet\n"                            

createObjectModelString :: Shape -> String 
createObjectModelString n = "solid Object01\n" ++ concat [createTriangleDef y | y<-n] ++ "endsolid Object01"

writeObjModel :: Shape -> String -> IO ()
writeObjModel x filename = do writeFile filename (createObjectModelString x)


sierpinskiTriangle :: Int -> Shape
sierpinskiTriangle 0 = [((0, 0, 0), (10, 0, 0), (5, sqrt 3 * 5, 0))]
sierpinskiTriangle n =
    let smallTriangles = sierpinskiTriangle (n - 1)
        allTriangles = concatMap subTriangles smallTriangles
    in allTriangles

subTriangles :: Triangle -> Shape
subTriangles ((x1, y1, z1), (x2, y2, z2), (x3, y3, z3)) =
    let mid12 = ((x1 + x2) / 2, (y1 + y2) / 2, (z1 + z2) / 2)
        mid23 = ((x2 + x3) / 2, (y2 + y3) / 2, (z2 + z3) / 2)
        mid31 = ((x3 + x1) / 2, (y3 + y1) / 2, (z3 + z1) / 2)
    in [((x1, y1, z1), mid12, mid31),
        (mid12, (x2, y2, z2), mid23),
        (mid31, mid23, (x3, y3, z3))]

main :: IO ()
main = do
    let shape = sierpinskiTriangle 5
    writeObjModel shape "output.stl"