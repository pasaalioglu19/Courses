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

kochsSnowFlake :: Int -> Shape
kochsSnowFlake 0 = [((0, 0, 0), (10, 0, 0), (5, sqrt 3 * 5, 0))]
kochsSnowFlake n =
    let smallTriangles = kochsSnowFlake (n - 1)
        allTriangles = concatMap subTriangles smallTriangles
    in allTriangles ++ smallTriangles

subTriangles :: Triangle -> Shape
subTriangles ((x1, y1, z1), (x2, y2, z2), (x3, y3, z3)) =
    let l1 = ((x3+2*x1) / 3, (y3+2*y1) / 3, 0)
        l2 = ((x1+2*x3) / 3, (y1+2*y3) / 3, 0)
        l3x = (((x1+2*x3) / 3 - (x3+2*x1) / 3)* cos (pi / 3) - ((y1+2*y3) / 3 - (y3+2*y1) / 3)* sin (pi / 3)) + (x3+2*x1) / 3
        l3y = (((x1+2*x3) / 3 - (x3+2*x1) / 3)* sin (pi / 3) + ((y1+2*y3) / 3 - (y3+2*y1) / 3)* cos (pi / 3)) + (y3+2*y1) / 3

        r1 = ((x2+2*x3) / 3, (y2+2*y3) / 3, 0)
        r2 = ((x3+2*x2) / 3, (y3+2*y2) / 3, 0)
        r3x = (((x2+2*x3) / 3 - (x3+2*x2) / 3)* cos (5*pi / 3) - ((y2+2*y3) / 3 - (y3+2*y2) / 3)* sin (5*pi / 3)) + (x3+2*x2) / 3
        r3y = (((x2+2*x3) / 3 - (x3+2*x2) / 3)* sin (5*pi / 3) + ((y2+2*y3) / 3 - (y3+2*y2) / 3)* cos (5*pi / 3)) + (y3+2*y2) / 3

        b1 = ((x1+2*x2) / 3, (y1+2*y2) / 3, 0)
        b2 = ((x2+2*x1) / 3, (y2+2*y1) / 3, 0)
        b3x = (((x1+2*x2) / 3 - (x2+2*x1) / 3)* cos (5*pi / 3) - ((y1+2*y2) / 3 - (y2+2*y1) / 3)* sin (5*pi / 3)) + (x2+2*x1) / 3
        b3y = (((x1+2*x2) / 3 - (x2+2*x1) / 3)* sin (5*pi / 3) + ((y1+2*y2) / 3 - (y2+2*y1) / 3)* cos (5*pi / 3)) + (y2+2*y1) / 3

    in [(l1, l2, (l3x,l3y,0)), (r1,r2,(r3x,r3y,0)), (b1,b2,(b3x,b3y,0))]

main :: IO ()
main = do
    let shape = kochsSnowFlake 4
    writeObjModel shape "output.stl"