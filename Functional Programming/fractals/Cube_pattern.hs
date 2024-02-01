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


cubeAtSpecifiedPosition :: Point -> Float -> Shape
cubeAtSpecifiedPosition (x, y, z) length =
  let halfLength = length / 2
      vertices =
        [ (x - halfLength, y - halfLength, z - halfLength),
          (x + halfLength, y - halfLength, z - halfLength),
          (x + halfLength, y + halfLength, z - halfLength),
          (x - halfLength, y + halfLength, z - halfLength),
          (x - halfLength, y - halfLength, z + halfLength),
          (x + halfLength, y - halfLength, z + halfLength),
          (x + halfLength, y + halfLength, z + halfLength),
          (x - halfLength, y + halfLength, z + halfLength)
        ]
      triangles =
        [ (head vertices, vertices !! 1, vertices !! 2),
          (head vertices, vertices !! 2, vertices !! 3),
          (vertices !! 1, vertices !! 5, vertices !! 6),
          (vertices !! 1, vertices !! 6, vertices !! 2),
          (vertices !! 4, head vertices, vertices !! 3),
          (vertices !! 4, vertices !! 3, vertices !! 7),
          (vertices !! 5, vertices !! 4, vertices !! 7),
          (vertices !! 5, vertices !! 7, vertices !! 6),
          (vertices !! 4, head vertices, vertices !! 1),
          (vertices !! 4, vertices !! 1, vertices !! 5),
          (vertices !! 3, vertices !! 2, vertices !! 6),
          (vertices !! 3, vertices !! 6, vertices !! 7)
        ]
   in triangles

cubeFractal :: Int -> Shape
cubeFractal x = createCubes (x+1) (0, 0, 0) 10.0

createCubes :: Int -> Point -> Float -> Shape
createCubes 0 _ _ = cubeAtSpecifiedPosition (0, 0, 0) 10.0
createCubes x position length = centerCube ++ subCubes
    where
        centerCube = cubeAtSpecifiedPosition position length
        subCubes = concatMap (\i -> createCubes (x - 1) (positionCube i position length) (lengthCube i length)) [1..6]

positionCube :: Int -> Point -> Float -> Point
positionCube i (x1, x2, x3) length
    | i == 1 = (x1 + length * 3/4, x2, x3)
    | i == 2 = (x1 - length * 3/4, x2, x3)
    | i == 3 = (x1, x2 + length * 3/4, x3)
    | i == 4 = (x1, x2 - length * 3/4, x3)
    | i == 5 = (x1, x2, x3 + length * 3/4)
    | i == 6 = (x1, x2, x3 - length * 3/4)
    | otherwise = (0, 0, 0)  

lengthCube :: Int -> Float -> Float
lengthCube i length
    | i == 1 || i == 2 || i == 3 || i == 4 || i == 5 || i == 6 = length / 2
    | otherwise = 10.0  

main :: IO ()
main = do
    let shape = cubeFractal 3
    writeObjModel shape "output.stl"