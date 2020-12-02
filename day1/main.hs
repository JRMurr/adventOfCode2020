toPairs :: Ord a => [a] -> [(a, a)]
toPairs l = [(x,y) | x <- l, y <- l, x < y]

toTriples :: Ord a => [a] -> [(a, a, a)]
toTriples l = [(x,y,z) | x <- l, y <- l, x < y, z <- l,  y < z]

part1 :: IO ()
part1 = do
    fp <- readFile "./in"
    let input = map (read::String->Int) (lines fp)
    let pairs = toPairs input
    let validPairs = [(x,y) | (x,y) <- pairs, x+y == 2020]
    let (x,y) = head validPairs
    print (x,y, x*y)


part2 :: IO ()
part2 = do
    fp <- readFile "./in"
    let input = map (read::String->Int) (lines fp)
    let triples = toTriples input
    let validTriples = [(x,y,z) | (x,y,z) <- triples, x+y+z == 2020]
    let (x,y,z) = head validTriples
    print (x,y,z, x*y*z)

main = do
    part2