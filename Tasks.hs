{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Tasks where

import Dataset
import Data.List
import Text.Printf

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
	TASK SET 1
-}

-- Task 1
intRead x = if x == "" then 0
    else read x :: Integer
floatRead x = if x == "" then 0
    else read x :: Float

compute_exam_grades :: Table -> Table
compute_exam_grades = map aux
    where
        aux :: Row -> Row
        aux row = if head row == "Nume" then ["Nume", "Punctaj Exam"]
        else [head row, printf "%.2f" (floatRead(last row) + (sum (map floatRead ((init.tail) row)) / 4))]

-- Task 2
-- Number of students who have passed the exam:
get_passed_students_num :: Table -> Int
get_passed_students_num = foldr (\row acc -> if floatRead(last row) > 2.5 then acc + 1 else acc) 0 . tail . compute_exam_grades

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage = \t -> fromIntegral (get_passed_students_num t) / (fromIntegral . length . tail) t

-- Average exam grade
get_exam_avg :: Table -> Float
get_exam_avg = \t -> (foldr (\row acc -> floatRead(last row) + acc) 0 . tail . compute_exam_grades) t / (fromIntegral . length . tail) t

-- Number of students who gained at least 1.5p from homework:
get_passed_hw_num :: Table -> Int
get_passed_hw_num = foldr aux 0 . tail
    where
        aux row acc = if foldr (\x acc1 -> floatRead x + acc1) 0 ((tail . tail . init . init . init . init) row) >= 1.5 then acc + 1 else acc

-- Task 3

crop :: [[a]] -> [[a]]
crop = (init . tail) . transpose . tail

get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs = \t -> ["Q1", "Q2", "Q3", "Q4", "Q5", "Q6"] :
                            [map (printf "%.2f") ((map (\row -> foldr (\x acc -> floatRead x + acc) 0 row / (fromIntegral . length) row). crop) t)]

-- Task 4
get_exam_summary :: Table -> Table
get_exam_summary = \t ->  ["Q", "0", "1", "2"] : (map aux . ((init.tail) . transpose)) t
    where
        aux :: Row -> Row
        aux r = foldr compute [head r, "0", "0", "0"] (tail r)
            where
                compute :: Value -> Row -> Row
                compute x acc
                    | x == "1" = head acc : (head . tail) acc : (show . (+1) . intRead . last . init) acc : (tail.tail.tail) acc
                    | x == "2" = init acc ++ [(show . (+1) . intRead . last) acc]
                    | otherwise = head acc : (show . (+1) . intRead . head . tail) acc : (tail.tail) acc

-- Task 5

cmp :: Row -> Row -> Ordering
cmp r1 r2
            | (floatRead . last) r1 < (floatRead . last) r2 = LT
            | ((floatRead . last) r1 == (floatRead . last) r2) && (head r1 < head r2) = LT
            | otherwise = GT

get_ranking :: Table -> Table
get_ranking = \t -> ["Nume", "Punctaj Exam"] : (sortBy cmp . tail . compute_exam_grades) t

-- Task 6

get_exam_diff_table :: Table -> Table
get_exam_diff_table = \t -> ["Nume", "Punctaj interviu", "Punctaj scris", "Diferenta"] :
                sortBy cmp (map aux (tail t))
                    where
                        aux :: Row -> Row
                        aux r = head r : printf "%.2f" (foldr (\x acc -> acc + floatRead x ) 0 ((tail.init) r) / 4) : (printf "%.2f" . floatRead .last) r : [(printf "%.2f" . abs) ((foldr (\x acc -> acc + floatRead x ) 0 ((tail.init) r) / 4) - floatRead(last r))]

-- SET 2

splitBy :: Char -> String -> [String]
splitBy ch = foldr aux [[]]
    where
        aux x acc
            | x == ch = [] : acc
            | otherwise = (x : head acc) : tail acc

read_csv :: CSV -> Table
read_csv csv =  map (splitBy ',' ) (splitBy '\n' csv)

write_csv :: Table -> CSV
write_csv = init . foldr (\row acc -> (init . aux) row ++ "\n" ++ acc) []
    where
        aux = foldr (\x acc -> x ++ "," ++ acc) []

-- Task 1

as_list :: String -> Table -> [String]
as_list s = tail . searchFor s . transpose
    where
        searchFor :: String -> Table -> Row
        searchFor s [] = []
        searchFor s (x:xs) = if s == head x then x else searchFor s xs

-- Task 2

searchForInt :: String -> Table -> Int -> Int
searchForInt s [] acc = 0
searchForInt s (x:xs) acc = if s == head x then acc else searchForInt s xs acc + 1

floatReadAux x = if x == "" then -1
    else read x :: Float


tsort :: String -> Table -> Table
tsort s t = head t : sortBy (tcompare $ searchForInt s (transpose t) 1) (tail t)
    where
        tcompare :: Int -> Row -> Row -> Ordering
        tcompare nr r1 r2
            | (floatReadAux . last . take nr) r1 > (floatReadAux . last . take nr) r2 = GT
            | ((floatReadAux . last . take nr) r1 == (floatReadAux . last . take nr) r2) && (head r1 > head r2) = GT
            | otherwise = LT

-- Task 3

vmap :: (Value -> Value) -> Table -> Table
vmap func = map (map func)

-- Task 4

rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap func header = (header:) . map func . tail
get_hw_grade_total :: Row -> Row
get_hw_grade_total row = head row : [printf "%.2f" $ foldr (\x acc -> floatRead x + acc) 0 (drop 2 row)]

-- Task 5

vunion :: Table -> Table -> Table
vunion t1 t2 = if check (head t1) (head t2) then t1 ++ tail t2 else t1
    where
        check :: Row -> Row -> Bool
        check [] [] = True
        check [] _ = False
        check _ [] = False
        check h1 h2 = (head h1 == head h2) && check (tail h1) (tail h2)

-- Task 6

generateRow :: Int -> Row
generateRow 0 = []
generateRow nr = "" : generateRow (nr - 1)

hunion :: Table -> Table -> Table
hunion [] t2 = t2
hunion t1 [] = t1
hunion t1 t2 = if length t1 > length t2
                then zipWith (++) t1 (extendTable t2 (length t1 - length t2))
                else zipWith (++) (extendTable t1 (length t2 - length t1)) t2
    where
        extendTable :: Table -> Int -> Table
        extendTable t 0 = t
        extendTable t nr = extendTable (t ++ [generateRow $ length $ head t]) (nr - 1)

-- Task 7

findList :: String -> Table -> Row
findList s [] = []
findList s (row:table) = if elem s row then row else findList s table

tjoin :: String -> Table -> Table -> Table
tjoin s t1 t2 = auxFunction t1 t2 (searchForInt s (transpose t1) 1) (searchForInt s (transpose t2) 1)
    where
        auxFunction :: Table -> Table -> Int -> Int -> Table
        auxFunction t1 t2 nr1 nr2 = map (\row1 ->
            if elem (last $ take nr1 row1) (last $ take nr2 $ transpose t2)
            then row1 ++ (tail . findList (last $ take nr1 row1)) t2
            else row1 ++ generateRow ((length . head) t2 - 1) ) t1

-- Task 8

cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian func header t1 t2 = header : auxCartesian func (tail t1) (tail t2)
    where
        auxCartesian :: (Row -> Row -> Row) -> Table -> Table -> Table
        auxCartesian func [] t2 = []
        auxCartesian func (row1:t1) t2 = map (\row2 -> func row1 row2) t2 ++ auxCartesian func t1 t2

-- Task 9

projection :: [String] -> Table -> Table
projection list = transpose . foldr (\row acc ->
    if elem (head row) list then row : acc else acc) [] . transpose


-- SET 3

data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | Graph EdgeOp Query
    | forall a. FEval a => Filter (FilterCondition a) Query

-- Task 1

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

data QResult = CSV CSV | Table Table | List [String]

instance Show QResult where
    show (CSV csv) = show csv
    show (Table table) = write_csv table
    show (List list) = show list

class Eval a where
    eval :: a -> QResult

transformTable :: Query -> Table
transformTable = read_csv . show . eval

instance Eval Query where
    eval (FromCSV str) = Table $ read_csv str
    eval (ToCSV query) = CSV $ show $ eval query
    eval (AsList colname query) = List $ as_list colname (transformTable query)
    eval (Sort colname query) = Table $ tsort colname (transformTable query)
    eval (ValueMap op query) = Table $ vmap op (transformTable query)
    eval (RowMap op colnames query) = Table $ rmap op colnames (transformTable query)
    eval (VUnion query1 query2) = Table $ vunion (transformTable query1)
        (transformTable query2)
    eval (HUnion query1 query2) = Table $ hunion (transformTable query1)
        (transformTable query2)
    eval (TableJoin colname query1 query2) = Table $ tjoin colname
        (transformTable query1) (transformTable query2)
    eval (Cartesian op colnames query1 query2) = Table $ cartesian op colnames
        (transformTable query1) (transformTable query2)
    eval (Projection colnames query) = Table $ projection colnames
        (transformTable query)
    eval (Filter cond query) = Table $ head (transformTable query) :
        filter (feval (head $ transformTable query) cond) (tail $ transformTable query)
    eval (Graph edgeop query) = Table $ ["From", "To", "Value"] : (tail . nub)
        (apply_func edgeop (tail $ transformTable query) (tail $ transformTable query))
        where
            apply_func op [] table = []
            apply_func op (x:xs) table = (map (\y -> createRow (op x y) x y) table)
                ++ apply_func op xs table
                where
                    createRow (Just a) x y
                        | head x < head y = [head x, head y, a]
                        | head x == head y = ["", "", ""]
                        | otherwise = [head y, head x, a]
                    createRow Nothing x y = ["", "", ""]

-- Task 2

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

type FilterOp = Row -> Bool

class FEval a where
    feval :: [String] -> FilterCondition a -> FilterOp

findInd :: [String] -> String -> Int -> Int
findInd [] str acc = -1
findInd (x:xs) str acc = if x == str then acc else findInd xs str (acc + 1)

instance FEval Float where
    feval colnames (Eq colname ref) = \row ->
        floatRead (last (take (findInd colnames colname 1) row)) == ref
    feval colnames (Lt colname ref) = \row ->
        floatRead (last (take (findInd colnames colname 1) row)) < ref
    feval colnames (Gt colname ref) = \row ->
        floatRead (last (take (findInd colnames colname 1) row)) > ref
    feval colnames (In colname list) = \row ->
        floatRead (last (take (findInd colnames colname 1) row)) `elem` list
    feval colnames (FNot cond) = \row -> not $ feval colnames cond row
    feval colnames (FieldEq colname1 colname2) = \row ->
        floatRead (last (take (findInd colnames colname1 1) row)) ==
        floatRead (last (take (findInd colnames colname2 1) row))

instance FEval String where
    feval colnames (Eq colname ref) = \row ->
        last (take (findInd colnames colname 1) row) == ref
    feval colnames (Lt colname ref) = \row ->
        last (take (findInd colnames colname 1) row) < ref
    feval colnames (Gt colname ref) = \row ->
        last (take (findInd colnames colname 1) row) > ref
    feval colnames (In colname list) = \row ->
        last (take (findInd colnames colname 1) row) `elem` list
    feval colnames (FNot cond) = \row -> not $ feval colnames cond row
    feval colnames (FieldEq colname1 colname2) = \row ->
        last (take (findInd colnames colname1 1) row) ==
        last (take (findInd colnames colname2 1) row)

-- eval for Filter is implemented above.

-- Task 3

-- eval for Graph is implemented above.

-- Task 4

edge_op :: EdgeOp
edge_op l1 l2
    | head l1 == "" || head l2 == "" = Nothing
    | otherwise = Just $ show $ compute_no (tail l1) (tail l2) 0
        where
            compute_no [] [] acc = acc
            compute_no (x:xs) (y:ys) acc
                | x == y = compute_no xs ys (acc + 1)
                | otherwise = compute_no xs ys acc

filter_cond = Gt "Value" (4 :: Float)

similarities_query :: Query
similarities_query =
    Sort "Email"
        (Filter filter_cond
            (Graph edge_op
                (FromCSV lecture_grades_csv)))


 -- SET 4

-- Typos

generate 0 s = []
generate nr s = s : generate (nr - 1) s

distance :: String -> String -> String
distance xs ys = head $ foldr
    (\xs -> map head . scanr1 combine . zipWith (\x y -> [x,y]) xs)
    empty common_chars
        where
            common_chars = map (\x -> (map (\y -> if x == y then [x] else []) ys) ++ []) xs
            empty = generate (length ys) []
            combine [x1,y1] [x2,y2]
                | length x1 == 0 && length y1 > length x2 = y1 : [y1]
                | length x1 == 0 && length y1 <= length x2 = x2 : [y1]
                | otherwise = (x1 ++ y2) : [y1]

correct_col :: Table -> Table -> Table
correct_col c1 c2 = ["Nume"] : auxCorrect (tail c1) (tail c2) where
    auxCorrect :: Table -> Table -> Table
    auxCorrect [] t2 = []
    auxCorrect (row1:t1) t2 = [findMax (head row1) ((head . transpose) t2)] : auxCorrect t1 t2 where
        findMax :: String -> Row -> String
        findMax name names = last $ foldr
            (\correct_name acc -> if length (distance name correct_name) > length (head acc)
                then [distance name correct_name, correct_name]
                else acc)
            ["",""] names

correct_table :: String -> CSV -> CSV -> CSV
correct_table colname t1 t2 = write_csv $
    hunion (transpose (take (searchForInt colname (transpose $ read_csv t1) 0) (transpose $ read_csv t1))) -- prima parte a tabelului
    (hunion (correct_col (projection [colname] (read_csv t1)) (projection [colname] (read_csv t2))) -- partea modificata (aflata la mijloc)
    (transpose (drop (1 + searchForInt colname (transpose $ read_csv t1) 0) (transpose $ read_csv t1)))) -- ultima parte a tabelului

-- Project Summary

compute_hw :: CSV -> Table
compute_hw hw = ["Nume", "Punctaj Teme"] : map
    (\row -> head row : [printf "%.2f" $ mySum $ tail row])
    ((tail . read_csv) hw)
    where
        mySum = foldr (\el acc -> acc + floatRead el) 0

compute_lecture :: CSV -> CSV -> CSV -> Table
compute_lecture email_map lec_gr hw = compute_lecture_aux $ tjoin "Email"
    (read_csv $ correct_table "Nume" email_map hw)
    (["Email", "Punctaj Curs"] : map
        (\row -> head row : [printf "%.2f" $ (/ (fromIntegral $ length row - 1)) $ mySum $ tail row])
        ((tail . read_csv) lec_gr))
    where
        mySum = foldr (\el acc -> acc + 2 * floatRead el) 0

compute_lecture_aux :: Table -> Table
compute_lecture_aux table = hunion
    (transpose $ take 1 $ transpose $ table)
    (transpose $ drop 2 $ transpose $ table)

string_tsort :: String -> Table -> Table
string_tsort s t = head t : sortBy tcompare (tail t)
    where
        tcompare :: Row -> Row -> Ordering
        tcompare r1 r2
            | head r1 > head r2 = GT
            | otherwise = LT

grades :: CSV -> CSV -> CSV -> CSV -> CSV
grades t1 t2 t3 t4 = write_csv $ string_tsort "Nume" $ compute_total $ compute_all t1 t2 t3 t4
    where
        compute_all t1 t2 t3 t4 = tjoin "Nume" (tjoin "Nume" (compute_hw t2) (compute_lecture t1 t4 t2)) (compute_exam_grades $ read_csv t3)
        compute_total = map (\row -> if head row == "Nume" then row ++ ["Punctaj Total"]
            else row ++ [printf "%.2f" $ compute_sum row])
                where
                    compute_sum :: Row -> Float
                    compute_sum row
                        | (floatRead . head . tail) row + (floatRead . last . init) row < 2.5 = 4
                        | (floatRead . last) row < 2.5 = 4
                        | otherwise = (floatRead . last) row + min ((floatRead . head . tail) row + (floatRead . last . init) row) 5
