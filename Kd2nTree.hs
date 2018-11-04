{-
	Implementation of kd 2^n-Tree Structure in Haskell with generic points of Point type class
	by joaquimgomez
	Definitions:
		-> Class Point p
		-> Point3d data (instanced as: Point, Show and Eq)
		-> Kd2nTree with points of class type Point p (instanced as: Eq, Show, Functor, Applicative and Monad)
	Functions included: insert, build, buildIni, get_all, remove, contains, nearest, allinInterval, translation, kfilter.
	Auxiliary Functions included: unionKd2nTree, mergePointsList, sortPointsList.
	Also included: examples of sets and points.
-}

import Control.Applicative -- The use of the applicative instance is mandatory in GHC 7.10 when we instance some type as Monad class.

-- Definition of Point type class of type p
class Point p where
	sel :: Int -> p -> Double -- Given an index and one element of type p, it returns value of the coordinate(index).
	dim :: p -> Int -- Given an element of type p, it returns the dimension of it.
	child :: p -> p -> [Int] -> Int -- Given two elements of type p and a list of selected coordinates, it returns the son's number of p2 for p1
	dist :: p -> p -> Double -- Given two points, it returns the Euclidean distance between them.
	list2Point :: [Double] -> p -- Given a list of doubles, it returns an element of type p.
	ptrans :: [Double] -> p -> p -- Given a point of type p and a list of doubles, it returns a point as a resoult of applying the translation of a list in the point.
	plargerOrEqualThan :: p -> p -> Bool -- Given two points, it returns true if the first point is larger or equal than the second point.
	psmallerOrEqualThan :: p -> p -> Bool -- Given two points, it returns true if the first point is smaller or equal than the second point.



-- Example of use of Point class (Point3d), ergo the example (Point3d) is an instance of class Point.
data Point3d = Point3d (Double, Double, Double) deriving (Eq) -- Instance of Eq because we need to compare pairs of points.

instance Point Point3d where -- Point3d is a instance of Point type class.
	sel s (Point3d (cd1,cd2,cd3))
		| s == 1 		= cd1
		| s == 2 		= cd2
		| otherwise = cd3

	dim (Point3d cds) = 3 -- In 3d points there are always 3 coordinates.

	child (Point3d (p1,p2,p3)) (Point3d (q1,q2,q3)) coords = bin2dec $ child' (p1:p2:p3:[]) (q1:q2:q3:[]) coords
	-- Explicit conversion of the coordinate lists to arrays, To make the operations (child' and bin2dec) more cool.
		where
			child' :: [Double] -> [Double] -> [Int] -> [Int] -- Determinates as binary code the son's number of the point 2 for the point 1.
			child' lp lq [] = []
			child' lp lq (i:is)
				| (lq !! (i - 1)) > (lp !! (i - 1)) = [1] ++ child' lp lq is
				| otherwise													= [0] ++ child' lp lq is
			bin2dec :: [Int] -> Int -- Needed to convert the son's number in binary to decimal (the form as haskell works).
			bin2dec [] = 0
			bin2dec ld = 2^(length ld - 1) * (head ld) + (bin2dec $ tail ld)

	dist (Point3d (p1,p2,p3)) (Point3d (q1,q2,q3)) = sqrt ((p1-q1)**2 + (p2-q2)**2 + (p3-q3)**2)

	list2Point [e1,e2,e3] = Point3d (e1,e2,e3)

	ptrans [tc1,tc2,tc3] (Point3d (c1,c2,c3)) = Point3d (c1 + tc1, c2 + tc2, c3 + tc3)

	plargerOrEqualThan (Point3d (p1,p2,p3)) (Point3d (q1,q2,q3)) = (p1 >= q1) && (p2 >= q2) && (p3 >= q3)

	psmallerOrEqualThan (Point3d (p1,p2,p3)) (Point3d (q1,q2,q3)) = (p1 <= q1) && (p2 <= q2) && (p3 <= q3)

instance Show Point3d where -- Instance of Show, as well, because maybe we need to show the points on the screen.
	show (Point3d (c1, c2, c3)) = ("(" ++ (show c1) ++ ", " ++ (show c2) ++ ", " ++ (show c3) ++ ")")



-- Definition of generic type Kd2nTree where Point type is generic.
-- For some operations, this type must be of Point/Show/Eq/... classes.
data Kd2nTree p = Node p [Int] [Kd2nTree p] | Empty -- A given Kd2nTree has: node of type p, a list to organize the soons/leaves, and a list of sons/leaves.

instance (Point p, Eq p) => Eq (Kd2nTree p) where -- Two Kd2nTrees are equal if both are Empty or  have the same points.
	Empty == Empty = True
	-- To verify that they have the same points, we should create a sorted array of both and compare them because Haskell make the comparison of arrays: p-list1_i == p-list2_i.
	-- An alternative is for each point of a set see if it is in the other one, but the fisrt option are more efficient.
	t1 == t2 = (sortPointsList $ map (fst) $ get_all t1) == (sortPointsList $ map (fst) $ get_all t2)

instance Show p => Show (Kd2nTree p) where -- To show a tree we need to show the current node and their sons/leaves.
	show Empty = ""
	show (Node point coords leaves) = (show point) ++ " " ++ (show coords) ++ "\n" ++ showLeaves leaves 0 0
		where
			showLeaves :: Show p => [Kd2nTree p] -> Int -> Int -> String -- The two last arguments indicates the current number of son and the current level to create the indentation.
			showLeaves [] soon level = ""
			showLeaves (Empty:xs) soon level = showLeaves xs (soon + 1) level
			showLeaves ((Node point2 coords2 leaves2):xs) soon level = (take (5 * level) (repeat ' ')) ++ "<" ++ (show soon) ++ "> " ++ (show point2)
																																	++ " " ++ (show coords2) ++ "\n" ++ (showLeaves leaves2 0 (level + 1))
																																	++ (showLeaves xs (soon + 1) level)



-- Returns a Kd2nTree as a result of inserting the new point.
insert :: Point p => Kd2nTree p -> p -> [Int] -> Kd2nTree p
insert Empty newP coords = Node newP coords (take (2^(length coords)) (repeat Empty))
-- Insertion of a given point is the insertion of it in some son of the current tree (guided by its corresponding number of son).
insert (Node p coords leaves) newP newCoords = (Node p coords (((take numSon leaves)) -- Take from the beginning to the corresponding leave for a given point.
																								++ [insert (leaves !! numSon) newP newCoords] -- Insert in it the point.
																								++ (drop (numSon + 1) leaves))) -- Put the rest of sons.
	where numSon = child p newP coords

-- For a given list of points with its corresponding lists to organize the soons, build a Kd2nTree.
build :: Point p => [(p,[Int])] -> Kd2nTree p
build l = build' $ reverse l -- Reverse of lists because of recursion we build the tree upside down.
	where
		build' :: Point p => [(p,[Int])] -> Kd2nTree p
		build' [] = Empty
		build' (x:xs) = insert (build' xs) (fst x) (snd x) -- Insert first the last point and then the current point.

-- For a given list of points (as arrays of coordinates) with its corresponding lists to organize the soons, build a Kd2nTree.
buildIni :: (Point p) => [([Double],[Int])] -> Kd2nTree p
buildIni l = build $ toPoint l -- Create the points with with the given coordinate lists.
	where
		toPoint :: Point p =>[([Double],[Int])] -> [(p,[Int])]
		toPoint [] = []
		toPoint (x:xs) = [(list2Point (fst x), snd x)] ++ (toPoint xs)



-- Returns a list of pairs with the points and its corresponding lists to organize the sons.
get_all :: Kd2nTree p -> [(p,[Int])]
get_all Empty = []
get_all (Node p coords leaves) = [(p, coords)] ++ (concat (map get_all leaves))



-- Returns a tree with the deletion of a given point.
remove :: (Point p, Eq p) => Kd2nTree p -> p -> Kd2nTree p
remove Empty p2 = Empty -- For a given empty tree the resoult is an empty tree.
-- Deletion of a given point is the deletion of it in some son of the current tree (guided by its corresponding number of son).
remove (Node p coords leaves) p2
	| p == p2 	= build (concat $ map (get_all) leaves) -- If the current point is the point to be deleted, it returns a rebuild tree with the sons (rebuild because we delete the root).
	| otherwise = (Node p coords ((take numSon leaves) -- Take from the beginning to the corresponding leave for a given point.
								 ++ [remove (leaves !! numSon) p2] -- Delete the point.
								 ++ (drop (numSon + 1) leaves))) -- Put the rest of sons.
	where numSon = child p p2 coords



-- Returns true if the tree contains a given point.
contains :: (Point p, Eq p) => Kd2nTree p -> p -> Bool
contains Empty p2 = False
contains (Node p coords leaves) p2
	| p == p2 		= True
	| otherwise 	= contains (leaves !! numSon) p2 -- Travel the tree guided by the corresponding son number of p2 for current p.
	where numSon = child p p2 coords
{- Alternative version:
contains (Node p coords leaves) p2 = isInTheList  (map (fst) $ get_all (Node p coords leaves)) p2 -- Transform the tree in a list of points.
	where
		isInTheList:: (Point p, Eq p) => [(p, [Int])] -> p -> Bool
		isInTheList [] p2 = False;
		isInTheList (p:ps) p2 = (p == p2) || (isInTheList ps p2)
-}



-- Returns the nearest point to a given point.
nearest :: (Point p, Eq p) => Kd2nTree p -> p -> p
nearest (Node p coords leaves) p2
	| p == p2 	= p -- The root is the nearest point.
	| otherwise = nearest' (map (fst) $ get_all (Node p coords leaves)) p2 p
	where
		nearest' :: (Point p, Eq p) => [p] -> p -> p -> p -- The last argument is an auxiliary argument to save the nearest point by the moment.
		nearest' [] p auxPoint = auxPoint -- When we have finished traveling the list we return the nearest point found.
		nearest' (p:ps) p2 auxPoint
			| p == p2 												 = nearest' [] p p 	-- The most nearest point is the same point.
			| (dist p2 auxPoint) > (dist p p2) = nearest' ps p2 p -- By the moment the neares point found is p.
			| otherwise 											 = nearest' ps p2 auxPoint -- The current point is not nearest than auxPoint.



-- Returns a sorted list with the points greaters or equals than a given point (1st) and the points smaller or equal than a given point (2nd).
allinInterval :: (Point p, Eq p) => Kd2nTree p -> p -> p -> [p]
allinInterval Empty p1 p2 = []
allinInterval t p1 p2 = sortPointsList ((largerOrEqualThan tList p1) ++ (smallerOrEqualThan tList p2))
	where
		tList = map (fst) (get_all t) -- List of points of the tree.

		largerOrEqualThan :: (Point p, Eq p) => [p] -> p -> [p]
		largerOrEqualThan [] q = []
		largerOrEqualThan (p:ps) q
			| plargerOrEqualThan p q = [p] ++ (largerOrEqualThan ps q) -- If p is larger or equal than q, we put it in the list.
			| otherwise							 = largerOrEqualThan ps q

		smallerOrEqualThan :: (Point p, Eq p) => [p] -> p -> [p]
		smallerOrEqualThan [] q = []
		smallerOrEqualThan (p:ps) q
			| psmallerOrEqualThan p q = [p] ++ (smallerOrEqualThan ps q) -- If p is smaller or equal than q, we put it in the list.
			| otherwise 						  = smallerOrEqualThan ps q



-- Kd2nTree structure as an instance of Functor class.
instance Functor Kd2nTree where
	fmap f Empty = Empty
	fmap f (Node p coords leaves) = (Node (f p) coords (map (fmap f) leaves))

-- Applies a translation to all points of the tree.
translation :: (Point t) => [Double] -> Kd2nTree t -> Kd2nTree t
translation trnsl Empty = Empty
translation trnsl t = fmap (ptrans trnsl) t



-- Kd2nTree structure as an instance of Applicative class (for GHC 7.10).
instance Applicative Kd2nTree where
	pure p = Node p [] []
	(Node p1 coords1 leaves1) <*> (Node p2 coords2 leaves2) = (Node (p1 p2) coords2 (zipWith (<*>) leaves1 leaves2))

-- Kd2nTree structure as an instance of Monad class.
instance Monad Kd2nTree where
	return p = Node p [] []

	Empty >>= f = Empty
	Node p coords leaves >>= f = Node p' coords (leaves' ++ map (>>=f) leaves)
		where Node p' coords leaves' = f p

	t1 >> t2 = t1 >>= \_ -> t2

-- Select the elements that satisfies a given property.
kfilter :: (Point p) => (p -> Bool) -> Kd2nTree p -> Kd2nTree p
kfilter prop (Node p coords leaves) = do
	if (prop p)
		then do
			unionKd2nTree $ [build [(p, coords)]] ++ (map (kfilter prop) leaves)
		else do
			unionKd2nTree $ map (kfilter prop) leaves



-- Auxiliary functions
unionKd2nTree :: (Point p) => [Kd2nTree p] -> Kd2nTree p
unionKd2nTree t = build $ concat $ map (get_all) t

-- Sorted merge of two list of points.
mergePointsList :: (Point p) => [p] -> [p] -> [p]
mergePointsList l1 [] = l1
mergePointsList [] l2 = l2
mergePointsList (head1:tail1) (head2:tail2)
	| psmallerOrEqualThan head1 head2 = head1:(mergePointsList tail1 (head2:tail2))
	| otherwise 											= head2:(mergePointsList (head1:tail1) tail2)

-- Point sort implemented following the Mergesort algorithm.
sortPointsList :: (Point p) => [p] -> [p]
sortPointsList [] = []
sortPointsList [e] = [e]
sortPointsList l = mergePointsList (sortPointsList (take (halfLength l) l)) (sortPointsList (drop (halfLength l) l))
	where
		halfLength l = length l `div` 2



-- Example sets and points
examplePoint1 :: Point3d
examplePoint1 = list2Point [3.1, 3.8, 4.7]

examplePoint2 :: Point3d
examplePoint2 = list2Point [3.0, -1.0, 2.1]

exampleSet1 :: Kd2nTree Point3d
exampleSet1 = buildIni [([3.0, -1.0, 2.1], [1, 3]), ([3.5, 2.8, 3.1], [1, 2]), ([3.5, 0.0, 2.1], [3]),
    ([3.0, -1.7, 3.1], [1, 2, 3]),([3.0, 5.1, 0.0], [2]), ([1.5, 8.0, 1.5], [1]), ([3.3, 2.8, 2.5], [3]),
    ([4.0, 5.1, 3.8], [2]),([3.1, 3.8, 4.8], [1, 3]), ([1.8, 1.1, -2.0], [1, 2])]
