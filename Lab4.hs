module Lab4 where
{-Определите представителей класса Functor для следующего типа данных,
 представляющего точку в трехмерном пространстве-}
data Point3D a = Point3D a a a deriving Show

-- Позволяет применять функцию к значениям, находящимся "внутри" контейнера Point3D
instance Functor Point3D where
    fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)  -- f - функция, которую нужно применить к каждой координате

    {-Примеры:
    fmap (+ 1) (Point3D 5 6 7)
    fmap (* 2) (Point3D 1 2 3)
    fmap (^ 2) (Point3D 2 3 4)-}