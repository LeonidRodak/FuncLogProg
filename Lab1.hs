module Lab1 where
import Data.Function (on)  -- для 2 задания

-- Задание 1: 
{-Реализуйте функцию, находящую сумму и количество цифр десятичной записи заданного целого числа.-}

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x  -- определение функциии с помощью guards
    | x == 0 = (0, 1)  -- если число 0
    | otherwise = loop (abs x) 0 0  -- если число не 0, то через рекурсивную функцию берем модуль числа
  where
    loop 0 s c = (s, c)  -- если число стало = 0, то возвращаем sum и count
    loop num s c = 
        loop (num `div` 10) (s + num `mod` 10) (c + 1)


-- Задание 2
{-Функция multSecond, перемножающая вторые элементы пар, реализована следующим образом
multSecond = g 'on' h
g = undefined
h = undefined
Напишите реализацию функций g и h.
Пример использования
multSecond ('A', 2) ('B', 7) -> 14 -}

multSecond :: Num b => (a, b) -> (a, b) -> b  -- тип a любой, b - число
multSecond = g `on` h  -- эквивалентно: x y -> g (h x) (h y)
  where
    g = (*)
    h = snd  -- извлечение второго элемента из пары