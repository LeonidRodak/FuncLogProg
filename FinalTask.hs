module FinalTask where

{-Вариант 3.

Используя стандартное представление для списков, запрограммировать на Haskell следующие предикаты работы со списками.
Каждый предикат должен вычислять сложность (число шагов, необходимых для вычисления) для самого себя.
1.Конкатенация двух списков
2.Инверсия списка
3.Вывод на экран элементов списка
4.Подсчет числа вхождений в список некоторого элемента.
5.Проверка, является ли один список подсписком другого списка.
6.Выделение подсписка 
7.Удаление повторяющихся элементов списка с сохранением порядка.-}

-- Каждый предикат возвращает кортеж: (результат, количество шагов)

-- 1. Конкатенация двух списков
-- Пример: concatLists [1,2] [3,4] = ([1,2,3,4], 3)
concatLists :: [a] -> [a] -> ([a], Integer)
concatLists a b = concatHelper a b 0  -- начинаем подсчёт с 0 шагов
  where
    -- Первый список пустой
    concatHelper [] a steps = (a, steps + 1)

    -- Берем голову первого списка, рекурсивно обрабатываем хвост, затем добавляем голову в начало результата
    concatHelper (a:as) b steps = -- (a:as), a-голова, as-хвост
        --let ... in - локальное определение переменных
        let (result, newSteps) = concatHelper as b (steps + 1)  -- рекурсия по хвосту ((result, newSteps), тк concatHelper -> ([a], Integer))
        in (a : result, newSteps)  -- добавляем голову в начало (:)


-- 2. Инверсия (разворот) списка
-- Пример: reverseList [1,2,3,4] = ([4,3,2,1], 5)
reverseList :: [a] -> ([a], Integer)
reverseList a = reverseHelper a [] 0  -- начинаем с пустого аккумулятора (развернутый список) и 0 шагов
  where
    -- Пустой список
    reverseHelper [] acc steps = (acc, steps + 1)
    
    -- Берём голову, добавляем в начало аккумулятора
    reverseHelper (a:as) acc steps = reverseHelper as (a:acc) (steps + 1)


-- 3. Вывод на экран элементов списка
-- Печатает каждый элемент списка с новой строки
-- Возвращает: ((), количество шагов) - () так как IO не возвращает значения
-- Пример: printList [1,2,3] выведет элементы и вернёт ((), 4)
printList :: Show a => [a] -> IO ((), Integer)  -- Show a чтобы элемент можно было напечатать на экране, => ограничение типа
printList a = printHelper a 0  -- начинаем подсчёт с 0 шагов
  where
    -- Пустой список
    printHelper [] steps = return ((), steps + 1)

    -- Печатаем голову, затем обрабатываем хвост, каждый print считается за 1 шаг
    printHelper (a:as) steps = do
        print a                    -- вывод элемента (1 шаг)
        (_, newSteps) <- printHelper as (steps + 1)  -- рекурсия (<- извлекает кортеж ((), Integer))
        return ((), newSteps)


-- 4. Подсчет числа вхождений элемента в список
-- Пример: countOccurrences 2 [1,2,3,2,4,2] = (3, 7)
countOccurrences :: Eq a => a -> [a] -> (Integer, Integer)  -- Eq a чтобы a можно было сравнивать, => ограничение типа
countOccurrences a as = countHelper as 0 0  -- начинаем с 0 вхождений и 0 шагов
  where
    -- Пустой список
    countHelper [] count steps = (count, steps + 1)
    
    -- Сравниваем голову с искомым элементом
    countHelper (b:bs) count steps
        | a == b = countHelper bs (count + 1) (steps + 1)  -- совпадение: +1 к счётчику
        | otherwise = countHelper bs count (steps + 1)  -- нет совпадения


-- 5. Проверка является ли один список подсписком другого
-- Пример: isSublist [2,3] [1,2,3,4] = (True, 8)
isSublist :: Eq a => [a] -> [a] -> (Bool, Integer)
isSublist sub main = isSublistHelper sub main 0  -- начинаем подсчёт с 0 шагов
  where
    -- Пустой подсписок всегда содержится в любом списке
    isSublistHelper [] _ steps = (True, steps + 1)
    
    -- Непустой подсписок не может быть в пустом списке
    isSublistHelper _ [] steps = (False, steps + 1)
    
    -- Проверяем, начинается ли основной список с подсписка (сначала идем в isPrefix по where, потом проверяем условия isPrefixResult)
    isSublistHelper sub (a:as) steps
        | isPrefixResult = (True, steps + prefixSteps)  -- найдено совпадение (выводим True)
        | otherwise = isSublistHelper sub as (steps + 1)  -- ищем в хвосте
      where
        -- Проверяем, начинается ли список с подсписка
        (isPrefixResult, prefixSteps) = isPrefix sub (a:as) 0
    
    -- Вспомогательная функция: проверка префикса
    isPrefix :: Eq a => [a] -> [a] -> Integer -> (Bool, Integer)
    isPrefix [] _ steps = (True, steps + 1)   -- пустой префикс всегда true
    isPrefix _ [] steps = (False, steps + 1)  -- непустой в пустом = false
    isPrefix (x:xs) (y:ys) steps
        | x == y = isPrefix xs ys (steps + 1)  -- головы равны, проверяем хвосты
        | otherwise = (False, steps + 1)  -- головы не равны


-- 6. Выделение подсписка (срез)
-- Извлекает подсписок начиная с позиции start длиной length
-- Пример: sublist 1 3 [1,2,3,4,5] = ([2,3,4], 5)
sublist :: Integer -> Integer -> [a] -> ([a], Integer)
sublist start len xs = sublistHelper start len xs [] 0  -- [] - аккумулятор для результата
  where
    -- Исходный список пустой
    sublistHelper _ _ [] acc steps = (reverse acc, steps + 1)  --reverse (разворачиваем), тк добавляли в начало списка
    
    -- Набрали нужное количество элементов или изначально len=0
    sublistHelper _ 0 _ acc steps = (reverse acc, steps + 1)
    
    -- Рекурсивный случай 
    sublistHelper start len (x:xs) acc steps
        -- Ещё не дошли до начальной позиции
        | start > 0 = sublistHelper (start - 1) len xs acc (steps + 1)
        
        -- Добавляем элемент в аккумулятор
        | otherwise = sublistHelper start (len - 1) xs (x:acc) (steps + 1)


-- 7. Удаление повторяющихся элементов с сохранением порядка
-- Оставляет только первое вхождение каждого элемента
-- Пример: removeDuplicates [1,2,1,3,2,4] = ([1,2,3,4], 11)
removeDuplicates :: Eq a => [a] -> ([a], Integer)
removeDuplicates xs = removeHelper xs [] 0  -- [] - аккумулятор для результата
  where
    -- Пустой список
    removeHelper [] _ steps = ([], steps + 1)
    
    -- Рекурсивный случай: проверяем, видели ли уже этот элемент
    removeHelper (x:xs) unic steps
        | x `elem` unic =  -- `elem` - проверка на дубликат
            -- Элемент уже был: пропускаем его
            let elemSteps = countElemSteps x unic 0  -- считаем сколько сравнений сделал 'elem'
            in removeHelper xs unic (steps + elemSteps + 1)
        
        | otherwise = 
            -- Новый элемент: добавляем в результат и запоминаем
            case removeHelper xs (x:unic) (steps + 1) of
                (result, newSteps) -> (x:result, newSteps)  -- распаковка кортежа для добавления x в результат
    
    -- Вспомогательная функция для подсчета шагов в 'elem'
    -- Проходит по списку unic и считает каждое сравнение
    -- Возвращает количество шагов (не результат elem!)
    countElemSteps :: Eq a => a -> [a] -> Integer -> Integer  -- искомый элемент, аккамулятор, накопленные шаги, результат
    countElemSteps _ [] steps = steps + 1  -- проверка пустого списка (начало) +1 шаг
    countElemSteps x (y:ys) steps
        | x == y = steps + 1  -- нашли совпадение: +1 шаг
        | otherwise = countElemSteps x ys (steps + 1)  -- не совпало: рекурсия +1 шаг


-- Тестирование
-- Тест 1: Конкатенация
testConcat :: ([Integer], Integer)
testConcat = concatLists [1, 2, 3] [4, 5, 6]

-- Тест 2: Инверсия
testReverse :: ([Integer], Integer)
testReverse = reverseList [1, 2, 3, 4, 5]

-- Тест 3: Подсчёт вхождений
testCount :: (Integer, Integer)
testCount = countOccurrences 'a' "ababababa"

-- Тест 4: Проверка подсписка
testIsSublist :: (Bool, Integer)
testIsSublist = isSublist [2, 3, 4] [1, 2, 3, 4, 5]

-- Тест 5: Выделение подсписка
testSublist :: ([Char], Integer)
testSublist = sublist 1 3 "hello"

-- Тест 6: Удаление дубликатов
testRemoveDup :: ([Integer], Integer)
testRemoveDup = removeDuplicates [1, 2, 1, 3, 2, 4, 3, 5]