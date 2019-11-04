# Haskell-course

## Homework 1

Основная часть
1) Написать функцию `bella n`, вычисляющую n-ое число белла по рекуррентой формуле (с сочетаниями).
2) Написать функцию, соединяющую конечное (n) количество списков, которые могут быть бесконечными так, чтобы для любого n и любого k нашлось m такое, чтобы для списка ls вызов `take m ls` вывел результат, в котором было бы хотя бы по k элементов из каждого списка. Иными словами, объединить списки так, чтобы можно было просматривать каждый из список в объединённом.

Дополнительная часть
1) Сделать задание 1 основной части, оптимизировав повторные вызовы (как в лекциях сделали с числам фибоначчи)

## Homework 2

1) Рассмотрим граф, задаваемый следующим образом (списками смежности): `type Graph = [(Int, [Int])]`. Вершины пронумерованы от 1 до n. В каждом кортеже в списке выше указан номер вершины и список номеров вершин, у которых с ней общее ребро.

1a) Проверить, корректно ли заданное описание графа (имеется в виду, что граф не ориентированный)

1b) Проверить, состоит ли граф (корректно заданный) из одной компоненты связности

2) Заданы следующие типы данных:

```
type Relation a = ((a, a) -> Bool)
type RelationProperty a = [a] -> Relation a -> Bool
```

`RelationProperty` описывает функцию, которая для данного списка (воспринимаемого, как множества) и отношения на его типе данных возвращает Bool (True означает, что отношение обладает данным свойством на данном множестве)

Нужно описать `isEquivalenceRelation :: RelationProperty`, обозначающу, что отношение является на данном множестве отношением эквивалентности