# Проект с кусови работи от курса по Системи, основани на знания

## 1. Алгоритъм А звезда за намиране на най-къс път в граф

Преди няколко месеца Мария и Иван решили да организират голямо парти, за да съберат всички свои приятели от различни градове в страната. 
Мария и Иван живеят в Пловдив и искат да улеснят гостите си в пътуването към града. 
Тъй като времето за организация е малко, Иван е изпратил покание веднага. 
В тях пише единствено датата на събитието и адрес на уеб-сайт. 
Постепенно той публикува останалата информация на сайта. Междувременно Мария трябва да направи страница, в която да показва най-бързия път от избран град до Пловдив. За съжаление тя все отлага тази задача. Вече остават само две седмици, а тя не е готова. Да помогнем да Мария! 

Напишете програма, която намира най-бързият път от избран град в България до Пловдив. Има множество API-та, които дават информация за съществуващите пътища, типа на пътя, разстоянието по права линия и тн. Мария обаче не иска да ползва допълнителни услуги. Тя е подготвила списък на всички градове в страната и пътищата между тях. Този списък може да бъде много голям, а ресурсите на машината, на която върви сайта, са органичени. Затова се налага да ползвате информирано търсене и добра евристика. 

За да можете да преизползвате решението си и по други поводи, не ограничавайте възможностите на програмата само до търсене на път до Пловдив.

Вашата програма трябва да приема 3 аргумента:

 - Име на начален град - символен низ, официално име на града на латиница

 - Име на града - крайна дестинация - символен низ, официално име на града на латиница

 - Име на файл с данни за пътищата - csv формат. Съдържа първи ред и първа колона имената на градовете. Всяко квадратче от таблицата съдържа композитна стойност от 3 компонента, разделени с pipe - |



 * дължината на наземния път между двата града или -1, ако няма изграден директен път между тези градове.

 * времето за пътуване по този път между двата града или -1, ако няма път

 * разстоянието между тези градове по права линия (по въздух)


Ще намерите прикачен примерен файл, за да нямате проблеми с формата на данните. Имайте предвид, че този файл може да съдържа значително повече данни.


Програмата трябва да намира най-бързия път (с най-кратко време за пътуване) между двата зададени града. Пътят се принтира в конзолата като поредица от градовете, през които трябва се мине.
 

Примерни елементи за решение: Тук можем да приложим алгоритъм за търсене А* (погледнете заадчата от упражнения, в която упътихме чудовището Митко).