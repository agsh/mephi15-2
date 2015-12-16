## Lab3
### Описание

В этой лабораторной вам предстоит работать со списками, полученными в результате разбора веб-страниц.
Для этих целей используются библиотеки [*-conduit](https://github.com/snoyberg/xml). Они позволяют изящно разбирать и запрашивать данные из таких иерархических структур как json, xml, html. В нашем случае мы будем работать с html-страницами, запрашивать содержимое тэгов с помощью селекторов и получать нужные нам данные в последовательностях и списках соответственно.

Пример работы с библиотеками находится в соответствующем boilerplate-файле: [Lab2.hs](./Lab2.hs). И в [интернете](https://www.fpcomplete.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/tagsoup)

Установка с использованием cabal: `cabal install xml-conduit http-conduit html-conduit`

### Вопросы
Вполне вероятно, что некоторые страницы будут парситься некорректно, данные будет сложно извлечь, возникнут проблемы с кодировкой. Все эти и другие вопросы, как обычно, задавайте в issue.

### Задание
|Вариант|Задание|
|---|---|
|1|По списку языков программирования википедии (http://en.wikipedia.org/wiki/List_of_programming_languages) составить список императивных, не функциональных ЯП.
|2|По списку языков программирования википедии (http://en.wikipedia.org/wiki/List_of_programming_languages) составить список кортежей: год появления, названия. Языки без указания годов появления исключить.
|3|По списку телефонных номеров МИФИ (http://mephi.ru/about/governance.php) выяснить, кто делит один номер с коллегами. Телефонные номера нормализовать
|4|Отсортировать пакеты Haskell по количеству тэгов их описывающих (http://hackage.haskell.org/packages/names)
|5|Составить список 50 самых умных и самых строгих преподавателей (http://www.mephist.ru/mephist/prepods.nsf/teachers)
|6|Составить список из ФИО преподавателей (http://cyber.mephi.ru/Faculty.html) и их страниц в соц.сетях (linkedin, facebook, vk)
|7|Составить список выпускников МИФИ, которые любят КВН (https://vk.com/vmephi)
|8|Составить список 50 самых комментируемых (по количеству отзывов) преподавателей (http://www.mephist.ru/mephist/prepods.nsf/teachers)
|9|Составить список 50 лучших отзывов для преподавателей с самой высокой оценкой (http://www.mephist.ru/mephist/prepods.nsf/teachers)
|10|Найти средний возраст участников студсовета МИФИ (https://vk.com/vmephi)
|11|Отсортировать список git-проектов Haskell, которые проходят тесты на Travis CI по количеству веток (https://github.com/haskell)
|12|Выяснить, какой процент из топ-500 проектов на языках Haskell и F# на github с наибольшим количеством звёзд составляют проекты на F# (https://github.com/search?utf8=%E2%9C%93&q=language%3AF%23&type=Repositories&ref=advsearch&l=F%23)
|13|Составить частоту обновлений проектов на F# на github (в течении последнего месяца, двух, и т. д.) (https://github.com/search?utf8=%E2%9C%93&q=language%3AF%23&type=Repositories&ref=advsearch&l=F%23)
|14|Узнать, от какой библиотеки зависит больше всего запрещённых пакетов на hackage (http://hackage.haskell.org/packages/deprecated)
|15|Найти пять самых скачиваемых пакетов за всё время на hackage (http://hackage.haskell.org/packages/top)
|16|Какой процент составляют issue с комментариями от общего количества issue для NuGet? (https://github.com/nuget/home/issues?page=1&q=is%3Aissue+is%3Aopen)
|17|Какое количество разработчиков NuGet уже перебазировались на github, если судить по их никам? (https://nuget.codeplex.com/team/view)
|18|Кто из разработчиков Microsoft на github ведёт блог на blogs.msdn.com и указал его в профиле? (https://github.com/Microsoft)
|19|Попытаться найти по списку языков программирования википедии человека, который разработал больше всего ЯП (http://en.wikipedia.org/wiki/List_of_programming_languages)
|20|Какое количество ссылок на странице о LISP ведут на англоязычные ресурсы, а какие - на русскоязычные? (https://lorwiki.ru/wiki/Часть_1._Общие_вопросы_о_Lisp)
|21|Под какой лицензией выпущено большинство проектов на F# на github, если судить по файлу LICENSE? (https://github.com/search?utf8=%E2%9C%93&q=language%3AF%23&type=Repositories&ref=advsearch&l=F%23)
|22|Сколько пакетов в Hackage (http://hackage.haskell.org/packages/) относятся к нескольким категориям?
|23|У какого факультета МИФИ (http://mephi.ru/about/faculty/) больше всего кафедр?
|24|Узнать, в каких группах больше всего тем с пометкой "Важно:" на форуме (http://www.sql.ru/forum)
|25|Какие группы появлялись в чартах last.fm с 2008 по 2013 года (исключая 2012) больше всего раз? (http://www.last.fm/bestof/2008 - http://www.last.fm/bestof/2013)
|26|Кто, кроме anonymous'а оставил больше всего комментариев в теме https://www.linux.org.ru/news/google/11404954?
|27|Какой тэг самый популярный среди ста самых популярных пакетов на NuGet? (https://www.nuget.org/stats/packages)
|28|В каком месяце какого года было больше всего вопросов в рассылке эрланга? (http://erlang.org/pipermail/erlang-questions/)
