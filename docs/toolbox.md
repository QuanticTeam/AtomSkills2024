
## Рабочая станция

*	Rider	IDE для разработки	https://www.jetbrains.com/ru-ru/rider/
*	VS Code	IDE для разработки	https://code.visualstudio.com/
*	pgAdmin	СУБД	https://www.pgadmin.org/
*	WSL	Система виртуализации для ОС Windows	https://learn.microsoft.com/en-us/windows/wsl/about
*	Docker desktop	UI утилита для системы контейнеризации	https://www.docker.com/products/docker-desktop/
*	Postman	UI утилита для тестирования http запросов	https://www.postman.com/
*	GIT	Система управления версиями	https://git-scm.com/
*	Microsoft Office	ПО для работы с документами	https://www.office.com/

## Используемые библиотеки

* [`Docker`](https://www.docker.com) - [(Apache-2.0 License)](https://en.wikipedia.org/wiki/Apache_License#Apache_License_2.0) - виртуализация для запуска приложения
* [`DotNet`](https://dotnet.microsoft.com/en-us/) - [MIT License](https://en.wikipedia.org/wiki/MIT_License) - серверная часть приложения
* [`PostgreSQL`](https://www.postgresql.org/) - [PostgreSQL License](https://opensource.org/license/postgresql) - база данных
* [`pgAdmin`](https://www.pgadmin.org/) - [PostgreSQL License](https://www.pgadmin.org/licence/#postgresql) администрирование базы данных
* [`MinIO`](https://min.io/) - [GNU Affero GPL](https://en.wikipedia.org/wiki/GNU_Affero_General_Public_License) - хранение файлов
* [`React`](https://react.dev/) - [MIT License](https://en.wikipedia.org/wiki/MIT_License) - фреймворк для интерфейса пользователя
* [Ant](https://ant.design/) - [MIT License](https://en.wikipedia.org/wiki/MIT_License) - библиотека визуальных компонентов
* [EF Core](https://learn.microsoft.com/en-us/aspnet/entity-framework) - [(Apache-2.0 License)](https://en.wikipedia.org/wiki/Apache_License#Apache_License_2.0) - ORM

## Домашние заготовки

Домашние заготовки сделаны на основании документа "ПРБ_Общая часть КЗ 2024" и на основании общепринятых подходов к разработке (какие компоненты чаще всего востребованы в Enterprise информационных системах)

* Авторизация/Аутентификация

    `Обоснование:` Для выполения КЗ понадобится разграничивать доступ и авторизовывать пользователей, т.к. у них могут быть разные роли - например: администратор системы, преподаватель, обучаемый сотрудник, проверяющий

* Сервис для хранения файлов

    `Обоснование:` Для выполнения КЗ скорее всего понадобится взаимодействие с файлами необходимыми для обучения (видеоуроки, сертификаты, видео/фото материалы практических заданий и т.п.)

* Работа с сущностями: создание, хранение, модификация, получение (CRUD)

    `Обоснование:` Система обучения должна уметь работать с разнообразными сущностями такими как уроки, расписание, преподаватели, обучаемые

* Работа с списком сущностей: создание, хранение, модификация, получение, фильтрация, сортировка

    `Обоснование:` Система обучения должна уметь работать с списком сущностей

* Шаблоны для документации:
  * Шаблон для `Инструкция по развёртыванию`
  * Шаблон для `Пользовательская инструкция`
  * Шаблон для `Техническая документация` **TODO:** добавить замену сертификатов
  * Шаблон для `Презентация продукта`

  `Обоснование:` В прошлом году на соревновании AtomSkills-2023 требовался набор документации, включая `Инструкция по развёртыванию`, `Пользовательская инструкция`, `Техническая документация`, `Презентация продукта`
