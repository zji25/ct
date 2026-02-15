**Домашнее задание 8. Индексирование**

1. Определите, какие индексы требуется добавить к таблицам базы данных «Университет» на основе запросов из ДЗ-5, 6 и 7.

    1.G. Индексы на таблицу Groups
    ```sql
    -- ДЗ-5.2.1. Полная информация о студентах по :StudentId
    -- ДЗ-5.2.2. Полная информация о студентах по :StudentName
    -- ДЗ-5.2.4. Полная информация об участниках по :ClubName
    -- первичный ключ, используется в join'ах
    create unique index groups_index_id on Groups using hash (GroupId);

    -- ДЗ-5.2.3. Полная информация о студентах по :GroupName
    -- ДЗ-7.3.4. Изменение имени всех студентов группы
    -- ДЗ-7.3.5. Перевод всех студентов группы по :*GroupName
    -- ключ, используется в 'where ='
    create unique index groups_index_name on Groups using hash (GroupName);
    ```
    1.S. Индексы на таблицу Students
    ```sql
    -- ДЗ-5.2.4. Полная информация об участниках по :ClubName
    -- ДЗ-5.2.5. Полная информация об участниках клуба по :StudentName руководителя
    -- ДЗ-5.3.1. Информация о студентах с :Mark по предмету :CourseId
    -- первичный ключ, ускоряет join'ы по StudentId
    create unique index students_index_id on Students using hash (StudentId);

    -- ДЗ-7.1.1. Удаление студентов по :GroupId
    -- ДЗ-7.3.3. Перевод всех студентов группы по :*GroupId
    -- ДЗ-7.3.4. Изменение имени всех студентов группы
    -- GroupId используется в 'where ='
    create index students_index_group on Students using hash (GroupId);

    -- ДЗ-5.1.2. Информация о студентах по :StudentName
    -- ДЗ-5.2.2. Полная информация о студентах по :StudentName
    -- ДЗ-5.2.5. Полная информация об участниках клуба по :StudentName руководителя
    -- StudentName используется в 'where ='
    create index students_index_name on Students using hash (StudentName);
    ```
    1.C. Индексы на таблицу Courses
    ```sql
    -- ДЗ-5.4.4. Информацию о студентах не имеющих оценки по :CourseName среди тех, у кого он есть
    -- ДЗ-5.5.2. ФИО студента и названия предметов которые у него есть без оценки
    -- ДЗ-5.5.3. ФИО студента и названия предметов которые у него есть, но не 4 или 5
    -- первичный ключ, используется в join'ах
    create unique index courses_index_id on Courses using hash (CourseId);

    -- ДЗ-5.4.1. Информацию о студентах не имеющих оценки по :CourseName среди всех
    -- ДЗ-5.4.2. Информацию о студентах не имеющих оценки по :CourseName среди всех
    -- ДЗ-5.4.3. Информацию об участниках :ClubName не имеющих оценки по :CourseName
    -- покрывающий индекс для поиска по CourseName
    create unique index courses_index_name_id on Courses using btree (CourseName, CourseId);
    ```
    1.L. Индексы на таблицу Lecturers
    ```sql
    -- первичный ключ, используется в join'ах
    create unique index lecturers_index_id on Lecturers using hash (LecturerId);

    -- ДЗ-5.5.4. ФИО студента и названия предметов которые у него вёл :LecturerName
    -- ДЗ-5.6.1. StudentId имеющих хотя бы одну оценку у :LecturerName
    -- ДЗ-5.6.2. StudentId не имеющих оценок у :LecturerName
    -- покрывающий индекс для поиска по LecturerName
    create unique index lecturers_index_name_id on Lecturers using btree (LecturerName, LecturerId);
    ```
    1.P. Индексы на таблицу Plan
    ```sql
    -- ДЗ-5.5.5. ФИО участника клуба :ClubName и названия предметов которые у него вели
    -- ДЗ-5.3.3. Информацию о студентах с :Mark по предмету :LecturerId
    -- ДЗ-5.3.5. Информацию о студентах с :Mark по предмету :LecturerId
    -- первичный ключ, ускоряет поиск по группе и по группе и курсу
    create unique index plan_index_group_course on Plan using btree (GroupId, CourseId);

    -- ДЗ-5.3.4. Информацию о студентах с :Mark по предмету :LecturerName
    -- ДЗ-5.3.6. Информацию о студентах с :Mark по предмету :LecturerName
    -- ДЗ-5.5.4. ФИО студента и названия предметов которые у него вёл :LecturerName
    -- ускоряет поиск по преподавателю 
    create index plan_index_lecturer on Plan using hash (LecturerId);
    ```
    1.M. Индексы на таблицу Marks
    ```sql
    -- ДЗ-6.2.4. Полная информация о студентах, не имеющих оценки по :CourseId
    -- ДЗ-5.4.1. Информацию о студентах не имеющих оценки по :CourseName среди всех
    -- ДЗ-5.4.3. Информацию об участниках :ClubName не имеющих оценки по :CourseName
    -- покрывающий индекс для поиска студентов с оценкой по определенному курсу 
    create unique index marks_index_course_student on Marks using btree (CourseId, StudentId);

    -- ДЗ-6.5.3. StudentId имеющих оценки по всем предметам :LecturerName
    -- ДЗ-6.5.4. StudentId имеющих оценки по всем предметам :LecturerName, которые он у него вёл
    -- ДЗ-5.5.2. ФИО студента и названия предметов которые у него есть без оценки
    -- покрывающий индекс для поиска курсов студента, по которым у него есть оценка
    create unique index marks_index_student_course on Marks using btree (StudentId, CourseId);

    -- ДЗ-6.4.2. StudentName, CourseName, долги по неудовлетворительной оценке
    -- ДЗ-6.4.3. StudentName, CourseName, долги по отсутствию или неудовлетворительной оценке
    -- покрывающий индекс для поиска по диапазону оценок по конкретному курсу
    create index marks_index_course_mark on Marks using btree (CourseId, Mark);

    -- ДЗ-5.3.1. Информация о студентах с :Mark по предмету :CourseId
    -- ДЗ-5.3.6. Информацию о студентах с :Mark по предмету :LecturerName
    -- ДЗ-6.1.4. Информация о студентах по :Mark и :CourseId
    -- покрывающий индекс для поиска по оценке по курсу
    create index marks_index_mark_course on Marks using btree (Mark, CourseId);
    ```
    1.C. Индексы на таблицу Clubs
    ```sql
    -- ДЗ-5.2.4. Полная информация об участниках по :ClubName
    -- ДЗ-5.2.5. Полная информация об участниках клуба по :StudentName руководителя
    -- ДЗ-6.1.3. Информация о студентах по :ClubName
    -- первичный ключ, используется в join'ах
    create unique index clubs_index_id on Clubs using hash (ClubId);

    -- ДЗ-5.2.4. Полная информация об участниках по :ClubName
    -- ДЗ-5.4.3. Информацию об участниках :ClubName не имеющих оценки по :CourseName
    -- ДЗ-6.1.3. Информация о студентах по :ClubName
    -- ключ, используется в 'where ='
    create unique index clubs_index_name on Clubs using hash (ClubName);

    -- ДЗ-5.5.7. ФИО студента и названия предметов руководителей клубов
    -- ДЗ-5.2.5. Полная информация об участниках клуба по :StudentName руководителя
    -- ДЗ-6.2.3. Полная информация о студентах по :StudentName
    -- ускоряет поиск по руководителю
    create index clubs_index_head on Clubs using hash (ClubStudentHeadId);
    ```
    1.CM. Индексы на таблицу ClubMembers
    ```sql
    -- ДЗ-5.2.4. Полная информация об участниках по :ClubName
    -- ДЗ-5.4.3. Информацию об участниках :ClubName не имеющих оценки по :CourseName
    -- ДЗ-5.5.5. ФИО участника клуба :ClubName и названия предметов которые у него вели
    -- первичный ключ, ускоряет поиск участников клуба
    create unique index clubmembers_index_club_student on ClubMembers using btree (ClubId, StudentId);

    -- ДЗ-7.5.5. Число долгов каждого участника клуба
    -- ключ, ускоряет поиск клубов по студенту
    create unique index clubmembers_index_student_club on ClubMembers using btree (StudentId, ClubId);
    ```
2. Пусть частым запросом является определение среднего балла участников клуба по дисциплине. Как будет выглядеть запрос и какие индексы могут помочь при его исполнении?

    2.Q. Запрос
    ```sql
    select avg(Mark)
    from Marks M
        natural join Courses C
        natural join Clubs CL
        natural join ClubMembers CM
    where C.CourseName = :CourseName
        and CL.ClubName = :ClubName;
    ```
    2.I. Индексы
    ```sql
    -- покрывающий индекс для поиска курса по названию
    create index courses_index_name_id on Courses using btree (CourseName, CourseId);

    -- ключ, покрывающий индекс для поиска клуба по названию
    create unique index clubs_index_name_id on Clubs using btree (ClubName, ClubId);

    -- первичный ключ, ускоряет поиск участников клуба
    create unique index clubmembers_index_club_student on ClubMembers using btree (ClubId, StudentId);

    -- покрывающий индекс для поиска студентов с оценкой по определенному курсу 
    create unique index marks_index_course_student on Marks using btree (CourseId, StudentId);

    -- покрывающий индекс для поиска курсов студента, по которым у него есть оценка
    create unique index marks_index_student_course on Marks using btree (StudentId, CourseId);
    ```
3. Придумайте три запроса, требующих новых индексов и запишите их. Если в результате, некоторые из старых индексов станут бесполезными, удалите их.

    3.1.Q. Запрос 1
    ```sql
    -- полная информация о студентах по фамилии
    select StudentId,
        StudentName,
        GroupName
    from Students
        natural join Groups
    where StudentName like (:LastName || ' %')
    ```
    3.1.I. Дополнительные индексы для запроса 1
    ```sql
    -- больше не нужен хеш-индекс на те же атрибуты
    drop index students_index_name;
    -- для поиска по префиксу строки
    create index students_index_name on Students using btree (StudentName);
    ```
    3.2.Q. Запрос 2
    ```sql
    -- курсы, по которым у студента хорошая оценка
    select CourseId
    from Marks
    where StudentId = :StudentId
        and Mark >= 4;
    ```
    3.2.I. Дополнительные индексы для запроса 2
    ```sql
    -- покрывающий индекс для поиска по диапазону оценок у определенного студента
    create index marks_index_student_mark on Marks using btree (StudentId, Mark);
    ```
    3.3.Q. Запрос 3
    ```sql
    -- полная информация о студентах на i'ом курсе
    -- (для групп с названиями составленными по структуре "два любых символа" + номер курса + ...)
    select StudentId,
        StudentName,
        GroupName
    from Students
        natural join Groups
    where GroupName like ('__' || :CourseNumber || '%');
    ```
    3.3.I. Дополнительные индексы для запроса 3
    ```sql
    -- больше не нужен хеш-индекс на те же атрибуты
    drop index groups_index_name;
    -- ключ, индекс для поиска по префиксу группы
    create unique index groups_index_name on Groups using btree (GroupName);
    ```
