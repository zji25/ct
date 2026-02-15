**Домашнее задание 4. Нормализация БД «Университет»**

Дано отношение.

Атрибуты: StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, ClubId, ClubName, ClubStudentHeadId, Mark.

Функциональные зависимости:
* StudentId → StudentName, GroupId, GroupName;
* GroupId → GroupName;
* GroupName → GroupId;
* CourseId → CourseName;
* LecturerId → LecturerName;
* StudentId, CourseId → Mark;
* GroupId, CourseId → LecturerId;
* ClubId → ClubName;
* ClubName → ClubId, ClubStudentHeadId.

Множественные зависимости (не все):
* GroupName ↠ StudentId;
* GroupName ↠ CourseId;
* ClubName ↠ StudentId.
#### 1. Инкрементально приведите данное отношение в пятую нормальную форму.
   ##### 1. Первая нормальная форма
      
   исходное отношение находится в 1 НФ: в отношении нет повторяющихся групп, все атрибуты атомарны, есть ключ (StudentId, CourseId, ClubId)
   
   ```
   (StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, ClubId, ClubName, ClubStudentHeadId, Mark) => (StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, ClubId, ClubName, ClubStudentHeadId, Mark).
   ```
   ##### 2. Вторая нормальная форма
   - отношение (StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, ClubId, ClubName, ClubStudentHeadId, Mark)

      ключ (StudentId, CourseId, ClubId).
      
      неключевой атрибут Mark функционально зависит от части ключа (StudentId, CourseId) (ФЗ StudentId, CourseId → Mark). строим декомпозицию:

   - ✅ (StudentId, CourseId, Mark)
      * StudentId, CourseId → Mark.

      отношение находится во 2 НФ: ключ (StudentId, CourseId)
   
   - отношение (StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, ClubId, ClubName, ClubStudentHeadId)
      * StudentId → StudentName, GroupId, GroupName;
      * GroupId → GroupName;
      * GroupName → GroupId;
      * CourseId → CourseName;
      * LecturerId → LecturerName;
      * GroupId, CourseId → LecturerId;
      * ClubId → ClubName;
      * ClubName → ClubId, ClubStudentHeadId.

      ключ (StudentId, CourseId, ClubId).
      
      неключевой атрибут LecturerId функционально зависит от части ключа (StudentId, CourseId) (ФЗ GroupId, CourseId → LecturerId, StudentId → GroupId). строим декомпозицию:

   - ✅ отношение (GroupId, CourseId, LecturerId, LecturerName)
      * LecturerId → LecturerName;
      * GroupId, CourseId → LecturerId.

      отношение находится во 2 НФ: ключ (GroupId, CourseId)

   - отношение (StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, ClubId, ClubName, ClubStudentHeadId)
      * StudentId → StudentName, GroupId, GroupName;
      * GroupId → GroupName;
      * GroupName → GroupId;
      * CourseId → CourseName;
      * ClubId → ClubName;
      * ClubName → ClubId, ClubStudentHeadId.

      ключ (StudentId, CourseId, ClubId).

      неключевой атрибут ClubStudentHeadId функционально зависит от части ключа (StudentId, CourseId) (ФЗ ClubId → ClubStudentHeadId). строим декомпозицию:

   - ✅ отношение (ClubId, ClubName, ClubStudentHeadId)
      * ClubId → ClubName;
      * ClubName → ClubId, ClubStudentHeadId.

      отношение находится во 2 НФ: ключ (ClubId). ключ простой, поэтому все неключевые атрибуты зависят от ключа в целом

   - отношение (StudentId, StudentName, GroupId, GroupName, CourseId, CourseName)
      * StudentId → StudentName, GroupId, GroupName;
      * GroupId → GroupName;
      * GroupName → GroupId;
      * CourseId → CourseName.

      ключ (StudentId, CourseId). 

      неключевой атрибут CourseName функционально зависит от части ключа (CourseId) (ФЗ CourseId → CourseName). строим декомпозицию:

   - ✅ отношение (CourseId, CourseName)
      * CourseId → CourseName.

      отношение находится во 2 НФ: ключ (CourseId)

   - ✅ отношение (StudentId, StudentName, GroupId, GroupName)
      * StudentId → StudentName, GroupId, GroupName;
      * GroupId → GroupName;
      * GroupName → GroupId;

      отношение находится во 2 НФ: ключ (StudentId)

   ```
   (StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, ClubId, ClubName, ClubStudentHeadId, Mark) => (StudentId, StudentName, GroupId, GroupName); (CourseId, CourseName); (ClubId, ClubName, ClubStudentHeadId); (GroupId, CourseId, LecturerId, LecturerName); (StudentId, CourseId, Mark).
   ```
   ##### 3. Третья нормальная форма
   1. отношение (StudentId, StudentName, GroupId, GroupName); ключ (StudentId)
      * StudentId → StudentName, GroupId, GroupName;
      * GroupId → GroupName;
      * GroupName → GroupId.

   - неключевой атрибут GroupName зависит от ключа транзитивно (StudentId → GroupId, GroupId → GroupName). строим декомпозицию:

   - ✅ отношение (StudentId, StudentName, GroupId); ключ (StudentId)
      * StudentId → StudentName, GroupId.

      отношение находится в 3 НФ

   - ✅ отношение (GroupId, GroupName); ключ (GroupId)
      * GroupId → GroupName;
      * GroupName → GroupId.

      отношение находится в 3 НФ
   2. ✅ отношение (CourseId, CourseName); ключ (CourseId)
      * CourseId → CourseName.

      отношение находится в 3 НФ
   3. отношение (ClubId, ClubName, ClubStudentHeadId); ключ (ClubId)
      * ClubId → ClubName;
      * ClubName → ClubId, ClubStudentHeadId.

   - неключевой атрибут ClubStudentHeadId зависит от ключа транзитивно (ClubId → ClubName, ClubName → ClubStudentHeadId). строим декомпозицию:

   - ✅ отношение (ClubId, ClubName); ключ (ClubId)
      * ClubId → ClubName;
      * ClubName → ClubId.

      отношение находится в 3 НФ
   - ✅ отношение (ClubName, ClubStudentHeadId); ключ (ClubName)
      * ClubName → ClubStudentHeadId.

      отношение находится в 3 НФ
   4. отношение (GroupId, CourseId, LecturerId, LecturerName); ключ (GroupId, CourseId)
      * LecturerId → LecturerName;
      * GroupId, CourseId → LecturerId.

   - неключевой атрибут LecturerName зависит от ключа транзитивно (GroupId, CourseId → LecturerId, LecturerId → LecturerName). строим декомпозицию:
      
   - ✅ отношение (GroupId, CourseId, LecturerId); ключ (GroupId, CourseId)
      * GroupId, CourseId → LecturerId.

      отношение находится в 3 НФ
   - ✅ отношение (LecturerId, LecturerName); ключ (LecturerId)
      * LecturerId → LecturerName.

      отношение находится в 3 НФ
   5. ✅ отношение (StudentId, CourseId, Mark); ключ (StudentId, CourseId)
      * StudentId, CourseId → Mark.

      отношение находится в 3 НФ

   Результат
   ```
   (StudentId, StudentName, GroupId, GroupName) => (StudentId, StudentName, GroupId); (GroupId, GroupName).
   (CourseId, CourseName) => (CourseId, CourseName).
   (ClubId, ClubName, ClubStudentHeadId) => (ClubId, ClubName), (ClubName, ClubStudentHeadId).
   (GroupId, CourseId, LecturerId, LecturerName) => (GroupId, CourseId, LecturerId); (LecturerId, LecturerName).
   (StudentId, CourseId, Mark) => (StudentId, CourseId, Mark).
   ```
   ##### 4. Нормальная форма Бойса-Кодда
   1. отношения (GroupId, GroupName), (CourseId, CourseName), (ClubId, ClubName), (ClubName, ClubStudentHeadId), (LecturerId, LecturerName) находятся в НФБК, так как состоят из двух атрибутов 
   2. отношение (StudentId, StudentName, GroupId)
      * StudentId → StudentName, GroupId.

      ключ (StudentId) - отношение находится в НФБК 

   3. отношение (GroupId, CourseId, LecturerId)
      * GroupId, CourseId → LecturerId.

      ключ (GroupId, CourseId) - отношение находится в НФБК
   4. отношение (StudentId, CourseId, Mark)
      * StudentId, CourseId → Mark.

      ключ (StudentId, CourseId) - отношение находится в НФБК
   
   Результат (без изменений)
   ```
   (StudentId, StudentName, GroupId) => (StudentId, StudentName, GroupId).
   (GroupId, GroupName) => (GroupId, GroupName).
   (CourseId, CourseName) => (CourseId, CourseName).
   (ClubId, ClubName) => (ClubId, ClubName).
   (ClubName, ClubStudentHeadId) => (ClubName, ClubStudentHeadId).
   (GroupId, CourseId, LecturerId) => (GroupId, CourseId, LecturerId).
   (LecturerId, LecturerName) => (LecturerId, LecturerName).
   (StudentId, CourseId, Mark) => (StudentId, CourseId, Mark).
   ```
   ##### 5. Четвёртая нормальная форма
   1. теорема Дейта-Фейгина 2: если отношение находится в НФБК и существует простой ключ → отношение находится в 4НФ

      следующие отношения находятся в 4 НФ по теореме:
      1. (StudentId, StudentName, GroupId); простой ключ StudentId
      2. (GroupId, GroupName); простой ключ GroupId
      3. (CourseId, CourseName); простой ключ CourseId
      4. (ClubId, ClubName); простой ключ ClubId
      5. (ClubName, ClubStudentHeadId); простой ключ ClubName
      6. (LecturerId, LecturerName); простой ключ LecturerId
      
      приведем контрпримеры для возможных нетривиальных МЗ
      1. отношение (StudentId, StudentName, GroupId)
         - StudentId ↠ StudentName | GroupId - является ФЗ
         
         - StudentName ↠ StudentId | GroupId

            (0, "Алексей", 0), (1, "Алексей", 1). МЗ требует наличия в отношении кортежа (0, "Алексей", 1), но у студента 0 может быть только одна группа по ФЗ

         - GroupId ↠ StudentId | StudentName
            
            (0, "Алексей", 0), (1, "Александр", 0). МЗ требует наличия в отношении кортежа (1, "Алексей", 0), но у студента 1 может быть только одно имя по ФЗ

         - ∅ ↠ StudentId | StudentName, GroupId

            (0, "Алексей", 0), (1, "Александр", 1). МЗ требует наличия в отношении кортежа (0, "Александр", 1), но у студента 0 может быть только одно имя по ФЗ

         - ∅ ↠ StudentName | StudentId, GroupId

            (0, "Алексей", 0), (1, "Александр", 1). МЗ требует наличия в отношении кортежа (1, "Алексей", 1), но у студента 1 может быть только одно имя по ФЗ

         - ∅ ↠ GroupId | StudentName, StudentId

            (0, "Алексей", 0), (1, "Александр", 1). МЗ требует наличия в отношении кортежа (0, "Алексей", 1), но у студента 0 может быть только одна группа по ФЗ

   2. отношение (GroupId, CourseId, LecturerId); ФЗ GroupId, CourseId → LecturerId

      рассмотрим возможные МЗ
      - GroupId ↠ CourseId | LecturerId

         (0, 0, 0), (0, 1, 1). МЗ требует наличия в отношении кортежа (0, 0, 1), но у группы может быть только один лектор по курсу по ФЗ

      - CourseId ↠ GroupId | LecturerId

         (0, 0, 0), (1, 0, 1). МЗ требует наличия в отношении кортежа (0, 0, 1), но у группы может быть только один лектор по курсу по ФЗ

      - LecturerId ↠ GroupId | CourseId

         (0, 1, 0), (1, 0, 0). МЗ требует наличия в отношении кортежа (0, 0, 0), но лектор не должен вести курс 0 у группы 0 из-за данных исходных кортежей

      - ∅ ↠ GroupId | CourseId, LecturerId

         (0, 0, 0), (1, 1, 1). МЗ требует наличия в отношении кортежа (0, 1, 1), но 
         группа 0 не должна иметь курс 1 

      - ∅ ↠ CourseId | GroupId, LecturerId

         (0, 0, 0), (1, 1, 1). МЗ требует наличия в отношении кортежа (1, 0, 1), но группа 1 не должна иметь курс 0

      - ∅ ↠ LecturerId | GroupId, CourseId

         (0, 0, 0), (1, 1, 1). МЗ требует наличия в отношении кортежа (0, 0, 1), но у группы может быть только один лектор по курсу по ФЗ
      
      нетривиальных МЗ нет, значит отношение находится в 4 НФ

   3. отношение (StudentId, CourseId, Mark); ФЗ StudentId, CourseId → Mark

      рассмотрим возможные МЗ
      - StudentId ↠ CourseId | Mark

         (0, 0, 5), (0, 1, 4). МЗ требует наличия в отношении кортежа (0, 1, 5), но у студента может быть только одна оценка за курс по ФЗ

      - CourseId ↠ StudentId | Mark

         (0, 0, 5), (1, 0, 4). МЗ требует наличия в отношении кортежа (0, 0, 4), но у студента может быть только одна оценка за курс по ФЗ 

      - Mark ↠ StudentId | CourseId

         (0, 1, 5), (1, 0, 5). МЗ требует наличия в отношении кортежа (0, 0, 5), но у студента может быть только одна оценка за курс по ФЗ 
      
      - ∅ ↠ Mark | StudentId, CourseId
         
         (0, 0, 5), (1, 1, 4). МЗ требует наличия в отношении кортежа (0, 0, 4), но студент может не иметь оценку по курсу 

      - ∅ ↠ StudentId | CourseId, Mark

         (0, 0, 5), (1, 1, 4). МЗ требует наличия в отношении кортежа (1, 0, 5), но у студента может быть только одна оценка за курс по ФЗ
      
      - ∅ ↠ CourseId | StudentId, Mark

         (0, 0, 5), (1, 1, 4). МЗ требует наличия в отношении кортежа (0, 1, 5), но у студента может быть только одна оценка за курс по ФЗ

      нетривиальных МЗ нет, значит отношение находится в 4 НФ

   Результат (без изменений)
   ```
   (StudentId, StudentName, GroupId) => (StudentId, StudentName, GroupId).
   (GroupId, GroupName) => (GroupId, GroupName).
   (CourseId, CourseName) => (CourseId, CourseName).
   (ClubId, ClubName) => (ClubId, ClubName).
   (ClubName, ClubStudentHeadId) => (ClubName, ClubStudentHeadId).
   (GroupId, CourseId, LecturerId) => (GroupId, CourseId, LecturerId).
   (LecturerId, LecturerName) => (LecturerId, LecturerName).
   (StudentId, CourseId, Mark) => (StudentId, CourseId, Mark).
   ```
   ##### 6. Пятая нормальная форма
   1. теорема Дейта-Фейгина 1: если отношение находится в 3НФ и все ключи простые → отношение находится в 5НФ

      следующие отношения находятся в 5 НФ по теореме:
         1. (StudentId, StudentName, GroupId); ключ StudentId
         2. (GroupId, GroupName); ключи GroupId, GroupName
         3. (CourseId, CourseName); ключ CourseId
         4. (ClubId, ClubName); ключи ClubId, ClubName
         5. (ClubName, ClubStudentHeadId); ключ ClubName
         6. (LecturerId, LecturerName); ключ LecturerId
      
      приведем контрпримеры для возможных нетривиальных ЗС
      1. отношение (StudentId, StudentName, GroupId)
         - *{(StudentId), (StudentName), (GroupId)}

            (0, "Алексей", 0), (1, "Александр", 1). ЗС требует наличия в отношении кортежа (1, "Александр", 0), но у студента может быть только одна группа по ФЗ 
         - *{(StudentId, StudentName), (StudentId, GroupId)} - существует по теореме Фейгина, так как существует МЗ 

            StudentId - ключ, (StudentId, StudentName), (StudentId, GroupId) - надключи

         - *{(StudentId, StudentName), (StudentName, GroupId)}

            (0, "Алексей", 0), (1, "Алексей", 1). ЗС требует наличия в отношении кортежа (1, "Александр", 0), но у студента может быть только одна группа по ФЗ 

         - *{(StudentId, GroupId), (StudentName, GroupId)}

            (0, "Алексей", 0), (1, "Александр", 0). ЗС требует наличия в отношении кортежа (1, "Алексей", 0), но у студента может быть только одно имя по ФЗ 
      2. отношение (GroupId, GroupName)
         - *{(GroupId), (GroupName)}

            (0, "M3434"), (1, "M3437"). ЗС требует наличия в отношении кортежа (1, "M3434"), но у группы может быть только одно имя по ФЗ 
      3. отношение (CourseId, CourseName)
         - *{(CourseId), (CourseName)}

            (0, "Математический анализ"), (1, "Алгоритмы"). ЗС требует наличия в отношении кортежа (1, "Математический анализ"), но у курса может быть только одно имя по ФЗ
      4. отношение (ClubId, ClubName)
         - *{(ClubId), (ClubName)}

            (0, "Беговой клуб"), (1, "Йога"). ЗС требует наличия в отношении кортежа (0, "Йога"), но у клуба может быть только одно имя по ФЗ

      5. отношение (ClubName, ClubStudentHeadId)
         - *{(ClubName), (ClubStudentHeadId)}
         
            ("Беговой клуб", 0), ("Йога", 1). ЗС требует наличия в отношении кортежа ("Йога", 0), но у клуба может быть только один глава по ФЗ
      6. отношение (LecturerId, LecturerName)
         - *{(LecturerId), (LecturerName)}

            (0, "Алексей"), (1, "Мария"). ЗС требует наличия в отношении кортежа (1, "Алексей"), но у учителя может быть только одно имя по ФЗ 
   2. отношение (GroupId, CourseId, LecturerId); ФЗ GroupId, CourseId → LecturerId

      рассмотрим возможные нетривиальные ЗС
      - *{(GroupId), (CourseId), (LecturerId)}

         (0, 0, 0), (0, 1, 1). ЗС требует наличия в отношении кортежа (0, 0, 1), но у группы может быть только один лектор по курсу по ФЗ 

      - *{(GroupId, CourseId), (GroupId, LecturerId)}

         (0, 0, 0), (0, 1, 1). ЗС требует наличия в отношении кортежа (0, 0, 1), но у группы может быть только один лектор по курсу по ФЗ 
      - *{(GroupId, CourseId), (CourseId, LecturerId)}

         (0, 0, 0), (1, 0, 1). ЗС требует наличия в отношении кортежа (0, 0, 1), но у группы может быть только один лектор по курсу по ФЗ 
      - *{(GroupId, LecturerId), (CourseId, LecturerId)}

         (0, 0, 0), (1, 1, 0). ЗС требует наличия в отношении кортежа (0, 1, 0), но у группы 0 может не быть лектора по курсу 1 
   3. отношение (StudentId, CourseId, Mark); ФЗ StudentId, CourseId → Mark

      рассмотрим возможные нетривиальные ЗС
      - *{(StudentId), (CourseId), (Mark)}

         (0, 0, 5), (0, 1, 4). ЗС требует наличия в отношении кортежа (0, 0, 4), но у студента может быть только одна оценка по курсу по ФЗ 
      - *{(StudentId, CourseId), (StudentId, Mark)}

         (0, 0, 5), (0, 1, 4). ЗС требует наличия в отношении кортежа (0, 0, 4), но у студента может быть только одна оценка по курсу по ФЗ 
      - *{(StudentId, CourseId), (CourseId, Mark)}

         (0, 0, 5), (1, 0, 4). ЗС требует наличия в отношении кортежа (0, 0, 4), но у студента может быть только одна оценка по курсу по ФЗ 
      - *{(StudentId, Mark), (CourseId, Mark)}

         (0, 0, 5), (1, 1, 5). ЗС требует наличия в отношении кортежа (0, 1, 5), но у студента 0 может не быть оценки по курсу 1 
#### 2. Постройте соответствующую модель сущность-связь.
#### 3. Постройте соответствующую физическую модель.
#### 4. Реализуйте SQL-скрипты, создающие схему базы данных.
```sql
create table groups (
	id int not null primary key,
    name varchar(10) not null unique
);
create table students (
	id int not null primary key,
    name varchar(50) not null,
    group_id int not null,
    foreign key (group_id) references groups(id),
    unique (id, group_id)
);
create table clubs (
	id int not null primary key,
    name varchar(50) not null unique,
    student_head_id int not null,
    foreign key (student_head_id) references students(id)
);
create table students_clubs (
	student_id int not null,
    club_id int not null,
    primary key (student_id, club_id),
    foreign key (student_id) references students(id),
    foreign key (club_id) references clubs(id)
);
create table lecturers (
	id int not null primary key,
    name varchar(50) not null
);
create table courses (
	id int not null primary key,
    name varchar(50) not null
);
create table assignments (
	lecturer_id int not null,
    course_id int not null,
    group_id int not null,
    primary key (course_id, group_id),
    foreign key (lecturer_id) references lecturers(id),
    foreign key (course_id) references courses(id),
    foreign key (group_id) references groups(id)
);
create table marks (
	student_id int not null,
    course_id int not null,
    group_id int not null,
    mark int not null,
    primary key (student_id, course_id, group_id),
    foreign key (student_id, group_id) references students(id, group_id),
    foreign key (course_id, group_id) references assignments(course_id, group_id)
);
```
#### 5. Создайте базу данных по спроектированной модели.
#### 6. Заполните базу тестовыми данными.
```sql
insert into groups values 
	(0, 'M3436'),
    (1, 'M3434'),
    (2, 'M3437');
insert into students values
	(0, 'Алексей Щербаков', 0),
    (1, 'Александр Хайруллин', 0),
    (2, 'Данила Белоус', 1),
    (3, 'Анна Швалова', 1),
    (4, 'Алексей Щербаков', 0);
insert into clubs values
	(0, 'Беговой клуб', 0),
    (1, 'Фронтенд', 1),
    (2, 'Продакт менеджмент', 0);
insert into students_clubs values 
	(0, 0),
    (0, 2),
    (4, 1);
insert into lecturers values
	(0, 'Константин Кохась'),
    (1, 'Даниил Орешников'),
    (2, 'Павел Маврин'),
    (3, 'Игорь Панасюк');
insert into courses values
	(0, 'Математический анализ'),
    (1, 'Алгоритмы'),
    (2, 'Продакт менеджмент');
insert into assignments values 
	(0, 0, 0),
    (1, 1, 0),
    (2, 1, 1);
insert into marks values
	(1, 0, 0, 100),
	 (0, 0, 0, 90),
    (0, 1, 0, 67),
    (2, 1, 1, 79); 
```
