**Домашнее задание 2. Моделирование БД «Университет»**

Спроектируйте базу данных «Университет», позволяющую хранить информацию о студентах, группах, преподавателях, дисциплинах, оценках и клубах.

1. Составьте модель сущность-связь.
   * Нотация\
     по умолчанию атрибуты и связи обязательны; по Чену
   * Сущности и атрибуты
     - Club - студенческий клуб\
     id - уникальный идентификатор\
     name - название клуба
     - Student - студент\
     id - уникальный идентификатор\
     first_name - имя\
     last_name - фамилия
     - Group - группа\
     id - уникальный идентификатор\
     name - номер (название) группы
     - Discipline - дисциплина\
     id - уникальный идентификатор\
     name - название дисциплины
     - Teacher - учитель\
     id - уникальный идентификатор\
     first_name - имя\
     last_name - фамилия
  * Связи и ассоциации
    - ассоциации\
      Grade - оценка, которую студент получил по дисциплине\
      Assignment - назначение учителя на преподавание дисциплины в группе
    - связи\
      Student is member of Club - студент может являться членом любого количества клубов. у клуба может быть любое количество членов\
      Student is president of Club - студент может являтся президентом любого количества клубов. у клуба должен быть ровно один президент\
      Student belongs to Group - студент принадлежит ровно одной группе. В группе может быть любое кол-во студентов\
      Student got Grade in Discipline - студент может иметь любое количество оценок, у каждой оценки есть ровно один студент. По дисциплине может быть выставлено любое количество оценок,  у каждой оценки есть ровно одна дисциплина\
      Teacher teaches Discipline in Group - у дисциплины и группы может быть ровно один учитель. у учителя и группы может быть любое количество дисциплин. у учителя и дисциплины может быть любое количество групп
2. Преобразуйте модель сущность-связь в физическую модель.
  * Нотация\
    подчеркиванием обозначен основной ключ. по умолчанию столбцы обязательны
  * Домены
    - id -> int
    - group_name -> varchar(10)
    - club_name -> varchar(10)
    - discipline_name -> varchar(50)
    - name -> varchar(50)
    - grade -> int
  * Преобразование
    - сущности Student, Group, Club, Discipline, Teacher -> таблицы Students, Groups, Clubs, Disciplines, Teachers
    - ассоциация Grade -> таблица Grades c PK (Student, Club) 
    - ассоциация Assignment -> таблица Assignments c PK (Teacher, Discipline, Group)
    - связь Student is member of Club -> таблица StudentsClubs 
    - связь Student is president of Club -> ключ FK4
    - связь Student belongs to Group -> ключ FK1
3. Запишите физическую модель на языке SQL. Запись должна включать объявления ограничений.
```sql
create table `groups` (
	id int not null primary key,
    name varchar(10) not null
);
create table `students` (
	id int not null primary key,
    first_name varchar(50) not null,
    last_name varchar(50) not null,
    group_id int not null,
    foreign key (group_id) references `groups`(id)
);
create table `clubs` (
	id int not null primary key,
    name varchar(50) not null,
    president_id int not null,
    foreign key (president_id) references students(id)
);
create table `students_clubs` (
	student_id int not null,
    club_id int not null,
    primary key (student_id, club_id),
    foreign key (student_id) references students(id),
    foreign key (club_id) references clubs(id)
);
create table `teachers` (
	id int not null primary key,
    first_name varchar(50) not null,
    last_name varchar(50) not null
);
create table `disciplines` (
	id int not null primary key,
    name varchar(50) not null
);
create table `grades` (
	student_id int not null,
    discipline_id int not null,
    grade int not null,
    primary key (student_id, discipline_id),
    foreign key (student_id) references students(id),
    foreign key (discipline_id) references disciplines(id)
);
create table `assignments` (
	teacher_id int not null,
    discipline_id int not null,
    group_id int not null,
    primary key (teacher_id, discipline_id, group_id),
    foreign key (teacher_id) references teachers(id),
    foreign key (discipline_id) references disciplines(id),
    foreign key (group_id) references `groups`(id)
);
```
4. Создайте базу данных по спроектированной модели.
5. Запишите операторы SQL, заполняющие базу тестовыми данными. Достаточно 2–3 записей на таблицу, если они в полной мере демонстрируют особенности БД.
```sql
insert into `groups` values 
	(0, "M3436"),
    (1, "M3434");
insert into `students` values 
	(0, "Данила", "Белоус", 0),
    (1, "Алексей", "Щербаков", 1),
    (2, "Владислав", "Рожко", 0);
insert into `clubs` values 
	(0, "Беговой клуб", 0),
    (1, "Фронтенд", 1),
    (2, "Дизайн", 0);
insert into `students_clubs` values 
	(0, 0),
    (1, 1),
    (0, 2),
    (1, 0);   
insert into `teachers` values 
	(0, "Константин", "Кохась"),
    (1, "Даниил", "Орешников"),
    (2, "Павел", "Маврин");
insert into `disciplines` values 
	(0, "Математический анализ"),
    (1, "Алгоритмы и структуры данных");
insert into `grades` values 
	(0, 0, 90),
	(1, 0, 100),
    (1, 1, 66),
	(2, 0, 88),
    (2, 1, 78);
insert into `assignments` values 
	(0, 0, 0),
    (1, 1, 0),
    (2, 1, 1);
