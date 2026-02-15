**Домашнее задание 6. Реляционное исчисление**

1.1. Информация о студентах по :StudentName
```
Response(StudentId, StudentName, GroupId) :-
  Students(StudentId, StudentName, GroupId),
  StudentName = :StudentName.
```
```sql
select StudentId,
    StudentName,
    GroupId
from Students
where StudentName = :StudentName;
```
1.2. Информация о студентах по :GroupName
```
Response(StudentId, StudentName, GroupId) :-
  Students(StudentId, StudentName, GroupId),
  Groups(GroupId, GroupName),
  GroupName = :GroupName.
```
```sql
select StudentId,
    StudentName,
    S.GroupId
from Students as S,
    Groups as G
where GroupName = :GroupName
    and S.GroupId = G.GroupId;
```
1.3. Информация о студентах по :ClubName
```
Response(StudentId, StudentName, GroupId) :-
  Students(StudentId, StudentName, GroupId),
  ClubMembers(ClubId, StudentId),
  Clubs(ClubId, ClubName, _),
  ClubName = :ClubName.
```
```sql
select distinct S.StudentId,
    StudentName,
    GroupId
from Students as S,
    ClubMembers as CM,
    Clubs as C
where S.StudentId = CM.StudentId
    and CM.ClubId = C.ClubId
    and C.ClubName = :ClubName;
```
1.4. Информация о студентах по :Mark и :CourseId
```
Response(StudentId, StudentName, GroupId) :-
  Students(StudentId, StudentName, GroupId),
  Marks(StudentId, CourseId, Mark),
  CourseId = :CourseId,
  Mark = :Mark.
```
```sql
select distinct S.StudentId,
    StudentName,
    GroupId
from Students as S,
    Marks as M
where S.StudentId = M.StudentId
    and M.CourseId = :CourseId
    and M.Mark = :Mark;
```
1.5. Информация о студентах по :Mark и :CourseName
```
Response(StudentId, StudentName, GroupId) :-
  Students(StudentId, StudentName, GroupId),
  Marks(StudentId, CourseId, Mark),
  Courses(CourseId, CourseName),
  CourseName = :CourseName,
  Mark = :Mark.
```
```sql
select distinct S.StudentId,
    StudentName,
    GroupId
from Students as S,
    Marks as M,
    Courses as C
where S.StudentId = M.StudentId
    and M.CourseId = C.CourseId
    and C.CourseName = :CourseName
    and M.Mark = :Mark;
```
---
2.1. Полная информация о всех студентах
```
Response(StudentId, StudentName, GroupName) :-
  Students(StudentId, StudentName, GroupId),
  Groups(GroupId, GroupName).
```
```sql
select distinct StudentId,
    StudentName,
    GroupName
from Students as S,
    Groups as G
where S.GroupId = G.GroupId;
```
2.2. Полная информация о студентах по :ClubName
```
Response(StudentId, StudentName, GroupName) :-
  Students(StudentId, StudentName, GroupId),
  Groups(GroupId, GroupName),
  ClubMembers(ClubId, StudentId),
  Clubs(ClubId, ClubName, _),
  ClubName = :ClubName.
```
```sql
select distinct S.StudentId,
    StudentName,
    GroupName
from Students as S,
    Groups as G,
    ClubMembers as CM,
    Clubs as C
where S.GroupId = G.GroupId
    and S.StudentId = CM.StudentId
    and CM.ClubId = C.ClubId
    and C.ClubName = :ClubName;
```
2.3. Полная информация о студентах по :StudentName
```
Response(StudentId, StudentName, GroupName) :-
  Students(StudentId, StudentName, GroupId),
  Groups(GroupId, GroupName),
  ClubMembers(ClubId, StudentId),
  Clubs(ClubId, _, ClubStudentHeadId),
  Students(ClubStudentHeadId, ClubStudentHeadName, _),
  ClubStudentHeadName = :StudentName.
```
```sql
select distinct S.StudentId,
    S.StudentName,
    GroupName
from Students as S,
    Groups as G,
    ClubMembers as CM,
    Clubs as C,
    Students as CSH
where S.GroupId = G.GroupId
    and S.StudentId = CM.StudentId
    and CM.ClubId = C.ClubId
    and C.ClubStudentHeadId = CSH.StudentId
    and CSH.StudentName = :StudentName;
```
2.4. Полная информация о студентах, не имеющих оценки по :CourseId
```
WithMark(StudentId) :-
  Students(StudentId, _, _),
  Marks(StudentId, CourseId, _),
  CourseId = :CourseId.
Response(StudentId, StudentName, GroupName) :-
  Students(StudentId, StudentName, GroupId),
  Groups(GroupId, GroupName),
  ¬ WithMark(StudentId).
```
```sql
select distinct StudentId,
    StudentName,
    GroupName
from Students as S,
    Groups as G
where S.GroupId = G.GroupId
    and StudentId not in (
        select S.StudentId
        from Students as S,
            Marks as M
        where S.StudentId = M.StudentId
            and M.CourseId = :CourseId
    );
```
2.5. Полная информация о студентах, не имеющих оценки по :CourseName
```
WithMark(StudentId) :-
  Students(StudentId, _, _),
  Marks(StudentId, CourseId, _),
  Courses(CourseId, CourseName),
  CourseName = :CourseName.
Response(StudentId, StudentName, GroupName) :-
  Students(StudentId, StudentName, GroupId),
  Groups(GroupId, GroupName),
  ¬ WithMark(StudentId).
```
```sql
select distinct StudentId,
    StudentName,
    GroupName
from Students as S,
    Groups as G
where S.GroupId = G.GroupId
    and StudentId not in (
        select S.StudentId
        from Students as S,
            Marks as M,
            Courses as C
        where S.StudentId = M.StudentId
            and M.CourseId = C.CourseId
            and C.CourseName = :CourseName
    );
```
2.6. Полная информация о студентах, не имеющих оценки по :CourseId, у которых есть этот предмет
```
WithMark(StudentId, CourseId) :-
  Students(StudentId, _, GroupId),
  Marks(StudentId, CourseId, _),
  CourseId = :CourseId.
Response(StudentId, StudentName, GroupName) :-
  Students(StudentId, StudentName, GroupId),
  Groups(GroupId, GroupName),
  Plan(GroupId, CourseId, _),
  ¬ WithMark(StudentId, CourseId),
  CourseId = :CourseId.
```
```sql
select distinct StudentId,
    StudentName,
    GroupName
from Students as S,
    Groups as G,
    Plan as P
where S.GroupId = G.GroupId
    and G.GroupId = P.GroupId
    and P.CourseId = :CourseId
    and StudentId not in (
        select S.StudentId
        from Students as S,
            Marks as M
        where S.StudentId = M.StudentId
            and M.CourseId = :CourseId
    );
```
2.7. Полная информация о студентах, не имеющих оценки по :CourseName, у которых есть этот предмет
```
WithMark(StudentId, CourseName) :-
  Students(StudentId, _, GroupId),
  Marks(StudentId, CourseId, _),
  Courses(CourseId, CourseName),
  CourseName = :CourseName.
Response(StudentId, StudentName, GroupName) :-
  Students(StudentId, StudentName, GroupId),
  Groups(GroupId, GroupName),
  Plan(GroupId, CourseId, _),
  Courses(CourseId, CourseName),
  ¬ WithMark(StudentId, CourseName),
  CourseName = :CourseName.
```
```sql
select distinct StudentId,
    StudentName,
    GroupName
from Students as S,
    Groups as G,
    Plan as P,
    Courses as C
where S.GroupId = G.GroupId
    and G.GroupId = P.GroupId
    and P.CourseId = C.CourseId
    and C.CourseName = :CourseName
    and StudentId not in (
        select S.StudentId
        from Students as S,
            Marks as M,
            Courses as C
        where S.StudentId = M.StudentId
            and M.CourseId = C.CourseId
            and C.CourseName = :CourseName
    );
```
---
3.1. StudentId, CourseId, такие, что у студента был предмет
```
Response(StudentId, CourseId) :- Marks(StudentId, CourseId, _).
Response(StudentId, CourseId) :-
  Students(StudentId, _, GroupId),
  Plan(GroupId, CourseId, _).
```
```sql
select distinct S.StudentId,
    P.CourseId
from Students as S,
    Plan as P
where S.GroupId = P.GroupId
union
select distinct StudentId,
    CourseId
from Marks;
```
3.2. StudentName, CourseName, такие что у студента был предмет
```
Ids(StudentId, CourseId) :- Marks(StudentId, CourseId, _).
Ids(StudentId, CourseId) :-
  Students(StudentId, _, GroupId),
  Plan(GroupId, CourseId, _).
Response(StudentName, CourseName) :-
  Ids(StudentId, CourseId),
  Students(StudentId, StudentName, _),
  Courses(CourseId, CourseName).
```
```sql
select StudentName,
    CourseName
from Students as RS,
    Courses as RC,
    (
        select distinct S.StudentId,
            P.CourseId
        from Students as S,
            Plan as P
        where S.GroupId = P.GroupId
        union
        select distinct StudentId,
            CourseId
        from Marks
    ) as R
where R.StudentId = RS.StudentId
    and R.CourseId = RC.CourseId;
```
3.3. StudentName, CourseName участников клуба
```
Response(StudentName, CourseName) :-
  Students(StudentId, StudentName, GroupId),
  Plan(GroupId, CourseId, _),
  Courses(CourseId, CourseName),
  ClubMembers(ClubId, StudentId),
  Clubs(ClubId, ClubName, _),
  ClubName = :ClubName.
Response(StudentName, CourseName) :-
  Students(StudentId, StudentName, _),
  Marks(StudentId, CourseId, _),
  Courses(CourseId, CourseName),
  ClubMembers(ClubId, StudentId),
  Clubs(ClubId, ClubName, _),
  ClubName = :ClubName.
```
```sql
select StudentName, CourseName from Students as S, Courses as C, (
  select distinct S.StudentId, C.CourseId 
  from Students as S, Plan as P, Courses as C, ClubMembers as CM, Clubs as CL
  where S.GroupId = P.GroupId and S.StudentId = CM.StudentId
  and P.CourseId = C.CourseId and CM.ClubId = CL.ClubId and CL.ClubName = :ClubName
  union
  select distinct S.StudentId, C.CourseId
  from Students as S, Marks as M, Courses as C, ClubMembers as CM, Clubs as CL
  where S.StudentId = M.StudentId and S.StudentId = CM.StudentId 
  and M.CourseId = C.CourseId and CM.ClubId = CL.ClubId and CL.ClubName = :ClubName
) as R where S.StudentId = R.StudentId and C.CourseId = R.CourseId;
```
3.4. StudentName1, StudentName2, CourseName участников клуба
```
Response(StudentName1, StudentName2, CourseName) :-
  Students(StudentId1, StudentName1, GroupId1),
  Plan(GroupId1, CourseId, _),
  Students(StudentId2, StudentName2, GroupId2),
  Plan(GroupId2, CourseId, _),
  Courses(CourseId, CourseName),
  ClubMembers(ClubId, StudentId1),
  ClubMembers(ClubId, StudentId2).
```
---
4.1. StudentName, CourseName, долги по отсутствию оценки
```
WithMark(StudentId, CourseId) :- Marks(StudentId, CourseId, _).
Response(StudentName, CourseName) :-
  Students(StudentId, StudentName, GroupId),
  Plan(GroupId, CourseId, _),
  ¬ WithMark(StudentId, CourseId),
  Courses(CourseId, CourseName).
```
```sql
select StudentName,
    CourseName
from Courses as C,
    (
        select S.StudentId,
            StudentName,
            CourseId
        from Students as S,
            Plan as P
        where S.GroupId = P.GroupId
        except
        select S.StudentId,
            S.StudentName,
            M.CourseId
        from Students as S,
            Courses as C,
            Marks as M
        where S.StudentId = M.StudentId
            and C.CourseId = M.CourseId
    ) as R
where R.CourseId = C.CourseId;
```
4.2. StudentName, CourseName, долги по неудовлетворительной оценке
```
Response(StudentName, CourseName) :-
  Students(StudentId, StudentName, GroupId),
  Plan(GroupId, CourseId, _),
  Marks(StudentId, CourseId, Mark),
  Mark ≤ 2,
  Courses(CourseId, CourseName).
```
```sql
select StudentName,
    CourseName
from Courses as C,
    (
        select distinct S.StudentId,
            StudentName,
            P.CourseId
        from Students as S,
            Plan as P,
            Marks as M
        where S.GroupId = P.GroupId
            and S.StudentId = M.StudentId
            and P.CourseId = M.CourseId
            and M.Mark <= 2
    ) as R
where R.CourseId = C.CourseId;
```
4.3. StudentName, CourseName, долги по отсутствию или неудовлетворительной оценке
```
WithMark(StudentId, CourseId) :- Marks(StudentId, CourseId, _).
WithMarkBelow2(StudentId, CourseId) :- Marks(StudentId, CourseId, Mark), Mark <= 2.
Response(StudentName, CourseName) :-
  Students(StudentId, StudentName, GroupId),
  Plan(GroupId, CourseId, _),
  not WithMark(StudentId, CourseId),
  Courses(CourseId, CourseName).
Response(StudentName, CourseName) :-
  Students(StudentId, StudentName, GroupId),
  Plan(GroupId, CourseId, _),
  WithMarkBelow2(StudentId, CourseId),
  Courses(CourseId, CourseName).
```
```sql
select StudentName, CourseName from Courses as C, (
  select distinct S.StudentId, StudentName, CourseId 
  from Students as S, Plan as P 
  where S.GroupId = P.GroupId
  except select S.StudentId, S.StudentName, M.CourseId 
  from Students as S, Courses as C, Marks as M 
  where S.StudentId = M.StudentId and C.CourseId = M.CourseId
  union
  select distinct S.StudentId, StudentName, P.CourseId
  from Students as S, Plan as P, Marks as M
  where S.GroupId = P.GroupId and S.StudentId = M.StudentId 
  and P.CourseId = M.CourseId and M.Mark <= 2
) as R where R.CourseId = C.CourseId;
```
---
5.1. StudentId имеющих хотя бы одну оценку у :LecturerName
```
Response(StudentId) :-
  Students(StudentId, _, GroupId),
  Plan(GroupId, CourseId, LecturerId),
  Marks(StudentId, CourseId, _),
  Lecturers(LecturerId, LecturerName),
  LecturerName = :LecturerName.
```
```sql
select distinct S.StudentId
from Students as S,
    Plan as P,
    Marks as M,
    Lecturers as L
where S.GroupId = P.GroupId
    and P.CourseId = M.CourseId
    and M.StudentId = S.StudentId
    and P.LecturerId = L.LecturerId
    and L.LecturerName = :LecturerName;
```
5.2. StudentId не имеющих оценок у :LecturerName
```
WithMark(StudentId) :-
  Students(StudentId, _, GroupId),
  Plan(GroupId, CourseId, LecturerId),
  Marks(StudentId, CourseId, _),
  Lecturers(LecturerId, LecturerName),
  LecturerName = :LecturerName.
Response(StudentId) :-
  Students(StudentId, _, _),
  ¬ WithMark(StudentId).
```
```sql
select distinct StudentId
from Students
where StudentId not in (
        select S.StudentId
        from Students as S,
            Plan as P,
            Marks as M,
            Lecturers as L
        where S.GroupId = P.GroupId
            and P.CourseId = M.CourseId
            and M.StudentId = S.StudentId
            and P.LecturerId = L.LecturerId
            and L.LecturerName = :LecturerName
    );
```
5.3. StudentId имеющих оценки по всем предметам :LecturerName
```
LecturerCourses(CourseId) :-
  Plan(_, CourseId, LecturerId),
  Lecturers(LecturerId, LecturerName),
  LecturerName = :LecturerName.
WithMark(StudentId, CourseId) :- Marks(StudentId, CourseId, _).
StudentsWithNotAllMarks(StudentId) :-
  Students(StudentId, _, _),
  LecturerCourses(CourseId),
  ¬ WithMark(StudentId, CourseId).
Response(StudentId) :-
  Students(StudentId, _, _),
  ¬ StudentsWithNotAllMarks(StudentId).
```
```sql
select distinct StudentId
from Students
where StudentId not in (
        select S.StudentId
        from Students as S,
            (
                select CourseId
                from Plan as P,
                    Lecturers as L
                where P.LecturerId = L.LecturerId
                    and L.LecturerName = :LecturerName
            ) as C
        where not exists (
                select *
                from Marks as M
                where M.StudentId = S.StudentId
                    and M.CourseId = C.CourseId
            )
    );
```
5.4. StudentId имеющих оценки по всем предметам :LecturerName, которые он у него вёл
```
LecturerCourses(GroupId, CourseId) :-
  Plan(GroupId, CourseId, LecturerId),
  Lecturers(LecturerId, LecturerName),
  LecturerName = :LecturerName.
WithMark(StudentId, CourseId) :- Marks(StudentId, CourseId, _).
StudentsWithNotAllMarks(StudentId) :-
  Students(StudentId, _, GroupId),
  LecturerCourses(GroupId, CourseId),
  ¬ WithMark(StudentId, CourseId).
Response(StudentId) :-
  Students(StudentId, _, GroupId),
  ¬ StudentsWithNotAllMarks(StudentId).
```
```sql
select distinct StudentId
from Students
where StudentId not in (
        select S.StudentId
        from Students as S,
            (
                select P.GroupId,
                    P.CourseId
                from Plan as P,
                    Lecturers as L
                where L.LecturerId = P.LecturerId
                    and L.LecturerName = :LecturerName
            ) as LC
        where LC.GroupId = S.GroupId
            and not exists (
                select *
                from Marks as M
                where M.StudentId = S.StudentId
                    and LC.CourseId = M.CourseId
            )
    );
```
---
6.1. GroupId и CourseId, такие что все студенты группы имеют оценку по предмет
```
WithMark(StudentId, CourseId) :- Marks(StudentId, CourseId, _).
BadStudent(GroupId, CourseId) :-
  Students(StudentId, _, GroupId),
  Courses(CourseId, _),
  ¬ WithMark(StudentId, CourseId).
Response(GroupId, CourseId) :-
  Groups(GroupId, _),
  Courses(CourseId, _),
  ¬ BadStudent(GroupId, CourseId).
```
```sql
select distinct GroupId,
    CourseId
from Groups as G,
    Courses as C1
where not exists (
        select S.GroupId,
            C.CourseId
        from Students as S,
            Courses as C
        where S.GroupId = G.GroupId
            and C.CourseId = C1.CourseId
            and not exists (
                select M.StudentId,
                    M.CourseId
                from Marks as M
                where M.StudentId = S.StudentId
                    and M.CourseId = C.CourseId
            )
    );
```
6.2. GroupName и CourseName, такие, что все студенты группы имеют оценку по предмету
```
WithMark(StudentId, CourseId) :- Marks(StudentId, CourseId, _).
BadStudent(GroupId, CourseId) :-
  Students(StudentId, _, GroupId),
  Courses(CourseId, _),
  ¬ WithMark(StudentId, CourseId).
Response(GroupName, CourseName) :-
  Groups(GroupId, GroupName),
  Courses(CourseId, CourseName),
  ¬ BadStudent(GroupId, CourseId).
```
```sql
select GroupName,
    CourseName
from Groups as RG,
    Courses as RC,
    (
        select distinct GroupId,
            CourseId
        from Groups as G,
            Courses as C1
        where not exists (
                select S.GroupId,
                    C.CourseId
                from Students as S,
                    Courses as C
                where S.GroupId = G.GroupId
                    and C.CourseId = C1.CourseId
                    and not exists (
                        select M.StudentId,
                            M.CourseId
                        from Marks as M
                        where M.StudentId = S.StudentId
                            and M.CourseId = C.CourseId
                    )
            )
    ) as R
where R.GroupId = RG.GroupId
    and R.CourseId = RC.CourseId;
```