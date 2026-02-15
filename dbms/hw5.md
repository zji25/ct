**Домашнее задание 5. Реляционная алгебра**

01.1. Информация о студентах по :StudentId
```
π{StudentId, StudentName, GroupId}(σ{StudentId = :StudentId}(Students))
```
```sql
select StudentId, StudentName, GroupId from Students where StudentId = :StudentId;
```
01.2. Информация о студентах по :StudentName
```
π{StudentId, StudentName, GroupId}(σ{StudentName = :StudentName}(Students))
```
```sql
select StudentId, StudentName, GroupId from Students where StudentName = :StudentName;
```
---
02.1. Полная информация о студентах по :StudentId
```
π{StudentId, StudentName, GroupName}(σ{StudentId = :StudentId}(Students ⋈ Groups))
```
```sql
select StudentId, StudentName, GroupName from Students natural join Groups where StudentId = :StudentId;
```
02.2. Полная информация о студентах по :StudentName
```
π{StudentId, StudentName, GroupName}(σ{StudentName = :StudentName}(Students ⋈ Groups))
```
```sql
select StudentId, StudentName, GroupName from Students natural join Groups where StudentName = :StudentName;
```
02.3. Полная информация о студентах по :GroupName
```
π{StudentId, StudentName, GroupName}(σ{GroupName = :GroupName}(Students ⋈ Groups))
```
```sql
select StudentId, StudentName, GroupName from Students natural join Groups where GroupName = :GroupName;
```
02.4. Полная информация об участниках по :ClubName
```
π{StudentId, StudentName, GroupName}(σ{ClubName = :ClubName}(Students ⋈ Groups ⋈ ClubMembers ⋈ Clubs))
```
```sql
select s.StudentId, s.StudentName, g.GroupName 
from Students as s
natural join Groups as g
inner join ClubMembers as cm on cm.StudentId = s.StudentId
inner join Clubs as c on cm.ClubId = c.ClubId
where c.ClubName = :ClubName;
```
02.5. Полная информация об участниках клуба по :StudentName руководителя
```
π{StudentId, StudentName, GroupName}(
  σ{ClubStudentHeadName = :StudentName}(
    Students ⋈ Groups ⋈ ClubMembers ⋈ Clubs ⋈ π{ClubStudentHeadId, ClubStudentHeadName}(
      ρ{ClubStudentHeadId = StudentId, ClubStudentHeadName = StudentName}(Students)
    )
  )
)
```
```sql
select s.StudentId, s.StudentName, g.GroupName 
from Students as s
natural join Groups as g
inner join ClubMembers as cm on cm.StudentId = s.StudentId
inner join Clubs as c on cm.ClubId = c.ClubId
inner join Students as s2 on s2.StudentId = c.ClubStudentHeadId
where s2.StudentName = :StudentName;
```
---
03.1. Информация о студентах с :Mark по предмету :CourseId
```
π{StudentId, StudentName, GroupId}(σ{Mark = :Mark ⋀ CourseId = :CourseId}(Students ⋈ Marks))
```
```sql
select StudentId, StudentName, GroupId from Students natural join Marks 
where Mark = :Mark and CourseId = :CourseId;
```
03.2. Информацию о студентах с :Mark по предмету :CourseName
```
π{StudentId, StudentName, GroupId}(σ{Mark = :Mark ⋀ CourseName = :CourseName}(Students ⋈ Marks ⋈ Courses))
```
```sql
select StudentId, StudentName, GroupId from Students 
natural join Marks 
natural join Courses
where Mark = :Mark and CourseName = :CourseName;
```
03.3. Информацию о студентах с :Mark по предмету :LecturerId
```
π{StudentId, StudentName, GroupId}(σ{Mark = :Mark ⋀ LecturerId = :LecturerId}(Students ⋈ Marks ⋈ Plan))
```
```sql
select StudentId, StudentName, GroupId from Students 
natural join Marks 
natural join Plan
where Mark = :Mark and LecturerId = :LecturerId;
```
03.4. Информацию о студентах с :Mark по предмету :LecturerName
```
π{StudentId, StudentName, GroupId}(
  σ{Mark = :Mark ⋀ LecturerName = :LecturerName}(Students ⋈ Marks ⋈ Plan ⋈ Lecturers)
)
```
```sql
select StudentId, StudentName, GroupId from Students 
natural join Marks 
natural join Plan
natural join Lecturers
where Mark = :Mark and LecturerName = :LecturerName;
```
03.5. Информацию о студентах с :Mark по предмету :LecturerId
```
π{StudentId, StudentName, GroupId}(
  σ{Mark = :Mark}(Students ⋈ Marks) 
  ⋈ π{CourseId}(σ{LecturerId = :LecturerId}(Plan))
)
```
```sql
select distinct StudentId, StudentName, GroupId 
from Students 
natural join Marks
natural join (select CourseId from Plan where LecturerId = :LecturerId) as Tmp
where Mark = :Mark;
```
03.6. Информацию о студентах с :Mark по предмету :LecturerName
```
π{StudentId, StudentName, GroupId}(
  σ{Mark = :Mark}(Students ⋈ Marks) 
  ⋈ π{CourseId}(σ{LecturerName = :LecturerName}(Plan ⋈ Lecturers))
)
```
```sql
select distinct StudentId, StudentName, GroupId 
from Students 
natural join Marks
natural join (select CourseId from Plan natural join Lecturers where LecturerName = :LecturerName) as Tmp
where Mark = :Mark;
```
---
04.1. Информацию о студентах не имеющих оценки по :CourseName среди всех
```
π{StudentId, StudentName, GroupId}(Students) 
∖ π{StudentId, StudentName, GroupId}(σ{CourseName = :CourseName}(Students ⋈ Marks ⋈ Courses))
```
```sql
select StudentId, StudentName, GroupId from Students 
except select StudentId, StudentName, GroupId from Students natural join Marks natural join Courses where CourseName = :CourseName;
```
04.2. Информацию о студентах не имеющих оценки по :CourseName среди всех
```
π{StudentId, StudentName, GroupId}(Students) 
∖ π{StudentId, StudentName, GroupId}(σ{CourseName = :CourseName}(Students ⋈ Marks ⋈ Courses))
```
```sql
select StudentId, StudentName, GroupId from Students 
except select StudentId, StudentName, GroupId from Students natural join Marks natural join Courses where CourseName = :CourseName;
```
04.3. Информацию об участниках :ClubName не имеющих оценки по :CourseName
```
π{StudentId, StudentName, GroupId}(σ{ClubName = :ClubName}(Students ⋈ ClubMembers ⋈ Clubs)) 
∖ π{StudentId, StudentName, GroupId}(σ{CourseName = :CourseName}(Students ⋈ Marks ⋈ Courses))
```
```sql
select StudentId, StudentName, GroupId from Students natural join ClubMembers natural join Clubs
where ClubName = :ClubName
except select StudentId, StudentName, GroupId from Students natural join Marks natural join Courses 
where CourseName = :CourseName;
```
04.4. Информацию о студентах не имеющих оценки по :CourseName среди тех, у кого он есть
```
π{StudentId, StudentName, GroupId}(σ{CourseName = :CourseName}(Students ⋈ Plan ⋈ Courses)) 
∖ π{StudentId, StudentName, GroupId}(σ{CourseName = :CourseName}(Students ⋈ Marks ⋈ Courses))
```
```sql
select StudentId, StudentName, GroupId from Students natural join Plan natural join Courses
where CourseName = :CourseName
except select StudentId, StudentName, GroupId from Students natural join Marks natural join Courses 
where CourseName = :CourseName;
```
---
05.1. ФИО студента и названия предметов которые у него есть по плану
```
π{StudentName, CourseName}(Students ⋈ Plan ⋈ Courses)
```
```sql
select min(StudentName) as StudentName, min(CourseName) as CourseName 
from Students natural join Plan natural join Courses 
group by StudentId, CourseId;
```
05.2. ФИО студента и названия предметов которые у него есть без оценки
```
π{StudentName, CourseName}(
  π{StudentId, StudentName, CourseId, CourseName}(Students ⋈ Plan ⋈ Courses) 
  ∖ π{StudentId, StudentName, CourseId, CourseName}(Students ⋈ Plan ⋈ Marks ⋈ Courses)
)
```
```sql
select StudentName, CourseName from (
  select StudentId, StudentName, CourseId, CourseName 
  from Students natural join Plan natural join Courses
  except select StudentId, StudentName, CourseId, CourseName 
  from Students natural join Plan natural join Courses natural join Marks
) as Tmp;
```
05.3. ФИО студента и названия предметов которые у него есть, но не 4 или 5
```
π{StudentName, CourseName}(
  π{StudentId, StudentName, CourseId, CourseName}(Students ⋈ Plan ⋈ Courses) 
  ∖ π{StudentId, StudentName, CourseId, CourseName}(σ{Mark = 4 ⋁ Mark = 5}(Students ⋈ Plan ⋈ Marks ⋈ Courses))
)
```
```sql
select StudentName, CourseName from (
  select StudentId, StudentName, CourseId, CourseName 
  from Students natural join Plan natural join Courses
  except select StudentId, StudentName, CourseId, CourseName 
  from Students natural join Plan natural join Courses natural join Marks
  where Mark = 4 or Mark = 5
) as Tmp;
```
05.4. ФИО студента и названия предметов которые у него вёл :LecturerName
```
π{StudentName, CourseName}(σ{LecturerName = :LecturerName}(Students ⋈ Plan ⋈ Lecturers ⋈ Courses))
```
```sql
select StudentName, CourseName
from Students natural join Plan natural join Courses natural join Lecturers
where LecturerName = :LecturerName
```
05.5. ФИО участника клуба :ClubName и названия предметов которые у него вели
```
π{StudentName, CourseName}(σ{ClubName = :ClubName}(Students ⋈ Plan ⋈ Courses ⋈ ClubMembers ⋈ Clubs))
```
```sql
select StudentName, CourseName
from Students natural join Plan natural join Courses natural join ClubMembers natural join Clubs
where ClubName = :ClubName
```
05.6. ФИО студентов участвующих в одном клубе
```
π{StudentName1, StudentName2, ClubName}(
  ρ{StudentName1=StudentName}(
    π{StudentName, StudentName2, ClubId}(
      π{StudentId, StudentName, ClubId}(Students ⋈ ClubMembers) 
      ⋈ π{StudentId2, StudentName2, ClubId}(
        ρ{StudentId2 = StudentId, StudentName2 = StudentName, GroupId2 = GroupId}(
          Students ⋈ ClubMembers
        )
      )
    )
  ) 
  ⋈ Clubs
)
```
05.7. ФИО студента и названия предметов руководителей клубов
```
π{StudentName, CourseName}(Students ⋈ Plan ⋈ Courses ⋈ ρ{StudentId=ClubStudentHeadId}(Clubs))
```
```sql
select StudentName, CourseName
from Students natural join Plan natural join Courses join Clubs on ClubStudentHeadId = StudentId
```
---
06.1. StudentId имеющих хотя бы одну оценку у :LecturerName
```
π{StudentId}(σ{LecturerName = :LecturerName}(Students ⋈ Plan ⋈ Marks ⋈ Lecturers))
```
```sql
select distinct StudentId
from Marks natural join Plan natural join Lecturers natural join Students
where LecturerName = :LecturerName;
```
06.2. StudentId не имеющих оценок у :LecturerName
```
π{StudentId}(Students) ∖ π{StudentId}(σ{LecturerName = :LecturerName}(Students ⋈ Plan ⋈ Marks ⋈ Lecturers))
```
```sql
select distinct StudentId from Students 
except select distinct StudentId
from Marks natural join Plan natural join Lecturers natural join Students
where LecturerName = :LecturerName;
```
06.3. StudentId имеющих оценки по всем предметам :LecturerName
```
π{StudentId, CourseId}(Marks) ÷ π{CourseId}(σ{LecturerName = :LecturerName}(Plan ⋈ Lecturers))
```
```sql
select StudentId from Marks 
except 
select StudentId from (
  select StudentId, S.CourseId from Marks cross join (
    select distinct CourseId from Plan natural join Lecturers where LecturerName = :LecturerName
  ) as S except select StudentId, CourseId from Marks
) as D
```
06.4. StudentId имеющих оценки по всем предметам :LecturerName, которые он у него вёл
```
π{StudentId}(σ{StudentId = StudentId2}(
  π{StudentId, CourseId}(Marks) 
  ⋇ ρ{StudentId2=StudentId}(
    π{StudentId, CourseId}(σ{LecturerName = :LecturerName}(Plan ⋈ Lecturers ⋈ Students))
  )
))
```
```sql
select distinct StudentId 
from Students natural join (
  select StudentId, S1.GroupId from Marks cross join (
    select GroupId 
    from Plan 
    natural join Lecturers
    natural join Students
    where LecturerName = :LecturerName
  ) as S1
  except select StudentId, GroupId from (
    select StudentId, S2.CourseId, S2.GroupId from Marks cross join (
      select CourseId, GroupId 
      from Plan 
      natural join Lecturers
      natural join Students
      where LecturerName = :LecturerName
    ) as S2
    except select StudentId, CourseId, GroupId from Marks natural join (
      select CourseId, GroupId 
      from Plan 
      natural join Lecturers
      natural join Students
      where LecturerName = :LecturerName
    ) as S3
  ) as S4
) as S5;
```
---
07.1. GroupId и CourseId, такие что все студенты группы имеют оценку по предмету
```
π{GroupId, CourseId}(π{StudentId, CourseId}(Marks) ⋇ π{StudentId, GroupId}(Students))
```
```sql
select distinct GroupId, CourseId from Students cross join Marks
except select distinct GroupId, CourseId from (
  select S.GroupId, M.CourseId, S.StudentId from Students as S cross join Marks as M
  except select GroupId, CourseId, StudentId from Students natural join Marks 
) as Tmp
```
07.2. GroupName и CourseName, такие что все студенты группы имеют оценку по предмету
```
π{GroupName, CourseName}((π{StudentId, CourseId}(Marks) ⋇ π{StudentId, GroupId}(Students)) ⋈ Groups ⋈ Courses)
```
```sql
select distinct GroupName, CourseName 
from (
  select distinct GroupId, CourseId from Students cross join Marks
  except select distinct GroupId, CourseId from (
    select S.GroupId, M.CourseId, S.StudentId from Students as S cross join Marks as M
    except select GroupId, CourseId, StudentId from Students natural join Marks 
  ) as Tmp
) as Tmp2 natural join Groups natural join Courses;
```
---
08.1. SumMark по :StudentId
```sql
select sum(Mark) as SumMark from Marks where StudentId = :StudentId;
```
08.2. SumMark для каждого студента (StudentName)
```sql
select StudentName, SumMark from Students as S 
left join (select StudentId, sum(Mark) as SumMark from Marks group by StudentId) as Tmp 
on S.StudentId = Tmp.StudentId;
```
08.3. SumMark студентов каждой группы (GroupName)
```sql
select GroupName, SumMark from Groups as G 
left join (select GroupId, sum(Mark) as SumMark from Marks natural join Students group by GroupId) as Tmp 
on G.GroupId = Tmp.GroupId;
```
---
09.1. AvgMark по :StudentId
```sql
select avg(cast(Mark as double precision)) as AvgMark from Marks where StudentId = :StudentId;
```
09.2. AvgMark для каждого студента (StudentName)
```sql
select StudentName, AvgMark from Students as S 
left join (
  select StudentId, avg(cast(Mark as double precision)) as AvgMark 
  from Marks group by StudentId
) as Tmp 
on S.StudentId = Tmp.StudentId;
```
09.3. AvgMark каждой группы (GroupName)
```sql
select GroupName, AvgMark from Groups as G 
left join (
  select GroupId, avg(cast(Mark as double precision)) as AvgMark 
  from Marks natural join Students group by GroupId
) as Tmp 
on G.GroupId = Tmp.GroupId;
```
09.4. AvgAvgMark студентов каждой группы (GroupName)
```sql
select GroupName, AvgAvgMark from Groups as G left join (
  select GroupId, avg(cast(AvgMark as double precision)) as AvgAvgMark
  from Students as S left join (
    select StudentId, avg(cast(Mark as double precision)) as AvgMark from Marks group by StudentId
  ) as StudentAvgMark on S.StudentId = StudentAvgMark.StudentId group by GroupId
) as GroupAvgMark on G.GroupId = GroupAvgMark.GroupId;
```
---
10. Статистика по студентам
```sql
select
  S.StudentId,
  count(distinct P.CourseId) as Total,
  count(distinct M.CourseId) as Passed,
  count(distinct P.CourseId)-count(distinct M.CourseId) as Failed
from Students S left join Plan P on P.GroupId = S.GroupId left join Marks M on S.StudentId = M.StudentId and P.CourseId = M.CourseId
group by S.StudentId
```