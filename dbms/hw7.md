**Домашнее задание 7. Изменение данных**

1.1. Удаление студентов по :GroupId
```sql
delete from Students
where GroupId = :GroupId;
```
1.2. Удаление студентов по :GroupName
```sql
delete from Students
where GroupId in (
    select GroupId
    from Groups
    where GroupName = :GroupName
  );
```
1.3. Удаление студентов по :ClubName
```sql
delete from Students
where StudentId in (
    select StudentId
    from ClubMembers
    where ClubId in (
        select ClubId
        from Clubs
        where ClubName = :ClubName
      )
  );
```
1.4. Удаление студентов без оценок
```sql
delete from Students
where StudentId not in (
    select StudentId
    from Marks
  );
```
1.5. Удаление студентов с 3+ оценками
```sql
delete from Students
where StudentId in (
    select StudentId
    from Marks
    group by StudentId
    having count(*) >= 3
  );
```
1.6. Удаление студентов с 3- оценками
```sql
delete from Students
where StudentId not in (
    select StudentId
    from Marks
    group by StudentId
    having count(*) > 3
  );
```
---
2.1. Удаление студентов с долгами
```sql
delete from Students
where StudentId in (
    select S.StudentId
    from Students as S
      natural join Plan as P
      left join Marks as M on S.StudentId = M.StudentId
      and P.CourseId = M.CourseId
    where Mark is null
  );
```
2.2. Удаление студентов c 2+ долгами
```sql
delete from Students
where StudentId in (
    select S.StudentId
    from Students as S
      natural join Plan as P
      left join Marks as M on S.StudentId = M.StudentId
      and P.CourseId = M.CourseId
    where Mark is null
    group by S.StudentId
    having count(distinct P.CourseId) >= 2
  );
```
2.3. Удаление участников клуба :ClubName c 2+ долгами
```sql
delete from Students
where StudentId in (
    select S.StudentId
    from Students as S
      natural join Plan as P
      left join Marks as M on S.StudentId = M.StudentId
      and P.CourseId = M.CourseId
    where Mark is null
    group by S.StudentId
    having count(distinct P.CourseId) >= 2
    intersect
    select S.StudentId
    from Students as S
      natural join ClubMembers as CM
      natural join Clubs as C
    where ClubName = :ClubName
  );
```
2.4. Удаление студентов c 3− долгами
```sql
delete from Students
where StudentId not in (
    select S.StudentId
    from Students as S
      natural join Plan as P
      left join Marks as M on S.StudentId = M.StudentId
      and P.CourseId = M.CourseId
    where Mark is null
    group by S.StudentId
    having count(distinct P.CourseId) > 3
  );
```
---
3.1. Изменение имени студента
```sql
update Students
set StudentName = :StudentName
where StudentId = :StudentId;
```
3.2. Перевод студента
```sql
update Students
set GroupId = :GroupId
where StudentId = :StudentId
  and GroupId = :FromGroupId;
```
3.3. Перевод всех студентов группы по :*GroupId
```sql
update Students
set GroupId = :GroupId
where GroupId = :FromGroupId;
```
3.4. Изменение имени всех студентов группы
```sql
update Students
set StudentName = :StudentName
where GroupId in (
    select GroupId
    from Groups
    where GroupName = :GroupName
  );
```
3.5. Перевод всех студентов группы по :*GroupName
```sql
update Students
set GroupId = (
    select GroupId
    from Groups
    where GroupName = :GroupName
  )
where GroupId in (
    select GroupId
    from Groups
    where GroupName = :FromGroupName
  );
```
3.6. Перевод всех студентов в существующую группу
```sql
update Students
set GroupId = (
    select GroupId
    from Groups
    where GroupName = :GroupName
  )
where GroupId in (
    select GroupId
    from Groups
    where GroupName = :FromGroupName
  )
  and exists (
    select GroupId
    from Groups
    where GroupName = :GroupName
  );
```
---
4.1. Число оценок студента
```sql
update Students
set Marks = (
    select count(*)
    from Marks
    where StudentId = :StudentId
  )
where StudentId = :StudentId;
```
4.2. Число оценок каждого студента
```sql
update Students as S
set Marks = (
    select count(*)
    from Marks as M
    where M.StudentId = S.StudentId
  );
```
4.3. Число оценок каждого участника :ClubName
```sql
update Students as S
set Marks = (
    select count(*)
    from Marks as M
    where M.StudentId = S.StudentId
  )
where S.StudentId in (
    select StudentId
    from ClubMembers
      natural join Clubs
    where ClubName = :ClubName
  );
```
4.4. Обновления числа оценок
```sql
update Students as S
set Marks = Marks + (
    select count(*)
    from NewMarks as M
    where M.StudentId = S.StudentId
  );
```
---
5.1. Число сданных дисциплин
```sql
update Students as S
set Marks = (
    select count(distinct M.CourseId)
    from Marks as M
    where M.StudentId = S.StudentId
  );
```
5.2. Число долгов одного студента
```sql
update Students
set Debts = (
    select count(*)
    from (
        select P.CourseId
        from Plan as P
          natural join Students as S
        where S.StudentId = :StudentId
        except
        select M.CourseId
        from Marks as M
        where M.StudentId = :StudentId
      ) as R
  )
where StudentId = :StudentId;
```
5.3. Число долгов каждого студента
```sql
update Students as S
set Debts = (
    select count(distinct P.CourseId)
    from Plan as P
      left join Marks as M on S.StudentId = M.StudentId
      and P.CourseId = M.CourseId
    where Mark is null
      and P.GroupId = S.GroupId
  );
```
5.4. Число долгов каждого студента группы
```sql
update Students as S
set Debts = (
    select count(distinct P.CourseId)
    from Plan as P
      left join Marks as M on S.StudentId = M.StudentId
      and P.CourseId = M.CourseId
    where Mark is null
      and P.GroupId = S.GroupId
  )
where GroupId in (
    select GroupId
    from Groups
    where GroupName = :GroupName
  );
```
5.5. Число долгов каждого участника клуба
```sql
update Students as S
set Debts = (
    select count(distinct P.CourseId)
    from Plan as P
      left join Marks as M on S.StudentId = M.StudentId
      and P.CourseId = M.CourseId
    where Mark is null
      and P.GroupId = S.GroupId
  )
where GroupId in (
    select GroupId
    from Groups
    where GroupName = :GroupName
  )
  and exists (
    select *
    from ClubMembers as CM
    where S.StudentId = CM.StudentId
  );
```
5.6. Число оценок и долгов каждого студента
```sql
update Students as S
set Marks = (
    select count(*)
    from Marks as M
    where M.StudentId = S.StudentId
  ),
  Debts = (
    select count(distinct P.CourseId)
    from Plan as P
      left join Marks as M on S.StudentId = M.StudentId
      and P.CourseId = M.CourseId
    where Mark is null
      and P.GroupId = S.GroupId
  );
```
---
6.1. Только новые оценки
```sql
insert into Marks (StudentId, CourseId, Mark)
select NM.StudentId,
  NM.CourseId,
  NM.Mark
from NewMarks as NM
  left join Marks as M on M.StudentId = NM.StudentId
  and M.CourseId = NM.CourseId
where M.Mark is null;
```
6.2. Только поверх старых оценок
```sql
update Marks as M
set Mark = (
    select NM.Mark
    from NewMarks as NM
    where M.StudentId = NM.StudentId
      and M.CourseId = NM.CourseId
  )
where exists (
    select Mark
    from NewMarks as NM
    where M.StudentId = NM.StudentId
      and M.CourseId = NM.CourseId
  );
```
6.3. Обновление старых оценок
```sql
update Marks as M
set Mark = (
    select NM.Mark
    from NewMarks as NM
    where M.StudentId = NM.StudentId
      and M.CourseId = NM.CourseId
  )
where exists (
    select Mark
    from NewMarks as NM
    where M.StudentId = NM.StudentId
      and M.CourseId = NM.CourseId
      and M.Mark < NM.Mark
  );
```
6.4. Обновление всех оценок
```sql
merge into Marks as M using NewMarks as NM on M.StudentId = NM.StudentId
and M.CourseId = NM.CourseId
when matched
and M.Mark < NM.Mark then
update
set Mark = NM.Mark
  when not matched then
insert (StudentId, CourseId, Mark)
values (NM.StudentId, NM.CourseId, NM.Mark);
```
---
7.1. Представление StudentMarks
```sql
create view StudentMarks as
select S.StudentId as StudentId,
  count(M.Mark) as Marks
from Students as S
  left join Marks as M on S.StudentId = M.StudentId
group by S.StudentId;
```
7.2. Представление AllMarks
```sql
create view AllMarks as
select S.StudentId as StudentId,
  count(Mark) as Marks
from Students as S
  left join (
    select StudentId,
      CourseId,
      Mark
    from Marks
    union all
    select StudentId,
      CourseId,
      Mark
    from NewMarks
  ) as U on U.StudentId = S.StudentId
group by S.StudentId;
```
7.3. Представление Debts
```sql
create view Debts as
select S.StudentId as StudentId,
  count(distinct P.CourseId) as Debts
from Students as S
  natural join Plan as P
  left join Marks as M on S.StudentId = M.StudentId
  and P.CourseId = M.CourseId
where Mark is null
  and P.GroupId = S.GroupId
group by S.StudentId;
```
7.4. Представление StudentDebts
```sql
create view StudentDebts as
select S1.StudentId,
  coalesce(R.Debts, 0) as Debts
from Students as S1
  left join (
    select S.StudentId as StudentId,
      count(distinct P.CourseId) as Debts
    from Students as S
      natural join Plan as P
      left join Marks as M on S.StudentId = M.StudentId
      and P.CourseId = M.CourseId
    where Mark is null
      and P.GroupId = S.GroupId
    group by S.StudentId
  ) as R on S1.StudentId = R.StudentId;
```
---
8.1. Проверка NoExtraMarks
```sql
-- PostgreSQL 18.0
create function no_extra_marks_in_marks() returns trigger as $$ begin if not exists (
  select *
  from Plan
    natural join Students
  where StudentId = NEW.StudentId
    and CourseId = NEW.CourseId
) then raise exception 'cant add mark for student and course if course not in the plan';
end if;
return new;
end;
$$ language plpgsql;

create trigger NoExtraMarksInMarks before
insert
  or
update on Marks for each row execute function no_extra_marks_in_marks();

create function no_extra_marks_in_students() returns trigger as $$ begin if exists (
  select *
  from Marks
    natural join Students
  where StudentId = OLD.StudentId
) then raise exception 'cant delete student if marks exist';
end if;
return new;
end;
$$ language plpgsql;

create trigger NoExtraMarksInStudents before delete on Students 
for each row execute function no_extra_marks_in_students();

create function no_extra_marks_in_plan() returns trigger as $$ begin if exists (
  select *
  from Marks
    natural join Students
  where GroupId = OLD.GroupId
    and CourseId = OLD.CourseId
) then raise exception 'cant delete plan for group and course if marks exist';
end if;
return new;
end;
$$ language plpgsql;

create trigger NoExtraMarksInPlan before delete on Plan
for each row execute function no_extra_marks_in_plan();
```
8.2. Проверка SameMarks
```sql
-- PostgreSQL 18.0
create function same_marks_in_students() returns trigger as $$ begin if (
  tg_op = 'INSERT'
  or tg_op = 'UPDATE'
  and old.GroupId <> new.GroupId
)
and exists (
  (
    select distinct CourseId
    from Marks
    where StudentId = new.StudentId
    except all
    select distinct CourseId
    from Students
      natural join Marks
    where GroupId = new.GroupId
  )
  union all
  (
    select distinct CourseId
    from Students
      natural join Marks
    where GroupId = new.GroupId
    except all
    select distinct CourseId
    from Marks
    where StudentId = new.StudentId
  )
) then raise exception 'cant add student to group without mark';
end if;
return new;
end;
$$ language plpgsql;
create trigger SameMarksInStudents before
insert
  or
update on Students for each row execute function same_marks_in_students();
create function same_marks_in_marks() returns trigger as $$ begin if (
  tg_op = 'INSERT'
  or tg_op = 'UPDATE'
  and (
    new.StudentId <> old.StudentId
    or new.CourseId <> old.CourseId
  )
)
and exists (
  select S.StudentId
  from Students as S
    left join Marks as M on S.StudentId = M.StudentId
    and M.CourseId = new.CourseId
  where GroupId in (
      select GroupId
      from Students
      where StudentId = new.StudentId
    )
    and Mark is null
    and S.StudentId <> new.StudentId
) then raise exception 'cant add mark for student in group';
elsif (
  tg_op = 'UPDATE'
  and (
    new.StudentId <> old.StudentId
    or new.CourseId <> old.CourseId
  )
  or tg_op = 'DELETE'
)
and (
  exists (
    select StudentId
    from Students
      natural join Marks
    where GroupId in (
        select GroupId
        from Students
        where StudentId = old.StudentId
      )
      and CourseId = old.CourseId
      and StudentId <> old.StudentId
  )
) then raise exception 'cant remove mark for student in group';
end if;
return new;
end;
$$ language plpgsql;
create trigger SameMarksInMarks before
insert
  or
update
  or delete on Marks for each row execute function same_marks_in_marks();
```
8.3. Триггер PreserveMarks
```sql
-- PostgreSQL 18.0
create function ignore_if_mark_lower() returns trigger as $$ begin 
if tg_op = 'UPDATE'
and new.StudentId = old.StudentId
and new.CourseId = old.CourseId
and new.Mark < old.Mark then return null;
elsif tg_op = 'UPDATE'
and (
  new.StudentId <> old.StudentId
  or new.CourseId <> old.CourseId
)
and (
  exists (
    select *
    from Marks
    where StudentId = new.StudentId
      and CourseId = new.CourseId
      and new.Mark < old.Mark
  )
  or exists (
    select *
    from Marks
    where StudentId = new.StudentId
      and CourseId = new.CourseId
      and (
        select Mark
        from Marks
        where StudentId = old.StudentId
          and CourseId = old.CourseId
      ) < (
        select Mark
        from Marks
        where StudentId = new.StudentId
          and CourseId = new.CourseId
      )
  )
) then return null;
elsif tg_op = 'DELETE' then return null;
end if;
return new;
end;
$$ language plpgsql;
create trigger PreserveMarks before
update
  or delete on Marks for each row execute function ignore_if_mark_lower();
```