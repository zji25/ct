**Домашнее задание 3. Функциональные зависимости в БД «Университет»**

Дано отношение с атрибутами StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark, ClubId, ClubName, ClubPresidentId.

1. Найдите функциональные зависимости в данном отношении.\
StudentId -> StudentName, GroupId\
GroupId -> GroupName\
CourseId -> CourseName\
LecturerId -> LecturerName\
ClubId -> ClubName, ClubPresidentId\
ClubPresidentId -> StudentId\
GroupId, CourseId -> LecturerId\
StudentId, CourseId -> Mark 

2. Найдите все ключи данного отношения.\
   Атрибуты CourseId, ClubId образуют ключ. Покажем, что атрибуты функционально обуславливают все остальные атрибуты отношения. 
   1. ClubId -> ClubName, ClubPresidentId (дано)
   2. CourseId -> CourseName (дано)
   3. ClubId, CourseId -> ClubName, ClubPresidentId, CourseName (по правилу композиции 1 и 2)
   4. ClubId -> ClubPresidentId (по правилу расщепления 1)
   5. ClubPresidentId -> StudentId (дано)
   6. ClubId -> StudentId (правило транзитивности 4 и 5)
   7. StudentId -> StudentName, GroupId (дано)
   8. ClubId -> StudentName, GroupId (правило транзитивности 6 и 7)
   9. GroupId -> GroupName (дано)
   10. GroupId, CourseId -> LecturerId (дано)
   11. StudentId -> GroupId (по правилу расщепления 7)
   12. StudentId, CourseId -> GroupId, CourseId (по правилу дополнения 11)
   13. StudentId, CourseId -> LecturerId (по правилу транзитивности 10 и 12)
   14. StudentId, CourseId -> Mark (дано)
   15. StudentId, CourseId -> LecturerId, Mark (по правилу объединения 13 и 14)
   16. ClubPresidentId, CourseId -> StudentId, CourseId (по правилу дополнения 5)
   17. ClubPresidentId, CourseId -> LecturerId, Mark (по правилу транзитивности 15 и 16)
   18. ClubId, CourseId -> ClubPresidentId, CourseId (по правилу дополнения 4)
   19. ClubId, CourseId -> LecturerId, Mark (по правилу транзитивности 15 и 16)
   20. ClubId, CourseId -> LecturerId (по правилу расщепления 19)
   21. LecturerId -> LecturerName (дано)
   22. ClubId, CourseId -> LecturerName (по правилу транзитивности 20 и 21)
   23. ClubId, CourseId -> ClubName, ClubPresidentId, CourseName, StudentId (по правилу композиции 3 и 6)
   24. ClubId, CourseId -> ClubName, ClubPresidentId, CourseName, StudentId, StudentName, GroupId (по правилу композиции 8 и 23)
   25. ClubId, CourseId -> ClubName, ClubPresidentId, CourseName, StudentId, StudentName, GroupId, GroupName (по правилу композиции 9 и 24)
   26. ClubId, CourseId -> ClubName, ClubPresidentId, CourseName, StudentId, StudentName, GroupId, GroupName, LecturerId, Mark (по правилу композиции 19 и 25)
   27. ClubId, CourseId -> ClubName, ClubPresidentId, CourseName, StudentId, StudentName, GroupId, GroupName, LecturerId, Mark, LecturerName (по правилу композиции 22 и 26)
       
   Теперь покажем, что ни одно из подмножеств множества {CourseId, ClubId} не является ключом. На основании CourseId нельзя однозначно определить LecturerId, так как один лектор может читать несколько разных курсов, то есть CourseId не является ключом. На основании ClubId нельзя определить Mark, так как участник клуба может иметь оценки по нескольким разным предметам. Значит CourseId, ClubId является ключом.
   
   Покажем, что больше ключей нет. Докажем следующее утверждение: если атрибут не встречается ни в одной из правых частей множества нетривиальных ФЗ (обозначим это множество X), то он обязан содержаться в ключе. Пусть атрибут A принадлежит множеству X, тогда он не встречается в правых частях ни одного из нетривиальных ФЗ, значит A нельзя вывести из других атрибутов. Значит A должен содержаться во всех множествах, определяющих все атрибуты отношения. Единственные атрибуты, не встречающиеся в правых частях ФЗ: CourseId, ClubId. Значит ключ CourseId, ClubId единственный.
   
4. Найдите замыкания множеств атрибутов:
   1. GroupId, CourseId\
      GroupId, CourseId, GroupName\
      GroupId, CourseId, GroupName, LecturerId\
      GroupId, CourseId, GroupName, LecturerId, LecturerName\
      GroupId, CourseId, GroupName, LecturerId, LecturerName, CourseName
   2. StudentId, CourseId\
      StudentId, CourseId, StudentName\
      StudentId, CourseId, StudentName, CourseName\
      StudentId, CourseId, StudentName, CourseName, LecturerId\
      StudentId, CourseId, StudentName, CourseName, LecturerId, Mark\
      StudentId, CourseId, StudentName, CourseName, LecturerId, Mark, LecturerName\
      StudentId, CourseId, StudentName, CourseName, LecturerId, Mark, LecturerName, GroupId\
      StudentId, CourseId, StudentName, CourseName, LecturerId, Mark, LecturerName, GroupId, GroupName
   3. StudentId, LecturerId\
      StudentId, LecturerId, StudentName\
      StudentId, LecturerId, StudentName, GroupId\
      StudentId, LecturerId, StudentName, GroupId, GroupName\
      StudentId, LecturerId, StudentName, GroupId, GroupName, LecturerName
   4. StudentId, ClubId\
      StudentId, ClubId, StudentName\
      StudentId, ClubId, StudentName, GroupId\
      StudentId, ClubId, StudentName, GroupId, GroupName\
      StudentId, ClubId, StudentName, GroupId, GroupName, ClubName\
      StudentId, ClubId, StudentName, GroupId, GroupName, ClubName, ClubPresidentId
   5. StudentId, ClubPresidentId\
      StudentId, ClubPresidentId, StudentName\
      StudentId, ClubPresidentId, StudentName, GroupId\
      StudentId, ClubPresidentId, StudentName, GroupId, GroupName\
   6. GroupName, LecturerId\
      GroupName, LecturerId, LecturerName

5. Найдите неприводимое множество функциональных зависимостей для данного отношения.
   1. Расщепление правых частей\
StudentId -> StudentName\
StudentId -> GroupId\
GroupId -> GroupName\
CourseId -> CourseName\
LecturerId -> LecturerName\
ClubId -> ClubName\
ClubId -> ClubPresidentId\
ClubPresidentId -> StudentId\
GroupId, CourseId -> LecturerId\
StudentId, CourseId -> Mark 
   2. Удаление атрибута $X \cup${A} $&#8594; Y$
      1. Рассмотрим ФЗ GroupId, CourseId -> LecturerId\
         Пробуем удалить GroupId: строим замыкание CourseId:\
         CourseId\
         CourseId, CourseName\
         GroupId в замыкание не входит, удалить нельзя\
         Пробуем удалить CourseId: строим замыкание GroupId:\
         GroupId\
         GroupId, GroupName\
         CourseId в замыкание не входит, удалить нельзя\
      2. Рассмотрим ФЗ StudentId, CourseId -> Mark\
         Пробуем удалить StudentId: строим замыкание CourseId:\
         CourseId\
         CourseId, CourseName\
         StudentId в замыкание не входит, удалить нельзя\
         Пробуем удалить CourseId: строим замыкание StudentId:\
         StudentId\
         StudentId, StudentName\
         StudentId, StudentName, GroupId\
         StudentId, StudentName, GroupId, GroupName\
         CourseId в замыкание не входит, удалить нельзя
   3. Удаление правила $X &#8594; Y$
      можем удалить правило, если замыкание левой части на множестве правил, кроме текущего, содержит правую часть
      1. ФЗ: StudentId -> StudentName; замыкание левой части StudentId, GroupId, GroupName; не содержит StudentName, значит удалить правило нельзя
      2. ФЗ: StudentId -> GroupId; замыкание - StudentId, StudentName; не содержит GroupId
      3. ФЗ: GroupId -> GroupName; замыкание - GroupId; не содержит GroupName
      4. ФЗ: CourseId -> CourseName; замыкание - CourseId; не содержит CourseName
      5. ФЗ: LecturerId -> LecturerName; замыкание - LecturerId; не содержит LecturerName
      6. ФЗ: ClubId -> ClubName; замыкание - ClubId, ClubPresidentId; не содержит ClubName
      7. ФЗ: ClubId -> ClubPresidentId; замыкание - ClubId, ClubName; не содержит ClubPresidentId
      8. ФЗ: ClubPresidentId -> StudentId; замыкание - ClubPresidentId; не содержит StudentId
      9. ФЗ: GroupId, CourseId -> LecturerId; замыкание - GroupId, CourseId, GroupName, CourseName; не содержит LecturerId
      10. ФЗ: StudentId, CourseId -> Mark; замыкание - StudentId, CourseId, StudentName, CourseName, GroupId, GroupName; не содержит Mark
      
      Значит ни одну из ФЗ удалить нельзя, множество является неприводимым


