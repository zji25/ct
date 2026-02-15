**Домашнее задание 9. Хранимые процедуры**

0. Подготовка
    1. Укажите название и версию СУБД, для которой вы писали хранимые процедуры

        PostgreSQL 18.0
    2. Таблицы и представления
        ```sql
        -- рейсы авиакомпании
        create table Flights (
          FlightId int not null primary key,
          FlightTime timestamp not null,
          PlaneId int not null,
          ReservationForbidden boolean default false,
          SellForbidden boolean default false
        );
        -- места в самолете
        create table Seats (
          PlaneId int not null,
          SeatNo varchar(4) not null,
          primary key (PlaneId, SeatNo)
        );
        -- пользователи авиакомпании. флаг admin позволяет пользователю изменять настройки рейса
        create table Users (
          UserId int not null primary key,
          Hash text not null,
          Admin boolean default false
        );
        create type reservation_status as enum ('reserved', 'paid');
        -- информация о бронированиях и выкупленных билетах
        create table Reservations (
          UserId int,
          FlightId int not null,
          SeatNo varchar(4) not null,
          Status reservation_status not null default 'reserved',
          ExpiresAt timestamp default now() + interval '1 day',
          constraint reservations_user_fk foreign key (UserId) references Users (UserId),
          constraint reservations_flight_fk foreign key (FlightId) references Flights (FlightId),
          unique (FlightId, SeatNo)
        );
        create function checkSeatExists() returns trigger as $$
        declare plane_id int;
        begin
        select f.PlaneId into plane_id
        from Flights f
        where f.FlightId = new.FlightId for share;
        if plane_id is null then raise exception 'flight % not found',
        new.FlightId;
        end if;
        if not exists (
          select 1
          from Seats s
          where s.PlaneId = plane_id
            and s.SeatNo = new.SeatNo
        ) then raise exception 'seat % does not exist on plane % for flight %',
        new.SeatNo,
        plane_id,
        new.FlightId;
        end if;
        return new;
        end;
        $$ language plpgsql;
        create trigger CheckSeatExists before
        insert
          or
        update of FlightId,
          SeatNo on Reservations for each row execute function checkSeatExists();
        ```
    3. Дополнительные процедуры
        ```sql
        create extension pgcrypto;
        -- функция для аутентификации пользователя. eсли checkAdmin = true, дополнительно проверяет наличие флага admin у пользователя
        create function validateUser(uid int, pass text, checkAdmin boolean) returns boolean as $$ begin return exists (
          select 1
          from Users u
          where u.UserId = uid
            and u.Hash = crypt(pass, u.Hash)
            and (
              not checkAdmin
              or u.Admin = true
            )
        );
        end;
        $$ language plpgsql;
        -- функция для регистрации пользователя. если, admin = true дополнительно проставит флаг admin у пользователя
        create function registerUser(uid int, pass text, admin boolean) returns boolean as $$ begin if validateUser(uid, pass, false) then return false;
        end if;
        insert into Users (UserId, Hash, Admin)
        values (uid, crypt(pass, gen_salt('md5')), admin);
        return true;
        end;
        $$ language plpgsql;
        -- проверяет возможность бронирования мест на рейс. возвращает true, если бронирование вохможно, иначе false
        create function reservationAllowed(fid int) returns boolean as $$ begin return exists (
          select 1
          from Flights f
          where f.FlightId = fid
            and (
              f.ReservationForbidden = false
              and (f.FlightTime - interval '3 days') > now()
            )
        );
        end;
        $$ language plpgsql;
        -- проверяет возможность продажи мест на рейс. возвращает true, если продажа вохможна, иначе false
        create function sellAllowed(fid int) returns boolean as $$ begin return exists (
          select 1
          from Flights f
          where f.FlightId = fid
            and (
              f.SellForbidden = false
              and (f.FlightTime - interval '3 hours') > now()
            )
        );
        end;
        $$ language plpgsql;
        -- проверяет наличие активного бронирования для пользователя и места на рейс
        create function reservationExists(uid int, fid int, sn varchar(4)) returns boolean as $$ begin return exists (
          select 1
          from Reservations r
          where r.UserId = uid
            and r.FlightId = fid
            and r.SeatNo = sn
            and r.Status = 'reserved'
            and r.ExpiresAt > now()
        );
        end;
        $$ language plpgsql;
        -- продлевает активное бронирование на сутки с момента продления
        create function extendExistingReservation(uid int, fid int, sn varchar(4)) returns void as $$ begin
        update Reservations
        set ExpiresAt = now() + interval '1 day'
        where UserId = uid
          and FlightId = fid
          and SeatNo = sn;
        end;
        $$ language plpgsql;
        ```
1. Администрирование
    1. RegisterUser
        ```sql
        create function RegisterUser(uid int, pass text) returns boolean as $$ begin return registerUser(uid, pass, false);
        end;
        $$ language plpgsql;
        ```
    2. ManageFlight
        ```sql
        create function ManageFlight(
          uid int,
          pass text,
          fid int,
          sa boolean,
          ra boolean
        ) returns void as $$ begin if not validateUser(uid, pass, true) then raise exception 'permission denied';
        end if;
        update Flights f
        set SellForbidden = not sa,
          ReservationForbidden = not ra
        where f.FlightId = fid;
        end;
        $$ language plpgsql;
        ```
2. Покупка и бронирование
    1. FreeSeats
        ```sql
        create function FreeSeats(fid int) returns table (SeatNo varchar(4)) as $$ begin if not reservationAllowed(fid)
        or not sellAllowed(fid) then return;
        end if;
        return query
        select s.SeatNo
        from Flights f
          natural join Seats s
        where f.FlightId = fid
        except
        select r.SeatNo
        from Reservations r
        where r.FlightId = fid
          and (
            r.Status = 'paid'
            or r.Status = 'reserved'
            and r.ExpiresAt > now()
          );
        end;
        $$ language plpgsql;
        ```
    2. Reserve
        ```sql
        create function isFree(fid int, sn varchar(4)) returns boolean as $$ begin return exists (
          select 1
          from FreeSeats(fid) f
          where f.SeatNo = sn
        );
        end;
        $$ language plpgsql;

        create function Reserve(uid int, pass text, fid int, sn varchar(4)) returns boolean as $$ begin if not validateUser(uid, pass, false)
        or not isFree(fid, sn) then return false;
        end if;
        if reservationExists(uid, fid, sn) then execute extendExistingReservation(uid, fid, sn);
        else
        insert into Reservations (UserId, FlightId, SeatNo)
        values (uid, fid, sn) on conflict (FlightId, SeatNo) do
        update
        set ExpiresAt = now() + interval '1 day';
        end if;
        return true;
        end;
        $$ language plpgsql;
        ```
    3. ExtendReservation
        ```sql
        create function ExtendReservation(uid int, pass text, fid int, sn varchar(4)) returns boolean as $$ begin if not reservationAllowed(fid)
        or not validateUser(uid, pass, false)
        or not reservationExists(uid, fid, sn) then return false;
        end if;
        execute extendExistingReservation(uid, fid, sn);
        return true;
        end;
        $$ language plpgsql;
        ```
    4. BuyFree
        ```sql
        create function BuyFree(fid int, sn varchar(4)) returns boolean as $$ begin if not sellAllowed(fid)
        or not isFree(fid, sn) then return false;
        end if;
        insert into Reservations (FlightId, SeatNo, Status)
        values (fid, sn, 'paid');
        return true;
        end;
        $$ language plpgsql;
        ```
    5. BuyReserved
        ```sql
        create function BuyReserved(uid int, pass text, fid int, sn varchar(4)) returns boolean as $$ begin if not sellAllowed(fid)
        or not validateUser(uid, pass, false)
        or not reservationExists(uid, fid, sn) then return false;
        end if;
        update Reservations
        set Status = 'paid'
        where UserId = uid
          and FlightId = fid
          and SeatNo = sn;
        return true;
        end;
        $$ language plpgsql;
        ```
3. Статистика
    1. FlightsStatistics
        ```sql
        create function FlightsStatistics(uid int, pass text) returns table(
          FlightId int,
          SellAllowed boolean,
          ReservationAllowed boolean,
          FreeSeats bigint,
          ReservedSeats bigint,
          SoldSeats bigint
        ) as $$ begin if not validateUser(uid, pass, false) then return;
        end if;
        return query
        select f.FlightId,
          sellAllowed(f.FlightId)
          and count(s.SeatNo) filter (
            where r.Status is null
              or r.Status = 'reserved'
              and (
                r.ExpiresAt <= now()
                or r.UserId = uid
              )
          ) > 0,
          reservationAllowed(f.FlightId)
          and count(s.SeatNo) filter (
            where r.Status is null
              or r.Status = 'reserved'
              and r.ExpiresAt <= now()
          ) > 0,
          count(s.SeatNo) filter (
            where r.Status is null
              or r.Status = 'reserved'
              and r.ExpiresAt <= now()
          ),
          count(s.SeatNo) filter (
            where r.UserId = uid
              and r.Status = 'reserved'
              and r.ExpiresAt > now()
          ),
          count(s.SeatNo) filter (
            where r.UserId = uid
              and r.Status = 'paid'
          )
        from Flights f
          natural join Seats s natural
          left join Reservations r
        group by f.FlightId;
        end;
        $$ language plpgsql;
        ```
    2. FlightStat
        ```sql
        create function FlightStat(uid int, pass text, fid int) returns table(
          SellAllowed boolean,
          ReservationAllowed boolean,
          FreeSeats bigint,
          ReservedSeats bigint,
          SoldSeats bigint
        ) as $$ begin return query
        select fs.SellAllowed,
          fs.ReservationAllowed,
          fs.FreeSeats,
          fs.ReservedSeats,
          fs.SoldSeats
        from FlightsStatistics(uid, pass) fs
        where FlightId = fid;
        end;
        $$ language plpgsql;
        ```
4. CompressSeats
    ```sql
    create function CompressSeats(uid int, pass text, fid int) returns void as $$
    declare sn_cursor cursor for
    select SeatNo
    from Seats
      natural join Flights
    where FlightId = fid
    order by SeatNo;
    declare current_uid int;
    declare current_sn varchar(4);
    begin if not validateUser(uid, pass, true) then raise exception 'permission denied';
    end if;
    if not exists (
      select 1
      from Flights
      where FlightId = fid
    ) then raise exception 'flight %s does not exist',
    fid;
    end if;
    create temp table TempReservations as (
      select *
      from Reservations
      where FlightId = fid
    );
    delete from Reservations
    where FlightId = fid;
    open sn_cursor;
    for current_uid in
    select UserId
    from TempReservations
    where Status = 'paid' loop fetch next
    from sn_cursor into current_sn;
    insert into Reservations (UserId, FlightId, SeatNo, Status)
    values (current_uid, fid, current_sn, 'paid');
    end loop;
    for current_uid in
    select UserId
    from TempReservations
    where Status = 'reserved'
      and ExpiresAt > now() loop fetch next
    from sn_cursor into current_sn;
    insert into Reservations (UserId, FlightId, SeatNo, Status)
    values (current_uid, fid, current_sn, 'reserved');
    end loop;
    close sn_cursor;
    end;
    $$ language plpgsql;
    ```
