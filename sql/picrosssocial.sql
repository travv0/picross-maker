ALTER TABLE picross
ADD user_id int,
ADD picross_complete_count int,
ADD picross_completable boolean;

UPDATE picross
SET picross_complete_count = 0,
    picross_completable = false;
