ALTER TABLE picross
DROP picross_complete_count,
DROP picross_completable,
DROP picross_attempt_count,
DROP picross_view_count,
ADD picross_complete_count int DEFAULT 0,
ADD picross_completable boolean DEFAULT false,
ADD picross_attempt_count int DEFAULT 0,
ADD picross_view_count int DEFAULT 0;
